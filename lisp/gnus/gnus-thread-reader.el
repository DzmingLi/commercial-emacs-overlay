;;; gnus-thread-reader.el --- Continuous Gnus conversation view -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: news, mail

;;; Commentary:

;; Run `gnus-thread-reader-open' in a Gnus summary to read its current
;; conversation as a continuous Outline tree.  Gnus owns all article marks
;; and reply composition.  Visiting a loaded article marks that article read;
;; background loading does not mark other articles read.
;; Only the thread headers already known to the summary are included.

;;; Code:

(require 'gnus-dispatch)
(require 'gnus-reading)
(require 'gnus-backend)
(require 'gnus-backend-state)
(require 'thread-reader)
(require 'gnus-sum)
(require 'gnus-art)
(require 'gnus-msg)
(require 'gnus-cache)
(require 'mm-decode)
(require 'mm-util)
(require 'mail-parse)
(require 'nnvirtual)
(declare-function nnfeed--message-id "nnfeed" (id))
(defvar gnus-inhibit-demon)
(defvar gnus-summary-refresh--refresh-running nil)

(defvar gnus-thread-reader-summary-mode)
(defvar-local gnus-thread-reader--summary nil)
(defvar-local gnus-thread-reader--group nil)
(defvar-local gnus-thread-reader--anchor nil)
(defvar-local gnus-thread-reader--anchor-id nil)
(defvar-local gnus-thread-reader--headers nil)
(defvar-local gnus-thread-reader--sources nil)
(defvar-local gnus-thread-reader--epochs nil)
(defvar-local gnus-thread-reader--bodies nil)
(defvar-local gnus-thread-reader--raw-articles nil)
(defvar-local gnus-thread-reader--worker nil)
(defvar-local gnus-thread-reader--loading-id nil)
(defvar-local gnus-thread-reader--thread-worker nil)
(defvar gnus-thread-reader-article-loaded-functions nil
  "Functions called on the main thread with an article ID after body loading.
The current buffer is its thread reader.  This can be made buffer-local.")
(defvar-local gnus-thread-reader--article-groups nil)
(defvar-local gnus-thread-reader--queue nil)
(defvar-local gnus-thread-reader--timer nil)
(defvar-local gnus-thread-reader--generation 0)
(defvar-local gnus-thread-reader--session nil)
(defvar-local gnus-thread-reader--last-visited-id nil)
(defvar-local gnus-thread-reader-focus-ids nil
  "Optional list of article IDs to visit with `N', such as notification targets.")

(defun gnus-thread-reader--source (&optional quiet)
  "Return this view's live summary, checking its group identity."
  (unless (eq gnus-thread-reader--session gnus-newsrc-hashtb)
    (user-error "The Gnus session changed; reopen this thread"))
  (let ((summary gnus-thread-reader--summary)
        (group gnus-thread-reader--group))
    (unless (and (buffer-live-p summary)
                 (with-current-buffer summary
                   (and (derived-mode-p 'gnus-summary-mode)
                        (equal group gnus-newsgroup-name))))
      ;; A background reselect can replace the Summary buffer.
      (setq summary (and group (get-buffer (format "*Summary %s*" group))))
      (unless (and (buffer-live-p summary)
                   (with-current-buffer summary
                     (and (derived-mode-p 'gnus-summary-mode)
                          (equal group gnus-newsgroup-name))))
        (setq summary nil))
      (setq gnus-thread-reader--summary summary))
    (unless (or summary quiet)
      (user-error "The source Gnus summary has closed; reopen the thread in Gnus"))
    summary))

(defun gnus-thread-reader--header-source (header &optional group)
  "Return HEADER's original (GROUP . ARTICLE), never its virtual ordinal."
  (setq group (or group gnus-thread-reader--group))
  (if (not (string-prefix-p "nnvirtual:" (or group "")))
      (cons group (mail-header-number header))
    ;; nnvirtual puts the full component group first in Xref. This survives
    ;; both re-numbering and removal of a read article from the Summary.
    (cl-loop for field in (cdr (split-string (or (mail-header-xref header) "")))
             when (string-match "\\`\\(.*\\):\\([0-9]+\\)\\'" field)
             return (cons (match-string 1 field)
                          (string-to-number (match-string 2 field))))))

(defun gnus-thread-reader--remember-source (id header)
  "Keep ID's original group and article immutable for this reading session."
  (unless (hash-table-p gnus-thread-reader--sources)
    (setq gnus-thread-reader--sources (make-hash-table :test #'equal)))
  (or (gethash id gnus-thread-reader--sources)
      (when-let* ((source (gnus-thread-reader--header-source header)))
        (gnus-thread-reader--check-source-epoch source)
        (puthash id source gnus-thread-reader--sources))))

(defun gnus-thread-reader--check-source-epoch (source)
  "Freeze SOURCE's mailbox epoch and reject reuse of its UID after a reset."
  (when (and source (string-prefix-p "nnimap+" (car source)))
    (unless (hash-table-p gnus-thread-reader--epochs)
      (setq gnus-thread-reader--epochs (make-hash-table :test #'equal)))
    (let* ((epoch (cdr (assq 'uidvalidity (gnus-info-params (gnus-get-info (car source))))))
           (saved (gethash source gnus-thread-reader--epochs)))
      (if saved
          (unless (equal epoch (cdr saved))
            (user-error "Mailbox UIDVALIDITY changed; reopen this thread"))
        (puthash source (cons t epoch) gnus-thread-reader--epochs))
      epoch)))

(defun gnus-thread-reader--virtual-number (source)
  "Resolve SOURCE in this group's current nnvirtual mapping and verify it."
  (let ((method (gnus-find-method-for-group gnus-thread-reader--group)))
    (nnvirtual-possibly-change-server (nth 1 method))
    (let ((number (condition-case nil
                      (nnvirtual-reverse-map-article (car source) (cdr source))
                    (error nil))))
      (unless (and number
                   (equal source (nnvirtual-map-article number)))
        (user-error "Original article is no longer in this virtual group's sources"))
      number)))

(defun gnus-thread-reader--checked-header (number expected-id)
  "Resolve NUMBER's frozen source, still checking EXPECTED-ID.
The article may be absent from an unread-only Summary after it was read."
  (let ((group gnus-thread-reader--group) cached source)
    (when (hash-table-p gnus-thread-reader--headers)
      (maphash (lambda (id header)
                 (when (and (= number (mail-header-number header))
                            (equal expected-id (mail-header-id header)))
                   (setq cached header
                         source (gnus-thread-reader--remember-source id header))))
               gnus-thread-reader--headers))
    (gnus-thread-reader--check-source-epoch source)
    (when (and source (string-prefix-p "nnvirtual:" gnus-thread-reader--group))
      (setq number (gnus-thread-reader--virtual-number source)))
    (with-current-buffer (gnus-thread-reader--source)
      (let ((header (or (gnus-summary-article-header number)
                        (gnus-number-to-header number))))
        (cond
         ((and (mail-header-p header) (equal expected-id (mail-header-id header))
               (or (not source)
                   (equal source (gnus-thread-reader--header-source header group)))) header)
         ;; A cached header is safe only after resolving and verifying its
         ;; source. Never accept a different live Message-ID at that number.
         ((and (not (mail-header-p header)) cached source)
          (setq header (copy-sequence cached))
          (setf (mail-header-number header) number)
          header)
         (t (user-error "Original article identity changed in Gnus")))))))

(defun gnus-thread-reader-current-article-id ()
  "Return this view's stable article ID, independent of virtual numbering."
  (let* ((id (thread-reader-entry-id (thread-reader-current-entry)))
         (header (gethash id gnus-thread-reader--headers)))
    (unless (and header (> (mail-header-number header) 0))
      (user-error "This is a missing ancestor"))
    id))

(defun gnus-thread-reader--ensure-target ()
  "Restore the current article's native Summary row for explicit actions."
  (let* ((id (gnus-thread-reader-current-article-id))
         (cached (gethash id gnus-thread-reader--headers))
         (header (copy-sequence
                  (gnus-thread-reader--checked-header
                   (mail-header-number cached) (mail-header-id cached))))
         (number (mail-header-number header)))
    (with-current-buffer (gnus-thread-reader--source)
      (unless (gnus-summary-article-header number)
        (unless (gnus-number-to-header number)
          (push header gnus-newsgroup-headers))
        (setq gnus-newsgroup-articles
              (gnus-sorted-union gnus-newsgroup-articles (list number)))
        (gnus-dependencies-add-header header gnus-newsgroup-dependencies t)
        ;; Reinsert just this retained article using Gnus's thread machinery;
        ;; no backend retrieval or second mark database is needed.
        (save-excursion (gnus-summary-insert-subject number header t))))
    number))

(defun gnus-thread-reader--flatten (tree)
  "Return (HEADER PARENT-NUMBER) records from Gnus TREE in display order.
Subject-only synthetic roots are omitted.  Sparse headers remain placeholders.
An explicit stack and identity set also tolerate malformed cyclic trees."
  (let ((stack (list (list tree nil)))
        (seen (make-hash-table :test #'eq)) records)
    (while stack
      (pcase-let ((`(,node ,parent) (pop stack)))
        (when (and (consp node) (not (gethash node seen)))
          (puthash node t seen)
          (let ((header (car node)))
            (when (mail-header-p header)
              (push (list header parent) records)
              (setq parent (mail-header-number header)))
            (dolist (child (reverse (cdr node)))
              (push (list child parent) stack))))))
    (nreverse records)))

(defun gnus-thread-reader--decode-header (value)
  "Decode RFC 2047 header VALUE."
  (mail-decode-encoded-word-string (or value "")))

(defun gnus-thread-reader--author (header)
  "Return HEADER's display author without the Douban transport placeholder."
  (let* ((from (gnus-thread-reader--decode-header (mail-header-from header)))
         (parts (mail-extract-address-components from)))
    (if (and (string-match-p "\\`\\(?:noreply\\|[0-9]+\\)@douban\\.invalid\\'" (or (cadr parts) ""))
             (car parts) (not (string-empty-p (car parts))))
        (car parts)
      from)))

(defun gnus-thread-reader--state (number &optional id)
  "Return a display label for NUMBER's current Gnus state."
  (let* ((cached (and id (gethash id gnus-thread-reader--headers)))
         (resolved (and cached
                        (gnus-thread-reader--checked-header number (mail-header-id cached)))))
    (setq number (if resolved (mail-header-number resolved) number))
    (with-current-buffer (gnus-thread-reader--source)
      (let ((mark (gnus-summary-article-mark number)))
        (cond ((eq mark gnus-ticked-mark) "[Ticked]")
              ((eq mark gnus-dormant-mark) "[Dormant]")
              ((eq mark gnus-unread-mark) "[Unread]")
              ((eq mark gnus-expirable-mark) "[Expirable]")
              ((memq number gnus-newsgroup-marked) "[Ticked]")
              ((memq number gnus-newsgroup-dormant) "[Dormant]")
              ((memq number gnus-newsgroup-unreads) "[Unread]")
              ((memq number gnus-newsgroup-cached) "[Saved]")
              (t "[Read]"))))))

(defun gnus-thread-reader--root-header (entry)
  "Insert Article-style headers for root ENTRY, without a reply heading."
  (let* ((id (thread-reader-entry-id entry))
         (header (gethash id gnus-thread-reader--headers))
         (xref (cadr (split-string (or (mail-header-xref header) ""))))
         (source-group (and xref (string-match "\\`\\(.*\\):[0-9]+\\'" xref)
                            (gnus-group-real-name (match-string 1 xref))))
         (fields `(("From" . ,(gnus-thread-reader--author header))
                   ("Subject" . ,(gnus-thread-reader--decode-header
                                  (mail-header-subject header)))
                   ("Newsgroups" . ,(or (gethash id gnus-thread-reader--article-groups)
                                        source-group))
                   ("Date" . ,(mail-header-date header)))))
    (dolist (field fields)
      (when (cdr field)
        (let* ((text (concat (car field) ": " (thread-reader--line (cdr field))))
               (faces (cl-find-if
                       (lambda (spec) (string-match-p (car spec) text))
                       gnus-header-face-alist)))
          (insert (propertize (concat (car field) ": ") 'face (nth 1 faces))
                  (propertize (thread-reader--line (cdr field)) 'face (nth 2 faces))
                  "\n"))))
    (insert "\n")))

(defun gnus-thread-reader--render ()
  "Render the view, retaining the viewport, point and folded subtrees."
  (let ((windows
         (mapcar
          (lambda (window)
            (let* ((pos (window-start window))
                   (id (get-text-property pos 'thread-reader-id))
                   (start (and id (gethash id thread-reader--positions))))
              (list window id (if start (- pos start) 0) pos
                    (window-hscroll window) (window-vscroll window t))))
          (get-buffer-window-list (current-buffer) nil t))))
    (maphash
     (lambda (id header)
       (let* ((entry (gethash id thread-reader--entries))
              (number (mail-header-number header)))
         (setf (thread-reader-entry-author entry)
               (concat (if (> number 0) (gnus-thread-reader--state number id)
                         "[Missing]")
                       " " (gnus-thread-reader--author header)))))
     gnus-thread-reader--headers)
    (thread-reader--render)
    (let ((inhibit-read-only t))
      (maphash
       (lambda (id header)
         (let* ((address (cadr (mail-extract-address-components
				(gnus-thread-reader--decode-header (mail-header-from header)))))
                (start (gethash id thread-reader--positions))
                (number (mail-header-number header)))
           (when (and start (> number 0)
                      (thread-reader-entry-parent-id
                       (gethash id thread-reader--entries))
                      (equal (gnus-thread-reader--state number id) "[Read]"))
             (save-excursion
               (goto-char start)
               (add-face-text-property start (line-end-position) 'shadow)))
           (when (and start (string-match "\\`\\([0-9]+\\)@douban\\.invalid\\'" (or address "")))
             (let ((url (concat "https://www.douban.com/people/" (match-string 1 address) "/"))
                   (name (gnus-thread-reader--author header)))
               (save-excursion
                 (goto-char start)
                 (when (search-forward name (line-end-position) t)
                   (make-text-button (- (point) (length name)) (point)
                                     'face 'link 'help-echo url 'follow-link t
                                     'action (lambda (_button) (browse-url url)))))))))
       gnus-thread-reader--headers))
    (dolist (state windows)
      (pcase-let ((`(,window ,id ,offset ,old ,hscroll ,vscroll) state))
        (when (window-live-p window)
          (let* ((start (and id (gethash id thread-reader--positions)))
                 (pos (min (point-max) (if start (+ start offset) old))))
            (when (or (invisible-p pos)
                      (and start (not (equal id (get-text-property pos 'thread-reader-id)))))
              (setq pos (or start old)))
            (set-window-start window pos t)
            (set-window-hscroll window hscroll)
            (set-window-vscroll window vscroll t)))))
    (setq header-line-format
          (format "Gnus · %d articles · %d loading   d read · U unread · ! tick · N next unread · r reply · o original"
                  (hash-table-count gnus-thread-reader--headers)
                  (length gnus-thread-reader--queue)))))

(defun gnus-thread-reader--cancel ()
  "Cancel pending body loading and invalidate old work."
  (cl-incf gnus-thread-reader--generation)
  (when (timerp gnus-thread-reader--timer)
    (cancel-timer gnus-thread-reader--timer))
  (dolist (worker (list gnus-thread-reader--worker gnus-thread-reader--thread-worker))
    (when (gnus-dispatch-job-p worker) (gnus-dispatch-job-cancel worker)))
  (setq gnus-thread-reader--timer nil
        gnus-thread-reader--worker nil
        gnus-thread-reader--loading-id nil
        gnus-thread-reader--thread-worker nil))

(defun gnus-thread-reader--schedule ()
  "Schedule one body request, yielding between articles."
  (when (and gnus-thread-reader--queue (not gnus-thread-reader--timer)
             (not (gnus-dispatch-job-pending-p gnus-thread-reader--worker)))
    (setq gnus-thread-reader--timer
          (run-at-time 0.05 nil #'gnus-dispatch-submit
                       #'gnus-thread-reader--load-one
                       (current-buffer) gnus-thread-reader--generation))))

(defun gnus-thread-reader--mime-parts (handle)
  "Extract safe inline text parts from MIME HANDLE as (FORMAT . TEXT).
Alternative bodies are shown once, preferring HTML.  Attachments, signatures
and encrypted data are left to Gnus's native article viewer."
  (let ((type (mm-handle-media-type handle)))
    (cond
     ((equal type "multipart/encrypted") nil)
     ((stringp (car handle))
      (let ((parts (cdr handle)))
        (cond
         ((equal type "multipart/alternative")
          (let ((alternatives (mapcar #'gnus-thread-reader--mime-parts parts)))
            (or (cl-find-if (lambda (items) (assq 'html items)) alternatives)
                (cl-find-if #'identity alternatives))))
         ((member type '("multipart/related" "multipart/signed"))
          (gnus-thread-reader--mime-parts (car parts)))
         (t (mapcan #'gnus-thread-reader--mime-parts parts)))))
     ((and (member type '("text/plain" "text/html"))
           (not (equal (car (mm-handle-disposition handle)) "attachment")))
      (let* ((charset (mail-content-type-get (mm-handle-type handle) 'charset))
             (decoded (or (eq charset 'gnus-decoded)
                          (equal charset "gnus-decoded")))
             (coding (or (mm-charset-to-coding-system charset) 'utf-8))
             (bytes (if decoded
                        (with-current-buffer (mm-handle-buffer handle)
                          (buffer-string))
                      (mm-get-part handle))))
        (list (cons (if (equal type "text/html") 'html 'plain)
                    (if decoded bytes
                      (decode-coding-string bytes coding)))))))))

(defun gnus-thread-reader--body-from-buffer ()
  "Decode the current raw message, returning (FORMAT . TEXT).
Never run an attachment viewer or fetch remote resources."
  (let ((mm-decrypt-option 'never) (mm-verify-option 'never) handles)
    (unwind-protect
        (let* ((parts (gnus-thread-reader--mime-parts
                       (setq handles (mm-dissect-buffer t))))
               (html (assq 'html parts)))
          (cond
           ((null parts) '(plain . "[Open the original article with o to view this MIME content.]"))
           ((not html) (cons 'plain (mapconcat #'cdr parts "\n\n")))
           (t (cons 'html
                    (mapconcat
                     (lambda (part)
                       (if (eq (car part) 'html) (cdr part)
                         (concat "<p>"
                                 (replace-regexp-in-string
                                  "\n" "<br>" (xml-escape-string (cdr part)))
                                 "</p>"))) parts "\n")))))
      (when handles (mm-destroy-parts handles)))))

(defun gnus-thread-reader--fetch (header &optional raw)
  "Fetch HEADER through Gnus, including its cache, without reading it."
  (let* ((virtual (string-prefix-p "nnvirtual:" (or gnus-thread-reader--group "")))
         (source (gnus-thread-reader--header-source header))
         (number (or (cdr source) (mail-header-number header)))
         (article-groups gnus-thread-reader--article-groups)
         ;; A virtual Summary's records and ordinals can change while I/O
         ;; yields. Fetch the immutable component address directly instead.
         (gnus-summary-buffer (unless virtual
                                (gnus-thread-reader--source)))
         (gnus-newsgroup-name (or (car source) gnus-thread-reader--group)))
    (unless virtual
      (gnus-thread-reader--checked-header (mail-header-number header) (mail-header-id header)))
    (with-temp-buffer
      (let ((nntp-server-buffer (current-buffer)))
        (unless (gnus-request-article-this-buffer number gnus-newsgroup-name)
          (error "Article unavailable from Gnus")))
      (let ((actual-id (mail-fetch-field "message-id")))
        ;; An older Gnus backlog/agent cache can still contain nnatom's
        ;; malformed legacy ID. Canonicalize that exact ID using the backend's
        ;; stable mapping; a different source ID still fails the comparison.
        (when (and actual-id (fboundp 'nnfeed--message-id)
                   (eq (car (gnus-find-method-for-group gnus-newsgroup-name)) 'nnatom))
          (let ((canonical (nnfeed--message-id actual-id)))
            (unless (equal canonical actual-id)
              (setq actual-id canonical)
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward "^Message-ID:.*$" (save-excursion (search-forward "\n\n" nil t)) t)
                  (replace-match (concat "Message-ID: " canonical) t t))))))
        ;; Ordinary Gnus summaries can intentionally rename duplicate IDs.
        ;; Virtual summaries preserve source IDs, so verify those on the wire.
        (when (and virtual actual-id (not (equal actual-id (mail-header-id header))))
          (error "Fetched article has a different Message-ID from its frozen source")))
      (let ((group (mail-fetch-field "newsgroups"))
            (message (and raw (buffer-string)))
            (body (gnus-thread-reader--body-from-buffer)))
        (if raw (list body group message)
          (when (hash-table-p article-groups)
            (puthash (number-to-string (mail-header-number header)) group article-groups))
          body)))))

(defun gnus-thread-reader--mark-current-read ()
  "Mark the loaded article at point read once when it is visited.
Prefetched articles and a manual unread mark on the same article are left alone."
  (when-let* ((id (thread-reader--current-id))
              ((not (equal id gnus-thread-reader--last-visited-id)))
              ((gethash id gnus-thread-reader--bodies))
              (header (gethash id gnus-thread-reader--headers))
              (number (mail-header-number header))
              ((> number 0)))
    (let ((gnus-summary-refresh--refresh-running t))
      (setq number (mail-header-number
                    (gnus-thread-reader--checked-header number (mail-header-id header))))
      (setq gnus-thread-reader--last-visited-id id)
      (with-current-buffer (gnus-thread-reader--source)
        (when (eq (gnus-summary-article-mark number) gnus-unread-mark)
          (gnus-summary-mark-article number gnus-read-mark)
          (gnus-set-mode-line 'summary)
          t)))))

(defun gnus-thread-reader--visit-at-point ()
  "Update Gnus read state after moving to a loaded article."
  (when (and (derived-mode-p 'gnus-thread-reader-mode)
             (gnus-thread-reader--mark-current-read))
    (gnus-thread-reader--render)))

(defun gnus-thread-reader-article-snapshot (&optional id)
  "Return the selected article's public, immutable renderer input.
Renderers must not use Summary ordinals or inspect private reader tables."
  (setq id (or id (gnus-thread-reader-current-article-id)))
  (let* ((header (gethash id gnus-thread-reader--headers))
         (source (gnus-thread-reader--header-source header)))
    (list :id id :group (car source) :article (cdr source)
          :epoch (gnus-thread-reader--check-source-epoch source)
          :message-id (mail-header-id header)
          :raw (gethash id gnus-thread-reader--raw-articles))))

(defun gnus-thread-reader--valid-request-p (buffer generation)
  "Whether BUFFER still owns the request from GENERATION."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'gnus-thread-reader-mode)
              (eq gnus-thread-reader--session gnus-newsrc-hashtb)
              (= generation gnus-thread-reader--generation)))))

(defun gnus-thread-reader--fail-body (id failure)
  "Replace ID's loading placeholder, including failures during publication."
  (when-let* ((entry (gethash id thread-reader--entries)))
    (setf (thread-reader-entry-body-format entry) 'plain
          (thread-reader-entry-body entry)
          (format "[Loading failed: %s. Press g to retry.]" failure)))
  (setq gnus-thread-reader--worker nil gnus-thread-reader--loading-id nil)
  (gnus-thread-reader--render)
  (gnus-thread-reader--schedule))

(defun gnus-thread-reader--load-one (buffer generation)
  "Start a body request, turning setup errors into a terminal view state."
  (gnus-dispatch-assert-main)
  (when (gnus-thread-reader--valid-request-p buffer generation)
    (with-current-buffer buffer
      (condition-case err
          (gnus-thread-reader--start-one buffer generation)
        ((error quit)
         (gnus-thread-reader--fail-body gnus-thread-reader--loading-id
                                       (error-message-string err)))))))

(defun gnus-thread-reader--start-one (buffer generation)
  "Start one request owned by BUFFER and GENERATION."
  (gnus-dispatch-assert-main)
  (when (gnus-thread-reader--valid-request-p buffer generation)
    (with-current-buffer buffer
      (setq gnus-thread-reader--timer nil)
      (when-let* ((id (pop gnus-thread-reader--queue))
                  (header (copy-sequence (gethash id gnus-thread-reader--headers))))
        (setq gnus-thread-reader--loading-id id)
        (let* ((group gnus-thread-reader--group)
               (source (gnus-thread-reader--header-source header))
               (method (gnus-find-method-for-group (car source)))
               (epoch (gnus-thread-reader--check-source-epoch source))
               (summary gnus-thread-reader--summary)
               (session gnus-thread-reader--session)
               (original gnus-original-article-buffer)
               (valid (lambda () (gnus-thread-reader--valid-request-p buffer generation)))
               (success (lambda (result)
                          (gnus-thread-reader--publish-body buffer generation id header result nil)))
               (failure (lambda (reason)
                          (with-current-buffer buffer (gnus-thread-reader--fail-body id reason)))))
          (setq gnus-thread-reader--loading-id id
                gnus-thread-reader--worker
                (if (gnus-backend-process-p method)
                    (gnus-backend-request
                     method 'article (list :group (car source) :article (cdr source) :epoch epoch)
                     (lambda (raw)
                       (unless (equal epoch (cdr (assq 'uidvalidity
                                                      (gnus-info-params (gnus-get-info (car source))))))
                         (error "Mailbox identity changed during retrieval"))
                       (with-temp-buffer
                         (insert raw)
                         (unless (equal (mail-fetch-field "message-id") (mail-header-id header))
                           (error "Fetched article has a different Message-ID"))
                         (funcall success (list (gnus-thread-reader--body-from-buffer)
                                                (mail-fetch-field "newsgroups") raw))))
                     failure :valid valid :priority t)
                  (gnus-dispatch-job-start
                   (lambda ()
                     (let ((gnus-summary-buffer summary) (gnus-newsgroup-name group)
                           (gnus-newsrc-hashtb session)
                           (gnus-original-article-buffer original)
                           (gnus-inhibit-demon t) (inhibit-debugger t)
                           (debug-on-error nil) (debug-on-quit nil))
                       (with-current-buffer buffer (gnus-thread-reader--fetch header t))))
                   success failure :valid valid
                   :name (format "gnus-thread-reader-%s-%s" group id)))))))))

(defun gnus-thread-reader--publish-body (buffer generation id header result failure)
  "Publish a background body on the main thread, discarding stale results."
  (gnus-dispatch-assert-main)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'gnus-thread-reader-mode)
                 (= generation gnus-thread-reader--generation)
                 (equal (mail-header-id header)
                        (mail-header-id (gethash id gnus-thread-reader--headers))))
        (setq gnus-thread-reader--worker nil gnus-thread-reader--loading-id nil)
        (condition-case err
            (progn
              (gnus-thread-reader--source)
              (gnus-thread-reader--checked-header (mail-header-number header) (mail-header-id header))
              (let ((entry (gethash id thread-reader--entries)) (body (car result)))
                (unless failure
                  (puthash id body gnus-thread-reader--bodies)
                  (puthash id (nth 1 result) gnus-thread-reader--article-groups)
                  (puthash id (nth 2 result) gnus-thread-reader--raw-articles))
                (setf (thread-reader-entry-body-format entry) (if failure 'plain (car body))
                      (thread-reader-entry-body entry)
                      (if failure (format "[Loading failed: %s. Press g to retry.]" failure)
                        (cdr body)))
                (when (and (not failure) (get-buffer-window buffer t))
                  (gnus-thread-reader--mark-current-read))
                (gnus-thread-reader--render)
                (unless failure
                  (run-hook-with-args 'gnus-thread-reader-article-loaded-functions id))
                (gnus-thread-reader--schedule)))
          (error (gnus-thread-reader--fail-body id (error-message-string err))))))))

(defun gnus-thread-reader--complete-thread (buffer header)
  "Request missing headers with a session-owned job."
  (with-current-buffer buffer
    (let ((generation gnus-thread-reader--generation)
          (group gnus-thread-reader--group)
          (summary gnus-thread-reader--summary))
      (setq gnus-thread-reader--thread-worker
            (gnus-dispatch-job-start
             (lambda ()
               (let ((gnus-summary-buffer summary) (gnus-newsgroup-name group)
                     (gnus-inhibit-demon t) (inhibit-debugger t)
                     (debug-on-error nil) (debug-on-quit nil))
                 (gnus-request-thread header group)))
             (lambda (headers)
               (gnus-thread-reader--publish-thread buffer generation header headers nil))
             (lambda (failure)
               (gnus-thread-reader--publish-thread buffer generation header nil failure))
             :valid (lambda () (gnus-thread-reader--valid-request-p buffer generation))
             :name (concat "gnus-thread-reader-headers-" group))))))

(defun gnus-thread-reader--publish-thread (buffer generation anchor headers failure)
  "Merge background HEADERS using Gnus's native summary logic on the main thread."
  (gnus-dispatch-assert-main)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (= generation gnus-thread-reader--generation)
        (setq gnus-thread-reader--thread-worker nil)
        (if failure (message "Gnus thread headers: %s" failure)
          (when headers
            (condition-case err
                (let ((summary (gnus-thread-reader--source))
                      (number (mail-header-number anchor)))
                  (gnus-thread-reader--checked-header number (mail-header-id anchor))
                  (save-window-excursion
                    (with-current-buffer summary
                      (save-excursion
                        (gnus-summary-goto-subject number t)
                        (cl-letf (((symbol-function 'gnus-request-thread)
                                   (lambda (&rest _) headers)))
                          (gnus-summary-refer-thread nil))
                        (when gnus-thread-reader-summary-mode
                          (gnus-summary-maybe-hide-threads)))))
                  (gnus-thread-reader-refresh))
              (error (message "Gnus thread headers: %s" (error-message-string err))))))))))

(defun gnus-thread-reader-refresh (&optional refetch &rest _)
  "Reconcile the view with its Gnus summary, retrying failed bodies.
With prefix REFETCH, reload all bodies.  This does not rescan the server;
use Gnus's normal update commands and then refresh or reopen this view."
  (interactive "P")
  (let* ((anchor-id gnus-thread-reader--anchor-id)
         (header (gnus-thread-reader--checked-header gnus-thread-reader--anchor anchor-id))
         (tree (with-current-buffer (gnus-thread-reader--source)
                 (or (gnus-remove-thread anchor-id t)
                     (list header))))
         (records (gnus-thread-reader--flatten tree))
         (headers (make-hash-table :test #'equal))
         (keys (make-hash-table :test #'equal))
         (old-ids (make-hash-table :test #'equal))
         (number-ids (make-hash-table :test #'eql)) entries queue)
    (cl-labels ((key (h) (list (gnus-thread-reader--header-source h) (mail-header-id h))))
      (maphash (lambda (id h) (puthash (key h) id old-ids)) gnus-thread-reader--headers)
      (dolist (record records) (puthash (key (car record)) t keys))
      ;; The unread Summary no longer owns the full discussion. Retain read
      ;; roots/replies and their parent links, merging any newly known headers.
      (dolist (id thread-reader--order)
        (let* ((old (gethash id gnus-thread-reader--headers))
               (entry (gethash id thread-reader--entries))
               (parent (and entry (gethash (thread-reader-entry-parent-id entry)
                                           gnus-thread-reader--headers))))
          (when (and old (not (gethash (key old) keys)))
            (push (list (if (> (mail-header-number old) 0)
                            (gnus-thread-reader--checked-header
                             (mail-header-number old) (mail-header-id old)) old)
                        (when parent
                          (mail-header-number
                           (if (> (mail-header-number parent) 0)
                               (gnus-thread-reader--checked-header
                                (mail-header-number parent) (mail-header-id parent)) parent))))
                  records)
            (puthash (key old) t keys))))
      ;; Entry IDs, cached MIME, focus IDs and fold state stay fixed when the
      ;; *current* virtual number is reassigned to a different source article.
      (dolist (record records)
        (let* ((h (car record)) (number (mail-header-number h))
               (identity (key h))
               (id (or (gethash identity old-ids) (number-to-string number))))
          (when (and (gethash id gnus-thread-reader--headers)
                     (not (equal identity (key (gethash id gnus-thread-reader--headers)))))
            (setq id (format "%s:%s:%s" number (car (car identity)) (cadr identity))))
          (puthash number id number-ids))))
    (gnus-thread-reader--cancel)
    (when refetch
      (clrhash gnus-thread-reader--bodies)
      (clrhash gnus-thread-reader--raw-articles))
    (dolist (record records)
      (pcase-let* ((`(,item ,parent) record)
                   (number (mail-header-number item))
                   (id (gethash number number-ids))
                   (previous (gethash id gnus-thread-reader--headers))
                   (body (and (or (null previous)
                                  (equal (mail-header-id previous) (mail-header-id item)))
                              (gethash id gnus-thread-reader--bodies))))
        (when (and previous (not (equal (mail-header-id previous) (mail-header-id item))))
          (remhash id gnus-thread-reader--bodies)
          (remhash id gnus-thread-reader--raw-articles))
        (puthash id (copy-sequence item) headers)
        (gnus-thread-reader--remember-source id item)
        (when (and (> number 0) (not body)) (push id queue))
        (push (make-thread-reader-entry
               :id id :parent-id (and parent (gethash parent number-ids))
               :author (gnus-thread-reader--author item)
               :time (or (mail-header-date item) "")
               :placeholder-p (<= number 0)
               :body-format (or (car body) 'plain)
               :body (or (cdr body) (if (> number 0) "Loading article…"
                                      "[Ancestor not available in this Gnus summary.]")))
              entries)))
    (thread-reader--merge (nreverse entries) t)
    (setq gnus-thread-reader--headers headers
          gnus-thread-reader--anchor (mail-header-number header)
          gnus-thread-reader--queue (nreverse queue)
          thread-reader--discussion
          (make-thread-reader-discussion
           :id gnus-thread-reader--anchor-id
           :title (gnus-thread-reader--decode-header (mail-header-subject header))))
    (gnus-thread-reader--render)
    (gnus-thread-reader--schedule)))

(defun gnus-thread-reader--target ()
  "Return this article's current Summary number after verifying its source."
  (let* ((id (thread-reader-entry-id (thread-reader-current-entry)))
         (header (gethash id gnus-thread-reader--headers))
         (number (and header (mail-header-number header))))
    (unless (and number (> number 0)) (user-error "This is a missing ancestor"))
    (mail-header-number
     (gnus-thread-reader--checked-header number (mail-header-id header)))))

(defun gnus-thread-reader-raw-article (id)
  "Return already loaded raw MIME for ID without fetching or marking it."
  (when (hash-table-p gnus-thread-reader--raw-articles)
    (gethash id gnus-thread-reader--raw-articles)))

(defun gnus-thread-reader-request-body (id)
  "Prioritize background loading of ID and return immediately.
Completion runs `gnus-thread-reader-article-loaded-functions'
on the main thread."
  (unless (gethash id gnus-thread-reader--headers) (user-error "Unknown thread article"))
  (unless (hash-table-p gnus-thread-reader--raw-articles)
    (setq gnus-thread-reader--raw-articles (make-hash-table :test #'equal)))
  (unless (or (gnus-thread-reader-raw-article id)
              (and (equal id gnus-thread-reader--loading-id)
                   (gnus-dispatch-job-pending-p gnus-thread-reader--worker)))
    (setq gnus-thread-reader--queue (cons id (delete id gnus-thread-reader--queue)))
    (gnus-thread-reader--schedule)))

(defun gnus-thread-reader--mark (mark)
  "Apply Gnus MARK to the current article, without touching other articles."
  (let* ((gnus-summary-refresh--refresh-running t)
         (number (gnus-thread-reader--ensure-target)))
    (save-window-excursion
      (with-current-buffer (gnus-thread-reader--source)
        (save-excursion
          (gnus-summary-mark-article number mark)
          (setq mark (gnus-summary-article-mark number))
          (unless (characterp mark)
            (user-error "Gnus could not restore the original article's mark"))
          (gnus-set-mode-line 'summary))))
    ;; Restored rows are absent from the last refresh baseline.  Persist
    ;; this explicit action before a new unread-only projection drops it.
    (let* ((id (gnus-thread-reader-current-article-id))
           (source (gnus-thread-reader--remember-source
                    id (gethash id gnus-thread-reader--headers)))
           (info (and source (gnus-get-info (car source)))))
      (when info (gnus-backend-state--apply-mark info (cdr source) mark)))
    (gnus-thread-reader--render)))

(defun gnus-thread-reader-mark-read ()
  "Mark only this article read in Gnus."
  (interactive) (gnus-thread-reader--mark gnus-read-mark))

(defun gnus-thread-reader-mark-unread ()
  "Mark only this article unread in Gnus."
  (interactive) (gnus-thread-reader--mark gnus-unread-mark))

(defun gnus-thread-reader-tick ()
  "Keep this article for later using Gnus's ticked mark."
  (interactive) (gnus-thread-reader--mark gnus-ticked-mark))

(defun gnus-thread-reader-dormant ()
  "Make this article dormant in Gnus."
  (interactive) (gnus-thread-reader--mark gnus-dormant-mark))

(defun gnus-thread-reader--reveal (id)
  "Reveal ID and its ancestors, updating the saved fold state."
  (let ((parent (thread-reader-entry-parent-id (gethash id thread-reader--entries))))
    (while parent
      (remhash parent thread-reader--collapsed)
      (setq parent (thread-reader-entry-parent-id (gethash parent thread-reader--entries)))))
  (remhash id thread-reader--collapsed)
  (gnus-thread-reader--render)
  (goto-char (gethash id thread-reader--positions)))

(defun gnus-thread-reader-next-unread ()
  "Jump to the next unread article, including replies hidden by folding."
  (interactive)
  (let* ((id (thread-reader--current-id))
         (order (sort (copy-sequence thread-reader--order)
                      (lambda (a b) (< (gethash a thread-reader--positions)
                                       (gethash b thread-reader--positions)))))
         (tail (cdr (member id order)))
         (candidates (append (or tail (unless id order))
                             (and id (cl-subseq order 0 (cl-position id order :test #'equal)))))
         (next (cl-find-if
                (lambda (candidate)
                  (and (or (null gnus-thread-reader-focus-ids)
                           (member candidate gnus-thread-reader-focus-ids))
                       (equal (gnus-thread-reader--state
                               (mail-header-number (gethash candidate gnus-thread-reader--headers))
                               candidate) "[Unread]")))
                candidates)))
    (unless next (user-error "No other unread article in this thread"))
    (gnus-thread-reader--reveal next)))

(defun gnus-thread-reader-original ()
  "Open this article in Gnus's native viewer, with full MIME support."
  (interactive)
  (let* ((gnus-summary-refresh--refresh-running t)
         (number (gnus-thread-reader--ensure-target))
        (summary (gnus-thread-reader--source)))
    (pop-to-buffer summary)
    (gnus-summary-goto-subject number t)
    (gnus-summary-select-article nil nil nil number)))

(defun gnus-thread-reader-reply (&optional quote)
  "Compose a native Gnus reply to this article; with prefix QUOTE it.
News articles use followup; mail articles use reply-to-author."
  (interactive "P")
  (let* ((gnus-summary-refresh--refresh-running t)
         (number (gnus-thread-reader--ensure-target))
        (summary (gnus-thread-reader--source)))
    (with-current-buffer summary
      (save-excursion
        (unless (gnus-summary-goto-subject number t)
          (user-error "Article is no longer available in the summary"))
        (let ((type (gnus-request-type gnus-newsgroup-name number)))
          (pcase type
            ('post (if quote (gnus-summary-followup-with-original 1)
                     (gnus-summary-followup nil)))
            ('mail (if quote (gnus-summary-reply-with-original 1)
                     (gnus-summary-reply)))
            (_ (user-error "Unknown reply transport; use the original Gnus article"))))))))

(defun gnus-thread-reader-save ()
  "Make this article persistent using Gnus's native cache."
  (interactive)
  (let* ((gnus-summary-refresh--refresh-running t)
         (number (gnus-thread-reader--ensure-target)))
    (save-window-excursion
      (with-current-buffer (gnus-thread-reader--source)
        (save-excursion
          (gnus-summary-goto-subject number t)
          ;; Explicit count avoids acting on unrelated process-marked articles.
          (gnus-cache-enter-article 1))))
    (gnus-thread-reader--render)))

(defun gnus-thread-reader-summary ()
  "Return to the source summary at this article without reading it."
  (interactive)
  (let* ((gnus-summary-refresh--refresh-running t)
         (number (gnus-thread-reader--ensure-target))
        (summary (gnus-thread-reader--source)))
    (pop-to-buffer summary)
    (gnus-summary-goto-subject number t)))

(defvar gnus-thread-reader-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map thread-reader-mode-map)
    (define-key map (kbd "g") #'gnus-thread-reader-refresh)
    (define-key map (kbd "r") #'gnus-thread-reader-reply)
    (define-key map (kbd "d") #'gnus-thread-reader-mark-read)
    (define-key map (kbd "U") #'gnus-thread-reader-mark-unread)
    (define-key map (kbd "!") #'gnus-thread-reader-tick)
    (define-key map (kbd "?") #'gnus-thread-reader-dormant)
    (define-key map (kbd "*") #'gnus-thread-reader-save)
    (define-key map (kbd "N") #'gnus-thread-reader-next-unread)
    (define-key map (kbd "o") #'gnus-thread-reader-original)
    (define-key map (kbd "s") #'gnus-thread-reader-summary)
    (define-key map (kbd "q") #'gnus-reading-quit)
    (define-key map (kbd "m") #'ignore)
    map))

(define-derived-mode gnus-thread-reader-mode thread-reader-mode "Gnus Thread"
  "Read a Gnus conversation continuously, with Outline folding.
Visiting a loaded article marks it read.  Background loading leaves other
articles unread.
\{gnus-thread-reader-mode-map}"
  (setq-local thread-reader-auto-load-replies nil)
  (setq-local thread-reader-dispatch-function #'gnus-dispatch-submit)
  (setq gnus-thread-reader--session gnus-newsrc-hashtb)
  (setq-local thread-reader-root-header-function #'gnus-thread-reader--root-header)
  (setq-local outline-regexp "\\(?:From: \\|\\*+ \\)")
  (setq-local outline-level
              (lambda () (if (looking-at "From: ") 1
                           (- (match-end 0) (match-beginning 0) 1))))
  (setq-local revert-buffer-function #'gnus-thread-reader-refresh)
  (setq gnus-thread-reader--headers (make-hash-table :test #'equal)
        gnus-thread-reader--sources (make-hash-table :test #'equal)
        gnus-thread-reader--epochs (make-hash-table :test #'equal)
        gnus-thread-reader--bodies (make-hash-table :test #'equal)
        gnus-thread-reader--raw-articles (make-hash-table :test #'equal)
        gnus-thread-reader--article-groups (make-hash-table :test #'equal))
  (add-hook 'post-command-hook #'gnus-thread-reader--visit-at-point nil t)
  (add-hook 'kill-buffer-hook #'gnus-thread-reader--cancel nil t)
  (add-hook 'change-major-mode-hook #'gnus-thread-reader--cancel nil t))

;;;###autoload
(defun gnus-thread-reader-open ()
  "Open the current Gnus summary article's thread as continuous prose."
  (interactive)
  (unless (derived-mode-p 'gnus-summary-mode)
    (user-error "Run this command in a Gnus summary"))
  (let* ((summary (current-buffer))
         (group gnus-newsgroup-name)
         (number (gnus-summary-article-number))
         (header (and number (gnus-summary-article-header number))))
    (unless (and (mail-header-p header) (> number 0))
      (user-error "Select a real article in the Gnus summary"))
    ;; Let Gnus complete the thread through any backend that implements
    ;; request-thread.  The reader remains independent of backend names.
    (let ((buffer (generate-new-buffer "*Gnus Thread*")))
      (condition-case err
          (with-current-buffer buffer
            (gnus-thread-reader-mode)
            (setq gnus-thread-reader--summary summary
                  gnus-thread-reader--group group
                  gnus-thread-reader--anchor number
                  gnus-thread-reader--anchor-id (mail-header-id header)
                  thread-reader--url group)
            (gnus-thread-reader-refresh)
            (goto-char (gethash (number-to-string number) thread-reader--positions)))
        (error (kill-buffer buffer) (signal (car err) (cdr err))))
      (gnus-reading-display buffer summary)
      (when (gnus-check-backend-function 'request-thread group)
        (gnus-thread-reader--complete-thread buffer header))
      buffer)))

(defvar gnus-thread-reader-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'gnus-thread-reader-open)
    map))

(defvar-local gnus-thread-reader--summary-settings nil)

(defun gnus-thread-reader--summary-policy ()
  "Use Gnus's native collapsed thread display in reader summaries.
Run after group parameters are applied, including when Gnus regenerates
an existing summary.  Gnus performs the folding after kill processing."
  (when gnus-thread-reader-summary-mode
    (setq-local gnus-show-threads t)
    (setq-local gnus-thread-hide-subtree t)))

;;;###autoload
(define-minor-mode gnus-thread-reader-summary-mode
  "Keep conversations collapsed and open them with RET in the reader.
This applies to any Gnus summary, independently of its backend or group."
  :lighter "" :keymap gnus-thread-reader-summary-mode-map
  (if gnus-thread-reader-summary-mode
      (progn
        (unless gnus-thread-reader--summary-settings
          (setq gnus-thread-reader--summary-settings
                (mapcar (lambda (symbol)
                          (list symbol (local-variable-p symbol)
                                (symbol-value symbol)))
                        '(gnus-show-threads gnus-thread-hide-subtree))))
        (add-hook 'gnus-summary-generate-hook
                  #'gnus-thread-reader--summary-policy nil t)
        (gnus-thread-reader--summary-policy)
        (when gnus-newsgroup-threads
          (gnus-summary-maybe-hide-threads)))
    (remove-hook 'gnus-summary-generate-hook
                 #'gnus-thread-reader--summary-policy t)
    (dolist (setting gnus-thread-reader--summary-settings)
      (if (nth 1 setting)
          (set (car setting) (nth 2 setting))
        (kill-local-variable (car setting))))
    (setq gnus-thread-reader--summary-settings nil)))

;;;###autoload
(define-globalized-minor-mode gnus-thread-reader-global-summary-mode
  gnus-thread-reader-summary-mode
  (lambda ()
    (when (derived-mode-p 'gnus-summary-mode)
      (gnus-thread-reader-summary-mode 1)))
  :group 'gnus)

(provide 'gnus-thread-reader)
;;; gnus-thread-reader.el ends here
