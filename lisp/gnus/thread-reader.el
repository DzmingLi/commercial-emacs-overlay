;;; thread-reader.el --- Read and reply to a discussion tree -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: comm, hypermedia

;;; Commentary:

;; Internal view and data structures used by `gnus-thread-reader'.  The
;; backend registry remains for source-parser compatibility, but this file
;; ships with Commercial Gnus rather than as a competing installed package.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'outline)
(require 'shr)
(require 'url-queue)
(require 'dom)
(require 'button)
(require 'message)
(require 'subr-x)

(defgroup thread-reader nil
  "Read a single discussion using a pluggable backend."
  :group 'applications)

(defcustom thread-reader-auto-load-replies t
  "Automatically load every available discussion and reply page.
Requests run sequentially and stop on an error.  Set nil for manual loading."
  :type 'boolean
  :group 'thread-reader)

(defcustom thread-reader-indent-offset 4
  "Number of display columns added for each reply level.
Indentation applies to headings, bodies, wrapped lines and load buttons."
  :type 'natnum
  :group 'thread-reader)

(defface thread-reader-heading
  '((t (:weight bold)))
  "Face for post headings."
  :group 'thread-reader)

(defface thread-reader-separator
  '((t (:inherit shadow :underline t)))
  "Face for the full-width separator below a post heading."
  :group 'thread-reader)

(cl-defstruct thread-reader-backend
  "Base type for a separately installed backend.  NAME is a symbol."
  name)

(cl-defstruct thread-reader-entry
  "One post or reply.  IDs are nonempty strings local to a discussion.
PARENT-ID is nil for a root.  BODY-FORMAT is `plain' or `html'.
CHILDREN-CURSOR is nil when complete, otherwise an opaque backend value.
PLACEHOLDER-P marks a partial ancestor whose parent may be resolved later."
  id parent-id (author "") (body "") (body-format 'plain) url time
  children-cursor placeholder-p)

(cl-defstruct thread-reader-send-error
  "A failed submission.  UNCERTAIN means the server may have accepted it.
An uncertain draft cannot be resubmitted without checking the website."
  message uncertain)

(cl-defstruct thread-reader-discussion
  "The result of opening a URL.
ENTRIES includes the post and initially available replies.  CURSOR is
nil when all discussion pages are loaded, otherwise an opaque value."
  id url (title "Discussion") entries cursor)

(cl-defstruct thread-reader-page
  "A page of ENTRIES and the next CURSOR, nil when complete."
  entries cursor)

(cl-defgeneric thread-reader-backend-match-p (backend url)
  "Return non-nil if BACKEND handles URL, without performing network I/O.")

(cl-defgeneric thread-reader-backend-open (backend url callback)
  "Open URL with BACKEND, then call CALLBACK with (DISCUSSION ERROR).
On success ERROR is nil; on failure DISCUSSION is nil and ERROR is a
human-readable string.  Call exactly once, synchronously or asynchronously.")

(cl-defgeneric thread-reader-backend-children
    (backend discussion parent cursor callback)
  "Load a page with BACKEND and call CALLBACK with (PAGE ERROR).
DISCUSSION identifies the discussion.  PARENT is an entry, or nil for
discussion pagination.  CURSOR is the opaque token returned earlier.")

(cl-defmethod thread-reader-backend-children
  ((_backend thread-reader-backend) _discussion _parent _cursor callback)
  (funcall callback nil "This backend does not support pagination"))

(cl-defgeneric thread-reader-backend-can-reply-p (backend discussion entry)
  "Return non-nil if BACKEND supports replying to ENTRY in DISCUSSION.")

(cl-defmethod thread-reader-backend-can-reply-p
  ((_backend thread-reader-backend) _discussion _entry)
  nil)

(cl-defgeneric thread-reader-backend-draft-body (backend discussion parent)
  "Return editable initial text for a reply to PARENT in DISCUSSION.
This synchronous method must not make network requests.")

(cl-defmethod thread-reader-backend-draft-body
  ((_backend thread-reader-backend) _discussion _parent)
  "")

(cl-defgeneric thread-reader-backend-reply
    (backend discussion parent body callback)
  "Submit BODY as a reply to PARENT using BACKEND.
Call CALLBACK with (ENTRY ERROR).  Report success only after the server
confirms it.  ENTRY must have a stable ID and PARENT's ID as parent-id.
BODY is plain draft text; the backend handles its site's input format.
ERROR may be a string or a `thread-reader-send-error'; mark an uncertain
submission so the editor retains the draft but prevents resubmission.")

(cl-defmethod thread-reader-backend-reply
  ((_backend thread-reader-backend) _discussion _parent _body callback)
  (funcall callback nil "This backend is read-only"))

(defvar thread-reader-backends nil
  "Registered backend instances, most recently registered first.
Install and load a backend package to register it.")

(defun thread-reader-register-backend (backend)
  "Register BACKEND, replacing any instance with the same name."
  (unless (and (thread-reader-backend-p backend)
               (symbolp (thread-reader-backend-name backend))
               (thread-reader-backend-name backend))
    (error "A backend needs a non-nil symbolic name"))
  (setq thread-reader-backends
        (cons backend
              (cl-remove (thread-reader-backend-name backend)
                         thread-reader-backends
                         :key #'thread-reader-backend-name)))
  backend)

(defun thread-reader-unregister-backend (name)
  "Unregister the backend named NAME."
  (setq thread-reader-backends
        (cl-remove name thread-reader-backends
                   :key #'thread-reader-backend-name)))

(defvar-local thread-reader--backend nil)
(defvar-local thread-reader--url nil)
(defvar-local thread-reader-root-header-function nil
  "Optional function inserting a root article's headers, given its entry.
When set, omit the separate discussion banner and root Outline stars.
Replies retain their ordinary Outline headings.")
(defvar-local thread-reader--discussion nil)
(defvar-local thread-reader--entries nil)
(defvar-local thread-reader--order nil)
(defvar-local thread-reader--positions nil)
(defvar-local thread-reader--collapsed nil)
(defvar-local thread-reader--pending nil)
(defvar-local thread-reader--errors nil)
(defvar-local thread-reader--generation 0)
(defvar-local thread-reader--render-token nil)
(defvar-local thread-reader-dispatch-function nil
  "Optional function enqueueing a callback on the view's owner thread.
Called with FUNCTION and its arguments.  Gnus supplies its main dispatcher;
standalone readers retain their ordinary callback behavior.")
(defvar-local thread-reader--auto-load-timer nil)
(defvar-local thread-reader--auto-load-seen nil)
(defvar thread-reader--auto-loading nil)

(defun thread-reader--cancel-auto-load ()
  "Cancel this buffer's scheduled automatic loading."
  (when (timerp thread-reader--auto-load-timer)
    (cancel-timer thread-reader--auto-load-timer))
  (setq thread-reader--auto-load-timer nil))

(defun thread-reader--next-page ()
  "Return (PARENT) for the next available page, or nil when complete."
  (when thread-reader--discussion
    (if (thread-reader-discussion-cursor thread-reader--discussion)
        (list nil)
      (cl-loop for id in thread-reader--order
               when (thread-reader-entry-children-cursor
                     (gethash id thread-reader--entries))
               return (list id)))))

(defun thread-reader--schedule-auto-load ()
  "Schedule one page, yielding to the event loop between requests."
  (when (and thread-reader-auto-load-replies
             (derived-mode-p 'thread-reader-mode)
             (not thread-reader--auto-load-timer)
             (zerop (hash-table-count thread-reader--pending))
             (zerop (hash-table-count thread-reader--errors))
             (thread-reader--next-page))
    (setq thread-reader--auto-load-timer
          (run-at-time 0.1 nil #'thread-reader--auto-load-step
                       (current-buffer) thread-reader--generation))))

(defun thread-reader--auto-load-step (buffer generation)
  "Load one page in BUFFER if GENERATION is still current."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'thread-reader-mode)
                 (= generation thread-reader--generation))
        (setq thread-reader--auto-load-timer nil)
        (when (and thread-reader-auto-load-replies
                   (zerop (hash-table-count thread-reader--pending))
                   (zerop (hash-table-count thread-reader--errors)))
          (when-let* ((target (thread-reader--next-page)))
            (let* ((parent (car target))
                   (cursor (if parent
                               (thread-reader-entry-children-cursor
                                (gethash parent thread-reader--entries))
                             (thread-reader-discussion-cursor thread-reader--discussion))))
              (unless thread-reader--auto-load-seen
                (setq thread-reader--auto-load-seen (make-hash-table :test #'equal)))
              (if (member cursor (gethash parent thread-reader--auto-load-seen))
                  (progn
                    (puthash parent "Backend repeated a pagination cursor; automatic loading stopped"
                             thread-reader--errors)
                    (thread-reader--render))
                (push (copy-tree cursor) (gethash parent thread-reader--auto-load-seen))
                (let ((thread-reader--auto-loading t))
                  (thread-reader-load-more parent))))))))))

(defun thread-reader--valid-id-p (id)
  "Return non-nil when ID is a valid entry identifier."
  (and (stringp id) (not (string-empty-p id))))

(defun thread-reader--merge (entries &optional reset)
  "Validate ENTRIES and merge them atomically into this view.
With RESET start with an empty tree.  Return no value of interest."
  (unless (listp entries) (error "Entries must be a list"))
  (let ((table (if reset (make-hash-table :test #'equal)
                 (copy-hash-table thread-reader--entries)))
        (order (unless reset (copy-sequence thread-reader--order)))
        (seen (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (unless (and (thread-reader-entry-p entry)
                   (thread-reader--valid-id-p (thread-reader-entry-id entry))
                   (or (null (thread-reader-entry-parent-id entry))
                       (thread-reader--valid-id-p
                        (thread-reader-entry-parent-id entry)))
                   (stringp (thread-reader-entry-author entry))
                   (stringp (thread-reader-entry-body entry))
                   (memq (thread-reader-entry-body-format entry) '(plain html))
                   (or (null (thread-reader-entry-url entry))
                       (stringp (thread-reader-entry-url entry)))
                   (or (null (thread-reader-entry-time entry))
                       (stringp (thread-reader-entry-time entry))))
        (error "Backend returned an invalid entry"))
      (let* ((id (thread-reader-entry-id entry))
             (old (gethash id table)))
        (when (gethash id seen) (error "Duplicate entry ID in page: %s" id))
        (puthash id t seen)
        (when (and old
                   (not (thread-reader-entry-placeholder-p old))
                   (not (equal (thread-reader-entry-parent-id old)
                               (thread-reader-entry-parent-id entry))))
          (error "Entry %s changed parents" id))
        (unless old (push id order))
        (puthash id (copy-thread-reader-entry entry) table)))
    ;; Keep original entries in order, then append new IDs in page order.
    (setq order
          (if reset (nreverse order)
            (append thread-reader--order
                    (cl-loop for entry in entries
                             for id = (thread-reader-entry-id entry)
                             unless (gethash id thread-reader--entries)
                             collect id))))
    (let ((done (make-hash-table :test #'equal)))
      (dolist (id order)
        (let ((path nil) (visiting (make-hash-table :test #'equal)) (next id))
          (while (and next (not (gethash next done)))
            (when (gethash next visiting) (error "Reply cycle at %s" next))
            (let ((entry (gethash next table)))
              (unless entry (error "Missing parent entry: %s" next))
              (puthash next t visiting)
              (push next path)
              (setq next (thread-reader-entry-parent-id entry))))
          (dolist (node path) (puthash node t done)))))
    (setq thread-reader--entries table
          thread-reader--order order)
    (when thread-reader--discussion
      (setf (thread-reader-discussion-entries thread-reader--discussion)
            (mapcar (lambda (id) (gethash id table)) order)))))

(defun thread-reader--current-id ()
  "Return the entry ID at point, or nil."
  (get-text-property (point) 'thread-reader-id))

(defun thread-reader-current-entry ()
  "Return the entry at point, or signal a user error."
  (or (gethash (thread-reader--current-id) thread-reader--entries)
      (user-error "Move to a post or reply first")))

(defun thread-reader--line (value)
  "Return VALUE as a single unpropertized line."
  (replace-regexp-in-string "[\n\r\t]+" " "
                            (substring-no-properties (or value ""))))

(defun thread-reader--shr-code (dom)
  "Render code DOM with SHR and retain fixed pitch in prose buffers."
  (let ((start (point)))
    (funcall (pcase (dom-tag dom)
               ('pre #'shr-tag-pre)
               ('tt #'shr-tag-tt)
               (_ #'shr-tag-code))
             dom)
    (add-face-text-property start (point) 'fixed-pitch)))

(defun thread-reader--shr-image (dom &optional url)
  "Render image DOM and optional URL using SHR's image renderer contract."
  (let ((image (if dom (copy-tree dom) '(img nil)))
        (buffer (current-buffer))
        (token thread-reader--render-token)
        (dispatch thread-reader-dispatch-function)
        (retrieve (symbol-function 'url-queue-retrieve)))
    (when (string-empty-p (string-trim (or (dom-attr image 'alt) "")))
      (dom-set-attribute image 'alt "[图片]"))
    ;; A later render erases SHR's placeholder markers.  Its old callback
    ;; must not insert an image into the newly rendered text at those markers.
    (cl-letf (((symbol-function 'url-queue-retrieve)
               (lambda (url callback &optional args silent inhibit-cookies)
                 ;; Body indentation inserts text at the placeholder's start.
                 ;; Keep SHR's start marker on the image, after that text.
                 (when (and (eq callback #'shr-image-fetched)
                            (markerp (nth 1 args)))
                   (set-marker-insertion-type (nth 1 args) t))
                 (funcall retrieve url
                          (lambda (status &rest callback-args)
                            (let* ((retrieval (current-buffer))
                                   (commit
                                    (lambda ()
                                      (when (buffer-live-p retrieval)
                                        (with-current-buffer retrieval
                                          (if (and (buffer-live-p buffer)
                                                   (eq token (buffer-local-value 'thread-reader--render-token buffer)))
                                              (apply callback status callback-args)
                                            (unless (plist-get status :error)
                                              (url-store-in-cache (current-buffer)))
                                            (kill-buffer (current-buffer))))))))
                              (if dispatch (funcall dispatch commit) (funcall commit))))
                          args silent inhibit-cookies))))
      (shr-tag-img image url))))

(defun thread-reader--body (entry &optional indent)
  "Insert ENTRY's body, allowing for INDENT display columns at the left.
Indent body text so its headings are not mistaken for tree headings."
  (let ((start (point)))
    (if (eq (thread-reader-entry-body-format entry) 'html)
        (condition-case err
            (let ((dom (with-temp-buffer
                         (insert (thread-reader-entry-body entry))
                         (libxml-parse-html-region (point-min) (point-max))))
                  (shr-inhibit-images nil)
                  (shr-use-fonts nil)
                  (shr-external-rendering-functions
                   (append '((img . thread-reader--shr-image)
                             (pre . thread-reader--shr-code)
                             (code . thread-reader--shr-code)
                             (tt . thread-reader--shr-code))
                           shr-external-rendering-functions))
                  (shr-width (max 20 (- (window-body-width
                                         (get-buffer-window (current-buffer)))
                                        4 (or indent 0))))
                  (shr-base (or (thread-reader-entry-url entry)
                                thread-reader--url)))
              (shr-insert-document dom))
          (error (insert (format "[HTML rendering failed: %s]\n"
                                 (error-message-string err)))))
      (insert (substring-no-properties (thread-reader-entry-body entry))))
    (unless (bolp) (insert "\n"))
    (let ((end (copy-marker (point) t)))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (insert "  ")
          (forward-line 1)))
      (set-marker end nil))
    (insert "\n")))

(defun thread-reader--more-button (parent cursor)
  "Insert a pagination button for PARENT and CURSOR."
  (when cursor
    (let ((start (point))
          (pending (gethash parent thread-reader--pending))
          (err (gethash parent thread-reader--errors)))
      (insert "  ")
      (insert-text-button
       (cond (pending "Loading…") (err "Retry loading replies")
             (t "Load more replies"))
       'follow-link t
       'action (lambda (_button) (thread-reader-load-more parent)))
      (when err (insert " — " (thread-reader--line err)))
      (insert "\n\n")
      (when parent
        (add-text-properties start (point) `(thread-reader-id ,parent))))))

(defun thread-reader--render ()
  "Render current state, preserving the selected entry and its offset."
  (let* ((id (thread-reader--current-id))
         (old-start (and id (gethash id thread-reader--positions)))
         (offset (if old-start (- (point) old-start) 0))
         (old-point (point))
         (inhibit-read-only t)
         (children (make-hash-table :test #'equal)))
    (remove-overlays (point-min) (point-max) 'invisible 'outline)
    (setq thread-reader--render-token (make-symbol "render"))
    (erase-buffer)
    (setq thread-reader--positions (make-hash-table :test #'equal))
    (unless thread-reader-root-header-function
      (insert (propertize
               (thread-reader--line
                (if thread-reader--discussion
                    (thread-reader-discussion-title thread-reader--discussion)
                  "Opening discussion…"))
               'face 'bold)
              "\n")
      (when-let* ((url (and thread-reader--discussion
                            (thread-reader-discussion-url thread-reader--discussion))))
        (insert (thread-reader--line url) "\n"))
      (insert "\n"))
    (when (gethash 'open thread-reader--pending) (insert "Loading discussion…\n\n"))
    (when-let* ((err (gethash 'open thread-reader--errors)))
      (insert "Could not open discussion: " (thread-reader--line err) "\n")
      (insert-text-button "Retry" 'follow-link t
                          'action (lambda (_) (thread-reader-refresh)))
      (insert "\n\n"))
    ;; Discussion pagination is outside every entry's foldable subtree.
    (when thread-reader--discussion
      (thread-reader--more-button
       nil (thread-reader-discussion-cursor thread-reader--discussion)))
    (dolist (node thread-reader--order)
      (let ((parent (thread-reader-entry-parent-id
                     (gethash node thread-reader--entries))))
        (push node (gethash parent children))))
    ;; An explicit stack avoids recursion limits for deeply nested replies.
    (let ((stack (mapcar (lambda (node) (list 'entry node 1))
                         (reverse (gethash nil children)))))
      (while stack
        (pcase-let ((`(,_kind ,node ,depth) (pop stack)))
          (let ((entry (gethash node thread-reader--entries))
                (entry-start (point))
                (indent (* (1- depth) thread-reader-indent-offset)))
            (let ((start (point)))
              (puthash node (copy-marker start) thread-reader--positions)
              (if (and (= depth 1) thread-reader-root-header-function)
                  (funcall thread-reader-root-header-function entry)
                (insert (make-string depth ?*) " "
                        (propertize (thread-reader--line
                                     (thread-reader-entry-author entry))
                                    'face 'font-lock-keyword-face))
                (when (thread-reader-entry-time entry)
                  (insert " · " (thread-reader--line
                                 (thread-reader-entry-time entry))))
                (insert "\n")
                (add-face-text-property start (1- (point)) 'thread-reader-heading t)
                (insert (propertize " " 'display '(space :align-to (- right-fringe 1))
                                    'face 'thread-reader-separator
                                    'rear-nonsticky t)
                        "\n"))
              (thread-reader--body entry indent)
              (add-text-properties start (point) `(thread-reader-id ,node)))
            ;; Put this control before descendants, so folding the final child
            ;; cannot accidentally hide its parent's pagination control.
            (thread-reader--more-button
             node (thread-reader-entry-children-cursor entry))
            ;; Display prefixes preserve Outline's real column-zero headings,
            ;; while indenting the entire reply, including wrapped body lines.
            (add-text-properties
             entry-start (point)
             `(line-prefix ,(propertize (make-string indent ?\s) 'face 'fixed-pitch)
                           wrap-prefix ,(propertize (make-string (+ indent 2) ?\s)
                                                    'face 'fixed-pitch)))
            (dolist (child (gethash node children))
              (push (list 'entry child (1+ depth)) stack))))))
    (maphash (lambda (node _)
               (when-let* ((pos (gethash node thread-reader--positions)))
                 (goto-char pos)
                 (outline-hide-subtree)))
             thread-reader--collapsed)
    (if-let* ((pos (and id (gethash id thread-reader--positions))))
        (progn
          (goto-char (min (point-max) (+ pos offset)))
          (unless (and (equal id (thread-reader--current-id))
                       (not (invisible-p (point))))
            (goto-char pos)))
      (goto-char (min old-point (point-max))))
    (set-buffer-modified-p nil)))

(defun thread-reader--request (key start accept)
  "Run START with a guarded callback and pass its result to ACCEPT.
KEY identifies an in-flight request.  Ignore callbacks after refresh,
buffer death, or an earlier completion of the same request."
  (when (gethash key thread-reader--pending)
    (user-error "This request is already running"))
  (puthash key t thread-reader--pending)
  (remhash key thread-reader--errors)
  (thread-reader--render)
  (let ((buffer (current-buffer))
        (generation thread-reader--generation)
        (completed nil))
    (let ((callback
           (lambda (result err)
             (unless completed
               (setq completed t)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when (= generation thread-reader--generation)
                     (remhash key thread-reader--pending)
                     (condition-case failure
                         (if err (error "%s" err) (funcall accept result))
                       (error (puthash key (error-message-string failure)
                                       thread-reader--errors)))
                     (thread-reader--render)
                     (thread-reader--schedule-auto-load))))))))
      (condition-case err (funcall start callback)
        (error (funcall callback nil (error-message-string err)))))))

(defun thread-reader-refresh (&rest _)
  "Reopen the discussion, retaining the old view on failure."
  (interactive)
  (thread-reader--cancel-auto-load)
  (setq thread-reader--auto-load-seen nil)
  (cl-incf thread-reader--generation)
  (clrhash thread-reader--pending)
  (clrhash thread-reader--errors)
  (thread-reader--request
   'open
   (lambda (callback)
     (thread-reader-backend-open thread-reader--backend thread-reader--url callback))
   (lambda (discussion)
     (unless (and (thread-reader-discussion-p discussion)
                  (thread-reader--valid-id-p
                   (thread-reader-discussion-id discussion))
                  (stringp (thread-reader-discussion-url discussion))
                  (stringp (thread-reader-discussion-title discussion)))
       (error "Backend returned an invalid discussion"))
     (let ((thread-reader--discussion nil))
       (thread-reader--merge (thread-reader-discussion-entries discussion) t))
     (setq thread-reader--discussion (copy-thread-reader-discussion discussion))
     (setf (thread-reader-discussion-entries thread-reader--discussion)
           (mapcar (lambda (id) (gethash id thread-reader--entries))
                   thread-reader--order)))))

(defun thread-reader-load-more (parent)
  "Load the next page for PARENT's ID, or the discussion if nil."
  (interactive (list (thread-reader--current-id)))
  (when (gethash 'open thread-reader--pending)
    (user-error "Wait for the discussion to finish opening"))
  (unless thread-reader--discussion (user-error "No discussion loaded"))
  (let* ((entry (and parent (gethash parent thread-reader--entries)))
         (cursor (if parent
                     (and entry (thread-reader-entry-children-cursor entry))
                   (thread-reader-discussion-cursor thread-reader--discussion))))
    (unless cursor (user-error "All replies are loaded"))
    (thread-reader--cancel-auto-load)
    (when (and (not thread-reader--auto-loading) thread-reader--auto-load-seen)
      (remhash parent thread-reader--auto-load-seen))
    (thread-reader--request
     parent
     (lambda (callback)
       (thread-reader-backend-children
        thread-reader--backend thread-reader--discussion entry cursor callback))
     (lambda (page)
       (unless (thread-reader-page-p page) (error "Backend returned an invalid page"))
       (thread-reader--merge (thread-reader-page-entries page))
       (if parent
           (setf (thread-reader-entry-children-cursor
                  (gethash parent thread-reader--entries))
                 (thread-reader-page-cursor page))
         (setf (thread-reader-discussion-cursor thread-reader--discussion)
               (thread-reader-page-cursor page)))))))

(defun thread-reader-toggle ()
  "Toggle this entry's subtree, or activate the button at point."
  (interactive)
  (if (button-at (point)) (push-button)
    (let ((id (thread-reader-entry-id (thread-reader-current-entry))))
      (if (gethash id thread-reader--collapsed)
          (remhash id thread-reader--collapsed)
        (puthash id t thread-reader--collapsed))
      (thread-reader--render))))

(defun thread-reader-next ()
  "Move to the next visible post or reply."
  (interactive)
  (outline-next-visible-heading 1))

(defun thread-reader-previous ()
  "Move to the previous visible post or reply."
  (interactive)
  (outline-previous-visible-heading 1))

(defun thread-reader-parent ()
  "Move to the parent entry."
  (interactive)
  (let ((parent (thread-reader-entry-parent-id (thread-reader-current-entry))))
    (unless parent (user-error "This entry has no parent"))
    (goto-char (gethash parent thread-reader--positions))))

(defun thread-reader-browse-url ()
  "Open this entry's permalink, falling back to the discussion URL."
  (interactive)
  (browse-url (or (thread-reader-entry-url (thread-reader-current-entry))
                  thread-reader--url)))

(defvar thread-reader-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "TAB") #'thread-reader-toggle)
    (define-key map (kbd "n") #'thread-reader-next)
    (define-key map (kbd "p") #'thread-reader-previous)
    (define-key map (kbd "u") #'thread-reader-parent)
    (define-key map (kbd "g") #'thread-reader-refresh)
    (define-key map (kbd "r") #'thread-reader-reply)
    (define-key map (kbd "m") #'thread-reader-load-more)
    (define-key map (kbd "o") #'thread-reader-browse-url)
    map))

(define-derived-mode thread-reader-mode special-mode "Thread"
  "Read a discussion tree.  \<thread-reader-mode-map>
\[thread-reader-toggle] folds an entry, \[thread-reader-reply] replies,
and \[thread-reader-refresh] refreshes the discussion."
  (setq-local outline-regexp "\\*+ ")
  (setq-local outline-level (lambda () (- (match-end 0) (match-beginning 0) 1)))
  (outline-minor-mode 1)
  (setq-local revert-buffer-function #'thread-reader-refresh)
  (setq-local truncate-lines nil)
  (add-hook 'kill-buffer-hook #'thread-reader--cancel-auto-load nil t)
  (add-hook 'change-major-mode-hook #'thread-reader--cancel-auto-load nil t)
  (setq thread-reader--entries (make-hash-table :test #'equal)
        thread-reader--positions (make-hash-table :test #'equal)
        thread-reader--collapsed (make-hash-table :test #'equal)
        thread-reader--pending (make-hash-table :test #'equal)
        thread-reader--errors (make-hash-table :test #'equal)))

;;;###autoload
(defun thread-reader-open (url &optional backend)
  "Open URL with BACKEND, or select a registered matching backend.
BACKEND is an instance, or the name of a registered backend."
  (interactive (list (read-string "Discussion URL: " (thing-at-point 'url t))))
  (when (and backend (symbolp backend))
    (let ((name backend))
      (setq backend (cl-find name thread-reader-backends
                             :key #'thread-reader-backend-name))
      (unless backend (user-error "Backend is not registered: %s" name))))
  (unless backend
    (let ((matches (cl-remove-if-not
                    (lambda (candidate)
                      (thread-reader-backend-match-p candidate url))
                    thread-reader-backends)))
      (setq backend
            (if (cdr matches)
                (let* ((names (mapcar (lambda (item)
                                        (symbol-name (thread-reader-backend-name item)))
                                      matches))
                       (name (completing-read "Backend: " names nil t)))
                  (cl-find name matches :test #'equal
                           :key (lambda (item)
                                  (symbol-name (thread-reader-backend-name item)))))
              (car matches)))))
  (unless backend
    (user-error "No backend handles this URL; install and load a backend package"))
  (let ((buffer (generate-new-buffer "*thread-reader*")))
    (with-current-buffer buffer
      (thread-reader-mode)
      (setq thread-reader--backend backend thread-reader--url url)
      (thread-reader-refresh))
    (pop-to-buffer buffer)
    buffer))

(defvar-local thread-reader--reply-context nil
  "Vector of backend, discussion, parent, source buffer, and generation.")
(defvar-local thread-reader--sending nil)
(defvar-local thread-reader--sent nil)
(defvar-local thread-reader--send-uncertain nil)

(defvar thread-reader-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map message-mode-map)
    (define-key map [remap message-send] #'thread-reader-send)
    (define-key map [remap message-send-and-exit] #'thread-reader-send-and-exit)
    (define-key map (kbd "C-c C-s") #'thread-reader-send)
    (define-key map (kbd "C-c C-c") #'thread-reader-send-and-exit)
    map))

(define-derived-mode thread-reader-compose-mode message-mode "Thread-Reply"
  "Compose a web reply.  Send using this mode's commands, not mail transport."
  ;; An explicit M-x message-send should also never invoke mail delivery.
  (setq-local message-send-method-alist
              '((thread-reader thread-reader--message-p thread-reader--message-send))))

(defun thread-reader--message-p ()
  "Recognize a thread-reader draft for Message's dispatch."
  (derived-mode-p 'thread-reader-compose-mode))

(defun thread-reader--message-send (&rest _)
  "Keep Message's synchronous send lifecycle out of web submissions."
  (user-error "Use thread-reader-send or C-c C-c for asynchronous web replies"))

(defun thread-reader-reply ()
  "Compose a reply to the entry at point."
  (interactive)
  (let* ((parent (thread-reader-current-entry))
         (discussion thread-reader--discussion)
         (backend thread-reader--backend)
         (context (vector backend discussion (copy-thread-reader-entry parent)
                          (current-buffer) thread-reader--generation)))
    (unless (thread-reader-backend-can-reply-p backend discussion parent)
      (user-error "This backend cannot reply to this entry"))
    (let ((buffer (generate-new-buffer "*thread-reader reply*")))
      (with-current-buffer buffer
        (thread-reader-compose-mode)
        (setq thread-reader--reply-context context)
        (insert "Subject: Re: "
                (thread-reader--line (thread-reader-discussion-title discussion))
                "\n" mail-header-separator "\n"
                (thread-reader-backend-draft-body backend discussion parent))
        (setq-local header-line-format
                    (list "Reply to " (thread-reader--line
                                       (thread-reader-entry-author parent))
                          " · " (thread-reader--line
                                 (thread-reader-discussion-url discussion))))
        (set-buffer-modified-p nil))
      (pop-to-buffer buffer)
      buffer)))

(defun thread-reader-send (&optional exit)
  "Submit the current draft.  With EXIT close it after confirmed success.
Pending drafts are read-only.  Failures preserve the text for retry."
  (interactive)
  (unless (and (derived-mode-p 'thread-reader-compose-mode)
               thread-reader--reply-context)
    (user-error "This is not a thread-reader draft"))
  (when thread-reader--sent (user-error "This draft was already sent"))
  (when thread-reader--send-uncertain
    (user-error "Previous submission may have succeeded; check the website before creating another draft"))
  (when thread-reader--sending (user-error "This reply is already being sent"))
  (let* ((body (save-excursion
                 (message-goto-body)
                 (buffer-substring-no-properties (point) (point-max))))
         (context thread-reader--reply-context)
         (draft (current-buffer))
         (backend (aref context 0))
         (discussion (aref context 1))
         (parent (aref context 2))
         (source (aref context 3))
         (generation (aref context 4))
         (completed nil))
    (when (string-empty-p (string-trim body)) (user-error "Reply is empty"))
    (setq thread-reader--sending t buffer-read-only t)
    (force-mode-line-update)
    (let ((callback
           (lambda (entry err)
             (unless completed
               (setq completed t)
               (if err
                   (when (buffer-live-p draft)
                     (with-current-buffer draft
                       (setq thread-reader--sending nil buffer-read-only nil)
                       (setq thread-reader--send-uncertain
                             (and (thread-reader-send-error-p err)
                                  (thread-reader-send-error-uncertain err)))
                       (message "Reply failed: %s (draft retained)"
                                (if (thread-reader-send-error-p err)
                                    (thread-reader-send-error-message err) err))))
                 ;; Server success is final even if its entry cannot be merged.
                 ;; Never encourage a duplicate submission after a UI failure.
                 (when (buffer-live-p draft)
                   (with-current-buffer draft
                     (setq thread-reader--sending nil thread-reader--sent t
                           buffer-read-only t)
                     (set-buffer-modified-p nil)))
                 (condition-case failure
                     (progn
                       (unless (and (thread-reader-entry-p entry)
                                    (equal (thread-reader-entry-parent-id entry)
                                           (thread-reader-entry-id parent)))
                         (error "Backend returned an invalid reply"))
                       (when (buffer-live-p source)
                         (with-current-buffer source
                           (when (and (= generation thread-reader--generation)
                                      (derived-mode-p 'thread-reader-mode))
                             (thread-reader--merge (list entry))
                             (thread-reader--render))))
                       (message "Reply sent"))
                   (error (message "Reply sent; refresh the discussion: %s"
                                   (error-message-string failure))))
                 (when (and exit (buffer-live-p draft))
                   (when-let* ((window (get-buffer-window draft)))
                     (quit-window nil window))
                   (kill-buffer draft)))))))
      (condition-case err
          (thread-reader-backend-reply backend discussion parent body callback)
        (error (funcall callback nil (error-message-string err)))))))

(defun thread-reader-send-and-exit ()
  "Submit this draft and close it only after confirmed success."
  (interactive)
  (thread-reader-send t))

(provide 'thread-reader)
;;; thread-reader.el ends here
