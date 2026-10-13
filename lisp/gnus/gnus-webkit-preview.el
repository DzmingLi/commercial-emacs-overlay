;;; gnus-webkit-preview.el --- On-demand Gnus HTML preview -*- lexical-binding: t; -*-

;; Author: Dzming Li
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (webkit "0"))
;; Keywords: mail, hypermedia
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Call `gnus-webkit-preview-open' from a Gnus Summary or Article buffer.
;; The preview owns an exported snapshot, never Gnus's article or MIME parts.
;; There is no automatic rendering hook or mail marking interface.

;;; Code:
(require 'cl-lib)
(require 'gnus-reading)
(require 'subr-x)
(require 'gnus-sum)
(require 'gnus-art)
(require 'mm-decode)
(require 'mm-util)
(require 'url-util)

(declare-function webkit-new "webkit" (&optional url buffer-name noquery))
(defvar webkit-own-window)
(declare-function gnus-thread-reader-article-snapshot "gnus-thread-reader" (&optional id))
(defvar gnus-webkit-preview--selecting-thread nil)
(declare-function thread-reader-next "thread-reader")
(declare-function thread-reader-previous "thread-reader")
(declare-function gnus-thread-reader-reply "gnus-thread-reader")

(defgroup gnus-webkit-preview nil
  "On-demand WebKit previews of Gnus articles."
  :group 'gnus-article)

(defcustom gnus-webkit-preview-created-functions nil
  "Functions called with ARTICLE, PREVIEW and WINDOW after opening a preview."
  :type 'hook)

(defcustom gnus-webkit-preview-close-hook nil
  "Hook run in the preview buffer before deleting its exported files."
  :type 'hook)

(defvar-local gnus-webkit-preview--source nil)
(defvar-local gnus-webkit-preview--return-buffer nil)
(defvar-local gnus-webkit-preview--thread nil)
(defvar-local gnus-webkit-preview--directory nil)
(defvar-local gnus-webkit-preview--owned-source nil)
(defvar gnus-webkit-preview-mode)

(defun gnus-webkit-preview--parts (handles)
  "Return leaf MIME handles from HANDLES without changing them."
  (cond ((not (consp handles)) nil)
        ((bufferp (car handles)) (list handles))
        (t (cl-mapcan #'gnus-webkit-preview--parts handles))))

(defun gnus-webkit-preview--html-part (parts)
  "Find the first non-attachment HTML body in PARTS."
  (cl-find-if
   (lambda (part)
     (and (equal (mm-handle-media-type part) "text/html")
          (not (equal (car (mm-handle-disposition part)) "attachment"))))
   parts))

(defun gnus-webkit-preview--html (part)
  "Decode PART's transfer encoding and declared character set."
  (let ((charset (mail-content-type-get (mm-handle-type part) 'charset))
        (content (mm-get-part part)))
    (if (eq charset 'gnus-decoded)
        content
      (decode-coding-string content
                            (or (mm-charset-to-coding-system charset)
                                'utf-8)))))

(defun gnus-webkit-preview--export (handles directory)
  "Export HANDLES' HTML body and referenced CID parts into DIRECTORY.
Return the HTML filename.  No native MIME handle is destroyed or modified."
  (let* ((parts (gnus-webkit-preview--parts handles))
         (part (gnus-webkit-preview--html-part parts))
         (saved (make-hash-table :test #'equal))
         (file (expand-file-name "index.html" directory))
         (number 0))
    (unless part (user-error "This article has no HTML body"))
    (with-temp-buffer
      (insert (gnus-webkit-preview--html part))
      (goto-char (point-min))
      (let ((case-fold-search t))
        ;; Covers attributes, CSS url() and percent-encoded CID values.
        (while (re-search-forward "cid:\\([^\"'<>[:space:]()]+\\)" nil t)
          (let* ((begin (match-beginning 0))
                 (end (match-end 0))
                 (id (decode-coding-string
                      (url-unhex-string (match-string 1)) 'utf-8))
                 (resource (cl-find-if
                            (lambda (p)
                              (equal (mm-handle-id p) (concat "<" id ">")))
                            parts)))
            (when resource
              (let ((path (or (gethash id saved)
                              (let* ((extension (car (rassoc (mm-handle-media-type resource)
                                                             mailcap-mime-extensions)))
                                     (path (expand-file-name
                                            (format "cid-%d%s" (cl-incf number)
                                                    (or extension ".bin")) directory)))
                                ;; Never use sender-controlled attachment paths.
                                (mm-save-part-to-file resource path)
                                (puthash id path saved)
                                path))))
                ;; Absolute URLs also work when the mail declares a remote base.
                (delete-region begin end)
                (goto-char begin)
                (insert (concat "file://" (url-encode-url path))))))))
      (let ((coding-system-for-write 'utf-8-with-signature-unix))
        (write-region (point-min) (point-max) file nil 'silent)))
    file))

(defun gnus-webkit-preview--close ()
  "Release this browser's snapshot while retaining the native Gnus reader."
  (unwind-protect
      (run-hooks 'gnus-webkit-preview-close-hook)
    (when (and gnus-webkit-preview--directory
               (file-directory-p gnus-webkit-preview--directory))
      (delete-directory gnus-webkit-preview--directory t))
    (setq gnus-webkit-preview--directory nil)
    (when (and gnus-webkit-preview--owned-source
               (buffer-live-p gnus-webkit-preview--source))
      (with-current-buffer gnus-webkit-preview--source
        (mm-destroy-parts gnus-article-mime-handles))
      (kill-buffer gnus-webkit-preview--source))))

(defun gnus-webkit-preview-quit ()
  "Close the preview and return its windows to the originating reader."
  (interactive)
  (let ((preview (current-buffer))
        (source (if (buffer-live-p gnus-webkit-preview--return-buffer)
                    gnus-webkit-preview--return-buffer
                  gnus-webkit-preview--source)))
    (unless gnus-webkit-preview-mode (user-error "This is not a Gnus WebKit preview"))
    (dolist (window (get-buffer-window-list preview nil t))
      (set-window-buffer window (if (buffer-live-p source) source
                                  (other-buffer preview t))))
    (when (buffer-live-p gnus-webkit-preview--thread)
      (with-current-buffer gnus-webkit-preview--thread
        (setq gnus-reading--renderer 'text)))
    (kill-buffer preview)))

(defvar gnus-webkit-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'gnus-webkit-preview-quit)
    map)
  "Preview controls; all other browser commands come from WebKit.")

(define-minor-mode gnus-webkit-preview-mode
  "Identify a Gnus snapshot preview and provide its quit command."
  :lighter " Gnus Preview" :keymap gnus-webkit-preview-mode-map
  :group 'gnus-webkit-preview)

(defvar gnus-webkit-preview-thread-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'gnus-reading-quit)
    (define-key map "n" #'gnus-webkit-preview-next)
    (define-key map "p" #'gnus-webkit-preview-previous)
    (define-key map "r" #'gnus-webkit-preview-reply)
    map))

(define-minor-mode gnus-webkit-preview-thread-mode
  "Keep thread navigation and reply available in its WebKit reading window."
  :lighter " Thread" :keymap gnus-webkit-preview-thread-mode-map
  :group 'gnus-webkit-preview)

;;;###autoload
(defun gnus-webkit-preview-html-p (&optional article)
  "Return non-nil when ARTICLE (or the current buffer) has an HTML body.
This predicate does not select or fetch an article."
  (when-let* ((buffer (if article (get-buffer article) (current-buffer))))
    (with-current-buffer buffer
      (and (derived-mode-p 'gnus-article-mode)
           (not (null (gnus-webkit-preview--html-part
                       (gnus-webkit-preview--parts gnus-article-mime-handles))))))))

(defun gnus-webkit-preview-render (snapshot window reader)
  "Render SNAPSHOT in WINDOW, owned by READER, without fetching or marking."
  (gnus-dispatch-assert-main)
  (unless (window-live-p window) (user-error "The reading window has closed"))
  (let ((raw (plist-get snapshot :raw))
        (old (window-buffer window)) source preview)
    (unless raw (error "HTML renderer requires a loaded article snapshot"))
    (unwind-protect
        (progn
          (setq source (generate-new-buffer " *gnus-preview-mime*"))
          (with-current-buffer source
            (setq major-mode 'gnus-article-mode)
            (setq-local gnus-article-current
                        (cons (plist-get snapshot :group) (plist-get snapshot :article))
                        gnus-article-mime-handles
                        (with-temp-buffer
                          (insert raw)
                          (let ((mm-decrypt-option 'never) (mm-verify-option 'never))
                            (mm-dissect-buffer t)))))
          (when (gnus-webkit-preview-html-p source)
            (setq preview (gnus-webkit-preview-open source window reader))
            (with-current-buffer preview
              (setq gnus-webkit-preview--owned-source t
                    gnus-webkit-preview--thread reader
                    header-line-format "Gnus thread · n/p article · r reply · q return to summary")
              (gnus-webkit-preview-thread-mode 1))))
      (when (and (not preview) (buffer-live-p source))
        (with-current-buffer source (mm-destroy-parts gnus-article-mime-handles))
        (kill-buffer source)))
    (unless preview (set-window-buffer window reader))
    (when (and (buffer-live-p old) (not (eq old preview))
               (buffer-local-value 'gnus-webkit-preview-mode old))
      (kill-buffer old))
    preview))

;;;###autoload
(defun gnus-webkit-preview-open-thread (&optional reader window)
  "Ask the reading controller for an HTML view in the current reading slot."
  (interactive)
  (unless (display-graphic-p) (user-error "WebKit needs a graphical Emacs frame"))
  (require 'gnus-thread-reader)
  (setq reader (or reader gnus-webkit-preview--thread (current-buffer)))
  (unless (and (buffer-live-p reader)
               (with-current-buffer reader (derived-mode-p 'gnus-thread-reader-mode)))
    (user-error "Select a Gnus thread reader first"))
  (with-current-buffer reader
    (setq gnus-reading--window
          (or window (and (window-live-p gnus-reading--window) gnus-reading--window)
              (get-buffer-window reader)))
    (unless (window-live-p gnus-reading--window)
      (user-error "Display the thread reader before opening its preview"))
    (add-hook 'gnus-thread-reader-article-loaded-functions #'gnus-reading--body-ready nil t)
    (gnus-reading-present 'webkit)))

(defun gnus-webkit-preview--thread-command (command &optional navigate)
  "Run COMMAND in this preview's thread reader; NAVIGATE updates the preview."
  (let ((reader gnus-webkit-preview--thread)
        (window (get-buffer-window (current-buffer) t)))
    (unless (buffer-live-p reader) (user-error "This preview has no live thread reader"))
    (let ((origin (with-current-buffer reader (point))))
      (condition-case err
          (progn
            (with-current-buffer reader (call-interactively command))
            (when navigate (gnus-webkit-preview-open-thread reader window)))
        (error
         ;; A failed replacement keeps the old snapshot visible.  Its reply
         ;; action must still refer to the old article, not the failed target.
         (when (and navigate (buffer-live-p reader))
           (with-current-buffer reader (goto-char origin)))
         (signal (car err) (cdr err)))))))

(defun gnus-webkit-preview-next ()
  "Preview the next visible article in the same thread window."
  (interactive) (gnus-webkit-preview--thread-command #'thread-reader-next t))

(defun gnus-webkit-preview-previous ()
  "Preview the previous visible article in the same thread window."
  (interactive) (gnus-webkit-preview--thread-command #'thread-reader-previous t))

(defun gnus-webkit-preview-reply ()
  "Compose a Gnus reply to this preview's current thread article."
  (interactive) (gnus-webkit-preview--thread-command #'gnus-thread-reader-reply))

;;;###autoload
(defun gnus-webkit-preview-open (&optional article window return-buffer)
  "Preview ARTICLE's HTML using emacs-webkit and return the browser buffer.
ARTICLE is an optional native Gnus Article buffer (or its name).  From a
Summary, select the current article using Gnus's normal reading behavior.
From an Article buffer, preview that article.  Bind this command to a key
or call it from your own rendering policy; it installs no automatic hook.
From a Gnus thread reader, preview its current article in the same window.
WINDOW optionally specifies an existing reading window; RETURN-BUFFER is the
buffer restored by q, defaulting to the native Article buffer.
The preview is a snapshot and remains valid when Gnus selects another mail.
Remote resources use WebKit's ordinary network behavior."
  (interactive)
  (gnus-dispatch-assert-main)
  (if (and (null article) (derived-mode-p 'gnus-thread-reader-mode))
      (gnus-webkit-preview-open-thread nil window)
    (unless (display-graphic-p) (user-error "WebKit needs a graphical Emacs frame"))
    (let* ((source (cond (article (get-buffer article))
			 ((derived-mode-p 'gnus-article-mode) (current-buffer))
			 ((derived-mode-p 'gnus-summary-mode)
                          (unless (gnus-summary-article-number)
                            (user-error "No article on this Summary line"))
                          (gnus-summary-select-article)
                          (and gnus-article-buffer (get-buffer gnus-article-buffer)))))
           (window (or window (and source (get-buffer-window source t))))
           (previous (and (window-live-p window) (window-buffer window)))
           directory preview)
      (unless (and (buffer-live-p source)
                   (with-current-buffer source (derived-mode-p 'gnus-article-mode)))
	(user-error "Select a Gnus Summary or Article buffer first"))
      (require 'webkit)
      (condition-case err
          (let ((file (progn
			(setq directory (make-temp-file "gnus-webkit-preview-" t))
			(with-current-buffer source
                          (gnus-webkit-preview--export gnus-article-mime-handles directory))))
		(webkit-own-window nil))
            (unless window (setq window (display-buffer source)))
            (with-selected-window window
              (setq preview (webkit-new (concat "file://" (url-encode-url file))
					"*gnus-webkit-preview*" t)))
            (with-current-buffer preview
              (setq gnus-webkit-preview--source source
                    gnus-webkit-preview--return-buffer (or return-buffer source)
                    gnus-webkit-preview--directory directory)
              (gnus-webkit-preview-mode 1)
              (add-hook 'kill-buffer-hook #'gnus-webkit-preview--close nil t))
            (run-hook-with-args 'gnus-webkit-preview-created-functions source preview window)
            preview)
	(error
	 (when (buffer-live-p preview) (kill-buffer preview))
	 (when (and directory (file-directory-p directory)) (delete-directory directory t))
	 (when (window-live-p window)
           (when-let* ((restore (if (buffer-live-p previous) previous source))
                       ((buffer-live-p restore)))
             (set-window-buffer window restore)))
	 (signal (car err) (cdr err)))))))

(provide 'gnus-webkit-preview)
;;; gnus-webkit-preview.el ends here
