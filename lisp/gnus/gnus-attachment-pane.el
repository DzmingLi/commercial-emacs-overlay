;;; gnus-attachment-pane.el --- Dedicated attachment pane for Gnus -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dzming Li
;; Author: Dzming Li
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: mail, files, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Display attachments of the current Gnus article in a dedicated pane below
;; the message body.  Each attachment occupies one selectable row.  Opening,
;; saving, placement, height, and keyboard bindings are customizable.

;;; Code:

(require 'cl-lib)
(require 'gnus-art)
(require 'gnus-sum)
(require 'mm-decode)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'gnus-dispatch)

(defgroup gnus-attachment-pane nil
  "A dedicated attachment pane for Gnus articles."
  :group 'gnus-article
  :prefix "gnus-attachment-pane-")

(defcustom gnus-attachment-pane-buffer-name "*gnus-attachments*"
  "Name of the attachment list buffer."
  :type 'string)

(defcustom gnus-attachment-pane-window-height 8
  "Maximum height of the attachment pane in lines."
  :type 'integer)

(defcustom gnus-attachment-pane-min-window-height 6
  "Preferred minimum total height of the attachment pane in lines.
Leave room for the column header, mode line and selectable attachment rows.
The height is still capped by `gnus-attachment-pane-window-height'."
  :type 'integer)

(defcustom gnus-attachment-pane-save-directory
  (expand-file-name "Downloads/" "~")
  "Directory used by the default save function.
This may be a directory name or a function receiving the MIME part plist and
returning a directory name."
  :type '(choice directory function))

(defcustom gnus-attachment-pane-open-function
  #'gnus-attachment-pane-open-default
  "Function called to open the attachment on the selected row.
The function receives a plist containing :handle, :filename, :mime-type and
:decoded-size-approx; WebKit snapshots also carry an independent :file path."
  :type 'function)

(defcustom gnus-attachment-pane-save-function
  #'gnus-attachment-pane-save-default
  "Function called to save the attachment on the selected row.
The function receives the same part plist as the configured open function."
  :type 'function)

(defcustom gnus-attachment-pane-key-bindings
  '(("RET" . gnus-attachment-pane-open)
    ("o" . gnus-attachment-pane-open)
    ("s" . gnus-attachment-pane-save)
    ("S" . gnus-attachment-pane-save-all)
    ("n" . gnus-attachment-pane-next-line)
    ("p" . gnus-attachment-pane-previous-line)
    ("q" . gnus-attachment-pane-close))
  "Key bindings installed in `gnus-attachment-pane-list-mode'.
Each element is a cons cell of a key description and a command symbol."
  :type '(alist :key-type string :value-type function))

(defvar gnus-attachment-pane--source-buffer nil)
(defvar gnus-attachment-pane--parts nil)
(defvar-local gnus-attachment-pane--snapshot-directory nil)
(defvar gnus-attachment-pane--buffers nil)
(defvar gnus-attachment-pane--opened-directory nil)
(defvar gnus-attachment-pane-mode)
(defvar gnus-webkit-preview-created-functions)
(defvar gnus-webkit-preview-close-hook)
(defvar gnus-webkit-preview--source)
(defvar gnus-webkit-preview--selecting-thread nil)

(defun gnus-attachment-pane--configured-keymap ()
  "Return a keymap built from `gnus-attachment-pane-key-bindings'."
  (let ((map (make-sparse-keymap)))
    (dolist (binding gnus-attachment-pane-key-bindings)
      (keymap-set map (car binding) (cdr binding)))
    (set-keymap-parent map tabulated-list-mode-map)
    map))

(define-derived-mode gnus-attachment-pane-list-mode tabulated-list-mode
  "Attachments"
  "Major mode for selecting and acting on Gnus attachments."
  (use-local-map (gnus-attachment-pane--configured-keymap))
  (setq tabulated-list-format
        [("Attachment" 48 t)
         ("Type" 24 t)
         ("Size" 12 gnus-attachment-pane--size-less-p)])
  (setq tabulated-list-padding 1
        tabulated-list-sort-key '("Attachment" . nil)
        truncate-lines t
        window-size-fixed 'height)
  (tabulated-list-init-header)
  (hl-line-mode 1))

(defun gnus-attachment-pane--parts ()
  "Return attachment plists from the current native Gnus MIME tree.
Inline CID images are omitted unless explicitly marked as attachments."
  (cl-labels ((leaves (tree)
                (cond ((not (consp tree)) nil)
                      ((bufferp (car tree)) (list tree))
                      (t (cl-mapcan #'leaves tree)))))
    (cl-loop for handle in (leaves gnus-article-mime-handles)
             for filename = (mm-handle-filename handle)
             for attachment = (equal (car (mm-handle-disposition handle)) "attachment")
             when (or attachment (and filename (not (mm-handle-id handle))))
             collect (list :handle handle :filename filename :attachment-like t
                           :mime-type (mm-handle-media-type handle)
                           :decoded-size-approx
                           (with-current-buffer (mm-handle-buffer handle)
                             (if (eq (mm-handle-encoding handle) 'base64)
                                 (/ (* (buffer-size) 3) 4)
                               (buffer-size)))))))

(defun gnus-attachment-pane--part-name (part)
  "Return a safe display and file name for PART."
  (or (and-let* ((name (plist-get part :filename)))
        (file-name-nondirectory name))
      "attachment"))

(defun gnus-attachment-pane--part-size (part)
  "Return the decoded size of PART, or zero when unavailable."
  (or (plist-get part :decoded-size-approx) 0))

(defun gnus-attachment-pane--size-less-p (a b)
  "Return non-nil when the attachment represented by row A is smaller than B."
  (< (gnus-attachment-pane--part-size (car a))
     (gnus-attachment-pane--part-size (car b))))

(defun gnus-attachment-pane--entries ()
  "Build tabulated-list entries from `gnus-attachment-pane--parts'."
  (mapcar
   (lambda (part)
     (list part
           (vector
            (gnus-attachment-pane--part-name part)
            (or (plist-get part :mime-type) "application/octet-stream")
            (let ((size (gnus-attachment-pane--part-size part)))
              (if (> size 0) (file-size-human-readable size) "—")))))
   gnus-attachment-pane--parts))

(defun gnus-attachment-pane--unique-path (directory filename)
  "Return an unused path below DIRECTORY for FILENAME."
  (let* ((base (file-name-nondirectory (or filename "attachment")))
         (base (if (member base '("" "." "..")) "attachment" base))
         (path (expand-file-name base directory))
         (counter 1))
    (while (file-exists-p path)
      (setq path
            (expand-file-name
             (format "%s-%d%s"
                     (file-name-sans-extension base) counter
                     (or (file-name-extension base t) ""))
             directory)
            counter (1+ counter)))
    path))

(defun gnus-attachment-pane--save-directory (part)
  "Return the configured save directory for PART."
  (file-name-as-directory
   (if (functionp gnus-attachment-pane-save-directory)
       (funcall gnus-attachment-pane-save-directory part)
     gnus-attachment-pane-save-directory)))

(defun gnus-attachment-pane--materialize (part directory)
  "Decode PART into a unique file below DIRECTORY and return its path."
  (make-directory directory t)
  (let ((target
         (gnus-attachment-pane--unique-path
          directory (gnus-attachment-pane--part-name part))))
    (if-let* ((file (plist-get part :file)))
        (copy-file file target)
      (let ((handle (plist-get part :handle)))
        (unless (buffer-live-p (mm-handle-buffer handle))
          (user-error "This attachment's article is no longer available"))
        (mm-save-part-to-file handle target)))
    target))

(defun gnus-attachment-pane-open-default (part)
  "Default implementation for opening MIME PART inside Emacs."
  (let* ((directory (or gnus-attachment-pane--opened-directory
                        (setq gnus-attachment-pane--opened-directory
                              (make-temp-file "gnus-opened-attachments-" t))))
         (target (gnus-attachment-pane--materialize part directory)))
    (find-file-other-window target)))

(defun gnus-attachment-pane-save-default (part)
  "Default implementation for saving MIME PART without prompting."
  (let ((target
         (gnus-attachment-pane--materialize
          part (gnus-attachment-pane--save-directory part))))
    (message "Saved attachment to %s" target)
    target))

(defun gnus-attachment-pane-current-part ()
  "Return the MIME part represented by the selected row."
  (or (tabulated-list-get-id)
      (user-error "No attachment is selected")))

(defun gnus-attachment-pane-open ()
  "Open the attachment on the selected row."
  (interactive)
  (funcall gnus-attachment-pane-open-function
           (gnus-attachment-pane-current-part)))

(defun gnus-attachment-pane-save ()
  "Save the attachment on the selected row."
  (interactive)
  (funcall gnus-attachment-pane-save-function
           (gnus-attachment-pane-current-part)))

(defun gnus-attachment-pane-save-all ()
  "Save every attachment in the pane using the configured save function."
  (interactive)
  (dolist (part gnus-attachment-pane--parts)
    (funcall gnus-attachment-pane-save-function part)))

(defun gnus-attachment-pane-next-line ()
  "Select the next attachment row."
  (interactive)
  (forward-line 1)
  (when (eobp) (forward-line -1)))

(defun gnus-attachment-pane-previous-line ()
  "Select the previous attachment row."
  (interactive)
  (forward-line -1)
  (when (bobp) (forward-line 1)))

(defun gnus-attachment-pane--window (&optional frame)
  "Return the attachment pane window on FRAME, if present."
  (car (gnus-attachment-pane--windows frame)))

(defun gnus-attachment-pane--windows (&optional frame)
  "Return all attachment pane windows on FRAME."
  (seq-filter
   (lambda (window)
     (window-parameter window 'gnus-attachment-pane))
   (window-list frame 'nomini)))

(defun gnus-attachment-pane--close-window (window)
  "Close attachment pane WINDOW and kill its buffer when no pane still uses it."
  (when (window-live-p window)
    (let ((buffer (window-buffer window)))
      (set-window-parameter window 'gnus-attachment-pane nil)
      (set-window-parameter window 'gnus-attachment-pane-source-window nil)
      (set-window-parameter window 'gnus-attachment-pane-source-buffer nil)
      (set-window-dedicated-p window nil)
      ;; `delete-window' cannot remove the sole ordinary window.  That case
      ;; can occur when the view window itself was deleted first; `quit-window'
      ;; still removes the attachment buffer from the surviving window.
      (condition-case nil
          (delete-window window)
        (error (quit-window nil window)))
      (unless (get-buffer-window buffer t)
        (kill-buffer buffer)))))

(defun gnus-attachment-pane--kill-unused-buffer ()
  "Kill attachment buffers that no window displays."
  (dolist (buffer (delete-dups (cons (get-buffer gnus-attachment-pane-buffer-name)
                                    gnus-attachment-pane--buffers)))
    (when (and (buffer-live-p buffer) (not (get-buffer-window buffer t)))
      (kill-buffer buffer)))
  (setq gnus-attachment-pane--buffers
        (seq-filter #'buffer-live-p gnus-attachment-pane--buffers)))

(defun gnus-attachment-pane--source-window-live-p (pane-window)
  "Return non-nil when PANE-WINDOW still belongs to its original Gnus article."
  (let ((source-window
         (window-parameter pane-window
                           'gnus-attachment-pane-source-window))
        (source-buffer
         (window-parameter pane-window
                           'gnus-attachment-pane-source-buffer)))
    (and (window-live-p source-window)
         (buffer-live-p source-buffer)
         (eq (window-buffer source-window) source-buffer))))

(defun gnus-attachment-pane--cleanup-frame (frame)
  "Close orphaned attachment panes on FRAME.
A pane becomes orphaned when its source view window is deleted, changes
buffer, or displays a killed buffer."
  (when (frame-live-p frame)
    (dolist (window (gnus-attachment-pane--windows frame))
      (unless (gnus-attachment-pane--source-window-live-p window)
        (gnus-attachment-pane--close-window window)))
    ;; Also covers a pane window deleted directly with generic window
    ;; commands, which bypasses `gnus-attachment-pane--close-window'.
    (gnus-attachment-pane--kill-unused-buffer)))

(defun gnus-attachment-pane-close ()
  "Close the visible attachment pane."
  (interactive)
  (dolist (window (gnus-attachment-pane--windows))
    (gnus-attachment-pane--close-window window))
  (gnus-attachment-pane--kill-unused-buffer))

(defun gnus-attachment-pane--render-buffer (source-buffer parts)
  "Render PARTS associated with SOURCE-BUFFER and return the pane buffer."
  (let ((buffer (generate-new-buffer gnus-attachment-pane-buffer-name)))
    (with-current-buffer buffer
      (gnus-attachment-pane-list-mode)
      (setq-local gnus-attachment-pane--source-buffer source-buffer
                  gnus-attachment-pane--parts parts
                  tabulated-list-entries #'gnus-attachment-pane--entries)
      (tabulated-list-print t)
      ;; The column headings live in `header-line-format'; the first buffer
      ;; line is already the first selectable attachment row.
      (goto-char (point-min)))
    (push buffer gnus-attachment-pane--buffers)
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook #'gnus-attachment-pane--delete-snapshot nil t))
    buffer))

(defun gnus-attachment-pane--delete-snapshot ()
  "Release this attachment pane's independent exported resources."
  (when (and gnus-attachment-pane--snapshot-directory
             (file-directory-p gnus-attachment-pane--snapshot-directory))
    (delete-directory gnus-attachment-pane--snapshot-directory t))
  (setq gnus-attachment-pane--snapshot-directory nil
        gnus-attachment-pane--buffers (delq (current-buffer) gnus-attachment-pane--buffers)))

(defun gnus-attachment-pane-display (&optional view-buffer view-window)
  "Display current Gnus attachments below VIEW-WINDOW.
VIEW-BUFFER optionally owns the pane instead of the native Article buffer."
  (when-let* ((view-window (or view-window (get-buffer-window (current-buffer) t)))
              (frame (window-frame view-window)))
    (when-let* ((old-window (gnus-attachment-pane--window frame)))
      (gnus-attachment-pane--close-window old-window))
    (when-let* ((parts (gnus-attachment-pane--parts))
                ((not (null parts)))
                (height (min gnus-attachment-pane-window-height
                             (max gnus-attachment-pane-min-window-height
                                  (+ 2 (length parts)))))
                (pane-window
                 (condition-case nil
                     (split-window view-window (- height) 'below)
                   (error nil))))
      (set-window-buffer
       pane-window
       (gnus-attachment-pane--render-buffer (current-buffer) parts))
      (set-window-parameter pane-window 'gnus-attachment-pane t)
      (set-window-parameter pane-window
                            'gnus-attachment-pane-source-window view-window)
      (set-window-parameter pane-window
                            'gnus-attachment-pane-source-buffer (or view-buffer (current-buffer)))
      (set-window-parameter pane-window 'no-other-window t)
      (set-window-dedicated-p pane-window t))))

(defun gnus-attachment-pane--after-select ()
  "Display the native article's attachments after Gnus selects it."
  (when (and (not gnus-webkit-preview--selecting-thread)
             gnus-article-buffer (get-buffer gnus-article-buffer))
    (with-current-buffer gnus-article-buffer
      (when (derived-mode-p 'gnus-article-mode)
        (gnus-attachment-pane-display)))))

(defun gnus-attachment-pane--adopt-preview (source preview view-window)
  "Transfer SOURCE's pane to PREVIEW and snapshot its attachments."
  ;; A thread preview keeps the native Article hidden and therefore has no
  ;; existing native pane.  Create it directly under the shared reading slot.
  (unless (cl-find-if
           (lambda (pane)
             (and (eq (window-parameter pane 'gnus-attachment-pane-source-window) view-window)
                  (eq (window-parameter pane 'gnus-attachment-pane-source-buffer) source)))
           (gnus-attachment-pane--windows (window-frame view-window)))
    (when gnus-attachment-pane-mode
      (with-current-buffer source
        (gnus-attachment-pane-display preview view-window))))
  (dolist (pane (gnus-attachment-pane--windows (window-frame view-window)))
    (when (and (eq (window-parameter pane 'gnus-attachment-pane-source-window) view-window)
               (memq (window-parameter pane 'gnus-attachment-pane-source-buffer)
                     (list source preview)))
      ;; Exporting can redisplay and run window-state cleanup.  Reassociate
      ;; the pane before file I/O so the new browser is already its owner.
      (set-window-parameter pane 'gnus-attachment-pane-source-buffer preview)
      (with-current-buffer (window-buffer pane)
        (let ((directory (make-temp-file "gnus-attachment-snapshot-" t)))
          (condition-case err
              (progn
                (setq gnus-attachment-pane--parts
                      (mapcar (lambda (part)
                                (let ((copy (copy-sequence part)))
                                  (plist-put copy :file
                                             (gnus-attachment-pane--materialize part directory))))
                              gnus-attachment-pane--parts)
                      gnus-attachment-pane--snapshot-directory directory)
                (tabulated-list-print t))
            (error (delete-directory directory t)
                   (signal (car err) (cdr err)))))))))

(defun gnus-attachment-pane--preview-closed ()
  "Close this preview's attachment panes and restore visible native ones."
  (let ((preview (current-buffer))
        (source (and (boundp 'gnus-webkit-preview--source)
                     gnus-webkit-preview--source))
        closed)
    (dolist (frame (frame-list))
      (dolist (pane (gnus-attachment-pane--windows frame))
        (when (eq (window-parameter pane 'gnus-attachment-pane-source-buffer) preview)
          (setq closed t)
          (gnus-attachment-pane--close-window pane))))
    (when (and closed gnus-attachment-pane-mode (buffer-live-p source))
      (with-current-buffer source (gnus-attachment-pane-display)))))

(defun gnus-attachment-pane-render (snapshot owner window)
  "Render SNAPSHOT's attachments below OWNER's WINDOW without network access.
Exported attachments outlive the temporary MIME parser and belong to the pane."
  (gnus-dispatch-assert-main)
  (when (and gnus-attachment-pane-mode (plist-get snapshot :raw))
    (with-temp-buffer
      (insert (plist-get snapshot :raw))
      (let ((mm-decrypt-option 'never) (mm-verify-option 'never))
        (setq-local gnus-article-mime-handles (mm-dissect-buffer t)))
      (unwind-protect
          (gnus-attachment-pane--adopt-preview (current-buffer) owner window)
        (mm-destroy-parts gnus-article-mime-handles)))))

;;;###autoload
(define-minor-mode gnus-attachment-pane-mode
  "Automatically display a dedicated attachment pane for Gnus articles."
  :global t
  :group 'gnus-attachment-pane
  (if gnus-attachment-pane-mode
      (progn
        (add-hook 'gnus-select-article-hook #'gnus-attachment-pane--after-select t)
        (add-hook 'gnus-webkit-preview-created-functions #'gnus-attachment-pane--adopt-preview)
        (add-hook 'gnus-webkit-preview-close-hook #'gnus-attachment-pane--preview-closed)
        (add-hook 'window-state-change-functions
                  #'gnus-attachment-pane--cleanup-frame))
    (remove-hook 'gnus-select-article-hook #'gnus-attachment-pane--after-select)
    (remove-hook 'gnus-webkit-preview-created-functions #'gnus-attachment-pane--adopt-preview)
    (remove-hook 'gnus-webkit-preview-close-hook #'gnus-attachment-pane--preview-closed)
    (remove-hook 'window-state-change-functions
                 #'gnus-attachment-pane--cleanup-frame)
    (dolist (frame (frame-list))
      (dolist (window (gnus-attachment-pane--windows frame))
        (gnus-attachment-pane--close-window window)))
    (gnus-attachment-pane--kill-unused-buffer)))

(provide 'gnus-attachment-pane)
;;; gnus-attachment-pane.el ends here
