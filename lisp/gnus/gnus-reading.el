;;; gnus-reading.el --- One reading slot and renderer policy -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; The controller owns the window and selection.  Renderers receive an article
;; snapshot and a live target window, and never initiate a network selection.
;;; Code:
(require 'cl-lib)
(require 'gnus-dispatch)
(defvar gnus-thread-reader-article-loaded-functions)
(defvar gnus-webkit-preview-mode nil)
(defvar gnus-webkit-preview--thread nil)
(defvar gnus-thread-reader--summary nil)
(defvar gnus-attachment-pane-mode nil)
(declare-function gnus-attachment-pane-render "gnus-attachment-pane" (snapshot owner window))
(declare-function gnus-thread-reader-article-snapshot "gnus-thread-reader")
(declare-function gnus-thread-reader-request-body "gnus-thread-reader")
(declare-function gnus-thread-reader--mark-current-read "gnus-thread-reader")
(declare-function gnus-webkit-preview-render "gnus-webkit-preview")

(defgroup gnus-reading nil "Gnus reading area." :group 'gnus)
(defcustom gnus-reading-renderer-function (lambda (_snapshot) 'text)
  "Function choosing `text' or `webkit' for an article snapshot.
A snapshot contains :group, :article, :message-id, :id and optional :raw.
Selection is evaluated on main.  WebKit is loaded only when requested."
  :type 'function :group 'gnus-reading)
(defcustom gnus-reading-width 0.58
  "Fraction of a Summary window given to its new reading area."
  :type 'float :group 'gnus-reading)
(defvar-local gnus-reading--window nil)
(defvar-local gnus-reading--renderer nil)
(defvar-local gnus-reading--presented-id nil)

(defun gnus-reading--show-text ()
  "Return the owned slot to this reader and retire its old browser snapshot."
  (let ((old (window-buffer gnus-reading--window)))
    (set-window-buffer gnus-reading--window (current-buffer))
    (when (and (not (eq old (current-buffer)))
               (buffer-local-value 'gnus-webkit-preview-mode old))
      (kill-buffer old))))

(defun gnus-reading-window (summary)
  "Find SUMMARY's reading slot on the selected frame."
  (cl-find-if (lambda (window)
                (eq (window-parameter window 'gnus-reading-summary) summary))
              (window-list nil 'nomini)))

(defun gnus-reading-display (reader summary)
  "Display READER in SUMMARY's single owned reading slot."
  (gnus-dispatch-assert-main)
  (let* ((window (or (gnus-reading-window summary)
                     (let ((source (or (get-buffer-window summary) (selected-window))))
                       (split-window source
                                     (max window-min-width
                                          (floor (* (window-total-width source)
                                                    (- 1 gnus-reading-width)))) 'right))))
         (previous (window-buffer window)))
    (set-window-parameter window 'gnus-reading-summary summary)
    (set-window-buffer window reader)
    (select-window window)
    (with-current-buffer reader
      (setq gnus-reading--window window)
      (add-hook 'gnus-thread-reader-article-loaded-functions #'gnus-reading--body-ready nil t)
      (add-hook 'post-command-hook #'gnus-reading--after-command t t)
      (gnus-reading-present))
    (when (and (buffer-live-p previous) (not (eq previous reader))
               (buffer-local-value 'gnus-webkit-preview-mode previous))
      (kill-buffer previous))
    reader))

(defun gnus-reading-present (&optional renderer)
  "Render the selected snapshot in the owned window, optionally using RENDERER."
  (gnus-dispatch-assert-main)
  (when renderer (setq gnus-reading--renderer renderer))
  (when (and (window-live-p gnus-reading--window)
             ;; Do not seize a window the user has reassigned to another task.
             (or (eq (window-buffer gnus-reading--window) (current-buffer))
                 (eq (buffer-local-value 'gnus-webkit-preview--thread
                                         (window-buffer gnus-reading--window))
                     (current-buffer))))
    (let* ((snapshot (gnus-thread-reader-article-snapshot))
           (choice (or gnus-reading--renderer
                       (funcall gnus-reading-renderer-function snapshot))))
      (setq gnus-reading--presented-id (plist-get snapshot :id))
      (prog1
          (if (eq choice 'webkit)
              (if (plist-get snapshot :raw)
                  (progn
                    (require 'gnus-webkit-preview)
                    (gnus-webkit-preview-render snapshot gnus-reading--window (current-buffer)))
                ;; The old browser must not remain visible with reply/navigation
                ;; already targeting the new selection while its body is pending.
                (gnus-reading--show-text)
                (gnus-thread-reader-request-body (plist-get snapshot :id))
                nil)
            (gnus-reading--show-text))
        (when (plist-get snapshot :raw)
          (when (and gnus-attachment-pane-mode
                     (eq (window-buffer gnus-reading--window) (current-buffer)))
            (gnus-attachment-pane-render snapshot (current-buffer) gnus-reading--window))
          ;; This is a visit by the controller, never a renderer side effect.
          ;; A mark failure does not invalidate the already displayed body.
          (condition-case err (gnus-thread-reader--mark-current-read)
            (error (message "Gnus read mark: %s" (error-message-string err)))))))))

(defun gnus-reading--body-ready (id)
  "Display ID only if it is still the selected article in this view."
  (when (equal id (plist-get (gnus-thread-reader-article-snapshot) :id))
    (gnus-reading-present)))

(defun gnus-reading--after-command ()
  "Apply renderer and attachment policy when text navigation changes articles."
  (condition-case err
      (unless (equal gnus-reading--presented-id
                     (plist-get (gnus-thread-reader-article-snapshot) :id))
        (gnus-reading-present))
    (error (message "Gnus reading: %s" (error-message-string err)))))

(defun gnus-reading-quit ()
  "Close this reading view and return to its existing Summary window."
  (interactive)
  (gnus-dispatch-assert-main)
  (let* ((reader (if (buffer-live-p gnus-webkit-preview--thread)
                     gnus-webkit-preview--thread (current-buffer)))
         (summary (buffer-local-value 'gnus-thread-reader--summary reader))
         (slot (buffer-local-value 'gnus-reading--window reader))
         (frame (if (window-live-p slot) (window-frame slot) (selected-frame)))
         (source (and (buffer-live-p summary) (get-buffer-window summary frame)))
         preview)
    (unless (buffer-live-p summary) (user-error "The source Summary has closed"))
    ;; Only retire a window still displaying this view.  A user-repurposed
    ;; window belongs to its new task, even if the old slot pointer survives.
    (when (and (window-live-p slot)
               (let ((shown (window-buffer slot)))
                 (or (eq shown reader)
                     (when (eq (buffer-local-value 'gnus-webkit-preview--thread shown) reader)
                       (setq preview shown)))))
      (set-window-parameter slot 'gnus-reading-summary nil)
      (if (and source (not (eq source slot)))
          (delete-window slot)
        (set-window-buffer slot summary)
        (setq source slot)))
    (when (buffer-live-p preview) (kill-buffer preview))
    (kill-buffer reader)
    (when (fboundp 'gnus-attachment-pane--cleanup-frame)
      (gnus-attachment-pane--cleanup-frame frame))
    (if (window-live-p source) (select-window source)
      (pop-to-buffer summary))))

(provide 'gnus-reading)
;;; gnus-reading.el ends here
