;;; commercial-gnus.el --- Commercial Gnus for GNU Emacs -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: news, mail
;; URL: https://github.com/DzmingLi/commercial-gnus
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Load before any Gnus, Message or Gnus MIME library.  The bundled Gnus
;; uses its upstream thread scheduler; this file only selects that library
;; tree and forwards completed-group notifications to the main event loop.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst commercial-gnus-upstream-revision
  "bf4184f985d0674a7c03c12eb33b66ac833d9e29")

(defconst commercial-gnus--directory
  (expand-file-name "lisp/gnus/"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defvar commercial-gnus-group-updated-hook nil
  "Hook called with a group name on the main thread after a background update.
Consumers may refresh their views.  This hook does not perform network work.")

(defun commercial-gnus--notify-group (group)
  "Forward the completed GROUP update from Gnus's scanner thread."
  (when (and (not (eq (current-thread) main-thread))
             (equal (thread-name (current-thread))
                    "gnus-get-unread-articles"))
    (run-at-time 0 nil #'commercial-gnus--run-group-hook group)))

(defun commercial-gnus--run-group-hook (group)
  "Run view hooks for GROUP without aborting the remaining refreshes."
  (condition-case err
      (run-hook-with-args 'commercial-gnus-group-updated-hook group)
    (error (message "Commercial Gnus view update: %s"
                    (error-message-string err)))))

;;;###autoload
(defun commercial-gnus-enable ()
  "Select the bundled Gnus before any competing library has been loaded.
Restart Emacs to change Gnus implementations; live replacement is unsupported."
  (unless (featurep 'commercial-gnus-enabled)
    (when (version< emacs-version "31.1")
      (error "Commercial Gnus requires GNU Emacs 31.1 or later"))
    (let ((libraries (mapcar #'file-name-base
                            (directory-files commercial-gnus--directory t "\\.el\\'"))))
      (dolist (entry load-history)
        (when-let* ((file (car entry))
                    (name (file-name-base (string-remove-suffix ".gz" file))))
          (when (and (member name libraries)
                     (not (file-in-directory-p file commercial-gnus--directory)))
            (error "Gnus library %s already loaded from %s; restart Emacs"
                   name file)))))
    (add-to-list 'load-path commercial-gnus--directory)
    (load (expand-file-name "commercial-gnus-loaddefs.el"
                            commercial-gnus--directory) nil t)
    (with-eval-after-load 'gnus-start
      (setq gnus-background-get-unread-articles t))
    (with-eval-after-load 'gnus-group
      (advice-add 'gnus-group-update-group :after
                  (lambda (group &rest _) (commercial-gnus--notify-group group))))
    (provide 'commercial-gnus-enabled)))

;;;###autoload
(commercial-gnus-enable)

(provide 'commercial-gnus)
;;; commercial-gnus.el ends here
