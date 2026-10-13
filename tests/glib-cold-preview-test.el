;;; glib-cold-preview-test.el --- Cold preview publication -*- lexical-binding: t; -*-

;; A reader uses a timer to publish its body and open the first preview.
;; Keep the main thread in thread-join until that timer becomes due, so a
;; worker I/O wait gets the opportunity to steal it on an unpatched core.
;; Never use personal configuration or the user's daemon for this test.
(require 'cl-lib)
(require 'webkit)
(let* ((directory (make-temp-file "gnus-cold-preview-" t))
       (document (expand-file-name "index.html" directory))
       callback-thread failure preview
       (timer
        (run-at-time 0.03 nil
                     (lambda ()
                       (setq callback-thread (current-thread))
                       (if (not (eq callback-thread main-thread))
                           (setq failure "Cold preview callback ran on a worker")
                         (condition-case err
                             (setq preview (webkit-new (concat "file://" document) nil t))
                           (error (setq failure (error-message-string err)))))))))
  (unwind-protect
      (progn
        (with-temp-file document (insert "<html><body>Cold preview fixture</body></html>"))
        (thread-join
         (make-thread
          (lambda ()
            (let ((deadline (+ (float-time) 0.15)))
              (while (< (float-time) deadline)
                (accept-process-output nil 0.01)))) "cold-preview-worker"))
        (let ((deadline (+ (float-time) 2)))
          (while (and (not callback-thread) (< (float-time) deadline))
            (accept-process-output nil 0.01)))
        (unless (and (eq callback-thread main-thread) preview (not failure))
          (error "Cold preview failed: %s" failure))
        ;; Exercise the actual child-process launch completion and GC path.
        (accept-process-output nil 0.4)
        (kill-buffer preview)
        (garbage-collect)
        (accept-process-output nil 0.05)
        (princ "glib-cold-preview-test: PASS\n" #'external-debugging-output))
    (cancel-timer timer)
    (when (buffer-live-p preview) (kill-buffer preview))
    (delete-directory directory t)))
(kill-emacs 0)
