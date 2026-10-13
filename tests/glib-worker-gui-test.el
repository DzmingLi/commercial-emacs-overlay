;;; glib-worker-gui-test.el --- Isolated native GUI stress -*- lexical-binding: t; -*-

;; Run in a fresh graphical Emacs with glib-main-thread-probe.so preloaded.
;; No personal configuration, subscriptions, credentials or mail are used.
(require 'cl-lib)
(require 'webkit)
(princ (format "glib-worker-gui-test: module %s\n" (locate-library "webkit-module"))
       #'external-debugging-output)

(let* ((directory (make-temp-file "gnus-gtk-worker-test-" t))
       (document (expand-file-name "index.html" directory))
       (deadline (+ (float-time) 8))
       (workers nil)
       (done 0)
       (errors nil)
       (heartbeat 0)
       (timer (run-at-time 0 0.02 (lambda () (cl-incf heartbeat))))
       preview)
  (unwind-protect
      (progn
        (with-temp-file document
          (insert "<html><body><p>Isolated worker/GTK regression.</p></body></html>"))
        (dotimes (index 3)
          (push
           (make-thread
            (lambda ()
              (unwind-protect
                  (condition-case err
                      (while (< (float-time) deadline)
                        ;; IMAP's UTF-7 conversion and buffer cleanup were
                        ;; on the crashing worker's stack. Add I/O waits.
                        (with-temp-buffer
                          (insert (make-string 1000 ?x))
                          (encode-coding-string (buffer-string) 'utf-7))
                        (accept-process-output nil 0.002))
                    (error (push (error-message-string err) errors)))
                (cl-incf done)))
            (format "gtk-test-worker-%d" index))
           workers))
        (dotimes (_ 16)
          (when (buffer-live-p preview) (kill-buffer preview))
          ;; Suppress process-query prompts, as the Gnus preview does.
          (setq preview (webkit-new (concat "file://" document) nil t))
          (accept-process-output nil 0.08))
        (while (< done 3) (accept-process-output nil 0.02))
        (when errors (error "Worker errors: %S" errors))
        (unless (> heartbeat 20) (error "Main loop stopped responding"))
        (princ (format "glib-worker-gui-test: PASS (%d heartbeats)\n" heartbeat)
               #'external-debugging-output))
    (cancel-timer timer)
    (dolist (worker workers)
      (when (thread-live-p worker) (thread-signal worker 'quit nil)))
    (when (buffer-live-p preview) (kill-buffer preview))
    (delete-directory directory t)))

(kill-emacs 0)
