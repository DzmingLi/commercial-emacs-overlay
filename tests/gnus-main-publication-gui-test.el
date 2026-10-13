;;; gnus-main-publication-gui-test.el --- Cold browser through real gateway -*- lexical-binding: t; -*-
(require 'gnus-dispatch)
(require 'webkit)
(let (published preview)
  ;; All native entrypoints reject a worker before parsing widget arguments.
  (thread-join
   (make-thread
    (lambda ()
      (mapatoms
       (lambda (symbol)
         (when (and (string-prefix-p "webkit--" (symbol-name symbol))
                    (fboundp symbol) (module-function-p (symbol-function symbol)))
           (let* ((arity (func-arity (symbol-function symbol)))
                  (args (make-list (car arity) nil)) failure)
             (condition-case err (apply symbol args)
               (error (setq failure (error-message-string err))))
             (unless (equal failure "WebKit calls require the main Emacs thread")
               (error "Missing native main-thread guard for %s: %s" symbol failure))))))
      (gnus-dispatch-submit
       (lambda ()
         (setq published (current-thread)
               preview (webkit-new "about:blank" "*Gnus cold preview test*" t))))
      (accept-process-output nil 0.15)) "cold-browser-adversary"))
  (when published (error "Browser callback executed during worker I/O"))
  (let ((deadline (+ (float-time) 5)))
    (while (and (not published) (< (float-time) deadline)) (read-event nil nil 0.05)))
  (unless (eq published main-thread) (error "Main publication did not run"))
  (accept-process-output nil 0.4)
  (kill-buffer preview)
  (garbage-collect)
  (accept-process-output nil 0.05)
  (princ "gnus-main-publication-gui-test: PASS\n" #'external-debugging-output))
(kill-emacs 0)
