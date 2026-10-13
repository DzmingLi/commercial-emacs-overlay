;;; glib-ready-source-test.el --- Bound native event draining -*- lexical-binding: t; -*-
(require 'cl-lib)

;; Load glib-ready-source.so first in a fresh graphical Emacs with -Q.
;; The source remains ready until a Lisp timer removes it. With the old
;; unbounded drain, Lisp timers and command input never get control back.
(let ((beats 0) done dispatches
      (deadline (+ (float-time) 2)))
  (let ((heartbeat (run-at-time 0 0.02 (lambda () (cl-incf beats))))
        (stopper (run-at-time 0.3 nil
                              (lambda ()
                                (setq dispatches (glib-test-stop) done t)))))
    (unwind-protect
        (progn
          (glib-test-start)
          (while (and (not done) (< (float-time) deadline))
            (accept-process-output nil 0.01))
          (unless (and done (> beats 3) (> dispatches 0))
            (error "Native event drain starved Lisp timers"))
          (princ (format "glib-ready-source-test: PASS (%d heartbeats, %d dispatches)\n"
                         beats dispatches) #'external-debugging-output))
      (cancel-timer heartbeat)
      (cancel-timer stopper)
      (glib-test-stop))))
(kill-emacs 0)
