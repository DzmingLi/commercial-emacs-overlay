;;; main-thread-timer-test.el --- Timer thread ownership -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)

(ert-deftest main-thread-timers-stay-on-main-during-worker-io ()
  "A worker's I/O wait must not steal a timer publishing GUI state."
  (let* (callback-thread worker-finished
        (timer (run-at-time 0.03 nil
                            (lambda () (setq callback-thread (current-thread))))))
    (unwind-protect
        (let ((worker
               (make-thread
                (lambda ()
                  (let ((deadline (+ (float-time) 0.15)))
                    (while (< (float-time) deadline)
                      (accept-process-output nil 0.01)))
                  (setq worker-finished t)) "timer-io-worker")))
          ;; Let only the worker wait for I/O while the timer becomes due.
          (thread-join worker)
          (should worker-finished)
          (accept-process-output nil 0.05)
          (should (eq callback-thread main-thread)))
      (cancel-timer timer))))
