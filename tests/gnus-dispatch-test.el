;;; gnus-dispatch-test.el --- Real scheduling and lifecycle tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'gnus-dispatch)

(ert-deftest gnus-dispatch-worker-io-cannot-run-commits ()
  (let (published)
    (thread-join
     (make-thread
      (lambda ()
        (gnus-dispatch-submit (lambda () (setq published (current-thread))))
        (gnus-dispatch--pump)
        ;; A timer fired by worker I/O tries the optional pump.  It must not
        ;; commit anything even when the main thread is blocked in join.
        (run-at-time 0 nil #'gnus-dispatch--pump)
        (accept-process-output nil 0.15)) "dispatch-adversary"))
    (should-not published)
    (gnus-dispatch-drain)
    (should (eq published main-thread))))

(ert-deftest gnus-dispatch-fifo-bounded-and-non-reentrant ()
  (let (order)
    (dotimes (n 40)
      (gnus-dispatch-submit
       (lambda ()
         (push n order)
         (gnus-dispatch-drain)
         (push (cons 'after n) order))))
    (gnus-dispatch-drain)
    (should (= (length order) 64))
    (should (equal (car order) '(after . 31)))
    (gnus-dispatch-drain)
    (should (equal (nreverse order)
                   (cl-loop for n below 40 append (list n (cons 'after n)))))))

(ert-deftest gnus-dispatch-job-cancel-rejects-late-success ()
  (let (events)
    (let ((job (gnus-dispatch-job-create
                (lambda (_) (push 'success events))
                (lambda (_) (push 'cancel events)) :timeout nil)))
      (gnus-dispatch-job-cancel job)
      (gnus-dispatch-job-deliver job 'late)
      (gnus-dispatch-drain)
      (should (eq (gnus-dispatch-job-state job) 'cancelled))
      (should (equal events '(cancel))))))

(ert-deftest gnus-dispatch-stale-owner-cannot-publish ()
  (let (events)
    (let ((job (gnus-dispatch-job-create
                (lambda (_) (push 'success events))
                (lambda (_) (push 'error events)) :valid (lambda () nil) :timeout nil)))
      (gnus-dispatch-job-deliver job 'old)
      (gnus-dispatch-drain)
      (should-not events)
      (should (eq (gnus-dispatch-job-state job) 'cancelled)))))

(ert-deftest gnus-dispatch-render-error-terminates-job ()
  (let (failure)
    (let ((job (gnus-dispatch-job-create
                (lambda (_) (error "renderer failed"))
                (lambda (err) (setq failure err)) :timeout nil)))
      (gnus-dispatch-job-deliver job 'body)
      (gnus-dispatch-drain)
      (should (eq (gnus-dispatch-job-state job) 'error))
      (should (equal failure "renderer failed")))))

(ert-deftest gnus-dispatch-timeout-is-main-thread-terminal-state ()
  (let (owner)
    (let ((job (gnus-dispatch-job-create #'ignore
                (lambda (_) (setq owner (current-thread))) :timeout 0.01)))
      (thread-join (make-thread (lambda () (accept-process-output nil 0.1))))
      (should-not owner)
      ;; Patched Emacs defers timers until main resumes; stock GNU Emacs
      ;; may already have enqueued the timeout during the worker wait.
      (let ((deadline (+ (float-time) 2)))
        (while (and (not owner) (< (float-time) deadline))
          ;; Consume wakeups just as the main command loop does.  A fixed
          ;; I/O sleep can return early while those events are still pending.
          (read-event nil nil 0.02)
          (gnus-dispatch-drain)))
      (should (eq owner main-thread))
      (should (eq (gnus-dispatch-job-state job) 'cancelled)))))

(ert-deftest gnus-dispatch-forwards-unrelated-thread-events ()
  (let* (forwarded
         (gnus-dispatch--previous-handler (lambda (event) (setq forwarded event)))
         (event (list 'thread-event main-thread 'other-package '(value))))
    (gnus-dispatch--event event)
    (should (eq forwarded event))))

(ert-deftest gnus-dispatch-failing-error-handler-still-releases-job ()
  (let* ((calls 0)
         (job (gnus-dispatch-job-create
               (lambda (_) (error "render error"))
               (lambda (_) (cl-incf calls) (error "error display unavailable")) :timeout nil)))
    (gnus-dispatch-job-deliver job 'body)
    (gnus-dispatch-drain)
    (should (= calls 1))
    (should (eq (gnus-dispatch-job-state job) 'error))
    (should-not (gnus-dispatch-job-success job))
    (should-not (gnus-dispatch-job-failure job))
    (should-not (gnus-dispatch-job-valid job))))
