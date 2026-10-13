;;; gnus-dispatch.el --- Main-thread commits and bounded async jobs -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; A timer is not a thread handoff on GNU Emacs.  Producers may only enqueue
;; work here; the command loop's private thread event commits it on main.
;; The timer is an opportunistic pump for main-thread I/O waits, never an
;; authorization to execute a callback on whichever thread runs the timer.

;;; Code:
(require 'cl-lib)
(require 'thread)

(defvar gnus-dispatch--queue nil)
(defvar gnus-dispatch--tail nil)
(defvar gnus-dispatch--mutex (make-mutex "gnus-dispatch"))
(defvar gnus-dispatch--draining nil)
(defvar gnus-dispatch--previous-handler nil)
(defvar gnus-dispatch--pump nil)
(defvar gnus-dispatch--installed nil)
(defconst gnus-dispatch--event 'gnus-dispatch-ready)

(defun gnus-dispatch-assert-main ()
  "Signal a recoverable error unless this is the main Lisp thread."
  (unless (eq (current-thread) main-thread)
    (error "Gnus state and display commits require the main thread")))

(defun gnus-dispatch--event (event)
  "Handle our wakeup EVENT; preserve other packages' thread events."
  (interactive "e")
  (if (eq (nth 2 event) gnus-dispatch--event)
      (gnus-dispatch-drain)
    (when (functionp gnus-dispatch--previous-handler)
      (funcall gnus-dispatch--previous-handler event))))

(defun gnus-dispatch--pump ()
  "Commit during main-thread I/O waits, without letting workers publish."
  (when (eq (current-thread) main-thread) (gnus-dispatch-drain)))

(defun gnus-dispatch--install ()
  "Install the command-loop entry once, before starting producers."
  (gnus-dispatch-assert-main)
  (unless gnus-dispatch--installed
    (setq gnus-dispatch--previous-handler
          (lookup-key special-event-map [thread-event]))
    (define-key special-event-map [thread-event] #'gnus-dispatch--event)
    (add-hook 'post-command-hook #'gnus-dispatch-drain)
    (setq gnus-dispatch--pump (run-at-time 0.05 0.05 #'gnus-dispatch--pump)
          gnus-dispatch--installed t)))

(defun gnus-dispatch--wake ()
  "Wake main from a worker; main submissions use the pump or command hook.
Do not put synthetic special events in `unread-command-events': reread
events bypass `special-event-map' and leak into commands and prompts."
  (unless (eq (current-thread) main-thread)
    (thread-signal main-thread gnus-dispatch--event nil)))

(defun gnus-dispatch-submit (function &rest arguments)
  "Queue FUNCTION with ARGUMENTS for a bounded main-thread commit.
Always defer, even on main, so callers finish their state transition first.
ARGUMENTS must be captured result values, not mutable protocol buffers."
  (unless gnus-dispatch--installed
    (error "Load gnus-dispatch on the main thread before starting workers"))
  (let ((cell (list (cons function arguments))) wake)
    (with-mutex gnus-dispatch--mutex
      (setq wake (null gnus-dispatch--queue))
      (if gnus-dispatch--tail (setcdr gnus-dispatch--tail cell)
        (setq gnus-dispatch--queue cell))
      (setq gnus-dispatch--tail cell))
    (when wake (gnus-dispatch--wake)))
  nil)

(defun gnus-dispatch-drain ()
  "Commit at most 32 queued results on main; nested event loops cannot reenter."
  (gnus-dispatch-assert-main)
  (unless gnus-dispatch--draining
    (let ((gnus-dispatch--draining t) (remaining 32) item)
      (while (and (> remaining 0)
                  (setq item
                        (with-mutex gnus-dispatch--mutex
                          (prog1 (pop gnus-dispatch--queue)
                            (unless gnus-dispatch--queue
                              (setq gnus-dispatch--tail nil))))))
        (cl-decf remaining)
        (condition-case err
            (apply (car item) (cdr item))
          ((error quit)
           (message "Gnus commit failed: %s" (error-message-string err)))))
      (when gnus-dispatch--queue
        (gnus-dispatch--wake)))))

(cl-defstruct (gnus-dispatch-job (:constructor gnus-dispatch--make-job))
  "A main-owned task.  A producer can only deliver a completion envelope."
  (state 'queued) worker timer valid success failure cancel-function)

(defun gnus-dispatch-job-pending-p (job)
  "Return whether JOB has not reached a terminal state."
  (and (gnus-dispatch-job-p job)
       (memq (gnus-dispatch-job-state job) '(queued fetching))))

(defun gnus-dispatch--finish (job state value)
  "Settle JOB exactly once and commit VALUE if its owner is still valid."
  (gnus-dispatch-assert-main)
  (when (gnus-dispatch-job-pending-p job)
    (when (timerp (gnus-dispatch-job-timer job))
      (cancel-timer (gnus-dispatch-job-timer job)))
    (unwind-protect
        (let ((valid (gnus-dispatch-job-valid job)))
          (condition-case err
              (if (and valid (not (funcall valid)))
                  (setf (gnus-dispatch-job-state job) 'cancelled)
                (setf (gnus-dispatch-job-state job) state)
                (when-let* ((callback (if (eq state 'ready)
                                          (gnus-dispatch-job-success job)
                                        (gnus-dispatch-job-failure job))))
                  (funcall callback value)))
            ((error quit)
             (setf (gnus-dispatch-job-state job) 'error)
             ;; Rendering may fail after a successful fetch.  Do not call a
             ;; failing error handler a second time, or retain its resources.
             (if (and (eq state 'ready) (gnus-dispatch-job-failure job))
                 (funcall (gnus-dispatch-job-failure job) (error-message-string err))
               (message "Gnus job commit failed: %s" (error-message-string err))))))
      (setf (gnus-dispatch-job-worker job) nil
            (gnus-dispatch-job-timer job) nil
            (gnus-dispatch-job-success job) nil
            (gnus-dispatch-job-failure job) nil
            (gnus-dispatch-job-valid job) nil
            (gnus-dispatch-job-cancel-function job) nil))))

(defun gnus-dispatch-job-cancel (job &optional reason)
  "Cancel JOB on main.  Deliver REASON and reject all late completions."
  (gnus-dispatch-assert-main)
  (when (gnus-dispatch-job-pending-p job)
    (let ((cancel (gnus-dispatch-job-cancel-function job)))
      (unwind-protect
          (when cancel (funcall cancel))
        (gnus-dispatch--finish job 'cancelled (or reason "Loading cancelled"))))))

(cl-defun gnus-dispatch-job-create (success failure &key valid (timeout 60) cancel)
  "Register callbacks and deadline before starting an asynchronous producer.
SUCCESS receives a result; FAILURE receives an error string.  VALID checks
session/view generations on main before either callback can change state."
  (gnus-dispatch-assert-main)
  (let ((job (gnus-dispatch--make-job :success success :failure failure
                                      :valid valid :cancel-function cancel)))
    (when timeout
      (setf (gnus-dispatch-job-timer job)
            (run-at-time timeout nil #'gnus-dispatch-submit
                         #'gnus-dispatch-job-cancel job "Loading timed out")))
    job))

(defun gnus-dispatch-job-deliver (job result &optional failure)
  "Send a producer's RESULT or FAILURE to JOB's main-thread owner."
  (gnus-dispatch-submit #'gnus-dispatch--finish job
                        (if failure 'error 'ready) (or failure result)))

(cl-defun gnus-dispatch-job-start (work success failure &key valid (timeout 60) name)
  "Run legacy WORK in a thread and settle its registered job on main.
WORK must not touch UI or foreground state.  New network adapters should use
`gnus-dispatch-job-create' and isolated processes instead of this bridge."
  (let ((job (gnus-dispatch-job-create success failure :valid valid :timeout timeout)))
    (setf (gnus-dispatch-job-state job) 'fetching
          (gnus-dispatch-job-worker job)
          (make-thread
           (lambda ()
             (condition-case err
                 (gnus-dispatch-job-deliver job (funcall work))
               ((error quit)
                (gnus-dispatch-job-deliver job nil (error-message-string err)))))
           (or name "gnus-job")))
    (setf (gnus-dispatch-job-cancel-function job)
          (lambda ()
            (let ((worker (gnus-dispatch-job-worker job)))
              (when (and worker (thread-live-p worker))
                (thread-signal worker 'quit nil)))))
    job))

(gnus-dispatch--install)
(provide 'gnus-dispatch)
;;; gnus-dispatch.el ends here
