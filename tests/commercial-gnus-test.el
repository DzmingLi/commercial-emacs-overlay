;;; async-test.el --- Exercise Commercial Gnus's actual scheduler -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'gnus-start)
(require 'gnus-group)

(ert-deftest commercial-gnus-refresh-yields-and-finishes ()
  ;; Keep the real gnus-get-unread-articles, scheduler and method runner.
  ;; Simulate a network wait at the backend boundary, without user state.
  (let* ((gnus-background-get-unread-articles t)
         (gnus-select-method '(nnnil ""))
         (gnus-select-methods '((nnnil "")))
         (gnus-newsrc-alist '(dummy))
         (gnus-group-list-mode '(2))
         (heartbeats 0)
         (done nil)
         (backend-started nil)
         (saved-hooks (default-value 'gnus-after-getting-new-news-hook))
         (timer (run-at-time 0 0.02 (lambda () (cl-incf heartbeats)))))
    (unwind-protect
        (progn
          ;; GNU Emacs threads do not inherit a caller's dynamic bindings.
          (set-default 'gnus-after-getting-new-news-hook
                       (list (lambda () (setq done t))))
        (cl-letf (((symbol-function 'gnus-get-function) (lambda (&rest _) #'ignore))
                  ((symbol-function 'gnus-check-backend-function) (lambda (&rest _) nil))
                  ((symbol-function 'gnus-method-denied-p) (lambda (&rest _) nil))
                  ((symbol-function 'gnus-open-server) (lambda (&rest _) t))
                  ((symbol-function 'gnus-group-list-groups) (lambda (&rest _) nil))
                  ((symbol-function 'gnus-read-active-for-groups)
                   (lambda (&rest _)
                     (setq backend-started t)
                     (let ((process (make-process :name "commercial-test-wait"
                                                  :command '("sleep" "0.4")
                                                  :noquery t)))
                       (while (process-live-p process)
                         (accept-process-output process 0.05)))))
                  ((symbol-function 'gnus-archive-server-wanted-p) (lambda () nil)))
          (let ((start (float-time)))
            (gnus-get-unread-articles 2)
            (should (< (- (float-time) start) 0.2)))
          (let ((deadline (+ (float-time) 5)))
            (while (and (not done) (< (float-time) deadline))
              (accept-process-output nil 0.02)))
          (should backend-started)
          (should done)
          (should (> heartbeats 5))
          (princ (format "MAIN-LOOP-HEARTBEATS %d\n" heartbeats))))
      (set-default 'gnus-after-getting-new-news-hook saved-hooks)
      (cancel-timer timer))))

(ert-deftest commercial-gnus-libraries-come-from-one-tree ()
  (dolist (feature '(gnus gnus-start gnus-group gnus-sum message mm-decode nnheader))
    (require feature)
    (should (file-in-directory-p (symbol-file feature 'provide)
                                commercial-gnus--directory))))

(ert-deftest commercial-gnus-message-editing-flushes-syntax-cache ()
  (require 'message)
  (with-temp-buffer
    (message-mode)
    (insert "Subject: Test\n\nBody")
    (should (memq #'syntax-ppss-flush-cache before-change-functions))))

(ert-deftest commercial-gnus-display-predicate-without-agent ()
  (require 'gnus-sum)
  (let ((gnus-agent nil)
        (gnus-summary-display-cache nil))
    (should (functionp (gnus-summary-display-make-predicate '(unread))))))

(ert-deftest commercial-gnus-notifications-run-on-main-thread ()
  (let* ((reported nil)
         (commercial-gnus-group-updated-hook
          (list (lambda (group) (setq reported (cons group (current-thread))))))
         worker)
    ;; Normal foreground updates are not background source completions.
    (commercial-gnus--notify-group "foreground")
    (should-not reported)
    (setq worker
          (make-thread (lambda () (commercial-gnus--notify-group "example"))
                       "gnus-get-unread-articles"))
    (let ((deadline (+ (float-time) 2)))
      (while (and (not reported) (< (float-time) deadline))
        (accept-process-output nil 0.01)))
    (thread-join worker)
    (should (equal (car reported) "example"))
    (should (eq (cdr reported) main-thread))))
