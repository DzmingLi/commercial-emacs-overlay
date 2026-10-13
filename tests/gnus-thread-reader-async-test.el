;;; gnus-thread-reader-async-test.el --- Main loop responsiveness -*- lexical-binding: t; -*-
(require 'ert)
(require 'gnus-thread-reader)

(defmacro gnus-reader-async-test--session (&rest body)
  (declare (indent 0))
  `(save-window-excursion
     (let* ((summary (generate-new-buffer " *async-summary*"))
            (reader (generate-new-buffer " *async-reader*"))
            (header (make-full-mail-header 1 "Title" "Author" "" "<1@test>" "" 0 0 "" nil))
            (gnus-newsrc-hashtb (make-hash-table :test #'equal))
            (gnus-select-method '(nnnil ""))
            (beats 0) fetched published marked done
            (heartbeat (run-at-time 0 0.02 (lambda () (cl-incf beats)))))
       (unwind-protect
           (progn
             (with-current-buffer summary
               (setq major-mode 'gnus-summary-mode)
               (setq-local gnus-newsgroup-name "test.group"
                           gnus-newsgroup-data (list (gnus-data-make 1 gnus-unread-mark nil header 1))))
             (switch-to-buffer reader)
             (gnus-thread-reader-mode)
             (setq gnus-thread-reader--summary summary gnus-thread-reader--group "test.group"
                   gnus-thread-reader--queue '("1"))
             (puthash "1" header gnus-thread-reader--headers)
             (puthash "1" (make-thread-reader-entry :id "1") thread-reader--entries)
             (add-hook 'gnus-thread-reader-article-loaded-functions
                       (lambda (_id) (setq published (current-thread) done t)) nil t)
             (cl-letf (((symbol-function 'gnus-thread-reader--render) (lambda () nil))
                       ((symbol-function 'gnus-thread-reader--mark-current-read)
                        (lambda () (setq marked (current-thread))))
                       ((symbol-function 'gnus-request-article-this-buffer)
                        (lambda (&rest _)
                          (setq fetched (current-thread))
                          (gnus-reader-async-test--network-wait)
                          (insert "Content-Type: text/html; charset=utf-8\nNewsgroups: real.group\n\n<p>body</p>")
                          t)))
               ,@body))
         (cancel-timer heartbeat)
         (with-current-buffer reader (gnus-thread-reader--cancel))
         (kill-buffer reader)
         (kill-buffer summary)))))

(defun gnus-reader-async-test--network-wait ()
  "Simulate a slow backend at its I/O boundary."
  (let ((process (make-process :name "reader-test-delay" :command '("sleep" "0.3") :noquery t)))
    (while (process-live-p process) (accept-process-output process 0.02))))

(defun gnus-reader-async-test--wait (predicate)
  (let ((deadline (+ (float-time) 3)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01)))
  (should (funcall predicate)))

(ert-deftest gnus-thread-reader-body-fetch-yields-and-publishes-on-main-thread ()
  (gnus-reader-async-test--session
    (let ((start (float-time)))
      (gnus-thread-reader--load-one reader gnus-thread-reader--generation)
      (should (< (- (float-time) start) 0.1)))
    (gnus-reader-async-test--wait (lambda () done))
    (should (> beats 5))
    (should (not (eq fetched main-thread)))
    (should (eq published main-thread))
    (should (eq marked main-thread))
    (should (equal (gethash "1" gnus-thread-reader--bodies) '(html . "<p>body</p>")))
    (should (string-match-p "Content-Type:" (gnus-thread-reader-raw-article "1")))
    (should (equal (gethash "1" gnus-thread-reader--article-groups) "real.group"))
    (princ (format "BODY-MAIN-LOOP-HEARTBEATS %d\n" beats))))

(ert-deftest gnus-thread-reader-cancel-discards-old-body-completion ()
  (gnus-reader-async-test--session
    (gnus-thread-reader--load-one reader gnus-thread-reader--generation)
    (gnus-reader-async-test--wait (lambda () fetched))
    (let ((worker (gnus-dispatch-job-worker gnus-thread-reader--worker)))
      (gnus-thread-reader--cancel)
      (gnus-reader-async-test--wait (lambda () (not (thread-live-p worker)))))
    (accept-process-output nil 0.05)
    (should-not done)
    (should-not (gnus-thread-reader-raw-article "1"))
    (should-not marked)))

(ert-deftest gnus-thread-reader-thread-header-fetch-yields-and-merges-on-main-thread ()
  (gnus-reader-async-test--session
    (setq gnus-thread-reader--queue nil)
    (cl-letf (((symbol-function 'gnus-request-thread)
               (lambda (&rest _)
                 (setq fetched (current-thread))
                 (gnus-reader-async-test--network-wait)
                 (list header)))
              ((symbol-function 'gnus-summary-goto-subject) (lambda (&rest _) t))
              ((symbol-function 'gnus-summary-refer-thread)
               (lambda (&rest _)
                 (should (equal (gnus-request-thread header "test.group") (list header)))
                 (setq published (current-thread))))
              ((symbol-function 'gnus-thread-reader-refresh) (lambda (&rest _) (setq done t))))
      (let ((start (float-time)))
        (gnus-thread-reader--complete-thread reader header)
        (should (< (- (float-time) start) 0.1)))
      (gnus-reader-async-test--wait (lambda () done)))
    (should (> beats 5))
    (should (not (eq fetched main-thread)))
    (should (eq published main-thread))
    (princ (format "HEADERS-MAIN-LOOP-HEARTBEATS %d\n" beats))))

(ert-deftest gnus-thread-reader-repeated-pending-body-request-is-idempotent ()
  (gnus-reader-async-test--session
    (setq gnus-thread-reader--queue nil
          gnus-thread-reader--loading-id "1"
          gnus-thread-reader--worker (gnus-dispatch-job-create #'ignore #'ignore :timeout nil))
    (gnus-thread-reader-request-body "1")
    (gnus-thread-reader-request-body "1")
    (should-not gnus-thread-reader--queue)
    (should-not gnus-thread-reader--timer)))

(ert-deftest gnus-thread-reader-setup-failure-replaces-loading-placeholder ()
  (gnus-reader-async-test--session
    (cl-letf (((symbol-function 'gnus-find-method-for-group)
               (lambda (&rest _) (error "Backend unavailable during setup"))))
      (gnus-thread-reader--load-one reader gnus-thread-reader--generation))
    (should-not gnus-thread-reader--loading-id)
    (should-not gnus-thread-reader--worker)
    (should (string-match-p "Loading failed: Backend unavailable during setup"
                            (thread-reader-entry-body (gethash "1" thread-reader--entries))))))

(ert-deftest gnus-thread-reader-session-restart-invalidates-pending-view ()
  (gnus-reader-async-test--session
    (should (gnus-thread-reader--valid-request-p reader gnus-thread-reader--generation))
    (let ((gnus-newsrc-hashtb (make-hash-table :test #'equal)))
      (should-not (gnus-thread-reader--valid-request-p reader gnus-thread-reader--generation)))))
