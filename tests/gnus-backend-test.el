;;; gnus-backend-test.el --- Exercise real batch processes -*- lexical-binding: t; -*-
(require 'ert)
(require 'gnus-start)
(require 'gnus-backend)

(defmacro gnus-backend-test--session (&rest forms)
  (declare (indent 0))
  `(let* ((gnus-backend--actors (make-hash-table :test #'equal))
          (gnus-backend-worker-variables nil)
          (gnus-backend-worker-init-file (make-temp-file "gnus-worker-fixture-" nil ".el"))
          (method '(nnimap "fixture"))
          (gnus-select-method method) (gnus-select-methods (list method))
          (gnus-newsrc-alist (list (gnus-info-make "dummy.group" 0 nil)
                                  (gnus-info-make "nnimap+fixture:INBOX" 2 nil nil method)))
          (gnus-newsrc-hashtb (make-hash-table :test #'equal))
          (gnus-active-hashtb (make-hash-table :test #'equal))
          result failure callback-thread)
     (unwind-protect
         (progn
           (with-temp-file gnus-backend-worker-init-file
             (insert ";;; -*- lexical-binding: t; -*-\n"
                     "(defun gnus-open-server (&rest _) t)\n"
                     "(defun gnus-request-article (number _group buffer)\n"
                     " (accept-process-output nil 0.15)\n"
                     " (when (= number 99) (error \"fixture retrieval failed\"))\n"
                     " (with-current-buffer buffer (insert (format \"%s:%s\" (emacs-pid) number))) t)\n"))
           ,@forms)
       (maphash (lambda (_ actor) (gnus-backend--broken actor "Test cleanup"))
                (copy-hash-table gnus-backend--actors))
       (gnus-dispatch-drain)
       (delete-file gnus-backend-worker-init-file))))

(defun gnus-backend-test--wait (predicate)
  (let ((deadline (+ (float-time) 8)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02)
      (gnus-dispatch-drain)))
  (should (funcall predicate)))

(ert-deftest gnus-backend-real-process-preserves-unicode-and-raw-bytes ()
  (gnus-backend-test--session
    (let ((expected (concat "Subject: 中文 café 😀\n\n"
                            (string-as-multibyte (unibyte-string 0 128 233 255)))))
      (let ((print-escape-nonascii t) (print-escape-multibyte t))
        (write-region
         (concat "\n(defun gnus-request-article (_number _group buffer)\n"
                 " (with-current-buffer buffer (insert "
                 (prin1-to-string expected) ")) t)\n")
         nil gnus-backend-worker-init-file t 'silent))
      (gnus-backend-request method 'article '(:article 1 :group "nnimap+fixture:INBOX")
                            (lambda (value) (setq result value))
                            (lambda (err) (setq failure err)))
      (gnus-backend-test--wait (lambda () (or failure result)))
      (should-not failure)
      (should (equal result expected)))))

(ert-deftest gnus-backend-real-process-preserves-unicode-requests-and-errors ()
  (gnus-backend-test--session
    (write-region
     "\n(defun gnus-request-article (number group buffer)\n (if (= number 99) (error \"邮件 café 😀\")\n (with-current-buffer buffer (insert group)) t))\n"
     nil gnus-backend-worker-init-file t 'silent)
    (let ((group "nnimap+fixture:收件箱/café/😀"))
      (gnus-backend-request method 'article (list :article 1 :group group)
                            (lambda (value) (setq result value))
                            (lambda (err) (setq failure err)))
      (gnus-backend-test--wait (lambda () (or failure result)))
      (should-not failure)
      (should (equal result group)))
    (gnus-backend-request method 'article '(:article 99 :group "nnimap+fixture:INBOX")
                          #'ignore (lambda (err) (setq failure err)))
    (gnus-backend-test--wait (lambda () failure))
    (should (equal failure "邮件 café 😀"))))

(ert-deftest gnus-backend-real-process-reuses-serial-connection-owner ()
  (gnus-backend-test--session
    (let (results)
      (dotimes (n 2)
        (gnus-backend-request method 'article (list :article (1+ n) :group "nnimap+fixture:INBOX")
                              (lambda (value) (setq callback-thread (current-thread)) (push value results))
                              (lambda (err) (setq failure err))))
      (gnus-backend-test--wait (lambda () (or failure (= (length results) 2))))
      (should-not failure)
      (should (eq callback-thread main-thread))
      (let ((first (split-string (cadr results) ":")) (second (split-string (car results) ":")))
        (should (equal (car first) (car second)))
        (should-not (= (string-to-number (car first)) (emacs-pid)))
        (should (equal (cadr first) "1"))
        (should (equal (cadr second) "2"))))))

(ert-deftest gnus-backend-failure-is-distinct-from-empty-result ()
  (gnus-backend-test--session
    (gnus-backend-request method 'article '(:article 99 :group "nnimap+fixture:INBOX")
                          (lambda (value) (setq result value))
                          (lambda (err) (setq failure err)))
    (gnus-backend-test--wait (lambda () failure))
    (should-not result)
    (should (equal failure "fixture retrieval failed"))))

(ert-deftest gnus-backend-cancel-active-settles-and-discards-late-result ()
  (gnus-backend-test--session
    (let ((job (gnus-backend-request method 'article '(:article 1 :group "nnimap+fixture:INBOX")
                                     (lambda (value) (setq result value))
                                     (lambda (err) (setq failure err)))))
      (gnus-backend-test--wait (lambda () (eq (gnus-dispatch-job-state job) 'fetching)))
      (gnus-dispatch-job-cancel job)
      (gnus-backend-test--wait (lambda () failure))
      (should (eq (gnus-dispatch-job-state job) 'cancelled))
      (should-not result))))

(ert-deftest gnus-backend-start-failure-still-settles-registered-job ()
  (let (failure)
    (cl-letf (((symbol-function 'gnus-backend--actor)
               (lambda (_) (error "Cannot launch backend"))))
      (let ((job (gnus-backend-request '(nnimap "failure") 'article nil #'ignore
                                       (lambda (reason) (setq failure reason)))))
        (should (gnus-dispatch-job-pending-p job))
        (gnus-dispatch-drain)
        (should (eq (gnus-dispatch-job-state job) 'error))
        (should (equal failure "Cannot launch backend"))))))

(ert-deftest gnus-backend-restart-discards-old-process-completion ()
  (gnus-backend-test--session
    (let* ((session gnus-newsrc-hashtb)
           (job (gnus-backend-request method 'article '(:article 1 :group "nnimap+fixture:INBOX")
                                      (lambda (value) (setq result value))
                                      (lambda (err) (setq failure err))
                                      :valid (lambda () (eq session gnus-newsrc-hashtb)))))
      (gnus-backend-test--wait (lambda () (eq (gnus-dispatch-job-state job) 'fetching)))
      (setq gnus-newsrc-hashtb (make-hash-table :test #'equal))
      (gnus-backend-ensure-session)
      (gnus-dispatch-drain)
      (should (eq (gnus-dispatch-job-state job) 'cancelled))
      (should-not result)
      (should-not failure)
      (should (zerop (hash-table-count gnus-backend--actors))))))

(ert-deftest gnus-backend-final-response-before-exit-is-not-lost ()
  (let* ((gnus-backend--actors (make-hash-table :test #'equal))
         result failure
         (job (gnus-dispatch-job-create (lambda (value) (setq result value))
                                        (lambda (reason) (setq failure reason)) :timeout nil))
         (process (make-pipe-process :name "gnus-frame-fixture" :noquery t))
         (actor (gnus-backend--make-actor :method '(nnimap "frames") :process process
                                         :active (list 7 job nil))))
    (unwind-protect
        (progn
          (thread-join
           (make-thread
            (lambda ()
              (gnus-backend--filter actor process "{\"version\":1,\"id\":7,\"result\":\"42\",\"error\":null}\n")
              (gnus-dispatch-submit #'gnus-backend--broken actor "Process exited"))))
          (should-not result)
          (should (equal (gnus-backend--actor-input actor) ""))
          (gnus-dispatch-drain)
          (should (= result 42))
          (should-not failure)
          (should (eq (gnus-dispatch-job-state job) 'ready)))
      (when (process-live-p process) (delete-process process)))))

(ert-deftest gnus-backend-exit-flushes-queued-marks-before-session-reset ()
  (let* ((gnus-backend--actors (make-hash-table :test #'equal))
         completed
         (job (gnus-dispatch-job-create (lambda (_) (setq completed t)) #'ignore :timeout nil))
         (actor (gnus-backend--make-actor :active (list 1 job '(:operation marks)))))
    (puthash '(nnimap "exit") actor gnus-backend--actors)
    (gnus-dispatch-job-deliver job t)
    (gnus-backend-flush-marks)
    (should completed)))
