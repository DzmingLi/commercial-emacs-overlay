;;; nnimap-wait-test.el --- Command reply isolation -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nnimap)

(ert-deftest commercial-gnus-imap-send-retains-its-own-tag-across-yield ()
  (let ((nnimap-sequence 5) (nnimap-streaming t)
        (nnimap-object (make-nnimap :newlinep nil)) sent)
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (_process text)
                 (setq sent text)
                 ;; Another connection can send a command while this call yields.
                 (setq nnimap-sequence 99))))
      (should (= (nnimap-send-command "NOOP") 6))
      (should (string-prefix-p "6 NOOP" sent)))))

(ert-deftest commercial-gnus-imap-wait-finds-its-completion-before-a-later-tag ()
  (with-temp-buffer
    (let ((process (make-pipe-process :name "imap-reply-fixture" :buffer (current-buffer) :noquery t))
          (nnimap-object (make-nnimap :initial-resync 0)))
      (unwind-protect
          (progn
            (insert "* 1 EXISTS\r\n11 OK completed\r\n12 OK another command\r\n")
            (cl-letf (((symbol-function 'nnheader-accept-process-output)
                       (lambda (&rest _) (ert-fail "Already-complete reply was missed"))))
              (should (nnimap-wait-for-response 11)))
            (should (looking-at "11 OK completed"))
            (should (equal (caar (nnimap-parse-response)) "OK")))
        (delete-process process)))))

(ert-deftest commercial-gnus-imap-wait-closes-only-the-stalled-connection-on-timeout ()
  (with-temp-buffer
    (let ((process (make-pipe-process :name "imap-stall-fixture" :buffer (current-buffer) :noquery t))
          (nnimap-object (make-nnimap :initial-resync 0))
          (nnimap-response-timeout 0))
      (unwind-protect
          (progn
            (should-error (nnimap-wait-for-response 11))
            (should-not (process-live-p process)))
        (when (process-live-p process) (delete-process process))))))

(ert-deftest commercial-gnus-imap-wait-yields-until-the-correct-complete-line-arrives ()
  (with-temp-buffer
    (let ((process (make-pipe-process :name "imap-delayed-fixture" :buffer (current-buffer) :noquery t))
          (nnimap-object (make-nnimap :initial-resync 0))
          (nnimap-response-timeout 1) waited)
      (unwind-protect
          (progn
            (insert "* 1 FETCH (UID 1 FLAGS ())\r\n11 O")
            (cl-letf (((symbol-function 'nnheader-accept-process-output)
                       (lambda (&rest _)
                         (setq waited t)
                         (goto-char (point-max))
                         (insert "K complete\r\n12 OK another command\r\n"))))
              (should (nnimap-wait-for-response 11)))
            (should waited)
            (should (looking-at "11 OK complete")))
        (delete-process process)))))

(ert-deftest commercial-gnus-imap-does-not-parse-another-reply-after-canceled-wait ()
  (cl-letf (((symbol-function 'nnimap-wait-for-response) (lambda (&rest _) nil))
            ((symbol-function 'nnimap-parse-response)
             (lambda () (ert-fail "Unconfirmed reply was parsed"))))
    (should-not (nnimap-get-response 11))))

(ert-deftest commercial-gnus-imap-wait-skips-fake-tags-in-literal-bodies ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((body (encode-coding-string "Subject: 邮件\r\n\r\n11 OK fake body text\r\n" 'utf-8)))
      (insert (format "* 1 FETCH (BODY[] {%d}\r\n" (length body)) body ")\r\n")
      (should-not (nnimap--find-command-completion 11))
      (goto-char (point-max))
      (let ((start (point)))
        (insert "11 OK real completion\r\n12 OK later command\r\n")
        (should (= (nnimap--find-command-completion 11) start))))))

(ert-deftest commercial-gnus-imap-wait-rejects-incomplete-literals-and-status-lines ()
  (with-temp-buffer
    (insert "* 1 FETCH (BODY[] {999}\r\n11 OK inside incomplete literal\r\n")
    (should-not (nnimap--find-command-completion 11))
    (erase-buffer)
    (insert "11 OK not yet complete")
    (should-not (nnimap--find-command-completion 11))
    (goto-char (point-max)) (insert "\r\n")
    (should (nnimap--find-command-completion 11))))
