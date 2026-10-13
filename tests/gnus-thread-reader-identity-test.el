;;; gnus-thread-reader-identity-test.el --- Refresh identity regressions -*- lexical-binding: t; -*-
(require 'ert)
(require 'gnus-thread-reader)

(defun gnus-reader-identity-test--header (number source-number message-id)
  (make-full-mail-header number "Title" "Author" "" message-id "" 0 0
                         (format "server nnrss:real.feed:%d" source-number) nil))

(defmacro gnus-reader-identity-test--session (&rest body)
  (declare (indent 0))
  `(save-window-excursion
     (let* ((summary (generate-new-buffer " *identity-summary*"))
            (reader (generate-new-buffer " *identity-reader*"))
            (original (gnus-reader-identity-test--header 10 7 "<original@test>"))
            (unrelated (gnus-reader-identity-test--header 10 99 "<other@test>"))
            (current (gnus-reader-identity-test--header 33 7 "<original@test>"))
            marked inserted)
       (unwind-protect
           (progn
             (with-current-buffer summary
               (setq major-mode 'gnus-summary-mode)
               (setq-local gnus-newsgroup-name "nnvirtual:identity-test"
                           gnus-newsgroup-data
                           (list (gnus-data-make 10 gnus-unread-mark nil unrelated 1)
                                 (gnus-data-make 33 gnus-read-mark nil current 2))
                           gnus-newsgroup-dependencies (make-hash-table :test #'equal)
                           gnus-newsgroup-articles '(10 33)))
             (switch-to-buffer reader)
             (gnus-thread-reader-mode)
             (setq gnus-thread-reader--summary summary
                   gnus-thread-reader--group "nnvirtual:identity-test"
                   gnus-thread-reader--anchor 10
                   gnus-thread-reader--anchor-id "<original@test>")
             (puthash "10" original gnus-thread-reader--headers)
             (puthash "10" (make-thread-reader-entry :id "10" :body "Body" :body-format 'plain)
                      thread-reader--entries)
             (setq thread-reader--order '("10"))
             (let ((inhibit-read-only t))
               (insert (propertize "Body\n" 'thread-reader-id "10")))
             (goto-char (point-min))
             (cl-letf (((symbol-function 'nnvirtual-possibly-change-server) #'ignore)
                       ((symbol-function 'gnus-find-method-for-group)
                        (lambda (&rest _) '(nnvirtual "identity-server")))
                       ((symbol-function 'nnvirtual-reverse-map-article)
                        (lambda (group number)
                          (should (equal group "nnrss:real.feed"))
                          (pcase number (7 33) (8 34))))
                       ((symbol-function 'nnvirtual-map-article)
                        (lambda (number)
                          (cons "nnrss:real.feed" (pcase number (33 7) (34 8)))))
                       ((symbol-function 'gnus-thread-reader--schedule) #'ignore))
               ,@body))
         (when (buffer-live-p reader) (kill-buffer reader))
         (when (buffer-live-p summary) (kill-buffer summary))))))

(ert-deftest gnus-reader-identity-remaps-without-changing-entry-or-mime ()
  (gnus-reader-identity-test--session
    (puthash "10" "original MIME" gnus-thread-reader--raw-articles)
    (should (= (gnus-thread-reader--target) 33))
    (should (equal (gnus-thread-reader-current-article-id) "10"))
    (should (equal (gnus-thread-reader-raw-article "10") "original MIME"))
    (should (= (mail-header-number original) 10))))

(ert-deftest gnus-reader-identity-restores-removed-read-row-only-for-explicit-action ()
  (gnus-reader-identity-test--session
    (with-current-buffer summary (setq gnus-newsgroup-data (list (car gnus-newsgroup-data))))
    (should (= (gnus-thread-reader--target) 33))
    (should-not (with-current-buffer summary (gnus-summary-article-header 33)))
    (cl-letf (((symbol-function 'gnus-summary-insert-subject)
               (lambda (number header use-header)
                 (should use-header)
                 (setq inserted header)
                 (push (gnus-data-make number gnus-read-mark nil header 2) gnus-newsgroup-data)))
              ((symbol-function 'gnus-summary-mark-article)
               (lambda (number mark) (setq marked (cons number mark))))
              ((symbol-function 'gnus-set-mode-line) #'ignore)
              ((symbol-function 'gnus-thread-reader--render) #'ignore))
      (gnus-thread-reader-mark-unread))
    (should (equal marked (cons 33 gnus-unread-mark)))
    (should (equal (mail-header-id inserted) "<original@test>"))
    (should (equal (with-current-buffer summary (mail-header-id (gnus-summary-article-header 10)))
                   "<other@test>"))))

(ert-deftest gnus-reader-identity-rejects-genuine-source-replacement ()
  (gnus-reader-identity-test--session
    (with-current-buffer summary
      (setf (gnus-data-header (cadr gnus-newsgroup-data))
            (gnus-reader-identity-test--header 33 7 "<replacement@test>")))
    (should-error (gnus-thread-reader--target) :type 'user-error)))

(ert-deftest gnus-reader-identity-restores-a-real-gnus-row-and-native-unread-mark ()
  (gnus-reader-identity-test--session
    (with-current-buffer summary
      (setq-local gnus-summary-buffer summary
                  gnus-newsgroup-headers (list unrelated)
                  gnus-newsgroup-threads nil
                  gnus-newsgroup-data nil
                  gnus-newsgroup-limit '(10)
                  gnus-newsgroup-unreads '(10)
                  gnus-show-threads t)
      (gnus-update-format-specifications 'summary 'summary-mode 'summary-dummy)
      (gnus-update-summary-mark-positions)
      (gnus-dependencies-add-header unrelated gnus-newsgroup-dependencies t)
      (gnus-summary-prepare))
    (let ((gnus-use-cache nil)
          (gnus-newsrc-hashtb (make-hash-table :test #'equal)))
      (setq gnus-thread-reader--session gnus-newsrc-hashtb)
      (cl-letf (((symbol-function 'gnus-request-update-mark)
                 (lambda (_group _number mark) mark))
                ((symbol-function 'gnus-set-mode-line) #'ignore)
                ((symbol-function 'gnus-thread-reader--render) #'ignore))
        (gnus-thread-reader-mark-unread)))
    (with-current-buffer summary
      (should (equal (mail-header-id (gnus-summary-article-header 33)) "<original@test>"))
      (should (eq (gnus-summary-article-mark 33) gnus-unread-mark))
      (should (eq (gnus-summary-article-mark 10) gnus-unread-mark))
      (should (equal gnus-newsgroup-unreads '(10 33))))))

(ert-deftest gnus-reader-identity-fetches-original-source-and-checks-wire-message-id ()
  (gnus-reader-identity-test--session
    (cl-letf (((symbol-function 'gnus-request-article-this-buffer)
               (lambda (number group)
                 (should (= number 7))
                 (should (equal group "nnrss:real.feed"))
                 (insert "Message-ID: <original@test>\nContent-Type: text/plain\n\nOriginal body") t)))
      (should (equal (car (gnus-thread-reader--fetch original t)) '(plain . "Original body"))))
    (cl-letf (((symbol-function 'gnus-request-article-this-buffer)
               (lambda (&rest _)
                 (insert "Message-ID: <replacement@test>\nContent-Type: text/plain\n\nWrong body") t)))
      (should-error (gnus-thread-reader--fetch original t)))))

(ert-deftest gnus-reader-identity-refresh-keeps-read-root-reply-links-mime-and-folds ()
  (gnus-reader-identity-test--session
    (let ((reply (gnus-reader-identity-test--header 20 8 "<reply@test>")))
      (puthash "20" reply gnus-thread-reader--headers)
      (puthash "20" (make-thread-reader-entry :id "20" :parent-id "10") thread-reader--entries)
      (setq thread-reader--order '("10" "20") gnus-thread-reader-focus-ids '("20"))
      (puthash "10" '(plain . "root") gnus-thread-reader--bodies)
      (puthash "20" '(plain . "reply") gnus-thread-reader--bodies)
      (puthash "10" "root MIME" gnus-thread-reader--raw-articles)
      (puthash "20" "reply MIME" gnus-thread-reader--raw-articles)
      (puthash "10" t thread-reader--collapsed)
      (with-current-buffer summary (setq gnus-newsgroup-data nil))
      (cl-letf (((symbol-function 'gnus-remove-thread) #'ignore)
                ((symbol-function 'gnus-thread-reader--render) #'ignore))
        (gnus-thread-reader-refresh))
      (should (= gnus-thread-reader--anchor 33))
      (should (= (mail-header-number (gethash "20" gnus-thread-reader--headers)) 34))
      (should (equal (thread-reader-entry-parent-id (gethash "20" thread-reader--entries)) "10"))
      (should (equal (gnus-thread-reader-raw-article "10") "root MIME"))
      (should (equal (gnus-thread-reader-raw-article "20") "reply MIME"))
      (should (gethash "10" thread-reader--collapsed))
      (should (equal gnus-thread-reader-focus-ids '("20"))))))

(ert-deftest gnus-reader-identity-late-body-does-not-mark-reused-virtual-number ()
  (gnus-reader-identity-test--session
    (with-current-buffer summary (setq gnus-newsgroup-data (list (car gnus-newsgroup-data))))
    (cl-letf (((symbol-function 'gnus-thread-reader--render) #'ignore)
              ((symbol-function 'gnus-summary-mark-article)
               (lambda (&rest args) (push args marked))))
      (gnus-thread-reader--publish-body
       reader gnus-thread-reader--generation "10" original
       '((plain . "Body") "real.feed" "raw MIME") nil))
    (should-not marked)
    (should (equal (gnus-thread-reader-raw-article "10") "raw MIME"))))

(ert-deftest gnus-reader-identity-canonicalizes-legacy-atom-mime-without-accepting-another-id ()
  (gnus-reader-identity-test--session
    (let* ((legacy "<https://feed.invalid/story@Blog With Spaces.nnatom>")
           (canonical (format "<%s@nnfeed.invalid>" (secure-hash 'sha256 legacy)))
           (header (copy-sequence original))
           (wire legacy))
      (setf (mail-header-id header) canonical
            (mail-header-xref header) "server nnatom+feed.invalid/atom.xml:feed:7")
      (cl-letf (((symbol-function 'gnus-find-method-for-group)
                 (lambda (&rest _) '(nnatom "feed.invalid/atom.xml")))
                ((symbol-function 'nnfeed--message-id)
                 (lambda (id) (format "<%s@nnfeed.invalid>" (secure-hash 'sha256 id))))
                ((symbol-function 'gnus-request-article-this-buffer)
                 (lambda (number group)
                   (should (= number 7))
                   (should (equal group "nnatom+feed.invalid/atom.xml:feed"))
                   (insert "Message-ID: " wire "\nContent-Type: text/plain\n\nCached body") t)))
        (let ((result (gnus-thread-reader--fetch header t)))
          (should (equal (car result) '(plain . "Cached body")))
          (with-temp-buffer
            (insert (nth 2 result))
            (should (equal (mail-fetch-field "message-id") canonical))))
        (setq wire "<https://feed.invalid/different@Blog With Spaces.nnatom>")
        (should-error (gnus-thread-reader--fetch header t))))))

(ert-deftest gnus-reader-mailbox-reset-rejects-reused-uid-even-with-same-message-id ()
  (with-temp-buffer
    (let* ((gnus-newsrc-hashtb (make-hash-table :test #'equal))
           (source '("nnimap+fixture:INBOX" . 7))
           (info (gnus-info-make (car source) 2 nil nil '(nnimap "fixture") '((uidvalidity . "old")))))
      (puthash (car source) (list 1 info) gnus-newsrc-hashtb)
      (gnus-thread-reader-mode)
      (should (equal (gnus-thread-reader--check-source-epoch source) "old"))
      (setf (gnus-info-params info) '((uidvalidity . "new")))
      (should-error (gnus-thread-reader--check-source-epoch source) :type 'user-error))))
