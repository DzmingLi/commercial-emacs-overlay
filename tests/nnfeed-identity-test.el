;;; nnfeed-identity-test.el --- Feed identity regressions -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nnatom)
(require 'gnus-sum)
(require 'nnvirtual)

(ert-deftest commercial-gnus-feed-wire-ids-preserve-valid-existing-identities ()
  (should (equal (nnfeed--message-id "<https://example.org/story@blog.nnatom>")
                 "<https://example.org/story@blog.nnatom>"))
  (let ((bad "<https://example.org/story@Farid Zakaria’s Blog.nnatom>"))
    (should (string-match-p "\\`<[[:xdigit:]]+@nnfeed.invalid>\\'"
                            (nnfeed--message-id bad)))
    (should (equal (nnfeed--message-id bad) (nnfeed--message-id bad)))
    (should-not (equal (nnfeed--message-id bad)
                       (nnfeed--message-id "<other@Farid Zakaria’s Blog.nnatom>")))))

(ert-deftest commercial-gnus-feed-summary-and-mime-share-identity-with-legacy-cache ()
  (dolist (stored-id '("<https://example.org/1@Blog With Spaces.nnatom>"
                       "<tag:example.org:1@中文博客.nnatom>"
                       "<valid@blog.nnatom>"))
    (let* ((ids (make-hash-table :test #'equal))
           (articles (make-hash-table :test #'eql))
           (entry (vector stored-id "Author" "Story" '(0 0) '(0 0) nil
                          '(("Body" (Content-Type . "text/plain"))) nil))
           (group (vector "Feed" ids articles 9 9 nil))
           (nntp-server-buffer (generate-new-buffer " *feed-id-test*"))
           (gnus-newsgroup-name "nnvirtual:feed-test")
           header wire)
      (puthash stored-id 9 ids)
      (puthash 9 entry articles)
      (unwind-protect
          (cl-letf (((symbol-function 'nnfeed--group-data) (lambda (&rest _) group)))
            ;; Parse actual HEAD output through Gnus's native parser/NOV bridge.
            (should (eq (nnatom-retrieve-headers '(9) "feed" "server") 'headers))
            (nnvirtual-convert-headers)
            (with-current-buffer nntp-server-buffer
              (goto-char (point-min))
              (setq header (nnheader-parse-nov)))
            (should-not (nnheader-fake-message-id-p (mail-header-id header)))
            (should (= (mail-header-number header) 9))
            (should (equal (nnatom-request-article 9 "feed" "server") '("feed" . 9)))
            (with-current-buffer nntp-server-buffer
              (setq wire (mail-fetch-field "message-id"))
              (should (equal wire (mail-header-id header))))
            ;; Emitted IDs also work for Message-ID article/header requests.
            (should (equal (nnatom-request-article wire "feed" "server") '("feed" . 9)))
            (should (eq (nnatom-retrieve-headers (list wire) "feed" "server") 'headers))
            (with-current-buffer nntp-server-buffer
              (goto-char (point-min))
              (should (search-forward "221 9 Article retrieved." nil t)))
            ;; Cache identities and numeric keys have not migrated.
            (should (equal (aref entry 0) stored-id))
            (should (= (gethash stored-id ids) 9)))
        (kill-buffer nntp-server-buffer)))))
