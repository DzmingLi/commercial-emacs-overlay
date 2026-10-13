;;; gnus-thread-reader-test.el --- Reader mark behavior -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
;; Load source so the Gnus mark accessors remain replaceable in this test.
(load (expand-file-name "../lisp/gnus/gnus-thread-reader.el"
                        (file-name-directory load-file-name)) nil t)

(ert-deftest gnus-thread-reader-loads-image-queue-on-cold-start ()
  "Image wrappers must capture a loaded function, not an autoload object."
  (let ((source-dir (file-name-directory (locate-library "thread-reader"))))
    (with-temp-buffer
      (should
       (zerop
        (call-process
         (expand-file-name invocation-name invocation-directory)
         nil t nil "--batch" "-Q" "-L" source-dir "--eval"
         "(progn (setq load-prefer-newer t) (require 'shr)
            (unless (autoloadp (symbol-function 'url-queue-retrieve))
              (error \"Expected a cold-start autoload\"))
            (require 'thread-reader)
            (when (autoloadp (symbol-function 'url-queue-retrieve))
              (error \"Image queue remains an autoload\")))"))))))

(ert-deftest gnus-thread-reader-marks-visited-loaded-articles-only ()
  (let ((summary (generate-new-buffer " *gnus-summary-test*"))
        (reader (generate-new-buffer " *gnus-reader-test*"))
        (marks (make-hash-table :test #'eql))
        (marked nil))
    (unwind-protect
        (with-current-buffer reader
          (gnus-thread-reader-mode)
          (let ((inhibit-read-only t))
            (insert "root\nreply\n")
            (put-text-property 1 5 'thread-reader-id "1")
            (put-text-property 6 11 'thread-reader-id "2"))
          (setq gnus-thread-reader--summary summary)
          (dolist (number '(1 2))
            (puthash (number-to-string number)
                     (make-full-mail-header
                      number "Subject" "Author" "" (format "<%d@test>" number)
                      "" 0 0 "" nil)
                     gnus-thread-reader--headers)
            (puthash number gnus-unread-mark marks))
          (puthash "1" '(plain . "root") gnus-thread-reader--bodies)
          (cl-letf (((symbol-function 'gnus-thread-reader--source)
                     (lambda () summary))
                    ((symbol-function 'gnus-thread-reader--checked-header)
                     (lambda (number &rest _)
                       (gethash (number-to-string number) gnus-thread-reader--headers)))
                    ((symbol-function 'gnus-summary-article-mark)
                     (lambda (number) (gethash number marks)))
                    ((symbol-function 'gnus-summary-mark-article)
                     (lambda (number mark)
                       (push number marked)
                       (puthash number mark marks)))
                    ((symbol-function 'gnus-set-mode-line)
                     (lambda (&rest _) nil)))
            (goto-char 1)
            (should (gnus-thread-reader--mark-current-read))
            (should (equal marked '(1)))
            ;; A manual unread mark persists until this article is left.
            (puthash 1 gnus-unread-mark marks)
            (should-not (gnus-thread-reader--mark-current-read))
            (goto-char 6)
            (should-not (gnus-thread-reader--mark-current-read))
            (puthash "2" '(plain . "reply") gnus-thread-reader--bodies)
            (should (gnus-thread-reader--mark-current-read))
            (should (equal marked '(2 1)))
            (goto-char 1)
            (should (gnus-thread-reader--mark-current-read))
            (should (equal marked '(1 2 1)))))
      (kill-buffer reader)
      (kill-buffer summary))))

(ert-deftest gnus-thread-reader-summary-policy-survives-group-parameters ()
  (with-temp-buffer
    (let ((gnus-newsgroup-threads nil))
      (setq-local gnus-show-threads nil)
      (setq-local gnus-thread-hide-subtree nil)
      (gnus-thread-reader-summary-mode 1)
      ;; Group parameters are applied after mode hooks on group entry.
      (setq-local gnus-show-threads nil)
      (setq-local gnus-thread-hide-subtree nil)
      (run-hooks 'gnus-summary-generate-hook)
      (should gnus-show-threads)
      (should gnus-thread-hide-subtree)
      (should (eq (lookup-key gnus-thread-reader-summary-mode-map
                              (kbd "RET"))
                  #'gnus-thread-reader-open))
      (gnus-thread-reader-summary-mode -1)
      (should-not gnus-show-threads)
      (should-not gnus-thread-hide-subtree)
      (should-not (memq #'gnus-thread-reader--summary-policy
                       gnus-summary-generate-hook)))))

(ert-deftest gnus-thread-reader-summary-enable-folds-existing-summary ()
  (with-temp-buffer
    (let ((gnus-newsgroup-threads '(existing-thread))
          folded)
      (cl-letf (((symbol-function 'gnus-summary-hide-all-threads)
                 (lambda (&optional predicate)
                   (should-not predicate)
                   (setq folded t))))
        (gnus-thread-reader-summary-mode 1)
        (should folded)
        ;; Re-enabling must not overwrite the saved native settings.
        (gnus-thread-reader-summary-mode 1)
        (gnus-thread-reader-summary-mode -1)
        (should-not (local-variable-p 'gnus-thread-hide-subtree))))))

(ert-deftest gnus-thread-reader-fetch-preserves-decoded-unicode ()
  (let ((header (make-full-mail-header 1 "Title" "Author" "" "<1@test>"
                                       "" 0 0 "" nil))
        (text "中文與數學，café"))
    (cl-letf (((symbol-function 'gnus-thread-reader--source)
               (lambda () (current-buffer)))
              ((symbol-function 'gnus-thread-reader--checked-header)
               (lambda (&rest _) t))
              ((symbol-function 'gnus-request-article-this-buffer)
               (lambda (&rest _)
                 ;; Backends such as nnrss write already decoded text.
                 (insert "Content-Type: text/plain; charset=gnus-decoded\n"
                         "Content-Transfer-Encoding: 8bit\n\n" text)
                 t)))
      (should (equal (gnus-thread-reader--fetch header) (cons 'plain text))))))

(ert-deftest gnus-thread-reader-fetch-decodes-wire-utf8 ()
  (let ((header (make-full-mail-header 1 "Title" "Author" "" "<1@test>"
                                       "" 0 0 "" nil))
        (text "<p>中文與數學，café</p>"))
    (cl-letf (((symbol-function 'gnus-thread-reader--source)
               (lambda () (current-buffer)))
              ((symbol-function 'gnus-thread-reader--checked-header)
               (lambda (&rest _) t))
              ((symbol-function 'gnus-request-article-this-buffer)
               (lambda (&rest _)
                 (insert "Content-Type: text/html; charset=utf-8\n"
                         "Content-Transfer-Encoding: base64\n\n"
                         (base64-encode-string (encode-coding-string text 'utf-8)))
                 t)))
      (should (equal (gnus-thread-reader--fetch header) (cons 'html text))))))

;;; gnus-thread-reader-test.el ends here

(ert-deftest gnus-thread-reader-image-renderer-accepts-shr-media-url ()
  ;; SHR dispatches linked images and video posters with a second argument,
  ;; and a video poster can have no DOM at all.
  (with-temp-buffer
    (let ((shr-external-rendering-functions
           '((img . thread-reader--shr-image)))
          calls)
      (cl-letf (((symbol-function 'shr-tag-img)
                 (lambda (dom &optional url)
                   (push (list (dom-tag dom) (dom-attr dom 'alt) url) calls))))
        (shr-indirect-call 'img '(img ((src . "inline.png"))))
        (shr-indirect-call 'img '(a nil) "linked.png")
        (shr-indirect-call 'img nil "poster.png"))
      (should (equal (nreverse calls)
                     '((img "[图片]" nil)
                       (a "[图片]" "linked.png")
                       (img "[图片]" "poster.png")))))))

(ert-deftest gnus-thread-reader-image-marker-follows-body-indentation ()
  (with-temp-buffer
    (let (request)
      (cl-letf (((symbol-function 'url-queue-retrieve)
                 (lambda (_url callback &optional args &rest _)
                   (setq request (cons callback args))))
                ((symbol-function 'shr-tag-img)
                 (lambda (_dom &optional _url)
                   (let ((start (point-marker)))
                     (insert (propertize "*" 'image-url "https://example.org/image"))
                     (url-queue-retrieve "https://example.org/image"
                                         #'shr-image-fetched
                                         (list (current-buffer) start (point-marker)))))))
        (thread-reader--body
         (make-thread-reader-entry :body-format 'html :body "<img src='image'>")))
      (let ((start (nth 2 request)) (end (nth 3 request)))
        (should (< start end))
        (should (equal (get-text-property start 'image-url)
                       "https://example.org/image"))
        (should (equal (buffer-substring-no-properties start end) "*"))))))

(ert-deftest gnus-thread-reader-does-not-display-backend-routing-key ()
  (with-temp-buffer
    (thread-reader-mode)
    (setq thread-reader--url "nnvirtual:timeline"
          thread-reader--discussion (make-thread-reader-discussion :title "Article"))
    (thread-reader--render)
    (should (equal (buffer-string) "Article\n\n"))
    (setf (thread-reader-discussion-url thread-reader--discussion) "https://example.org/post")
    (thread-reader--render)
    (should (equal (buffer-string) "Article\nhttps://example.org/post\n\n"))))

(ert-deftest gnus-thread-reader-root-is-article-replies-remain-outline ()
  (with-temp-buffer
    (gnus-thread-reader-mode)
    (let ((header (make-full-mail-header 1 "Root subject" "Alice <alice@example.org>"
                                         "Sun, 4 Oct 2026 00:00:00 +0000"
                                         "<root@example.org>" "" 0 0 nil nil)))
      (puthash "1" header gnus-thread-reader--headers)
      (puthash "1" "real.group" gnus-thread-reader--article-groups)
      (setq thread-reader--url "nnvirtual:timeline"
            thread-reader--discussion (make-thread-reader-discussion :title "Root subject"))
      (thread-reader--merge
       (list (make-thread-reader-entry :id "1" :author "Alice" :body "Root body")
             (make-thread-reader-entry :id "2" :parent-id "1" :author "Bob" :body "Reply body")) t)
      (thread-reader--render)
      (goto-char (point-min))
      (should (looking-at "From: Alice"))
      (should (search-forward "Subject: Root subject\nNewsgroups: real.group\n" nil t))
      (should-not (string-match-p "nnvirtual:timeline" (buffer-string)))
      (goto-char (gethash "1" thread-reader--positions))
      (thread-reader-next)
      (should (looking-at "\\*\\* Bob"))
      (thread-reader-parent)
      (should (looking-at "From: Alice"))
      (goto-char (gethash "2" thread-reader--positions))
      (thread-reader-toggle)
      (should (invisible-p (save-excursion (search-forward "Reply body") (1- (point)))))
      (should-not (invisible-p (save-excursion (goto-char (point-min))
                                             (search-forward "Root body") (1- (point))))))))
