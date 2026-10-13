;;; gnus-webkit-preview-test.el --- MIME and lifecycle tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'gnus-webkit-preview)
;; Replace only the browser; use Gnus's real MIME decoder and handles.
(provide 'webkit)

(defun gnus-webkit-test--fixture (&optional text-only)
  (let ((buffer (generate-new-buffer " *gnus-webkit-article*")))
    (with-current-buffer buffer
      (setq major-mode 'gnus-article-mode)
      (setq-local gnus-article-mime-handles
                  (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (if text-only
                        (insert "MIME-Version: 1.0\nContent-Type: text/plain\n\nPlain mail\n")
                      (insert
                       "MIME-Version: 1.0\nContent-Type: multipart/related; boundary=outer\n\n"
                       "--outer\nContent-Type: multipart/alternative; boundary=inner\n\n"
                       "--inner\nContent-Type: text/plain; charset=utf-8\n\nPlain mail\n"
                       "--inner\nContent-Type: text/html; charset=utf-8\n"
                       "Content-Transfer-Encoding: base64\n\n"
                       (base64-encode-string
                        (encode-coding-string
                         "<html><head><base href='https://example.invalid/'></head><body>中文邮件<img src='cid:picture%40mail'><style>div {background:url(CID:picture%40mail)}</style></body></html>"
                         'utf-8) t)
                       "\n--inner--\n--outer\nContent-Type: image/png\n"
                       "Content-ID: <picture@mail>\n"
                       "Content-Disposition: inline; filename=../../escape.png\n"
                       "Content-Transfer-Encoding: base64\n\nUE5HREFUQQ==\n--outer--\n"))
                    (let ((raw (buffer-string)))
                      (with-current-buffer buffer (insert raw)))
                    (mm-dissect-buffer t))))
    buffer))

(defmacro gnus-webkit-test--with-preview (&rest body)
  (declare (indent 0))
  `(save-window-excursion
     (let* ((source (gnus-webkit-test--fixture))
            (handles (buffer-local-value 'gnus-article-mime-handles source))
            (gnus-webkit-preview-created-functions nil)
            (gnus-webkit-preview-close-hook nil)
            preview directory)
       (unwind-protect
           (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                     ((symbol-function 'webkit-new)
                      (lambda (&rest _)
                        (let ((buffer (generate-new-buffer " *gnus-webkit-browser*")))
                          (with-current-buffer buffer (setq major-mode 'webkit-mode))
                          (switch-to-buffer buffer)
                          buffer))))
             (switch-to-buffer source)
             (setq preview (gnus-webkit-preview-open)
                   directory (buffer-local-value 'gnus-webkit-preview--directory preview))
             ,@body)
         (when (buffer-live-p preview) (kill-buffer preview))
         (mm-destroy-parts handles)
         (when (buffer-live-p source) (kill-buffer source))))))

(ert-deftest gnus-webkit-preview-decodes-nested-html-and-cid-resources ()
  (gnus-webkit-test--with-preview
    (let ((file (expand-file-name "index.html" directory)))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file)
        (should (string-prefix-p (unibyte-string #xef #xbb #xbf) (buffer-string))))
      (with-temp-buffer
        (insert-file-contents file)
        (should (string-match-p "中文邮件" (buffer-string)))
        (should-not (string-match-p "[cC][iI][dD]:" (buffer-string)))
        (should (string-match-p (regexp-quote (concat "file://" directory "/cid-1.png"))
                                (buffer-string)))))
    (should (equal (directory-files directory nil "^[^.]") '("cid-1.png" "index.html")))
    (with-temp-buffer
      (insert-file-contents-literally (expand-file-name "cid-1.png" directory))
      (should (equal (buffer-string) "PNGDATA")))))

(ert-deftest gnus-webkit-preview-quit-keeps-native-mime-and-returns-to-article ()
  (gnus-webkit-test--with-preview
    (with-current-buffer preview (gnus-webkit-preview-quit))
    (should (eq (window-buffer) source))
    (should (buffer-live-p source))
    (should-not (file-exists-p directory))
    (should (eq handles (buffer-local-value 'gnus-article-mime-handles source)))
    (dolist (part (gnus-webkit-preview--parts handles))
      (should (buffer-live-p (mm-handle-buffer part)))
      (should (stringp (mm-get-part part))))))

(ert-deftest gnus-webkit-preview-snapshot-survives-source-navigation ()
  (gnus-webkit-test--with-preview
    (kill-buffer source)
    (should (buffer-live-p preview))
    (should (file-exists-p (expand-file-name "index.html" directory)))
    (with-current-buffer preview (gnus-webkit-preview-quit))
    (should-not (file-exists-p directory))))

(ert-deftest gnus-webkit-preview-cleans-up-on-renderer-failure ()
  (save-window-excursion
    (let* ((source (gnus-webkit-test--fixture))
           (handles (buffer-local-value 'gnus-article-mime-handles source))
           directory)
      (unwind-protect
          (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                    ((symbol-function 'webkit-new)
                     (lambda (uri &rest _)
                       (setq directory (file-name-directory (substring uri 7)))
                       (switch-to-buffer (get-buffer-create " *failed-webkit*"))
                       (error "Renderer failed"))))
            (switch-to-buffer source)
            (should-error (gnus-webkit-preview-open))
            (should (eq (window-buffer) source))
            (should-not (file-exists-p directory))
            (should (buffer-live-p source)))
        (mm-destroy-parts handles)
        (kill-buffer source)
        (when (get-buffer " *failed-webkit*") (kill-buffer " *failed-webkit*"))))))

(ert-deftest gnus-webkit-preview-plain-text-does-not-create-browser ()
  (let* ((source (gnus-webkit-test--fixture t))
         (handles (buffer-local-value 'gnus-article-mime-handles source)))
    (unwind-protect
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'webkit-new)
                   (lambda (&rest _) (ert-fail "Plain text opened a browser"))))
          (with-current-buffer source
            (should-error (gnus-webkit-preview-open) :type 'user-error)))
      (mm-destroy-parts handles)
      (kill-buffer source))))

(ert-deftest gnus-webkit-preview-summary-selects-current-article ()
  (gnus-webkit-test--with-preview
    (let ((summary (generate-new-buffer " *gnus-webkit-summary*")) selected second)
      (unwind-protect
          (with-current-buffer summary
            (setq major-mode 'gnus-summary-mode)
            (setq-local gnus-article-buffer source)
            (cl-letf (((symbol-function 'gnus-summary-article-number) (lambda () 42))
                      ((symbol-function 'gnus-summary-select-article)
                       (lambda (&rest _) (setq selected t))))
              (setq second (gnus-webkit-preview-open)))
            (should selected)
            (should (eq (buffer-local-value 'gnus-webkit-preview--source second) source)))
        (when (buffer-live-p second) (kill-buffer second))
        (kill-buffer summary)))))

(ert-deftest gnus-webkit-preview-respects-non-utf8-charsets ()
  (let ((part (generate-new-buffer " *gnus-webkit-latin1*")))
    (unwind-protect
        (with-current-buffer part
          (set-buffer-multibyte nil)
          (insert (concat "<p>caf" (unibyte-string #xe9) "</p>"))
          (should (equal (gnus-webkit-preview--html
                          (mm-make-handle part '("text/html" (charset . iso-8859-1))))
                         "<p>café</p>")))
      (kill-buffer part))))

(ert-deftest gnus-webkit-preview-hidden-source-reuses-window-and-returns-to-reader ()
  (gnus-webkit-test--with-preview
    (let ((reader (generate-new-buffer " *thread-return*"))
          (window (selected-window)) second second-directory)
      (unwind-protect
          (progn
            (set-window-buffer window reader)
            (let ((count (length (window-list))))
              (setq second (gnus-webkit-preview-open source window reader)
                    second-directory (buffer-local-value 'gnus-webkit-preview--directory second))
              (should (= (length (window-list)) count)))
            (should (eq (window-buffer window) second))
            (should-not (get-buffer-window source t))
            (with-current-buffer second (gnus-webkit-preview-quit))
            (should (eq (window-buffer window) reader))
            (should-not (file-exists-p second-directory)))
        (when (buffer-live-p second) (kill-buffer second))
        (kill-buffer reader)))))
