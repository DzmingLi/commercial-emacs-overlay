;;; gnus-reading-test.el --- Reading slot and pure snapshot renderer -*- lexical-binding: t; -*-
(require 'ert)
(require 'gnus-thread-reader)
(require 'gnus-webkit-preview)

(ert-deftest gnus-reading-reuses-slot-without-growing-window-count ()
  (save-window-excursion
    (delete-other-windows)
    (let ((summary (generate-new-buffer " *reading-summary*"))
          (first (generate-new-buffer " *reading-first*"))
          (second (generate-new-buffer " *reading-second*"))
          (gnus-reading-renderer-function (lambda (_) 'text)))
      (unwind-protect
          (cl-letf (((symbol-function 'gnus-thread-reader-article-snapshot)
                     (lambda (&rest _) '(:id "1" :group "nnrss:feed" :article 1))))
            (switch-to-buffer summary)
            (gnus-reading-display first summary)
            (let ((slot (selected-window)))
              (should (= (length (window-list)) 2))
              (select-window (get-buffer-window summary))
              (gnus-reading-display second summary)
              (should (eq slot (selected-window)))
              (should (= (length (window-list)) 2))
              (should (eq (window-buffer slot) second))))
        (mapc #'kill-buffer (list first second summary))))))

(ert-deftest gnus-reading-late-body-cannot-replace-another-selection ()
  (let (rendered)
    (cl-letf (((symbol-function 'gnus-thread-reader-article-snapshot)
               (lambda (&rest _) '(:id "new")))
              ((symbol-function 'gnus-reading-present)
               (lambda (&rest _) (setq rendered t))))
      (gnus-reading--body-ready "old")
      (should-not rendered)
      (gnus-reading--body-ready "new")
      (should rendered))))

(ert-deftest gnus-reading-renderer-does-not-fetch-or-mark ()
  (save-window-excursion
    (let ((reader (generate-new-buffer " *snapshot-reader*")) preview)
      (unwind-protect
          (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                    ((symbol-function 'gnus-request-article) (lambda (&rest _) (error "Unexpected I/O")))
                    ((symbol-function 'gnus-summary-mark-article) (lambda (&rest _) (error "Unexpected mark")))
                    ((symbol-function 'webkit-new)
                     (lambda (&rest _) (setq preview (generate-new-buffer " *snapshot-browser*"))
                       (switch-to-buffer preview) preview)))
            (provide 'webkit)
            (switch-to-buffer reader)
            (gnus-webkit-preview-render
             '(:id "stable" :group "nnimap+test:INBOX" :article 7
               :raw "Message-ID: <fixture>\nContent-Type: text/html\n\n<p>Fixture</p>")
             (selected-window) reader)
            (should (eq (window-buffer) preview))
            (with-current-buffer preview (gnus-webkit-preview-quit))
            (should (eq (window-buffer) reader)))
        (when (buffer-live-p preview) (kill-buffer preview))
        (kill-buffer reader)))))

(ert-deftest gnus-reading-image-callback-crosses-main-gateway ()
  (let ((view (generate-new-buffer " *image-view*"))
        (retrieval (generate-new-buffer " *image-http*")) callback committed)
    (unwind-protect
        (progn
          (with-current-buffer view
            (setq-local thread-reader--render-token 'fixture
                        thread-reader-dispatch-function #'gnus-dispatch-submit)
            (cl-letf (((symbol-function 'url-queue-retrieve)
                       (lambda (_url fn &rest _) (setq callback fn)))
                      ((symbol-function 'shr-tag-img)
                       (lambda (&rest _)
                         (url-queue-retrieve "fixture"
                          (lambda (&rest _)
                            (setq committed (list (current-thread) (current-buffer))))))))
              (thread-reader--shr-image '(img nil))))
          (thread-join (make-thread (lambda () (with-current-buffer retrieval (funcall callback nil)))))
          (should-not committed)
          (gnus-dispatch-drain)
          (should (equal committed (list main-thread retrieval))))
      (kill-buffer view) (kill-buffer retrieval))))

(ert-deftest gnus-reading-pending-selection-retires-old-html-snapshot ()
  (save-window-excursion
    (let ((reader (generate-new-buffer " *pending-reader*"))
          (preview (generate-new-buffer " *old-html*")) requested)
      (unwind-protect
          (progn
            (switch-to-buffer preview)
            (setq-local gnus-webkit-preview-mode t gnus-webkit-preview--thread reader)
            (with-current-buffer reader
              (setq-local gnus-reading--window (selected-window)
                          gnus-reading--renderer 'webkit)
              (cl-letf (((symbol-function 'gnus-thread-reader-article-snapshot)
                         (lambda (&rest _) '(:id "new" :raw nil)))
                        ((symbol-function 'gnus-thread-reader-request-body)
                         (lambda (id) (setq requested id))))
                (gnus-reading-present)))
            (should (equal requested "new"))
            (should (eq (window-buffer) reader))
            (should-not (buffer-live-p preview)))
        (when (buffer-live-p preview) (kill-buffer preview))
        (kill-buffer reader)))))
