;;; gnus-attachment-pane-test.el --- Tests for attachment pane -*- lexical-binding: t; -*-

(require 'ert)
(require 'gnus-attachment-pane)

(defconst gnus-attachment-pane-test--part
  '(:mime-type "application/pdf"
    :attachment-like t
    :filename "report.pdf"
    :decoded-size-approx 1200
    :handle fake-handle))

(ert-deftest gnus-attachment-pane-renders-one-selectable-row-per-part ()
  (let ((buffer
         (gnus-attachment-pane--render-buffer
          (current-buffer) (list gnus-attachment-pane-test--part))))
    (unwind-protect
        (with-current-buffer buffer
          (should (derived-mode-p 'gnus-attachment-pane-list-mode))
          (should hl-line-mode)
          (should (equal (gnus-attachment-pane-current-part)
                         gnus-attachment-pane-test--part))
          (should (string-match-p "report.pdf" (buffer-string)))
          (should (string-match-p "application/pdf" (buffer-string))))
      (kill-buffer buffer))))

(ert-deftest gnus-attachment-pane-default-keymap-supports-row-actions ()
  (with-temp-buffer
    (gnus-attachment-pane-list-mode)
    (should (eq (key-binding (kbd "RET")) #'gnus-attachment-pane-open))
    (should (eq (key-binding (kbd "o")) #'gnus-attachment-pane-open))
    (should (eq (key-binding (kbd "s")) #'gnus-attachment-pane-save))
    (should (eq (key-binding (kbd "S")) #'gnus-attachment-pane-save-all))
    (should (eq (key-binding (kbd "n")) #'gnus-attachment-pane-next-line))
    (should (eq (key-binding (kbd "p")) #'gnus-attachment-pane-previous-line))
    (should (eq (key-binding (kbd "q")) #'gnus-attachment-pane-close))))

(ert-deftest gnus-attachment-pane-actions-use-custom-handlers ()
  (let* ((opened nil)
         (saved nil)
        (gnus-attachment-pane-open-function
         (lambda (part) (setq opened part)))
        (gnus-attachment-pane-save-function
         (lambda (part) (push part saved)))
        (buffer
         (gnus-attachment-pane--render-buffer
          (current-buffer) (list gnus-attachment-pane-test--part))))
    (unwind-protect
        (with-current-buffer buffer
          (gnus-attachment-pane-open)
          (gnus-attachment-pane-save)
          (gnus-attachment-pane-save-all)
          (should (equal opened gnus-attachment-pane-test--part))
          (should (equal saved
                         (list gnus-attachment-pane-test--part
                               gnus-attachment-pane-test--part))))
      (kill-buffer buffer))))

(ert-deftest gnus-attachment-pane-default-save-does-not-overwrite-files ()
  (let* ((directory (make-temp-file "gnus-attachment-pane-test-" t))
         (gnus-attachment-pane-save-directory directory)
         (part (plist-put (copy-sequence gnus-attachment-pane-test--part)
                          :handle (list (current-buffer))))
         first second)
    (unwind-protect
        (cl-letf (((symbol-function 'mm-save-part-to-file)
                   (lambda (_handle path)
                     (write-region "pdf" nil path nil 'silent))))
          (setq first
                (gnus-attachment-pane-save-default
                 part)
                second
                (gnus-attachment-pane-save-default
                 part))
          (should (file-exists-p first))
          (should (file-exists-p second))
          (should-not (equal first second)))
      (delete-directory directory t))))

(defun gnus-attachment-pane-test--mark-pane (pane-window source-window source-buffer)
  "Associate PANE-WINDOW with SOURCE-WINDOW showing SOURCE-BUFFER."
  (set-window-parameter pane-window 'gnus-attachment-pane t)
  (set-window-parameter pane-window
                        'gnus-attachment-pane-source-window source-window)
  (set-window-parameter pane-window
                        'gnus-attachment-pane-source-buffer source-buffer)
  (set-window-dedicated-p pane-window t))

(ert-deftest gnus-attachment-pane-keeps-pane-while-source-view-is-unchanged ()
  (save-window-excursion
    (let* ((source-buffer (generate-new-buffer " *gnus-pane-source*"))
           (pane-buffer (generate-new-buffer " *gnus-pane-list*"))
           (source-window (selected-window))
           (pane-window (split-window source-window nil 'below)))
      (unwind-protect
          (progn
            (set-window-buffer source-window source-buffer)
            (set-window-buffer pane-window pane-buffer)
            (gnus-attachment-pane-test--mark-pane
             pane-window source-window source-buffer)
            (gnus-attachment-pane--cleanup-frame (selected-frame))
            (should (window-live-p pane-window)))
        (when (window-live-p pane-window)
          (set-window-dedicated-p pane-window nil))
        (kill-buffer source-buffer)
        (kill-buffer pane-buffer)))))

(ert-deftest gnus-attachment-pane-closes-when-source-window-changes-buffer ()
  (save-window-excursion
    (let* ((source-buffer (generate-new-buffer " *gnus-pane-source*"))
           (replacement-buffer (generate-new-buffer " *gnus-pane-replacement*"))
           (pane-buffer (generate-new-buffer " *gnus-pane-list*"))
           (source-window (selected-window))
           (pane-window (split-window source-window nil 'below)))
      (unwind-protect
          (progn
            (set-window-buffer source-window source-buffer)
            (set-window-buffer pane-window pane-buffer)
            (gnus-attachment-pane-test--mark-pane
             pane-window source-window source-buffer)
            (set-window-buffer source-window replacement-buffer)
            (gnus-attachment-pane--cleanup-frame (selected-frame))
            (should-not (window-live-p pane-window))
            (should-not (buffer-live-p pane-buffer)))
        (kill-buffer source-buffer)
        (kill-buffer replacement-buffer)
        (kill-buffer pane-buffer)))))

(ert-deftest gnus-attachment-pane-closes-when-source-window-is-deleted ()
  (save-window-excursion
    (let* ((source-buffer (generate-new-buffer " *gnus-pane-source*"))
           (pane-buffer (generate-new-buffer " *gnus-pane-list*"))
           (source-window (selected-window))
           (_other-window (split-window source-window nil 'right))
           (pane-window (split-window source-window nil 'below)))
      (unwind-protect
          (progn
            (set-window-buffer source-window source-buffer)
            (set-window-buffer pane-window pane-buffer)
            (gnus-attachment-pane-test--mark-pane
             pane-window source-window source-buffer)
            (delete-window source-window)
            (gnus-attachment-pane--cleanup-frame (selected-frame))
            (should-not (window-live-p pane-window))
            (should-not (buffer-live-p pane-buffer)))
        (kill-buffer source-buffer)
        (kill-buffer pane-buffer)))))

(ert-deftest gnus-attachment-pane-kills-buffer-after-pane-is-deleted-directly ()
  (save-window-excursion
    (let* ((source-buffer (generate-new-buffer " *gnus-pane-source*"))
           (pane-buffer
            (get-buffer-create gnus-attachment-pane-buffer-name))
           (source-window (selected-window))
           (pane-window (split-window source-window nil 'below)))
      (unwind-protect
          (progn
            (set-window-buffer source-window source-buffer)
            (set-window-buffer pane-window pane-buffer)
            (gnus-attachment-pane-test--mark-pane
             pane-window source-window source-buffer)
            (set-window-dedicated-p pane-window nil)
            (delete-window pane-window)
            (gnus-attachment-pane--cleanup-frame (selected-frame))
            (should-not (buffer-live-p pane-buffer)))
        (kill-buffer source-buffer)
        (kill-buffer pane-buffer)))))

(ert-deftest gnus-attachment-pane-close-kills-an-undisplayed-pane-buffer ()
  (let ((pane-buffer
         (get-buffer-create gnus-attachment-pane-buffer-name)))
    (unwind-protect
        (progn
          (gnus-attachment-pane-close)
          (should-not (buffer-live-p pane-buffer)))
      (kill-buffer pane-buffer))))

(ert-deftest gnus-attachment-pane-snapshot-saves-after-native-mime-is-destroyed ()
  (save-window-excursion
    (let* ((source (generate-new-buffer " *gnus-pane-source*"))
           (preview (generate-new-buffer " *gnus-pane-preview*"))
           (mime (generate-new-buffer " *gnus-pane-mime*"))
           (save-directory (make-temp-file "gnus-pane-saved-" t))
           (gnus-attachment-pane-save-directory save-directory)
           (gnus-attachment-pane-mode nil)
           handle pane snapshot parts)
      (unwind-protect
          (progn
            (with-current-buffer mime (insert "PDFDATA"))
            (setq handle (mm-make-handle mime '("application/pdf") nil nil
                                         '("attachment" (filename . "../../report.pdf"))))
            (with-current-buffer source
              (setq major-mode 'gnus-article-mode)
              (setq-local gnus-article-mime-handles (list handle))
              (setq parts (gnus-attachment-pane--parts))
              (should (= (length parts) 1)))
            (switch-to-buffer source)
            (with-current-buffer source (gnus-attachment-pane-display))
            (setq pane (gnus-attachment-pane--window))
            (should pane)
            (set-window-buffer (selected-window) preview)
            (let ((materialize (symbol-function 'gnus-attachment-pane--materialize)))
              (cl-letf (((symbol-function 'gnus-attachment-pane--materialize)
                         (lambda (part directory)
                           ;; File I/O can redisplay while the browser replaces
                           ;; the native article.  The pane must already belong
                           ;; to that browser when window cleanup runs.
                           (gnus-attachment-pane--cleanup-frame (selected-frame))
                           (should (window-live-p pane))
                           (funcall materialize part directory))))
                (gnus-attachment-pane--adopt-preview source preview (selected-window))))
            (with-current-buffer (window-buffer pane)
              (setq snapshot gnus-attachment-pane--snapshot-directory)
              (should (file-exists-p (expand-file-name "report.pdf" snapshot)))
              (mm-destroy-parts (list handle))
              (gnus-attachment-pane-save)
              (with-temp-buffer
                (insert-file-contents (expand-file-name "report.pdf" save-directory))
                (should (equal (buffer-string) "PDFDATA"))))
            (gnus-attachment-pane--cleanup-frame (selected-frame))
            (should (window-live-p pane))
            (with-current-buffer preview (gnus-attachment-pane--preview-closed))
            (should-not (window-live-p pane))
            (should-not (file-exists-p snapshot)))
        (when (window-live-p pane) (gnus-attachment-pane--close-window pane))
        (when (buffer-live-p mime) (kill-buffer mime))
        (when (buffer-live-p source) (kill-buffer source))
        (when (buffer-live-p preview) (kill-buffer preview))
        (delete-directory save-directory t)))))

(provide 'gnus-attachment-pane-test)
;;; gnus-attachment-pane-test.el ends here

(ert-deftest gnus-attachment-pane-text-snapshot-survives-mime-parser-cleanup ()
  (save-window-excursion
    (delete-other-windows)
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let ((gnus-attachment-pane-mode t)
            (raw "MIME-Version: 1.0\nContent-Type: multipart/mixed; boundary=x\n\n--x\nContent-Type: text/plain\n\nBody\n--x\nContent-Type: application/pdf\nContent-Disposition: attachment; filename=fixture.pdf\n\nPDFDATA\n--x--\n")
            part directory)
        (unwind-protect
            (progn
              (gnus-attachment-pane-render (list :raw raw) (current-buffer) (selected-window))
              (let ((pane (gnus-attachment-pane--window)))
                (should (window-live-p pane))
                (should (>= (window-total-height pane) gnus-attachment-pane-min-window-height))
                (with-current-buffer (window-buffer pane)
                  (setq part (gnus-attachment-pane-current-part)
                        directory gnus-attachment-pane--snapshot-directory)))
              (should (file-exists-p (plist-get part :file)))
              (should-not (buffer-live-p (mm-handle-buffer (plist-get part :handle))))
              (gnus-attachment-pane-close)
              (should-not (file-exists-p directory)))
          (gnus-attachment-pane-close))))))
