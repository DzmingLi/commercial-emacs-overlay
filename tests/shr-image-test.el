;;; shr-image-test.el --- Deferred HTML image lifecycle tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'shr)

(defun commercial-gnus-test-image-callback (target start end)
  (let ((response (generate-new-buffer " *shr-test-response*")) cached replaced)
    (with-current-buffer response
      (insert "HTTP/1.1 200 OK\nContent-Type: image/png\n\nbytes")
      (cl-letf (((symbol-function 'url-store-in-cache) (lambda (_) (setq cached t)))
                ((symbol-function 'shr-parse-image-data) (lambda () '("bytes" "image/png")))
                ((symbol-function 'shr-replace-image)
                 (lambda (&rest _) (setq replaced t))))
        (shr-image-fetched nil target start end)))
    (should cached)
    (should-not (buffer-live-p response))
    replaced))

(ert-deftest shr-image-valid-placeholder-survives-header-insertion ()
  (with-temp-buffer
    (insert "Header\n" (propertize "*" 'image-url "https://example.invalid/image"))
    (let ((start (copy-marker 8)) (end (copy-marker 9)))
      (goto-char 1)
      (insert "New header\n")
      (should (commercial-gnus-test-image-callback (current-buffer) start end)))))

(ert-deftest shr-image-erased-placeholder-cannot-write-into-new-article ()
  (with-temp-buffer
    (insert "Header\n" (propertize "*" 'image-url "https://example.invalid/image"))
    (let ((start (copy-marker 8)) (end (copy-marker 9)))
      (erase-buffer)
      (insert "From: New author\n\nNew body")
      (should-not (commercial-gnus-test-image-callback (current-buffer) start end))
      (should (equal (buffer-string) "From: New author\n\nNew body")))))

(ert-deftest shr-image-detached-markers-cannot-write-into-document ()
  (with-temp-buffer
    (insert "Unrelated text")
    (should-not (commercial-gnus-test-image-callback (current-buffer) (make-marker) (make-marker)))))
