;;; gnus-reading-gui-test.el --- Actor to native HTML and attachment slot -*- lexical-binding: t; -*-
;; Run in a disposable graphical Emacs -Q, with emacs-webkit on load-path.
;; Uses synthetic mail and a real batch actor; never reads the user's mail.
(require 'gnus-thread-reader)
(require 'gnus-attachment-pane)
(require 'gnus-webkit-preview)
(let* ((fixture (make-temp-file "gnus-reading-actor-" nil ".el"))
       (gnus-backend-worker-init-file fixture)
       (gnus-backend-worker-variables nil)
       (gnus-backend--actors (make-hash-table :test #'equal))
       (method '(nnimap "reading-fixture"))
       (group "nnimap+reading-fixture:INBOX")
       (info (gnus-info-make group 2 '(1) nil method '((uidvalidity . "fixture"))))
       (gnus-newsrc-alist (list (gnus-info-make "dummy.group" 0 nil) info))
       (gnus-newsrc-hashtb (make-hash-table :test #'equal))
       (gnus-active-hashtb (make-hash-table :test #'equal))
       (gnus-select-method method) (gnus-select-methods (list method))
       (gnus-reading-renderer-function (lambda (_) 'webkit))
       (summary (generate-new-buffer " *GUI Summary*"))
       (reader (generate-new-buffer " *GUI Thread*"))
       (header (make-full-mail-header 1 "Synthetic mail" "Fixture" "" "<fixture>" "" 0 0 "" nil))
       (raw "From: Fixture <fixture@example.invalid>\nSubject: Synthetic mail\nMessage-ID: <fixture>\nMIME-Version: 1.0\nContent-Type: multipart/mixed; boundary=x\n\n--x\nContent-Type: text/html\n\n<h1>Actor to WebKit</h1><p>Synthetic body</p>\n--x\nContent-Type: text/plain\nContent-Disposition: attachment; filename=fixture.txt\n\nSynthetic attachment\n--x--\n")
       (beats 0) preview pane timer)
  (unwind-protect
      (progn
        (with-temp-file fixture
          (insert "(require 'nnimap)\n"
                  "(defun gnus-open-server (&rest _) t)\n"
                  "(defun gnus-retrieve-group-data-early (&rest _) '(nnimap-scan-data ((\"INBOX\" nil nil nil nil nil nil nil \"fixture\"))))\n"
                  "(defun gnus-request-article (_number _group buffer)\n"
                  " (accept-process-output nil 0.2)\n"
                  " (with-current-buffer buffer (insert " (prin1-to-string raw) ")) t)\n"))
        (puthash group (list 0 info) gnus-newsrc-hashtb)
        (gnus-set-active group '(1 . 1))
        (with-current-buffer summary
          (setq major-mode 'gnus-summary-mode)
          (setq-local gnus-newsgroup-name group
                      gnus-newsgroup-data (list (gnus-data-make 1 gnus-read-mark nil header 1))))
        (switch-to-buffer summary)
        (delete-other-windows)
        (with-current-buffer reader
          (gnus-thread-reader-mode)
          (setq gnus-thread-reader--summary summary gnus-thread-reader--group group
                gnus-thread-reader--queue '("1") thread-reader--order '("1")
                thread-reader--discussion (make-thread-reader-discussion :id "fixture" :url group))
          (puthash "1" header gnus-thread-reader--headers)
          (puthash "1" (make-thread-reader-entry :id "1" :body "Loading article…") thread-reader--entries)
          (gnus-thread-reader--render))
        (gnus-attachment-pane-mode 1)
        (setq timer (run-at-time 0 0.02 (lambda () (cl-incf beats))))
        (gnus-reading-display reader summary)
        (let ((deadline (+ (float-time) 15)))
          (while (and (< (float-time) deadline)
                      (not (setq preview (cl-find-if
                                           (lambda (b) (buffer-local-value 'gnus-webkit-preview-mode b))
                                           (buffer-list)))))
            (read-event nil nil 0.03)))
        (unless preview
          (error "No browser after actor completion; reader=%s"
                 (with-current-buffer reader (buffer-substring-no-properties (point-min) (point-max)))))
        (setq pane (gnus-attachment-pane--window))
        (unless (and (> beats 3) (= (length (window-list)) 3)
                     (window-live-p pane) (get-buffer-window summary))
          (error "Reading slot or main-loop responsiveness failed"))
        (accept-process-output nil 0.3)
        (with-current-buffer preview (gnus-webkit-preview-quit))
        (unless (and (= (length (window-list)) 2) (get-buffer-window reader))
          (error "Returning from HTML did not restore one reader"))
        (garbage-collect)
        (princ (format "gnus-reading-gui-test: PASS (%d heartbeats)\n" beats)
               #'external-debugging-output))
    (when (timerp timer) (cancel-timer timer))
    (gnus-backend-reset)
    (when (buffer-live-p preview) (kill-buffer preview))
    (kill-buffer reader) (kill-buffer summary)
    (delete-file fixture)))
(kill-emacs 0)
