;;; gnus-backend-worker.el --- Isolated serial backend actor -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Run only in a -Q --batch child. Requests are data, never evaluated code.
;; stdout is a versioned JSON protocol; ordinary backend messages use stderr.
;;; Code:
(require 'cl-lib)
(require 'json)
(defvar gnus-backend-worker-p t)
(require 'gnus-start)
(require 'gnus-int)
(require 'gnus-sum)
(declare-function nnimap-request-set-mark "nnimap" (group actions &optional server))

(defvar gnus-backend-worker-method nil)

(defun gnus-backend-worker--context (request)
  "Install the request's private Gnus metadata, never the user's newsrc file."
  (setq gnus-select-method gnus-backend-worker-method
        gnus-select-methods (list gnus-backend-worker-method)
        gnus-agent nil gnus-use-cache nil gnus-plugged t
        gnus-read-cached nil gnus-background-get-unread-articles nil
        gnus-newsrc-alist (cons (gnus-info-make "dummy.group" 0 nil)
                               (copy-tree (plist-get request :infos))))
  (gnus-make-hashtable-from-newsrc-alist)
  (setq gnus-active-hashtb (make-hash-table :test #'equal))
  (dolist (entry (plist-get request :active))
    (gnus-set-active (car entry) (cdr entry))))

(defun gnus-backend-worker--check-epoch (request)
  "Verify mailbox identity before reading or changing an IMAP UID."
  (when (and (eq (car gnus-backend-worker-method) 'nnimap)
             (plist-member request :epoch))
    (let* ((info (gnus-get-info (plist-get request :group)))
           (expected (plist-get request :epoch))
           (scan (gnus-retrieve-group-data-early gnus-backend-worker-method (list info)))
           (marks (cdr (assoc (gnus-group-real-name (plist-get request :group)) (cadr scan)))))
      (unless (and expected marks (equal expected (nth 7 marks)))
        (error "Mailbox UIDVALIDITY changed; refresh the group before retrying")))))

(defun gnus-backend-worker--run (request)
  "Execute one allowlisted REQUEST against this actor's connection."
  (pcase (plist-get request :operation)
    ('init
     (setq gnus-backend-worker-method (plist-get request :method))
     (dolist (binding (plist-get request :variables))
       (set (car binding) (cdr binding)))
     (when-let* ((file (plist-get request :init-file))) (load file nil t))
     (require (car gnus-backend-worker-method))
     'ready)
    ((or 'article 'thread 'scan 'marks)
     (gnus-backend-worker--context request)
     (unless (gnus-open-server gnus-backend-worker-method)
       (error "Backend connection unavailable"))
     (with-temp-buffer
       (let ((nntp-server-buffer (current-buffer))
             (gnus-newsgroup-name (plist-get request :group)))
         (pcase (plist-get request :operation)
           ('article
            (gnus-backend-worker--check-epoch request)
            (unless (gnus-request-article (plist-get request :article)
                                          gnus-newsgroup-name (current-buffer))
              (error "Backend article unavailable"))
            (buffer-string))
           ('thread
            (gnus-request-thread (plist-get request :header) gnus-newsgroup-name))
           ('marks
            (gnus-backend-worker--check-epoch request)
            (unless (nnimap-request-set-mark
                     (gnus-group-real-name gnus-newsgroup-name)
                     (plist-get request :action) (cadr gnus-backend-worker-method))
              (error "IMAP mark update unavailable"))
            t)
           ('scan
            (let* ((infos (cdr gnus-newsrc-alist))
                   (early (gnus-retrieve-group-data-early gnus-backend-worker-method infos)))
              (unless (and (eq (car-safe early) 'nnimap-scan-data)
                           (cl-every (lambda (info)
                                       (assoc (gnus-group-real-name (gnus-info-group info))
                                              (cadr early))) infos))
                (error "Incomplete IMAP scan; previous state retained"))
              (gnus-read-active-for-groups gnus-backend-worker-method infos early)
              (dolist (info infos)
                (gnus-get-unread-articles-in-group
                 info (gnus-active (gnus-info-group info)) nil))
              (list :marks (cadr early) :infos infos
                    :active (mapcar (lambda (info)
                                      (cons (gnus-info-group info)
                                            (gnus-active (gnus-info-group info)))) infos))))))))
    (_ (error "Unsupported backend operation"))))

(defun gnus-backend-worker-main ()
  "Read one bounded JSON envelope at a time until the parent closes stdin."
  (unless noninteractive (error "Backend workers require batch mode"))
  (let ((debug-on-error nil) (inhibit-message t) line)
    (condition-case nil
        (while (setq line (read-from-minibuffer ""))
          (when (> (length line) (* 32 1024 1024)) (error "Request too large"))
          (let* ((wire (json-parse-string line :object-type 'plist))
                 (id (plist-get wire :id)) result failure)
            (condition-case err
                (let ((standard-output (get-buffer-create " *backend-output*")))
                  (unless (equal (plist-get wire :version) 1) (error "Protocol mismatch"))
                  (setq result (gnus-backend-worker--run
                                (let ((read-circle nil))
                                  (car (read-from-string (plist-get wire :payload)))))))
              ((error quit) (setq failure (error-message-string err))))
            (let ((print-length nil) (print-level nil) (print-circle nil))
              ;; The serializer returns UTF-8 bytes, but `princ' expects
              ;; text.  Printing the bytes directly emits Emacs raw-byte
              ;; sequences for non-ASCII characters, which are not UTF-8.
              (princ (concat (decode-coding-string
                              (json-serialize
                               (list :version 1 :id id
                                     :result (prin1-to-string result)
                                     :error (or failure :null)))
                              'utf-8) "\n") t))))
      (end-of-file nil))))

(provide 'gnus-backend-worker)
;;; gnus-backend-worker.el ends here
