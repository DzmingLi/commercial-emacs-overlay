;;; gnus-backend-state.el --- Main-owned IMAP scan transactions -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Code:
(require 'gnus-backend)
(require 'gnus-start)
(require 'gnus-sum)
(require 'nnimap)
(require 'range)
(defvar gnus-backend--scans (make-hash-table :test #'equal))

(defvar gnus-backend-state--sequence 0)
(defvar gnus-backend-state--marks (make-hash-table :test #'equal))

(defun gnus-backend-state--reset ()
  "Release records belonging to a closed session."
  (clrhash gnus-backend--scans)
  (clrhash gnus-backend-state--marks)
  (setq gnus-backend-state--sequence 0))
(add-hook 'gnus-backend-reset-hook #'gnus-backend-state--reset)

(defun gnus-backend-state--apply-mark (info number mark)
  "Commit one user MARK to the native INFO record."
  (setf (gnus-info-read info)
        (if (memq mark (list gnus-unread-mark gnus-ticked-mark gnus-dormant-mark gnus-spam-mark))
            (range-difference (gnus-info-read info) (list number))
          (range-concat (gnus-info-read info) (list number))))
  (dolist (kind `((tick . ,gnus-ticked-mark) (dormant . ,gnus-dormant-mark)))
    (setf (alist-get (car kind) (gnus-info-marks info))
          (if (eq mark (cdr kind))
              (range-concat (cdr (assq (car kind) (gnus-info-marks info))) (list number))
            (range-difference (cdr (assq (car kind) (gnus-info-marks info))) (list number))))))

(defun gnus-backend-note-mark (group number mark)
  "Record a local Summary action by source key and queue its IMAP write."
  (let* ((source (if (string-prefix-p "nnvirtual:" group)
                     (nnvirtual-find-group-art (gnus-group-real-name group) number)
                   (cons group number)))
         (method (and source (gnus-find-method-for-group (car source)))))
    (when (gnus-backend-process-p method)
      (gnus-dispatch-assert-main)
      (gnus-backend-ensure-session)
      (let* ((session gnus-newsrc-hashtb)
             (info (gnus-get-info (car source)))
             (epoch (cdr (assq 'uidvalidity (gnus-info-params info))))
             (key (list session source epoch))
             (entry (list (cl-incf gnus-backend-state--sequence) mark 'pending))
             (unread (memq mark (list gnus-unread-mark gnus-dormant-mark gnus-spam-mark)))
             (flags (append (unless unread '(read)) (when (eq mark gnus-ticked-mark) '(tick)))))
        (gnus-backend-state--apply-mark info (cdr source) mark)
        (puthash key entry gnus-backend-state--marks)
        (gnus-backend-request
         method 'marks (list :group (car source) :epoch epoch
                             :action (list (list (list (cdr source)) 'del
                                                 (cl-set-difference '(read tick) flags))
                                           (list (list (cdr source)) 'add flags)))
         (lambda (_)
           ;; Acknowledgements never rewrite native read state.
           (setf (nth 2 entry) 'acknowledged))
         (lambda (reason)
           (setf (nth 2 entry) 'failed)
           (message "Gnus mark not confirmed: %s" reason))
         :valid (lambda () (eq session gnus-newsrc-hashtb)))))))

(defun gnus-backend-state--rebase-range (remote base local)
  "Apply the user's BASE to LOCAL changes on top of REMOTE ranges."
  (range-difference
   (range-concat remote (range-difference local base))
   (range-difference base local)))

(defun gnus-backend-state--visible-marks ()
  "Capture source identities and user marks currently held by open Summaries."
  (let ((marks (make-hash-table :test #'equal)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'gnus-summary-mode)
          (dolist (data gnus-newsgroup-data)
            (let* ((number (gnus-data-number data))
                   (source (if (string-prefix-p "nnvirtual:" gnus-newsgroup-name)
                               (nnvirtual-find-group-art (gnus-group-real-name gnus-newsgroup-name) number)
                             (cons gnus-newsgroup-name number))))
              (when source (puthash source (gnus-data-mark data) marks)))))))
    marks))

(defun gnus-backend-state--commit (method base view-base result &optional sequence)
  "Merge scan RESULT without replacing subscription or local user changes."
  (gnus-dispatch-assert-main)
  (let ((current-view (gnus-backend-state--visible-marks)) changes)
    (dolist (old base)
      (when-let* ((info (gnus-get-info (gnus-info-group old))))
        (push (list info (copy-tree (gnus-info-read info))
                    (copy-tree (gnus-info-marks info)) old) changes)))
    ;; nnimap-update-infos is a data-only protocol adapter; the actor has
    ;; already finished every socket operation and header-cache write.
    (let ((nnimap-current-infos nil) (nnmail-split-history nil))
      (nnimap-update-infos (plist-get result :marks) (mapcar #'car changes)))
    (dolist (change changes)
      (pcase-let ((`(,info ,local-read ,local-marks ,old) change))
        (when (equal (cdr (assq 'uidvalidity (gnus-info-params old)))
                     (cdr (assq 'uidvalidity (gnus-info-params info))))
          (setf (gnus-info-read info)
                (gnus-backend-state--rebase-range (gnus-info-read info)
                                                  (gnus-info-read old) local-read))
          (dolist (mark (delete-dups (append (mapcar #'car (gnus-info-marks old))
                                             (mapcar #'car local-marks))))
            (setf (alist-get mark (gnus-info-marks info))
                  (gnus-backend-state--rebase-range
                   (cdr (assq mark (gnus-info-marks info)))
                   (cdr (assq mark (gnus-info-marks old)))
                   (cdr (assq mark local-marks)))))
          ;; Summary marks have not necessarily been flushed into newsrc.
          (maphash
           (lambda (source mark)
             (when (and (equal (car source) (gnus-info-group info))
                        (not (equal mark (gethash source view-base mark))))
               (setf (gnus-info-read info)
                     (if (memq mark (list gnus-unread-mark gnus-ticked-mark
                                          gnus-dormant-mark gnus-spam-mark))
                         (range-difference (gnus-info-read info) (list (cdr source)))
                       (range-concat (gnus-info-read info) (list (cdr source)))))))
           current-view))
        (maphash
         (lambda (key entry)
           (when (and (eq (car key) gnus-newsrc-hashtb)
                      (equal (car (cadr key)) (gnus-info-group info))
                      (equal (nth 2 key) (cdr (assq 'uidvalidity (gnus-info-params info))))
                      (or (> (car entry) (or sequence 0))
                          (not (eq (nth 2 entry) 'acknowledged))))
             (gnus-backend-state--apply-mark info (cdr (cadr key)) (cadr entry))))
         gnus-backend-state--marks)
        (gnus-get-unread-articles-in-group info (gnus-active (gnus-info-group info)) nil)))
    ;; Header files are atomic; invalidate only this server's in-memory copies.
    (maphash (lambda (key _)
               (when (equal (car key) (cadr method)) (remhash key nnimap--header-caches)))
             nnimap--header-caches)
    (gnus-publish-unread-articles gnus-newsrc-hashtb (mapcar #'car changes) nil nil t)))

(defun gnus-backend-scan (method infos)
  "Scan METHOD in its actor, returning immediately with a main-owned job."
  (gnus-backend-ensure-session)
  (or (let ((job (gethash method gnus-backend--scans)))
        (and (gnus-dispatch-job-pending-p job) job))
      (let ((session gnus-newsrc-hashtb)
            (base (copy-tree infos))
            (sequence gnus-backend-state--sequence)
            (view-base (gnus-backend-state--visible-marks)))
        (puthash
         method
         (gnus-backend-request
          method 'scan (list :infos base)
          (lambda (result) (gnus-backend-state--commit method base view-base result sequence))
          (lambda (failure) (message "Gnus scan failed (%s): %s" (car method) failure))
          :valid (lambda () (eq session gnus-newsrc-hashtb))
          :timeout 180)
         gnus-backend--scans))))

(provide 'gnus-backend-state)
;;; gnus-backend-state.el ends here
