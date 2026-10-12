;;; gnus-summary-refresh.el --- Refresh visible summaries after scans -*- lexical-binding: t; -*-

;;; Commentary:
;; Refresh native and virtual summaries on the main thread, retaining article
;; rendering and source-based cursor and window anchors.  Group definitions
;; and subscription policies belong to the user's configuration.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'gnus-group)
(require 'gnus-sum)
(require 'nnvirtual)
(defvar gnus-thread-reader--summary)
(defgroup gnus-summary-refresh nil
  "Background Summary updates." :group 'gnus)
(defcustom gnus-summary-refresh-enabled t
  "Whether completed scans refresh visible Summary buffers."
  :type 'boolean :group 'gnus-summary-refresh)

(defvar gnus-summary-refresh--cached-view nil)
(defvar gnus-summary-refresh--preserving-article nil)
(defvar gnus-summary-refresh--refresh-timer nil)
(defvar gnus-summary-refresh--refresh-pending nil)
(defvar gnus-summary-refresh--refresh-running nil)

(defun gnus-summary-refresh--schedule-refresh ()
  "Arrange one list rebuild after a brief pause in user input."
  (unless gnus-summary-refresh--refresh-timer
    (setq gnus-summary-refresh--refresh-timer
          (run-with-idle-timer 0.15 nil #'gnus-summary-refresh--flush-refresh))))

(defun gnus-summary-refresh--refresh-after-scan (&rest _)
  "Queue a Summary update without blocking a completed group callback.
Nearby source completions share one rebuild of the latest group state."
  (when gnus-summary-refresh-enabled
    (setq gnus-summary-refresh--refresh-pending t)
    (gnus-summary-refresh--schedule-refresh)))

(defun gnus-summary-refresh--flush-refresh ()
  "Render pending updates on the main thread when input is quiet."
  (setq gnus-summary-refresh--refresh-timer nil)
  (when gnus-summary-refresh--refresh-pending
    (if (or gnus-summary-refresh--refresh-running
            (not (eq (current-thread) main-thread))
            (input-pending-p))
        (gnus-summary-refresh--schedule-refresh)
      (setq gnus-summary-refresh--refresh-pending nil)
      (unwind-protect
          (let ((gnus-summary-refresh--refresh-running t)
                ;; A full native Summary allocates enough temporary data
                ;; to trigger several collections at the default 800 KB.
                (gc-cons-threshold (max gc-cons-threshold (* 32 1024 1024))))
            (gnus-summary-refresh--rebuild-visible))
        (when gnus-summary-refresh--refresh-pending
          (gnus-summary-refresh--schedule-refresh))))))

(defun gnus-summary-refresh--exit-for-refresh (exit &rest args)
  "Keep per-group article buffers when EXIT is part of a list refresh.
Bind this only during exit: binding it for the subsequent group entry
would redirect the Summary to an empty shared `*Article*' buffer."
  (let ((gnus-single-article-buffer
         (or gnus-single-article-buffer
             (and gnus-summary-refresh--preserving-article
                  (eq (current-thread) main-thread)))))
    (apply exit args)))

(advice-add 'gnus-summary-exit :around #'gnus-summary-refresh--exit-for-refresh)

(defun gnus-summary-refresh--activate-cached-group (activate group &optional scan dont-check method dont-sub-check)
  "Use existing backend data when building the foreground Summary."
  (funcall activate group scan dont-check method
           (or dont-sub-check
               (and gnus-summary-refresh--cached-view
                    (eq (current-thread) main-thread)))))

(advice-add 'gnus-activate-group :around #'gnus-summary-refresh--activate-cached-group)

(defun gnus-summary-refresh--preserve-article (refresh)
  "Run REFRESH without destroying the current article's MIME rendering.
Reselecting a Summary exits the group, which normally undisplays MIME
parts.  A background list update must leave the article and pending SHR
image markers intact."
  (let ((article (and gnus-article-buffer (get-buffer gnus-article-buffer)))
        (gnus-summary-refresh--preserving-article t)
        (gnus-auto-select-first nil))
    (if (not article)
        (funcall refresh)
      (with-current-buffer article
        (let ((gnus-article-mime-handles nil)
              (gnus-article-mime-handle-alist nil))
          (funcall refresh))))))

(defun gnus-summary-refresh--view-rows ()
  "Return visible rows keyed by stable source group and article number."
  (save-excursion
    (goto-char (point-min))
    (let (rows)
      (while (not (eobp))
        (when-let* ((number (gnus-summary-article-number)))
          (unless (invisible-p (point))
            (push (cons (if (string-prefix-p "nnvirtual:" gnus-newsgroup-name)
                         (nnvirtual-find-group-art
                          (gnus-group-real-name gnus-newsgroup-name) number)
                       (cons gnus-newsgroup-name number))
                        (point)) rows)))
        (forward-line 1))
      (nreverse rows))))

(defun gnus-summary-refresh--view-anchor (rows position)
  "Prefer the row at POSITION, then its successors and predecessors."
  (let ((line (save-excursion (goto-char position)
                             (line-beginning-position))))
    (append (cl-remove-if (lambda (row) (< (cdr row) line)) rows)
            (reverse (cl-remove-if-not (lambda (row) (< (cdr row) line)) rows)))))

(defun gnus-summary-refresh--save-view ()
  "Capture source identities for point and every Summary window."
  (let ((rows (gnus-summary-refresh--view-rows)))
    (list
     (gnus-summary-refresh--view-anchor rows (point))
     (mapcar
      (lambda (window)
        (let* ((start (window-start window))
               (top (gnus-summary-refresh--view-anchor rows start)))
          (list window
                (gnus-summary-refresh--view-anchor rows (window-point window))
                top (and top (count-lines start (cdar top)))
                (window-hscroll window) (window-vscroll window t))))
      (get-buffer-window-list (current-buffer) nil t)))))

(defun gnus-summary-refresh--restore-view (state)
  "Restore STATE after virtual article numbers have been regenerated."
  (let ((positions (make-hash-table :test #'equal)))
    (dolist (row (gnus-summary-refresh--view-rows))
      (puthash (car row) (cdr row) positions))
    (cl-labels ((survivor (rows)
                 (cl-find-if (lambda (row) (gethash (car row) positions)) rows)))
      (when-let* ((row (survivor (car state))))
        (goto-char (gethash (car row) positions))
        (gnus-summary-position-point))
      (dolist (view (cadr state))
        (pcase-let ((`(,window ,cursor ,top ,offset ,hscroll ,vscroll) view))
          (when (window-live-p window)
            (set-window-buffer window (current-buffer))
            (when-let* ((row (survivor cursor)))
              (save-excursion
                (goto-char (gethash (car row) positions))
                (gnus-summary-position-point)
                (set-window-point window (point))))
            (when-let* ((row (survivor top)))
              (save-excursion
                (goto-char (gethash (car row) positions))
                (when (eq row (car top)) (forward-line (- offset)))
                (set-window-start window (point) t)))
            (set-window-hscroll window hscroll)
            (set-window-vscroll window vscroll t)))))))

(defun gnus-summary-refresh--rebuild-after-scan ()
  "Rebuild the visible Summary from the latest completed group scans."
  (let* ((summary (current-buffer))
         (group gnus-newsgroup-name)
         (all (eq gnus-newsgroup-display #'gnus-not-ignore))
         (summary-window (and summary (get-buffer-window summary)))
         (view (and summary-window
                    (with-current-buffer summary (gnus-summary-refresh--save-view)))))
    (when summary-window
      ;; Reselecting rebuilds nnvirtual's article mapping, but its normal
      ;; window changes must not interrupt the user's current reading view.
      (let ((inhibit-redisplay t)
            (gnus-summary-refresh--cached-view t)
            (gnus-kill-summary-on-exit nil))
        (save-window-excursion
          (with-current-buffer summary
            (gnus-summary-refresh--preserve-article
             (lambda ()
               (with-selected-window summary-window
                 (with-current-buffer summary
                   (when (derived-mode-p 'gnus-summary-mode)
                     (gnus-summary-reselect-current-group (and all (list 4)))))))))))
      ;; Gnus may replace the Summary buffer while reselecting.  Native
      ;; articles also retain a separate current-Summary reference for
      ;; forwarded keys; repair it together with the thread reader's link.
      (let ((replacement (get-buffer (format "*Summary %s*"
                                              group))))
        (when replacement
          (with-current-buffer replacement
            (gnus-summary-refresh--restore-view view))
          (dolist (buffer (buffer-list))
            (with-current-buffer buffer
              (when (and (derived-mode-p 'gnus-article-mode)
                         (equal gnus-newsgroup-name group))
                (setq gnus-article-current-summary replacement
                      gnus-summary-buffer replacement))
              (when (and (local-variable-p 'gnus-thread-reader--summary)
                         (eq gnus-thread-reader--summary summary))
                (setq gnus-thread-reader--summary replacement)))))))))


(defun gnus-summary-refresh--rebuild-visible ()
  "Refresh visible summaries without discarding an active user limit."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (derived-mode-p 'gnus-summary-mode)
                   (get-buffer-window buffer)
                   (not gnus-newsgroup-limits)
                   (not (gnus-ephemeral-group-p gnus-newsgroup-name)))
          (condition-case err
              (gnus-summary-refresh--rebuild-after-scan)
            (error (message "Summary refresh: %s" (error-message-string err)))))))))

(defun gnus-summary-refresh--after-group-update (&rest _)
  "Queue a view refresh when a backend worker has updated a group.
Foreground group updates also occur while reselecting the Summary;
ignoring those prevents a render from scheduling another render."
  (unless (eq (current-thread) main-thread)
    (gnus-summary-refresh--refresh-after-scan)))

(unless (advice-member-p #'gnus-summary-refresh--after-group-update
                         'gnus-group-update-group)
  (advice-add 'gnus-group-update-group :after
              #'gnus-summary-refresh--after-group-update))
(add-hook 'gnus-after-getting-new-news-hook
          #'gnus-summary-refresh--refresh-after-scan)


(provide 'gnus-summary-refresh)
;;; gnus-summary-refresh.el ends here
