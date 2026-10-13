;;; gnus-backend.el --- Serial backend processes and request ownership -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Code:
(require 'cl-lib)
(require 'json)
(require 'gnus-dispatch)
(require 'gnus-int)

(defgroup gnus-backend nil "Isolated Gnus network operations." :group 'gnus)
(defcustom gnus-backend-process-types '(nnimap)
  "Backends served by isolated processes; currently only IMAP is supported."
  :type '(set (const nnimap)) :group 'gnus-backend)
(defcustom gnus-backend-worker-init-file nil
  "Optional minimal backend configuration loaded by batch workers.
Use it for credential providers.  Do not load interactive Gnus or GUI packages."
  :type '(choice (const nil) file) :group 'gnus-backend)
(defcustom gnus-backend-worker-variables
  '(gnus-directory nnimap-header-cache-directory auth-sources nnmail-extra-headers)
  "Data variables copied to a worker before its first request.
Prefer method parameters and a small init file over copying frontend state."
  :type '(repeat symbol) :group 'gnus-backend)
(defvar gnus-backend--actors (make-hash-table :test #'equal))
(defvar gnus-backend--serial 0)
(defvar gnus-backend--session nil)
(defvar gnus-backend-reset-hook nil)
(defvar gnus-backend-worker-p nil)
(cl-defstruct (gnus-backend--actor (:constructor gnus-backend--make-actor))
  method process (input "") ready active queue closed)

(defun gnus-backend-reset ()
  "Close old connection owners and invalidate their session bookkeeping."
  (gnus-dispatch-assert-main)
  (maphash (lambda (_ actor) (gnus-backend--broken actor "Gnus session closed"))
           (copy-hash-table gnus-backend--actors))
  (clrhash gnus-backend--actors)
  (setq gnus-backend--session nil)
  (run-hooks 'gnus-backend-reset-hook))

(defun gnus-backend-ensure-session ()
  "Drop old actors before accepting work for another Gnus session."
  (unless (eq gnus-backend--session gnus-newsrc-hashtb)
    (gnus-backend-reset)
    (setq gnus-backend--session gnus-newsrc-hashtb)))

(add-hook 'gnus-after-exiting-gnus-hook #'gnus-backend-reset)

(defun gnus-backend-flush-marks ()
  "Finish already queued mark writes before Gnus discards its session.
This explicit exit boundary yields to process I/O and remains interruptible.
Ordinary reading and marking never wait here.  Each job retains its deadline."
  (gnus-dispatch-assert-main)
  (while
      (let (pending)
        (maphash
         (lambda (_ actor)
           (dolist (entry (cons (gnus-backend--actor-active actor)
                                (gnus-backend--actor-queue actor)))
             (when (and entry (eq (plist-get (nth 2 entry) :operation) 'marks)
                        (gnus-dispatch-job-pending-p (nth 1 entry)))
               (setq pending t)))) gnus-backend--actors)
        pending)
    (accept-process-output nil 0.05)
    (gnus-dispatch-drain)))

(defun gnus-backend-process-p (method)
  "Whether METHOD has an isolated actor in this frontend."
  (and (not gnus-backend-worker-p) (memq (car method) gnus-backend-process-types)))

(defun gnus-backend--wire (process id payload)
  "Send data PAYLOAD to PROCESS, without putting credentials on its argv."
  (let ((print-length nil) (print-level nil) (print-circle nil))
    (process-send-string process
                         (concat (json-serialize
                                  (list :version 1 :id id
                                        :payload (prin1-to-string payload))) "\n"))))

(defun gnus-backend--filter (actor _process text)
  "Hand off immutable TEXT; process filters may run during worker I/O."
  (gnus-dispatch-submit #'gnus-backend--receive actor text))

(defun gnus-backend--receive (actor text)
  "Frame TEXT on main, serializing partial responses with actor state."
  (gnus-dispatch-assert-main)
  (unless (gnus-backend--actor-closed actor)
    (let ((input (concat (gnus-backend--actor-input actor) text)) end)
      (if (> (length input) (* 32 1024 1024))
          (progn (setq input "")
                 (gnus-backend--broken actor "Backend response too large"))
        (while (setq end (string-search "\n" input))
          ;; Decode before a queued process-exit event can fail the request
          ;; whose final response was already delivered by the filter.
          (gnus-backend--response actor (substring input 0 end))
          (setq input (substring input (1+ end)))))
      (setf (gnus-backend--actor-input actor) input))))

(defun gnus-backend--response (actor line)
  "Decode LINE and settle the matching active request on main."
  (gnus-dispatch-assert-main)
  (unless (gnus-backend--actor-closed actor)
    (condition-case err
        (let* ((wire (json-parse-string line :object-type 'plist :null-object nil))
               (id (plist-get wire :id)) (failure (plist-get wire :error)))
          (unless (= (plist-get wire :version) 1) (error "Backend protocol mismatch"))
          (if (= id 0)
              (if failure (gnus-backend--broken actor failure)
                (setf (gnus-backend--actor-ready actor) t)
                (gnus-backend--next actor))
            (when-let* ((entry (gnus-backend--actor-active actor))
                        ((= id (car entry))))
              (let* ((read-circle nil)
                     (result (car (read-from-string (plist-get wire :result)))))
                (setf (gnus-backend--actor-active actor) nil)
                (gnus-dispatch-job-deliver
                 (nth 1 entry) result failure))
              (gnus-backend--next actor))))
      (error (gnus-backend--broken actor (error-message-string err))))))

(defun gnus-backend--broken (actor reason)
  "Fail ACTOR's requests once, including process exit without a response."
  (gnus-dispatch-assert-main)
  (setf (gnus-backend--actor-closed actor) t)
  (when (eq actor (gethash (gnus-backend--actor-method actor) gnus-backend--actors))
    (remhash (gnus-backend--actor-method actor) gnus-backend--actors))
  (when (process-live-p (gnus-backend--actor-process actor))
    (delete-process (gnus-backend--actor-process actor)))
  (dolist (entry (cons (gnus-backend--actor-active actor) (gnus-backend--actor-queue actor)))
    (when entry (gnus-dispatch-job-deliver (nth 1 entry) nil reason)))
  (setf (gnus-backend--actor-active actor) nil (gnus-backend--actor-queue actor) nil))

(defun gnus-backend--actor (method)
  "Find or start METHOD's serial network owner."
  (gnus-dispatch-assert-main)
  (or (gethash method gnus-backend--actors)
      (let* ((actor (gnus-backend--make-actor :method method))
             (process-connection-type nil)
             (worker (locate-library "gnus-backend-worker"))
             (process
              (make-process
               :name (format "gnus-backend-%s" (car method)) :noquery t
               :connection-type 'pipe :coding 'utf-8-unix
               :command (list (expand-file-name invocation-name invocation-directory)
                              "-Q" "--batch" "--eval"
                              (prin1-to-string `(setq load-path ',load-path))
                              "-l" worker "-f" "gnus-backend-worker-main")
               :stderr (get-buffer-create (format " *gnus-backend-%s-log*" (car method)))
               :filter (apply-partially #'gnus-backend--filter actor)
               :sentinel (lambda (p _event)
                           (unless (process-live-p p)
                             (gnus-dispatch-submit #'gnus-backend--broken actor
                                                   "Backend process exited"))))))
        (setf (gnus-backend--actor-process actor) process)
        (puthash method actor gnus-backend--actors)
        (condition-case err
            (gnus-backend--wire
             process 0 (list :operation 'init :method method
                             :init-file gnus-backend-worker-init-file
                             :variables (cl-loop for var in gnus-backend-worker-variables
                                                 when (boundp var) collect (cons var (symbol-value var)))))
          ((error quit)
           (gnus-backend--broken actor "Backend initialization failed")
           (signal (car err) (cdr err))))
        actor)))

(defun gnus-backend--next (actor)
  "Start the next live queued request, keeping one owner per connection."
  (when (and (gnus-backend--actor-ready actor)
             (not (gnus-backend--actor-active actor)))
    (let (entry)
      (while (and (setq entry (pop (gnus-backend--actor-queue actor)))
                  (not (gnus-dispatch-job-pending-p (nth 1 entry)))))
      (when entry
        (setf (gnus-backend--actor-active actor) entry
              (gnus-dispatch-job-state (nth 1 entry)) 'fetching)
        (condition-case err
            (gnus-backend--wire (gnus-backend--actor-process actor) (car entry) (nth 2 entry))
          (error (gnus-backend--broken actor (error-message-string err))))))))

(cl-defun gnus-backend-request (method operation data success failure &key valid (timeout 90) priority)
  "Queue OPERATION and DATA for METHOD; callbacks always run on main.
An actor serializes network access.  PRIORITY puts reads ahead of queued scans.
Cancellation of an active operation closes its process; no connection with an
unknown protocol position is reused.  Its other requests fail explicitly."
  (gnus-dispatch-assert-main)
  (gnus-backend-ensure-session)
  (let ((job (gnus-dispatch-job-create success failure :valid valid :timeout timeout)))
    (condition-case err
        (let* ((actor (gnus-backend--actor method))
               (infos (cl-remove-if-not
                       (lambda (info) (gnus-methods-equal-p method
                                                            (gnus-find-method-for-group (gnus-info-group info) info)))
                       (cdr gnus-newsrc-alist)))
               (entry (list (cl-incf gnus-backend--serial) job
                            (append data (list :operation operation :infos (copy-tree infos)
                                               :active (mapcar (lambda (info)
                                                                 (cons (gnus-info-group info)
                                                                       (copy-tree (gnus-active (gnus-info-group info)))))
                                                               infos))))))
          (setf (gnus-dispatch-job-cancel-function job)
                (lambda ()
                  (when (or (not (gnus-backend--actor-ready actor))
                            (eq entry (gnus-backend--actor-active actor)))
                    (gnus-backend--broken actor "Backend operation cancelled"))))
          (if priority (push entry (gnus-backend--actor-queue actor))
            (setf (gnus-backend--actor-queue actor)
                  (nconc (gnus-backend--actor-queue actor) (list entry))))
          (gnus-backend--next actor))
      ((error quit) (gnus-dispatch-job-deliver job nil (error-message-string err))))
    job))

(provide 'gnus-backend)
;;; gnus-backend.el ends here
