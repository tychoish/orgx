;;; orgx-bulk-asq-test.el --- Tests for orgx-bulk-asq.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-agenda)
(require 'orgx-bulk)
(require 'orgx-bulk-ops)
(require 'orgx-bulk-agenda)
(require 'orgx-bulk-asq)
(require 'agent-shell-queue)

(defmacro orgx-bulk-asq-test--with-env (file-var content &rest body)
  "Create a temp `orgx-bulk-directory', temp Org file, and a real, isolated queue."
  (declare (indent 2))
  `(let* ((orgx-bulk-directory (make-temp-file "orgx-bulk-asq-dir-" t))
          (,file-var (make-temp-file "orgx-bulk-asq-" nil ".org" ,content))
          (org-todo-keywords '((sequence "TODO" "INPROGRESS" "|" "DONE" "CANCELED")))
          (hitl-plan--store (make-hash-table :test #'equal))
          (orgx-bulk-log-notes 'ignore)
          (orgx-bulk-asq-max-items 5)
          (completed-items nil)
          (agent-shell-queue--store
           (agent-shell-queue--make-store :items nil :format 'plist :file nil))
          (agent-shell-queue--queue
           (agent-shell-queue-queue--make
            :store 'agent-shell-queue--store :session-paused nil
            :editing-ids nil :interjection-pending nil))
          (agent-shell-queue--loaded t))
     (cl-letf (((symbol-function 'agent-shell-queue--save) #'ignore)
               ((symbol-function 'agent-shell-queue--refresh-buffer) #'ignore)
               ((symbol-function 'agent-shell-queue--ensure-subscription) #'ignore)
               ((symbol-function 'alert) #'ignore)
               ((symbol-function 'agent-shell-queue-complete-item)
                (lambda (item buf-name)
                  (setf (agent-shell-queue-item-status item) 'done)
                  (push (cons buf-name item) completed-items))))
       (unwind-protect
           (progn
             (with-current-buffer (find-file-noselect ,file-var)
               (org-mode)
               (org-set-regexps-and-options))
             ,@body)
         (dolist (buf (buffer-list))
           (when (or (and (buffer-file-name buf)
                          (equal (expand-file-name (buffer-file-name buf))
                                 (expand-file-name ,file-var)))
                     (string-prefix-p "*Org Agenda" (buffer-name buf)))
             (with-current-buffer buf (set-buffer-modified-p nil))
             (kill-buffer buf)))
         (when (file-exists-p ,file-var)
           (delete-file ,file-var))
         (when (file-directory-p orgx-bulk-directory)
           (delete-directory orgx-bulk-directory t))))))

(defun orgx-bulk-asq-test--items (buf)
  "Return the queue items of BUF."
  (cdr (assoc (buffer-name buf)
              (agent-shell-queue-store-items agent-shell-queue--store))))

(ert-deftest orgx-bulk-asq-register-and-buffer-pred-test ()
  (orgx-bulk-asq-test--with-env f "* TODO Task1\n"
    ;; The `hitl-plan' item type is registered by `agent-shell-queue-plan'
    ;; and agenda buffers are accepted via the domain's :accept-buffer.
    (should (agent-shell-queue--type-for-kind 'hitl-plan))
    (pcase-let ((`(,ag-buf . _)
                 (orgx-bulk-batch-for-scope
                  (list :type 'files :files (list f) :only-open t))))
      (should (agent-shell-queue-plan-buffer-p ag-buf))
      (with-temp-buffer
        (should-not (agent-shell-queue-plan-buffer-p (current-buffer)))))))

(ert-deftest orgx-bulk-asq-max-items-and-queueable-check-test ()
  (orgx-bulk-asq-test--with-env f "* TODO Task1\n* TODO Task2\n"
    (pcase-let ((`(,ag-buf . _)
                 (orgx-bulk-batch-for-scope
                  (list :type 'files :files (list f) :only-open t))))
      ;; Non-queueable op (`function') is rejected.
      (should-error
       (orgx-bulk-asq-enqueue ag-buf (orgx-bulk-op--create :name 'function :args '(ignore)))
       :type 'user-error)
      ;; Enqueue up to `orgx-bulk-asq-max-items' (5) succeeds; 6th signals `user-error'.
      (dotimes (_ 5)
        (orgx-bulk-asq-enqueue ag-buf (orgx-bulk-op--create :name 'tag-add :args '("q"))))
      (should (= (agent-shell-queue-plan--pending-count (buffer-name ag-buf)) 5))
      (should-error
       (orgx-bulk-asq-enqueue ag-buf (orgx-bulk-op--create :name 'tag-add :args '("q6")))
       :type 'user-error))))

(ert-deftest orgx-bulk-asq-lock-marks-and-dispatch-test ()
  (orgx-bulk-asq-test--with-env f "* TODO Alpha\n* TODO Beta\n"
    (pcase-let ((`(,ag-buf . _)
                 (orgx-bulk-batch-for-scope
                  (list :type 'files :files (list f) :only-open t))))
      (let ((plan (orgx-bulk-asq-enqueue
                   ag-buf
                   (orgx-bulk-op--create :name 'todo-state :args '("DONE")))))
        ;; Agenda marks are locked while the item is queued.
        (with-current-buffer ag-buf
          (should orgx-bulk-batch-locked)
          (should-error (org-agenda-bulk-mark) :type 'user-error))
        ;; Dispatch the queued item; the plan is found in `orgx-bulk-directory'.
        (let ((queued-item (car (orgx-bulk-asq-test--items ag-buf))))
          (orgx-bulk-asq-dispatch queued-item (buffer-name ag-buf))
          (should (eq (agent-shell-queue-item-status queued-item) 'done))
          (should (= (length completed-items) 1))
          (should (eq (hitl-plan-status plan) 'complete))
          ;; Agenda marks are unlocked once no queued items remain.
          (with-current-buffer ag-buf
            (should-not orgx-bulk-batch-locked))
          (with-current-buffer (find-file-noselect f)
            (should (equal (org-map-entries #'org-get-todo-state)
                           '("DONE" "DONE")))))))))

(ert-deftest orgx-bulk-asq-dispatch-finds-plan-after-restart-test ()
  ;; Only the saved file remains, in `orgx-bulk-directory' rather than
  ;; `hitl-plan-directory'; the queued item carries the directory.
  (orgx-bulk-asq-test--with-env f "* TODO Alpha\n"
    (pcase-let ((`(,ag-buf . _)
                 (orgx-bulk-batch-for-scope
                  (list :type 'files :files (list f) :only-open t))))
      (let* ((plan (orgx-bulk-asq-enqueue
                    ag-buf (orgx-bulk-op--create :name 'todo-state :args '("DONE"))))
             (id (hitl-plan-id plan))
             (item (car (orgx-bulk-asq-test--items ag-buf))))
        (should (equal (file-name-as-directory orgx-bulk-directory)
                       (file-name-as-directory
                        (plist-get (read (agent-shell-queue-item-args item)) :directory))))
        (clrhash hitl-plan--store)
        (orgx-bulk-asq-dispatch item (buffer-name ag-buf))
        (should (eq (hitl-plan-status (hitl-plan-get id orgx-bulk-directory)) 'complete))))))

(ert-deftest orgx-bulk-asq-enqueue-failure-does-not-leave-lock-test ()
  (orgx-bulk-asq-test--with-env f "* TODO Alpha\n"
    (pcase-let ((`(,ag-buf . _)
                 (orgx-bulk-batch-for-scope
                  (list :type 'files :files (list f) :only-open t)))
                (op (orgx-bulk-op--create :name 'todo-state :args '("DONE"))))
      ;; Queue backend errors out.
      (cl-letf (((symbol-function 'agent-shell-queue--enqueue-args)
                 (lambda (&rest _) (error "queue exploded"))))
        (should-error (orgx-bulk-asq-enqueue ag-buf op)))
      (with-current-buffer ag-buf (should-not orgx-bulk-batch-locked)))))

(ert-deftest orgx-bulk-asq-enqueue-delay-after-test ()
  (orgx-bulk-asq-test--with-env f "* TODO Alpha\n"
    (pcase-let ((`(,ag-buf . _)
                 (orgx-bulk-batch-for-scope
                  (list :type 'files :files (list f) :only-open t)))
                (delays nil))
      (cl-letf (((symbol-function 'agent-shell-queue-plan--set-delay-after)
                 (lambda (item delay) (push (cons item delay) delays))))
        (orgx-bulk-asq-enqueue
         ag-buf (orgx-bulk-op--create :name 'todo-state :args '("DONE")) 30))
      (should (equal (mapcar #'cdr delays) '(30)))
      (should (eq (agent-shell-queue-item-kind (caar delays)) 'hitl-plan)))))

(provide 'orgx-bulk-asq-test)
;;; orgx-bulk-asq-test.el ends here
