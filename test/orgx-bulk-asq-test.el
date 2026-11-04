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

(defmacro orgx-bulk-asq-test--with-env (file-var content &rest body)
  "Create a temp `orgx-bulk-directory', temp Org file, and fake ASQ store."
  (declare (indent 2))
  `(let* ((orgx-bulk-directory (make-temp-file "orgx-bulk-asq-dir-" t))
          (,file-var (make-temp-file "orgx-bulk-asq-" nil ".org" ,content))
          (org-todo-keywords '((sequence "TODO" "INPROGRESS" "|" "DONE" "CANCELED")))
          (orgx-bulk--plans (make-hash-table :test #'equal))
          (orgx-bulk--counter 0)
          (orgx-bulk-log-notes 'ignore)
          (orgx-bulk-asq-max-items 5)
          (fake-buckets nil)
          (completed-items nil)
          (registered-type nil)
          (agent-shell-queue--store 'fake-store))
     (cl-letf (((symbol-function 'agent-shell-queue-register-item-type)
                (lambda (&rest plist) (setq registered-type plist)))
               ((symbol-function 'agent-shell-queue-store-items)
                (lambda (_store) fake-buckets))
               ((symbol-function 'agent-shell-queue-item-kind)
                (lambda (item) (plist-get item :kind)))
               ((symbol-function 'agent-shell-queue-item-status)
                (lambda (item) (plist-get item :status)))
               ((symbol-function 'agent-shell-queue-item-args)
                (lambda (item) (plist-get item :args)))
               ((symbol-function 'agent-shell-queue--enqueue-args)
                (lambda (args kind buf)
                  (let* ((bname (buffer-name buf))
                         (cell (assoc bname fake-buckets))
                         (item (list :kind kind :status 'active :args args)))
                    (if cell
                        (setcdr cell (append (cdr cell) (list item)))
                      (push (cons bname (list item)) fake-buckets)))))
               ((symbol-function 'agent-shell-queue-complete-item)
                (lambda (item buf-name)
                  (plist-put item :status 'done)
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

(ert-deftest orgx-bulk-asq-register-and-buffer-pred-test ()
  (orgx-bulk-asq-test--with-env f "* TODO Task1\n"
    (orgx-bulk-asq-register)
    (should (eq (plist-get registered-type :kind) 'orgx-bulk))
    (let ((pred (plist-get registered-type :buffer-pred)))
      (pcase-let ((`(,ag-buf . _)
                   (orgx-bulk-batch-for-scope
                    (list :type 'files :files (list f) :only-open t))))
        (should (funcall pred ag-buf))
        (with-temp-buffer
          (should-not (funcall pred (current-buffer))))))))

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
      (should (= (orgx-bulk-asq--pending-count (buffer-name ag-buf)) 5))
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
        ;; Dispatch the queued item.
        (let ((queued-item (car (orgx-bulk-asq--bucket-items (buffer-name ag-buf)))))
          (orgx-bulk-asq-dispatch queued-item (buffer-name ag-buf))
          (should (eq (plist-get queued-item :status) 'done))
          (should (= (length completed-items) 1))
          (should (eq (orgx-bulk-plan-status plan) 'complete))
          ;; Agenda marks are unlocked once no queued items remain.
          (with-current-buffer ag-buf
            (should-not orgx-bulk-batch-locked))
          (with-current-buffer (find-file-noselect f)
            (should (equal (org-map-entries #'org-get-todo-state)
                           '("DONE" "DONE")))))))))

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
      (with-current-buffer ag-buf (should-not orgx-bulk-batch-locked))
      ;; Queue backend not installed.
      (cl-letf (((symbol-function 'agent-shell-queue--enqueue-args) nil))
        (should-error (orgx-bulk-asq-enqueue ag-buf op) :type 'user-error))
      (with-current-buffer ag-buf (should-not orgx-bulk-batch-locked)))))

(ert-deftest orgx-bulk-asq-enqueue-delay-after-test ()
  (orgx-bulk-asq-test--with-env f "* TODO Alpha\n"
    (pcase-let ((`(,ag-buf . _)
                 (orgx-bulk-batch-for-scope
                  (list :type 'files :files (list f) :only-open t)))
                (delays nil))
      (cl-letf (((symbol-function 'orgx-bulk-asq--set-delay-after)
                 (lambda (item delay) (push (cons item delay) delays))))
        (orgx-bulk-asq-enqueue
         ag-buf (orgx-bulk-op--create :name 'todo-state :args '("DONE")) 30))
      (should (equal (mapcar #'cdr delays) '(30)))
      (should (eq (plist-get (caar delays) :kind) 'orgx-bulk)))))

(provide 'orgx-bulk-asq-test)
;;; orgx-bulk-asq-test.el ends here
