;;; orgx-bulk-buffer-test.el --- Tests for orgx-bulk-buffer.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'orgx-bulk)
(require 'orgx-bulk-ops)
(require 'orgx-bulk-agenda)
(require 'orgx-bulk-buffer)
(require 'hitl-plan-ui)

(defmacro orgx-bulk-buffer-test--with-env (file-var content &rest body)
  "Create a temp `orgx-bulk-directory' and temp Org file FILE-VAR with CONTENT."
  (declare (indent 2))
  `(let* ((orgx-bulk-directory (make-temp-file "orgx-bulk-buf-dir-" t))
          (,file-var (make-temp-file "orgx-bulk-buf-" nil ".org" ,content))
          (org-todo-keywords '((sequence "TODO" "INPROGRESS" "|" "DONE" "CANCELED")))
          (hitl-plan--store (make-hash-table :test #'equal))
          (orgx-bulk-log-notes 'ignore))
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
                   (string-prefix-p "*Org Agenda" (buffer-name buf))
                   (string-prefix-p "*hitl-plan:" (buffer-name buf)))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (when (file-exists-p ,file-var)
         (delete-file ,file-var))
       (when (file-directory-p orgx-bulk-directory)
         (delete-directory orgx-bulk-directory t)))))

(defun orgx-bulk-buffer-test--targets (file)
  "Return target plists for all headings in FILE."
  (with-current-buffer (find-file-noselect file)
    (org-map-entries #'orgx-bulk--target-at-point)))

(ert-deftest orgx-bulk-buffer-render-toggle-note-add-op-execute-test ()
  (orgx-bulk-buffer-test--with-env f "* TODO First\n* TODO Second\n"
    (let* ((targets (orgx-bulk-buffer-test--targets f))
           (plan (orgx-bulk-create-plan
                  :scope (list :type 'files :files (list f))
                  :targets targets
                  :ops '((todo-state "DONE"))))
           (buf (orgx-bulk-review plan)))
      (with-current-buffer buf
        (should (derived-mode-p 'hitl-plan-mode))
        (should (equal (buffer-name) (hitl-plan-ui-buffer-name plan)))
        (should (eq (keymap-lookup hitl-plan-mode-map "c")
                    #'hitl-plan-ui-execute))
        (should (eq (keymap-lookup hitl-plan-mode-map "C-c C-c")
                    #'hitl-plan-ui-execute))
        ;; The orgx names remain as aliases.
        (should (eq (indirect-function #'orgx-bulk-buffer-execute)
                    (indirect-function #'hitl-plan-ui-execute)))
        ;; Move to first step line and toggle skip with `x'.
        (goto-char (point-min))
        (re-search-forward "First")
        (beginning-of-line)
        (orgx-bulk-buffer-toggle-skip)
        (should (eq (hitl-plan-step-status (car (hitl-plan-steps plan))) 'skipped))
        ;; Edit note with `n' -> promotes `ignore' to `one'.
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Plan note")))
          (orgx-bulk-buffer-edit-note))
        (should (eq (hitl-plan-log-notes plan) 'one))
        (should (equal (hitl-plan-note plan) "Plan note"))
        ;; Add operation with `a' (`archive-sibling' should order after `todo-state').
        (cl-letf (((symbol-function 'annotated-completing-read)
                   (lambda (&rest _) "archive-sibling")))
          (orgx-bulk-buffer-add-op))
        (should (equal (mapcar #'hitl-plan-op-name (hitl-plan-ops plan))
                       '(todo-state archive-sibling)))
        ;; Skip archive-sibling steps so we test executing only the remaining pending step.
        (dolist (s (hitl-plan-steps plan))
          (when (eq (hitl-plan-op-name (hitl-plan-step-op s)) 'archive-sibling)
            (setf (hitl-plan-step-status s) 'skipped)))
        ;; Execute via `orgx-bulk-buffer-execute' (`c' / `C-c C-c').
        (orgx-bulk-buffer-execute)
        (should (eq (hitl-plan-status plan) 'complete))
        (should (string-match-p "\\[done " (buffer-string)))
        (goto-char (point-min))
        (re-search-forward "\\[done ")
        (should (eq (get-text-property (match-beginning 0) 'face)
                    'hitl-plan-done-face))))))

(ert-deftest orgx-bulk-buffer-visit-and-discard-test ()
  (orgx-bulk-buffer-test--with-env f "* TODO VisitMe\n"
    (let* ((targets (orgx-bulk-buffer-test--targets f))
           (plan (orgx-bulk-create-plan
                  :scope (list :type 'files :files (list f))
                  :targets targets
                  :ops '((todo-state "DONE"))))
           (buf (orgx-bulk-review plan)))
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "VisitMe")
        (beginning-of-line)
        (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (set-buffer b))))
          (orgx-bulk-buffer-visit)
          (should (equal (org-get-heading t t t t) "VisitMe"))))
      (with-current-buffer buf
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (orgx-bulk-buffer-discard)))
      (should (eq (hitl-plan-status plan) 'discarded))
      (should-not (buffer-live-p buf)))))

(ert-deftest orgx-bulk-resume-command-test ()
  (orgx-bulk-buffer-test--with-env f "* TODO ResumeTask\n"
    (let* ((targets (orgx-bulk-buffer-test--targets f))
           (plan (orgx-bulk-create-plan
                  :scope (list :type 'files :files (list f))
                  :targets targets
                  :ops '((todo-state "DONE"))
                  :status 'paused)))
      (cl-letf (((symbol-function 'annotated-completing-read)
                 (lambda (cands &rest _)
                   (should (assoc (hitl-plan-id plan) cands))
                   (hitl-plan-id plan)))
                ((symbol-function 'pop-to-buffer) (lambda (b &rest _) (set-buffer b))))
        (let ((buf (orgx-bulk-resume)))
          (should (buffer-live-p buf))
          (with-current-buffer buf
            (should (equal (hitl-plan-id hitl-plan-ui--plan) (hitl-plan-id plan)))))))))

(ert-deftest orgx-bulk-resume-only-offers-org-agenda-plans-in-directory-test ()
  (orgx-bulk-buffer-test--with-env f "* TODO ResumeTask\n"
    (let* ((plan (orgx-bulk-create-plan
                  :scope (list :type 'files :files (list f))
                  :targets (orgx-bulk-buffer-test--targets f)
                  :ops '((todo-state "DONE"))
                  :status 'paused))
           (other (hitl-plan-create :domain 'file-target :save nil
                                    :targets (list (list :file f :line 1))
                                    :ops '(todo-state) :status 'paused))
           (offered nil))
      (should (hitl-plan-p other))
      (cl-letf (((symbol-function 'annotated-completing-read)
                 (lambda (cands &rest _)
                   (setq offered (mapcar #'car cands))
                   (hitl-plan-id plan)))
                ((symbol-function 'pop-to-buffer) (lambda (b &rest _) (set-buffer b))))
        (orgx-bulk-resume))
      (should (equal offered (list (hitl-plan-id plan))))
      ;; An unknown plan ID is a user error rather than a silent no-op.
      (should-error (orgx-bulk-resume "no-such-plan") :type 'user-error))))

(ert-deftest orgx-bulk-review-display-test ()
  (orgx-bulk-buffer-test--with-env f "* TODO Shown\n"
    (let* ((plan (orgx-bulk-create-plan
                  :scope (list :type 'files :files (list f))
                  :targets (orgx-bulk-buffer-test--targets f)
                  :ops '((todo-state "DONE"))))
           (popped nil))
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (b &rest _) (setq popped b) (set-buffer b))))
        (should (eq (orgx-bulk-review plan t) popped))
        (should (equal (buffer-name popped) (hitl-plan-ui-buffer-name plan)))))))

;;; Review follow-ups

(ert-deftest orgx-bulk-resume-reports-no-unfinished-plans-test ()
  (orgx-bulk-buffer-test--with-env f "* TODO ResumeTask\n"
    (let ((err (should-error (orgx-bulk-resume) :type 'user-error)))
      (should (string-match-p "No unfinished bulk plans" (error-message-string err))))))

(ert-deftest orgx-bulk-resume-picker-errors-are-not-rewritten-test ()
  "Only the empty-list case is reworded; other picker errors pass through."
  (orgx-bulk-buffer-test--with-env f "* TODO ResumeTask\n"
    (orgx-bulk-create-plan :scope (list :type 'files :files (list f))
                           :targets (orgx-bulk-buffer-test--targets f)
                           :ops '((todo-state "DONE")) :status 'paused)
    (cl-letf (((symbol-function 'hitl-plan-ui-read-plan)
               (lambda (&rest _) (user-error "Some other picker failure"))))
      (let ((err (should-error (orgx-bulk-resume) :type 'user-error)))
        (should (equal "Some other picker failure" (error-message-string err)))))))

(provide 'orgx-bulk-buffer-test)

;;; orgx-bulk-buffer-test.el ends here
