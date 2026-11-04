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

(defmacro orgx-bulk-buffer-test--with-env (file-var content &rest body)
  "Create a temp `orgx-bulk-directory' and temp Org file FILE-VAR with CONTENT."
  (declare (indent 2))
  `(let* ((orgx-bulk-directory (make-temp-file "orgx-bulk-buf-dir-" t))
          (,file-var (make-temp-file "orgx-bulk-buf-" nil ".org" ,content))
          (org-todo-keywords '((sequence "TODO" "INPROGRESS" "|" "DONE" "CANCELED")))
          (orgx-bulk--plans (make-hash-table :test #'equal))
          (orgx-bulk--counter 0)
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
                   (string-prefix-p "*orgx-bulk:" (buffer-name buf)))
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
        (should (derived-mode-p 'orgx-bulk-plan-mode))
        (should (eq (keymap-lookup orgx-bulk-plan-mode-map "c")
                    #'orgx-bulk-buffer-execute))
        (should (eq (keymap-lookup orgx-bulk-plan-mode-map "C-c C-c")
                    #'orgx-bulk-buffer-execute))
        ;; Move to first step line and toggle skip with `x'.
        (goto-char (point-min))
        (re-search-forward "First")
        (beginning-of-line)
        (orgx-bulk-buffer-toggle-skip)
        (should (eq (orgx-bulk-step-status (car (orgx-bulk-plan-steps plan))) 'skipped))
        ;; Edit note with `n' -> promotes `ignore' to `one'.
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Plan note")))
          (orgx-bulk-buffer-edit-note))
        (should (eq (orgx-bulk-plan-log-notes plan) 'one))
        (should (equal (orgx-bulk-plan-note plan) "Plan note"))
        ;; Add operation with `a' (`archive-sibling' should order after `todo-state').
        (cl-letf (((symbol-function 'annotated-completing-read)
                   (lambda (&rest _) "archive-sibling")))
          (orgx-bulk-buffer-add-op))
        (should (equal (mapcar #'orgx-bulk-op-name (orgx-bulk-plan-ops plan))
                       '(todo-state archive-sibling)))
        ;; Skip archive-sibling steps so we test executing only the remaining pending step.
        (dolist (s (orgx-bulk-plan-steps plan))
          (when (eq (orgx-bulk-step-op-name s) 'archive-sibling)
            (setf (orgx-bulk-step-status s) 'skipped)))
        ;; Execute via `orgx-bulk-buffer-execute' (`c' / `C-c C-c').
        (orgx-bulk-buffer-execute)
        (should (eq (orgx-bulk-plan-status plan) 'complete))
        (should (string-match-p "\\[✓\\]" (buffer-string)))
        (goto-char (point-min))
        (re-search-forward "\\[✓\\]")
        (should (eq (get-text-property (match-beginning 0) 'face)
                    'orgx-bulk-done-face))))))

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
      (should (eq (orgx-bulk-plan-status plan) 'discarded))
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
                 (lambda (&rest args)
                   (let ((cands (plist-get args :candidates)))
                     (should (assoc (orgx-bulk-plan-id plan) cands))
                     (orgx-bulk-plan-id plan))))
                ((symbol-function 'pop-to-buffer) (lambda (b &rest _) (set-buffer b))))
        (let ((buf (orgx-bulk-resume)))
          (should (buffer-live-p buf))
          (with-current-buffer buf
            (should (equal (orgx-bulk-plan-id orgx-bulk--current-plan) (orgx-bulk-plan-id plan)))))))))

(provide 'orgx-bulk-buffer-test)
;;; orgx-bulk-buffer-test.el ends here
