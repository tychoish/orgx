;;; orgx-bulk-test.el --- Tests for orgx-bulk.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'orgx-bulk)
(require 'orgx-bulk-ops)
(require 'orgx-bulk-agenda)

(defmacro orgx-bulk-test--with-env (file-var content &rest body)
  "Create a temp `orgx-bulk-directory' and temp Org file FILE-VAR with CONTENT."
  (declare (indent 2))
  `(let* ((orgx-bulk-directory (make-temp-file "orgx-bulk-dir-" t))
          (,file-var (make-temp-file "orgx-bulk-" nil ".org" ,content))
          (org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(p)" "|" "DONE(d@)" "CANCELED(c@)")))
          (org-agenda-files (list ,file-var))
          (orgx-bulk--plans (make-hash-table :test #'equal))
          (orgx-bulk--counter 0)
          (orgx-bulk-log-notes 'ignore)
          (orgx-bulk-assign-ids nil)
          (orgx-bulk-inhibit-blocking nil))
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

(defun orgx-bulk-test--states (file)
  "Return the list of TODO states in FILE."
  (with-current-buffer (find-file-noselect file)
    (org-map-entries #'org-get-todo-state)))

(defun orgx-bulk-test--targets (file)
  "Return target plists for all headings in FILE."
  (with-current-buffer (find-file-noselect file)
    (org-map-entries #'orgx-bulk--target-at-point)))

;;; Persistence and Data Model

(ert-deftest orgx-bulk-save-load-roundtrip-test ()
  (orgx-bulk-test--with-env f "* TODO Alpha\n* TODO Beta\n"
    (let* ((targets (orgx-bulk-test--targets f))
           (plan (orgx-bulk-create-plan
                  :scope (list :type 'files :files (list f))
                  :targets targets
                  :ops (list (orgx-bulk-op--create :name 'todo-state :args '("DONE")))
                  :description "Roundtrip test"
                  :log-notes 'one
                  :note "Finished batch")))
      (setf (orgx-bulk-step-note (car (orgx-bulk-plan-steps plan))) "step-note")
      (orgx-bulk-save plan)
      (clrhash orgx-bulk--plans)
      (let ((loaded (orgx-bulk-load (orgx-bulk-plan-id plan))))
        (should (equal (orgx-bulk-plan-id loaded) (orgx-bulk-plan-id plan)))
        (should (equal (orgx-bulk-plan-description loaded) "Roundtrip test"))
        (should (eq (orgx-bulk-plan-status loaded) 'draft))
        (should (eq (orgx-bulk-plan-log-notes loaded) 'one))
        (should (equal (orgx-bulk-plan-note loaded) "Finished batch"))
        (should (= (length (orgx-bulk-plan-ops loaded)) 1))
        (should (= (length (orgx-bulk-plan-steps loaded)) 2))
        (should (eq (orgx-bulk-step-op (car (orgx-bulk-plan-steps loaded)))
                    (car (orgx-bulk-plan-ops loaded))))
        (should (equal (orgx-bulk-step-note (car (orgx-bulk-plan-steps loaded)))
                       "step-note"))))))

(ert-deftest orgx-bulk-corrupt-file-error-test ()
  (orgx-bulk-test--with-env f "* TODO Alpha\n"
    (let ((bad-file (expand-file-name "corrupt.eld" orgx-bulk-directory)))
      (with-temp-file bad-file
        (insert "(not-a-valid-plan"))
      (condition-case err
          (progn
            (orgx-bulk-load bad-file)
            (ert-fail "Expected error on corrupt plan file"))
        (error
         (should (string-match-p "corrupt\\.eld" (error-message-string err))))))))

(ert-deftest orgx-bulk-list-and-discard-plans-test ()
  (orgx-bulk-test--with-env f "* TODO Alpha\n"
    (let* ((targets (orgx-bulk-test--targets f))
           (p1 (orgx-bulk-create-plan :targets targets :ops '((todo-state "DONE"))))
           (p2 (orgx-bulk-create-plan :targets targets :ops '((todo-state "CANCELED")))))
      (should (= (length (orgx-bulk-list-plans '(draft))) 2))
      (orgx-bulk-discard p1)
      (should (eq (orgx-bulk-plan-status p1) 'discarded))
      (should (equal (mapcar #'orgx-bulk-plan-id (orgx-bulk-list-plans '(draft)))
                     (list (orgx-bulk-plan-id p2)))))))

(ert-deftest orgx-bulk-list-plans-keeps-live-plan-objects-test ()
  ;; Listing must not replace in-memory plans with stale on-disk copies.
  (orgx-bulk-test--with-env f "* TODO Alpha\n"
    (let ((plan (orgx-bulk-create-plan :targets (orgx-bulk-test--targets f)
                                       :ops '((todo-state "DONE")))))
      (setf (orgx-bulk-plan-description plan) "unsaved edit")
      (orgx-bulk-list-plans)
      (should (eq (orgx-bulk-get-plan (orgx-bulk-plan-id plan)) plan))
      ;; Plans only on disk are still discovered.
      (clrhash orgx-bulk--plans)
      (should (= (length (orgx-bulk-list-plans)) 1)))))

;;; Target Resolution

(ert-deftest orgx-bulk-resolve-target-id-path-and-siblings-test ()
  (orgx-bulk-test--with-env f "* Parent\n** TODO Sibling\n** TODO Sibling\n* TODO WithID\n:PROPERTIES:\n:ID:       fixed-id-123\n:END:\n"
    (let* ((targets (orgx-bulk-test--targets f))
           (sib1 (nth 1 targets))
           (sib2 (nth 2 targets))
           (with-id (nth 3 targets)))
      ;; Siblings with identical outline-path and heading resolve to distinct positions.
      (let ((m1 (orgx-bulk--resolve-target sib1))
            (m2 (orgx-bulk--resolve-target sib2)))
        (should m1)
        (should m2)
        (should (< (marker-position m1) (marker-position m2))))
      ;; Insert lines at top of file so positions shift; ID and outline-path still resolve.
      (with-current-buffer (find-file-noselect f)
        (goto-char (point-min))
        (insert "# Preamble line 1\n# Preamble line 2\n# Preamble line 3\n")
        (save-buffer))
      (let ((mid (orgx-bulk--resolve-target with-id))
            (ms1 (orgx-bulk--resolve-target sib1)))
        (should mid)
        (should (equal (org-with-point-at mid (org-get-heading t t t t)) "WithID"))
        (should ms1)
        (should (equal (org-with-point-at ms1 (org-get-heading t t t t)) "Sibling"))))))

(ert-deftest orgx-bulk-assign-ids-option-test ()
  (orgx-bulk-test--with-env f "* TODO NoID\n"
    (let ((orgx-bulk-assign-ids nil))
      (should-not (plist-get (car (orgx-bulk-test--targets f)) :id)))
    (let ((orgx-bulk-assign-ids t))
      (should (stringp (plist-get (car (orgx-bulk-test--targets f)) :id))))))

;;; Operation Registry

(ert-deftest orgx-bulk-registry-collision-test ()
  ;; Colliding with built-in oab key (`?t') when not `:builtin-oab' must signal error.
  (should-error
   (orgx-bulk-register-op 'test-bad-builtin-key
     :key ?t
     :apply #'ignore))
  ;; Custom non-oab key bridges into `org-agenda-bulk-custom-functions' and
  ;; colliding with another custom op key signals error.
  (unwind-protect
      (progn
        (orgx-bulk-register-op 'test-custom-op-1
          :key ?Z
          :apply #'ignore)
        (should (assoc ?Z org-agenda-bulk-custom-functions))
        (should-error
         (orgx-bulk-register-op 'test-custom-op-2
           :key ?Z
           :apply #'ignore)))
    (remhash 'test-custom-op-1 orgx-bulk--op-registry)
    (remhash ?Z orgx-bulk--oab-owned-keys)
    (setq org-agenda-bulk-custom-functions
          (assq-delete-all ?Z org-agenda-bulk-custom-functions))))

;;; Executor, Quit/Resume, Missing Targets, Hooks, and Denote Sync

(ert-deftest orgx-bulk-execute-and-skip-test ()
  (orgx-bulk-test--with-env f "* TODO First\n* TODO Second\n* DONE Already\n"
    (let* ((targets (orgx-bulk-test--targets f))
           (plan (orgx-bulk-create-plan
                  :scope (list :type 'files :files (list f))
                  :targets targets
                  :ops '((todo-state "DONE")))))
      ;; Third step is already DONE -> automatically marked `skipped'.
      (should (eq (orgx-bulk-step-status (nth 2 (orgx-bulk-plan-steps plan))) 'skipped))
      ;; Manually skip the first step.
      (setf (orgx-bulk-step-status (nth 0 (orgx-bulk-plan-steps plan))) 'skipped)
      (orgx-bulk-execute plan)
      (should (equal (orgx-bulk-test--states f) '("TODO" "DONE" "DONE")))
      (should (eq (orgx-bulk-plan-status plan) 'complete))
      (should-error (orgx-bulk-execute plan) :type 'user-error))))

(ert-deftest orgx-bulk-execute-quit-and-resume-test ()
  (orgx-bulk-test--with-env f "* TODO One\n* TODO Two\n* TODO Three\n"
    (let* ((targets (orgx-bulk-test--targets f))
           (plan (orgx-bulk-create-plan
                  :scope (list :type 'files :files (list f))
                  :targets targets
                  :ops '((todo-state "DONE"))))
           (calls 0))
      ;; Simulate `C-g' (`quit') after the first step completes.
      (let ((orgx-bulk-step-functions
             (list (lambda (_step _plan)
                     (setq calls (1+ calls))
                     (when (= calls 1)
                       (signal 'quit nil))))))
        (orgx-bulk-execute plan))
      (should (eq (orgx-bulk-plan-status plan) 'paused))
      (should (equal (mapcar #'orgx-bulk-step-status (orgx-bulk-plan-steps plan))
                     '(done pending pending)))
      (should (equal (orgx-bulk-test--states f) '("DONE" "TODO" "TODO")))
      ;; Reload from disk and resume execution: only remaining pending steps run.
      (let ((reloaded (orgx-bulk-load (orgx-bulk-plan-id plan)))
            (resumed-headings nil))
        (let ((orgx-bulk-step-functions
               (list (lambda (step _plan)
                       (push (plist-get (orgx-bulk-step-target step) :heading)
                             resumed-headings)))))
          (orgx-bulk-execute reloaded))
        (should (eq (orgx-bulk-plan-status reloaded) 'complete))
        (should (equal (nreverse resumed-headings) '("Two" "Three")))
        (should (equal (orgx-bulk-test--states f) '("DONE" "DONE" "DONE")))))))

(ert-deftest orgx-bulk-missing-target-and-unknown-op-test ()
  (orgx-bulk-test--with-env f "* TODO Keep\n* TODO Vanish\n"
    (let* ((targets (orgx-bulk-test--targets f))
           (plan (orgx-bulk-create-plan
                  :scope (list :type 'files :files (list f))
                  :targets targets
                  :ops '((todo-state "DONE")))))
      ;; Delete "Vanish" before executing.
      (with-current-buffer (find-file-noselect f)
        (goto-char (point-min))
        (re-search-forward "^\\* TODO Vanish")
        (delete-region (line-beginning-position) (point-max))
        (save-buffer))
      (orgx-bulk-execute plan)
      (should (equal (mapcar #'orgx-bulk-step-status (orgx-bulk-plan-steps plan))
                     '(done missing)))
      (should (eq (orgx-bulk-plan-status plan) 'complete)))
    ;; Unknown operation marks step `failed' without aborting.
    (let* ((targets (orgx-bulk-test--targets f))
           (bad-plan (orgx-bulk-create-plan
                      :scope (list :type 'files :files (list f))
                      :targets targets
                      :ops '((nonexistent-op "arg")))))
      (orgx-bulk-execute bad-plan)
      (should (eq (orgx-bulk-step-status (car (orgx-bulk-plan-steps bad-plan))) 'failed)))))

(ert-deftest orgx-bulk-hooks-and-denote-sync-test ()
  (orgx-bulk-test--with-env f "#+execution_status: in-progress\n* TODO TaskA\n* TODO TaskB\n"
    (cl-letf (((symbol-function 'orgx-denote-plan-file-p) (lambda (_) t)))
      (let* ((targets (orgx-bulk-test--targets f))
             (plan1 (orgx-bulk-create-plan
                     :scope (list :type 'files :files (list f))
                     :targets (list (car targets))
                     :ops '((todo-state "DONE")))))
        (orgx-bulk-execute plan1)
        (should (string-match-p "^#\\+execution_status: in-progress"
                                (with-temp-buffer (insert-file-contents f) (buffer-string))))
        (let ((plan2 (orgx-bulk-create-plan
                      :scope (list :type 'files :files (list f))
                      :targets (cdr targets)
                      :ops '((todo-state "DONE")))))
          (orgx-bulk-execute plan2)
          (should (string-match-p "^#\\+execution_status: complete"
                                  (with-temp-buffer (insert-file-contents f) (buffer-string)))))))))

(provide 'orgx-bulk-test)
;;; orgx-bulk-test.el ends here
