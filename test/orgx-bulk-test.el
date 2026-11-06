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
          (hitl-plan--store (make-hash-table :test #'equal))
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
                   (string-prefix-p "*hitl-plan:" (buffer-name buf)))
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
      (setf (hitl-plan-step-note (car (hitl-plan-steps plan))) "step-note")
      (hitl-plan-save plan)
      (clrhash hitl-plan--store)
      (let ((loaded (orgx-bulk-load (hitl-plan-id plan))))
        (should (equal (hitl-plan-id loaded) (hitl-plan-id plan)))
        (should (equal (hitl-plan-description loaded) "Roundtrip test"))
        (should (eq (hitl-plan-status loaded) 'draft))
        (should (eq (hitl-plan-log-notes loaded) 'one))
        (should (equal (hitl-plan-note loaded) "Finished batch"))
        (should (= (length (hitl-plan-ops loaded)) 1))
        (should (= (length (hitl-plan-steps loaded)) 2))
        (should (eq (hitl-plan-step-op (car (hitl-plan-steps loaded)))
                    (car (hitl-plan-ops loaded))))
        (should (equal (hitl-plan-step-note (car (hitl-plan-steps loaded)))
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
      (should (eq (hitl-plan-status p1) 'discarded))
      (should (equal (mapcar #'hitl-plan-id (orgx-bulk-list-plans '(draft)))
                     (list (hitl-plan-id p2)))))))

(ert-deftest orgx-bulk-list-plans-keeps-live-plan-objects-test ()
  ;; Listing must not replace in-memory plans with stale on-disk copies.
  (orgx-bulk-test--with-env f "* TODO Alpha\n"
    (let ((plan (orgx-bulk-create-plan :targets (orgx-bulk-test--targets f)
                                       :ops '((todo-state "DONE")))))
      (setf (hitl-plan-description plan) "unsaved edit")
      (orgx-bulk-list-plans)
      (should (eq (orgx-bulk-get-plan (hitl-plan-id plan)) plan))
      ;; Plans only on disk are still discovered.
      (clrhash hitl-plan--store)
      (should (= (length (orgx-bulk-list-plans)) 1)))))

(ert-deftest orgx-bulk-add-op-reopens-complete-plan-test ()
  (orgx-bulk-test--with-env f "* TODO Alpha\n"
    (let ((plan (orgx-bulk-create-plan :targets (orgx-bulk-test--targets f)
                                       :ops '((todo-state "DONE")))))
      (orgx-bulk-execute plan)
      (should (eq (hitl-plan-status plan) 'complete))
      (hitl-plan-add-op plan (orgx-bulk-op--create :name 'tag-add :args '("x")))
      (should (eq (hitl-plan-status plan) 'paused))
      (orgx-bulk-execute plan)
      (should (eq (hitl-plan-status plan) 'complete))
      (with-current-buffer (find-file-noselect f)
        (should (member "x" (org-with-wide-buffer (goto-char (point-min))
                                                  (org-get-tags))))))))

(ert-deftest orgx-bulk-loads-legacy-orgx-plan-file-test ()
  ;; Pre-hitl orgx files have no `:domain' and steps carry an inline `:op'.
  (orgx-bulk-test--with-env f "* TODO Alpha\n"
    (with-temp-file (expand-file-name "old-1.eld" orgx-bulk-directory)
      (pp (list :version hitl-plan-format-version :id "old-1" :status 'paused
                :created "2026-01-01T00:00:00+0000"
                :ops (list (list :name 'todo-state :args '("DONE")))
                :steps (list (list :id 1 :target (list :file f :heading "Alpha")
                                   :op (list :name 'todo-state :args '("DONE"))
                                   :status 'pending)))
          (current-buffer)))
    (let ((plan (car (orgx-bulk-list-plans '(paused)))))
      (should (equal (hitl-plan-id plan) "old-1"))
      (should (eq (hitl-plan-domain plan) 'org-agenda))
      (should (eq (hitl-plan-step-op (car (hitl-plan-steps plan)))
                  (car (hitl-plan-ops plan))))
      (should (eq (orgx-bulk-get-plan "old-1") plan))
      (orgx-bulk-execute plan)
      (should (equal (orgx-bulk-test--states f) '("DONE"))))))

;; Legacy files in `orgx-bulk-directory' derive `org-agenda' from the domain
;; store, with no explicit domain or resolver.
(defun orgx-bulk-test--write-legacy (f dir)
  (with-temp-file (expand-file-name "old-1.eld" dir)
    (pp (list :version hitl-plan-format-version :id "old-1" :status 'paused
              :created "2026-01-01T00:00:00+0000"
              :ops (list (list :name 'todo-state :args '("DONE")))
              :steps (list (list :id 1 :target (list :file f :heading "Alpha")
                                 :op (list :name 'todo-state :args '("DONE"))
                                 :status 'pending)))
        (current-buffer))))

(ert-deftest orgx-bulk-legacy-derived-from-store-test ()
  (orgx-bulk-test--with-env f "* TODO Alpha\n"
    (orgx-bulk-test--write-legacy f orgx-bulk-directory)
    ;; hitl-plan-get with the store directory, no domain.
    (let ((plan (hitl-plan-get "old-1" orgx-bulk-directory)))
      (should (eq 'org-agenda (hitl-plan-domain plan))))
    (clrhash hitl-plan--store)
    ;; hitl-plan-get with the domain only.
    (should (eq 'org-agenda (hitl-plan-domain (hitl-plan-get "old-1" nil 'org-agenda))))
    (clrhash hitl-plan--store)
    ;; hitl-plan-list.
    (should (equal '(org-agenda)
                   (mapcar #'hitl-plan-domain
                           (hitl-plan-list '(paused) orgx-bulk-directory))))
    (clrhash hitl-plan--store)
    ;; UI refresh path: reload by id from the plan's directory.
    (let ((plan (hitl-plan-load "old-1" orgx-bulk-directory)))
      (clrhash hitl-plan--store)
      (should (eq 'org-agenda
                  (hitl-plan-domain
                   (hitl-plan-load (hitl-plan-id plan) (hitl-plan-directory plan))))))
    (clrhash hitl-plan--store)
    ;; ASQ dispatch lookup: default dir, then each domain store.
    (should (eq 'org-agenda
                (hitl-plan-domain
                 (seq-some (lambda (dir) (hitl-plan-get "old-1" dir))
                           (delq nil (mapcar #'hitl-plan-domain-store
                                             (hitl-plan-registered-domains)))))))))

(ert-deftest orgx-bulk-skips-satisfied-on-create-and-add-op-test ()
  (orgx-bulk-test--with-env f "* DONE Alpha\n* TODO Beta\n"
    (let* ((targets (list (list :file f :heading "Alpha" :todo-state "DONE")
                          (list :file f :heading "Beta" :todo-state "TODO")))
           (plan (orgx-bulk-create-plan :targets targets :ops '((todo-state "DONE")))))
      (should (equal '(skipped pending)
                     (mapcar #'hitl-plan-step-status (hitl-plan-steps plan))))
      (hitl-plan-add-op plan '(todo-state "DONE") targets)
      (should (equal '(skipped pending)
                     (mapcar #'hitl-plan-step-status
                             (last (hitl-plan-steps plan) 2)))))))

(ert-deftest orgx-bulk-domain-registration-test ()
  ;; Unresolvable targets use the domain's status; no orgx-side special case.
  (let ((domain (hitl-plan-get-domain 'org-agenda)))
    (should (eq (hitl-plan-domain-unresolved-status domain) 'missing))
    (should (stringp (hitl-plan-domain-unresolved-message domain)))
    (should (hitl-plan-domain-accept-buffer domain))
    (should (orgx-bulk-op-spec-builtin-oab (hitl-plan-get-op 'org-agenda 'todo-state)))
    (should-not (orgx-bulk-op-spec-builtin-oab (hitl-plan-get-op 'org-agenda 'property-set)))))

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
    (remhash (cons 'org-agenda 'test-custom-op-1) hitl-plan--op-registry)
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
      (should (eq (hitl-plan-step-status (nth 2 (hitl-plan-steps plan))) 'skipped))
      ;; Manually skip the first step.
      (setf (hitl-plan-step-status (nth 0 (hitl-plan-steps plan))) 'skipped)
      (orgx-bulk-execute plan)
      (should (equal (orgx-bulk-test--states f) '("TODO" "DONE" "DONE")))
      (should (eq (hitl-plan-status plan) 'complete))
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
      (should (eq (hitl-plan-status plan) 'paused))
      (should (equal (mapcar #'hitl-plan-step-status (hitl-plan-steps plan))
                     '(done pending pending)))
      (should (equal (orgx-bulk-test--states f) '("DONE" "TODO" "TODO")))
      ;; Reload from disk and resume execution: only remaining pending steps run.
      (let ((reloaded (orgx-bulk-load (hitl-plan-id plan)))
            (resumed-headings nil))
        (let ((orgx-bulk-step-functions
               (list (lambda (step _plan)
                       (push (plist-get (hitl-plan-step-target step) :heading)
                             resumed-headings)))))
          (orgx-bulk-execute reloaded))
        (should (eq (hitl-plan-status reloaded) 'complete))
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
      (should (equal (mapcar #'hitl-plan-step-status (hitl-plan-steps plan))
                     '(done missing)))
      (should (eq (hitl-plan-status plan) 'complete)))
    ;; Unknown operation marks step `failed' without aborting.
    (let* ((targets (orgx-bulk-test--targets f))
           (bad-plan (orgx-bulk-create-plan
                      :scope (list :type 'files :files (list f))
                      :targets targets
                      :ops '((nonexistent-op "arg")))))
      (orgx-bulk-execute bad-plan)
      (should (eq (hitl-plan-step-status (car (hitl-plan-steps bad-plan))) 'failed)))))

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

;;; Review follow-ups

(ert-deftest orgx-bulk-plan-ids-are-hitl-ids-test ()
  (orgx-bulk-test--with-env f "* TODO Alpha\n"
    (let ((plan (orgx-bulk-create-plan
                 :scope (list :type 'files :files (list f))
                 :targets (orgx-bulk-test--targets f) :ops '((todo-state "DONE")))))
      (should (string-match-p "\\`plan-[0-9]\\{8\\}-[0-9]\\{6\\}-[0-9a-f]\\{4\\}\\'"
                              (hitl-plan-id plan)))
      (should (file-exists-p (hitl-plan-file (hitl-plan-id plan) orgx-bulk-directory)))
      (should (equal (file-name-as-directory orgx-bulk-directory)
                     (file-name-as-directory (hitl-plan-directory plan)))))))

(ert-deftest orgx-bulk-plan-directory-follows-domain-on-load-and-add-op-test ()
  "No per-call directory fix-ups: create, add-op, discard and reload agree."
  (orgx-bulk-test--with-env f "* TODO Alpha\n"
    (let* ((plan (orgx-bulk-create-plan
                  :scope (list :type 'files :files (list f))
                  :targets (orgx-bulk-test--targets f) :ops '((todo-state "DONE"))))
           (id (hitl-plan-id plan)))
      (hitl-plan-add-op plan '(tag-add "x"))
      (should (= 1 (length (directory-files orgx-bulk-directory nil "\\.eld\\'"))))
      (clrhash hitl-plan--store)
      (let ((loaded (orgx-bulk-get-plan id)))
        (should loaded)
        (should (equal (file-name-as-directory orgx-bulk-directory)
                       (file-name-as-directory (hitl-plan-directory loaded))))
        (orgx-bulk-discard loaded)
        (should (eq 'discarded (hitl-plan-status
                                (hitl-plan-load (hitl-plan-file id orgx-bulk-directory)))))))))

(ert-deftest orgx-bulk-load-id-versus-file-test ()
  (orgx-bulk-test--with-env f "* TODO Alpha\n"
    (let* ((plan (orgx-bulk-create-plan
                  :scope (list :type 'files :files (list f))
                  :targets (orgx-bulk-test--targets f) :ops '((todo-state "DONE"))))
           (id (hitl-plan-id plan))
           (file (hitl-plan-file id orgx-bulk-directory)))
      ;; A bare ID, an absolute path and a `.eld' name all load the plan.
      (should (equal id (hitl-plan-id (orgx-bulk-load id))))
      (should (equal id (hitl-plan-id (orgx-bulk-load file))))
      ;; A relative `.eld' name is a file, resolved from `default-directory'.
      (let ((default-directory (file-name-as-directory orgx-bulk-directory)))
        (should (equal id (hitl-plan-id (orgx-bulk-load (concat id ".eld"))))))
      (should-error (orgx-bulk-load (concat id ".eld")))
      ;; A relative path without `.eld' is an ID, never a file: no cwd lookup.
      (let ((default-directory temporary-file-directory))
        (should (equal id (hitl-plan-id (orgx-bulk-load id)))))
      (should-error (orgx-bulk-load "no-such-plan")))))

(provide 'orgx-bulk-test)
;;; orgx-bulk-test.el ends here
