;;; orgx-bulk-ops-test.el --- Tests for orgx-bulk-ops.el and notes policy -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'orgx-bulk)
(require 'orgx-bulk-ops)
(require 'orgx-bulk-agenda)

(defmacro orgx-bulk-ops-test--with-env (file-var content &rest body)
  "Create a temp `orgx-bulk-directory' and temp Org file FILE-VAR with CONTENT."
  (declare (indent 2))
  `(let* ((orgx-bulk-directory (make-temp-file "orgx-bulk-ops-dir-" t))
          (,file-var (make-temp-file "orgx-bulk-ops-" nil ".org" ,content))
          (org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(p)" "|" "DONE(d@)" "CANCELED(c@)")))
          (org-agenda-files (list ,file-var))
          (orgx-bulk--plans (make-hash-table :test #'equal))
          (orgx-bulk--counter 0)
          (orgx-bulk-log-notes 'ignore)
          (orgx-bulk-inhibit-blocking nil))
     (unwind-protect
         (progn
           (with-current-buffer (find-file-noselect ,file-var)
             (org-mode)
             (org-set-regexps-and-options))
           ,@body)
       (dolist (buf (buffer-list))
         (when (or (and (buffer-file-name buf)
                        (string-prefix-p (file-name-sans-extension ,file-var)
                                         (buffer-file-name buf)))
                   (string-prefix-p "*Org Agenda" (buffer-name buf)))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (dolist (path (list ,file-var (concat ,file-var "_archive")))
         (when (file-exists-p path)
           (delete-file path)))
       (when (file-directory-p orgx-bulk-directory)
         (delete-directory orgx-bulk-directory t)))))

(defun orgx-bulk-ops-test--targets (file)
  "Return target plists for all headings in FILE."
  (with-current-buffer (find-file-noselect file)
    (org-map-entries #'orgx-bulk--target-at-point)))

(defun orgx-bulk-ops-test--run (file ops &rest plist)
  "Create and execute a plan on all headings in FILE with OPS."
  (let ((plan (apply #'orgx-bulk-create-plan
                     :scope (list :type 'files :files (list file))
                     :targets (orgx-bulk-ops-test--targets file)
                     :ops ops
                     plist)))
    (orgx-bulk-execute plan)))

;;; Built-in Operations

(ert-deftest orgx-bulk-ops-schedule-and-deadline-test ()
  (orgx-bulk-ops-test--with-env f "* TODO Task1\n"
    (orgx-bulk-ops-test--run f '((schedule "2026-11-15") (deadline "2026-11-20")))
    (let ((txt (with-temp-buffer (insert-file-contents f) (buffer-string))))
      (should (string-match-p "SCHEDULED: <2026-11-15" txt))
      (should (string-match-p "DEADLINE: <2026-11-20" txt)))
    ;; Clear schedule and deadline.
    (orgx-bulk-ops-test--run f '((schedule "") (deadline "")))
    (let ((txt (with-temp-buffer (insert-file-contents f) (buffer-string))))
      (should-not (string-match-p "SCHEDULED:" txt))
      (should-not (string-match-p "DEADLINE:" txt)))))

(ert-deftest orgx-bulk-ops-tags-priority-property-function-test ()
  (orgx-bulk-ops-test--with-env f "* TODO Task1 :old:\n"
    (defvar orgx-bulk-ops-test--fn-called nil)
    (setq orgx-bulk-ops-test--fn-called nil)
    (fset 'orgx-bulk-ops-test--mark-fn
          (lambda () (setq orgx-bulk-ops-test--fn-called (org-get-heading t t t t))))
    (orgx-bulk-ops-test--run
     f
     '((tag-add "urgent")
       (tag-remove "old")
       (priority "A")
       (property-set "EFFORT" "2h")
       (function orgx-bulk-ops-test--mark-fn)))
    (should (equal orgx-bulk-ops-test--fn-called "Task1"))
    (with-current-buffer (find-file-noselect f)
      (goto-char (point-min))
      (org-back-to-heading t)
      (should (member "urgent" (org-get-tags nil t)))
      (should-not (member "old" (org-get-tags nil t)))
      (should (equal (org-entry-get nil "EFFORT") "2h"))
      (should (string-match-p "\\[#A\\]" (buffer-string))))
    ;; Remove priority.
    (orgx-bulk-ops-test--run f '((priority "remove")))
    (with-current-buffer (find-file-noselect f)
      (should-not (string-match-p "\\[#A\\]" (buffer-string))))))

(ert-deftest orgx-bulk-ops-archive-and-sibling-ordering-test ()
  ;; Even when `archive-sibling' is listed BEFORE `todo-state' and `tag-add',
  ;; `orgx-bulk-order-ops' runs `archive-sibling' last so state and tag changes
  ;; happen before the entry moves to the Archive sibling.
  (orgx-bulk-ops-test--with-env f "* Project\n** TODO Subtask\n"
    (let* ((sub-target (nth 1 (orgx-bulk-ops-test--targets f)))
           (plan (orgx-bulk-create-plan
                  :scope (list :type 'files :files (list f))
                  :targets (list sub-target)
                  :ops '((archive-sibling)
                         (todo-state "DONE")
                         (tag-add "shipped")))))
      (should (equal (mapcar #'orgx-bulk-op-name (orgx-bulk-plan-ops plan))
                     '(todo-state tag-add archive-sibling)))
      (orgx-bulk-execute plan)
      (let ((txt (with-temp-buffer (insert-file-contents f) (buffer-string))))
        (should (string-match-p "\\*\\* Archive" txt))
        (should (string-match-p "\\*\\*\\* DONE Subtask" txt))
        (should (string-match-p ":shipped:" txt)))))
  ;; Standard `archive' operation moves heading to `<file>_archive'.
  (orgx-bulk-ops-test--with-env f2 "* TODO ArchiveMe\n* TODO StayHere\n"
    (let* ((first-target (car (orgx-bulk-ops-test--targets f2)))
           (plan (orgx-bulk-create-plan
                  :scope (list :type 'files :files (list f2))
                  :targets (list first-target)
                  :ops '((archive)))))
      (orgx-bulk-execute plan)
      (should (file-exists-p (concat f2 "_archive")))
      (let ((main-txt (with-temp-buffer (insert-file-contents f2) (buffer-string))))
        (should-not (string-match-p "ArchiveMe" main-txt))
        (should (string-match-p "StayHere" main-txt))))))

(ert-deftest orgx-bulk-ops-refile-test ()
  (orgx-bulk-ops-test--with-env f "* Inbox\n** TODO MoveMe\n* TargetBucket\n"
    (let* ((targets (orgx-bulk-ops-test--targets f))
           (move-target (nth 1 targets))
           (bucket-target (nth 2 targets))
           (plan (orgx-bulk-create-plan
                  :scope (list :type 'files :files (list f))
                  :targets (list move-target)
                  :ops (list (list 'refile
                                   (list :file f
                                         :heading "TargetBucket"
                                         :pos (plist-get bucket-target :pos)))
                             '(todo-state "INPROGRESS")))))
      ;; `refile' is ordered after `todo-state'.
      (should (equal (mapcar #'orgx-bulk-op-name (orgx-bulk-plan-ops plan))
                     '(todo-state refile)))
      (orgx-bulk-execute plan)
      (with-current-buffer (find-file-noselect f)
        (goto-char (point-min))
        (re-search-forward "^\\*\\* INPROGRESS MoveMe")
        (should (equal (org-get-outline-path) '("TargetBucket")))))))

(ert-deftest orgx-bulk-ops-refile-unresolved-heading-fails-test ()
  ;; A vanished destination heading must fail the step, not refile under
  ;; whatever heading now sits at the stale position hint.
  (orgx-bulk-ops-test--with-env f "* TODO MoveMe\n"
    (let ((dst (make-temp-file "orgx-bulk-ops-dst-" nil ".org"
                               "* Alpha\n* Beta\n** child\n")))
      (unwind-protect
          (let ((plan (orgx-bulk-create-plan
                       :scope (list :type 'files :files (list f))
                       :targets (orgx-bulk-ops-test--targets f)
                       :ops (list (list 'refile (list :file dst
                                                      :heading "Missing"
                                                      :pos 10))))))
            (orgx-bulk-execute plan)
            (should (eq (orgx-bulk-step-status (car (orgx-bulk-plan-steps plan)))
                        'failed))
            (with-current-buffer (find-file-noselect f)
              (should (equal (org-map-entries #'org-get-todo-state) '("TODO"))))
            (with-temp-buffer
              (insert-file-contents dst)
              (should-not (string-match-p "MoveMe" (buffer-string)))))
        (when-let* ((b (get-file-buffer dst)))
          (with-current-buffer b (set-buffer-modified-p nil))
          (kill-buffer b))
        (delete-file dst)))))

(ert-deftest orgx-bulk-ops-refile-file-level-and-persistence-test ()
  ;; A file-level destination files at the top level, the destination is saved
  ;; to disk, and the step keeps describing the refiled heading.
  (orgx-bulk-ops-test--with-env f "* Keep\n* TODO MoveMe\n* Also\n"
    (let ((dst (make-temp-file "orgx-bulk-ops-dst-" nil ".org"
                               "* Alpha\n** sub\n")))
      (unwind-protect
          (let* ((target (nth 1 (orgx-bulk-ops-test--targets f)))
                 (plan (orgx-bulk-create-plan
                        :scope (list :type 'files :files (list f))
                        :targets (list target)
                        :ops (list (list 'refile
                                         (list :file dst
                                               :heading (file-name-nondirectory dst)))))))
            (orgx-bulk-execute plan)
            (let ((step (car (orgx-bulk-plan-steps plan))))
              (should (eq (orgx-bulk-step-status step) 'done))
              (should (equal (plist-get (orgx-bulk-step-target step) :heading)
                             "MoveMe")))
            (with-temp-buffer
              (insert-file-contents dst)
              (should (string-match-p "^\\* TODO MoveMe$" (buffer-string)))))
        (when-let* ((b (get-file-buffer dst)))
          (with-current-buffer b (set-buffer-modified-p nil))
          (kill-buffer b))
        (delete-file dst)))))

;;; Blocking Policy

(ert-deftest orgx-bulk-ops-blocking-policy-test ()
  (orgx-bulk-ops-test--with-env f "* TODO Parent\n** TODO UndoneChild\n"
    (let* ((org-enforce-todo-dependencies t)
           (parent-target (car (orgx-bulk-ops-test--targets f)))
           (plan (orgx-bulk-create-plan
                  :scope (list :type 'files :files (list f))
                  :targets (list parent-target)
                  :ops '((todo-state "DONE")))))
      ;; Default (`orgx-bulk-inhibit-blocking' = nil): blocked step fails.
      (orgx-bulk-execute plan)
      (should (eq (orgx-bulk-step-status (car (orgx-bulk-plan-steps plan))) 'failed))
      ;; With `orgx-bulk-inhibit-blocking' = t: blocked step succeeds.
      (let ((orgx-bulk-inhibit-blocking t)
            (plan2 (orgx-bulk-create-plan
                    :scope (list :type 'files :files (list f))
                    :targets (list parent-target)
                    :ops '((todo-state "DONE")))))
        (orgx-bulk-execute plan2)
        (should (eq (orgx-bulk-step-status (car (orgx-bulk-plan-steps plan2))) 'done))))))

(ert-deftest orgx-bulk-ops-todo-state-repeater-test ()
  ;; A repeating task reverts to TODO after DONE; that is success, not "blocked".
  (orgx-bulk-ops-test--with-env f "* TODO Rep\nSCHEDULED: <2026-01-01 Thu +1d>\n"
    (let ((plan (orgx-bulk-ops-test--run f '((todo-state "DONE")))))
      (should (eq (orgx-bulk-step-status (car (orgx-bulk-plan-steps plan))) 'done))
      (should (eq (orgx-bulk-plan-status plan) 'complete))
      (with-temp-buffer
        (insert-file-contents f)
        (should-not (string-match-p "<2026-01-01" (buffer-string)))))))

;;; Notes Policy (`ignore', `one', `each')

(ert-deftest orgx-bulk-ops-notes-ignore-and-one-test ()
  ;; `ignore' writes no note even though DONE is configured as `DONE(d@)'.
  (orgx-bulk-ops-test--with-env f "* TODO First\n* TODO Second\n"
    (orgx-bulk-ops-test--run f '((todo-state "DONE")) :log-notes 'ignore)
    (let ((txt (with-temp-buffer (insert-file-contents f) (buffer-string))))
      (should-not (string-match-p "CLOSING NOTE" txt))
      (should-not (string-match-p "State \"DONE\"" txt))))
  ;; `one' writes the shared note to every changed heading without prompting.
  (orgx-bulk-ops-test--with-env f2 "* TODO First\n* TODO Second\n"
    (orgx-bulk-ops-test--run f2 '((todo-state "DONE"))
                             :log-notes 'one
                             :note "Batch completed reason")
    (let ((txt (with-temp-buffer (insert-file-contents f2) (buffer-string))))
      (with-temp-buffer
        (insert txt)
        (goto-char (point-min))
        (let ((count 0))
          (while (search-forward "Batch completed reason" nil t)
            (setq count (1+ count)))
          (should (= count 2)))))))

(ert-deftest orgx-bulk-ops-notes-each-and-resume-test ()
  (orgx-bulk-ops-test--with-env f "* TODO Alpha\n* TODO Beta\n"
    (let* ((targets (orgx-bulk-ops-test--targets f))
           (plan (orgx-bulk-create-plan
                  :scope (list :type 'files :files (list f))
                  :targets targets
                  :ops '((todo-state "DONE"))
                  :log-notes 'each))
           (prompt-count 0))
      ;; Pre-populate first step's note (simulating a resumed plan that already
      ;; collected the note for Alpha before pausing) and verify `read-string'
      ;; is only called once (for Beta).
      (setf (orgx-bulk-step-note (car (orgx-bulk-plan-steps plan)))
            "Pre-existing note for Alpha")
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt &rest _)
                   (setq prompt-count (1+ prompt-count))
                   "Prompted note for Beta")))
        (orgx-bulk-execute plan))
      (should (= prompt-count 1))
      (let ((txt (with-temp-buffer (insert-file-contents f) (buffer-string))))
        (should (string-match-p "Pre-existing note for Alpha" txt))
        (should (string-match-p "Prompted note for Beta" txt))))))

(provide 'orgx-bulk-ops-test)
;;; orgx-bulk-ops-test.el ends here
