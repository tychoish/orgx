;;; orgx-capture-shell-test.el --- Tests for orgx-capture-shell -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'org)
(require 'org-capture)
(require 'org-agenda)
(require 'orgx-capture-shell)

(ert-deftest orgx-capture-shell-target-resolution-test ()
  "Test directory-based target cascading."
  (let ((orgx-capture-shell-target-directory-alist
         '(("/tmp/project-a" . ("/tmp/project-a/notes.org" . "ProjectTasks"))
           ("/tmp/project-b" . "/tmp/project-b/inbox.org")))
        (orgx-capture-shell-default-file "default.org")
        (orgx-capture-shell-default-headline "Inbox"))
    ;; Match with cons target
    (let ((res (orgx-capture-shell--resolve-target "/tmp/project-a/src")))
      (should (string-equal (car res) "/tmp/project-a/notes.org"))
      (should (string-equal (cdr res) "ProjectTasks")))

    ;; Match with string target
    (let ((res (orgx-capture-shell--resolve-target "/tmp/project-b/sub/dir")))
      (should (string-equal (car res) "/tmp/project-b/inbox.org"))
      (should (string-equal (cdr res) "Inbox")))

    ;; Fallback target
    (let ((res (orgx-capture-shell--resolve-target "/other/unmatched/path")))
      (should (string-suffix-p "default.org" (car res)))
      (should (string-equal (cdr res) "Inbox")))))

(ert-deftest orgx-capture-shell-formatting-test ()
  "Test entry formatting across templates."
  ;; Task template
  (should (string-equal (orgx-capture-shell--format-entry "Write unit tests" 'task)
                        "* TODO Write unit tests"))
  ;; Explicit heading preserved
  (should (string-equal (orgx-capture-shell--format-entry "* DONE Existing task" 'task)
                        "* DONE Existing task"))
  ;; Note template
  (should (string-equal (orgx-capture-shell--format-entry "Meeting summary" 'note)
                        "* Meeting summary"))
  ;; Journal template contains date
  (let ((j (orgx-capture-shell--format-entry "Daily standup" 'journal)))
    (should (string-prefix-p "* Daily standup <" j))
    (should (string-suffix-p ">" j))))

(ert-deftest orgx-capture-shell-insert-and-undo-test ()
  "Test inserting entry into Org file and undoing it."
  (let* ((tmp-file (make-temp-file "orgx-test-" nil ".org" "* Tasks\n** Existing item\n"))
         (orgx-capture-shell-undo-ring nil))
    (unwind-protect
        (let ((orgx-capture-shell-current-file tmp-file)
              (orgx-capture-shell-current-headline "Tasks")
              (orgx-capture-shell-current-template 'task))
          ;; Insert item (automatically recorded in undo-ring)
          (orgx-capture-shell--insert-entry
           tmp-file "Tasks" "* TODO Test item 1")
          ;; Verify inserted
          (with-temp-buffer
            (insert-file-contents tmp-file)
            (should (string-match-p "\\* TODO Test item 1" (buffer-string))))
          ;; Undo item
          (let ((msg (orgx-capture-shell--undo-last)))
            (should (string-match-p "Undid capture" msg)))
          ;; Verify deleted
          (with-temp-buffer
            (insert-file-contents tmp-file)
            (should-not (string-match-p "\\* TODO Test item 1" (buffer-string)))))
      (when (file-exists-p tmp-file)
        (delete-file tmp-file)))))

(ert-deftest orgx-capture-shell-slash-commands-test ()
  "Test slash command dispatching."
  (orgx-capture-shell-test--with-file "* Backlog\n"
    (let ((orgx-capture-shell-current-file file)
          (orgx-capture-shell-current-headline "Tasks")
          (orgx-capture-shell-current-template 'task)
          (orgx-capture-shell-undo-ring nil))
      ;; Switch target
      (let ((msg (orgx-capture-shell--execute-slash-command (format "/target %s:Backlog" file))))
        (should (string-match-p (format "Target set to %s \\[Backlog\\]" (regexp-quote (file-name-nondirectory file))) msg))
        (should (string-equal orgx-capture-shell-current-file (expand-file-name file)))
        (should (string-equal orgx-capture-shell-current-headline "Backlog")))
      ;; Switch template
      (let ((msg (orgx-capture-shell--execute-slash-command "/template journal")))
        (should (string-match-p "Template switched to: journal" msg))
        (should (eq orgx-capture-shell-current-template 'journal)))
      ;; Query help
      (let ((msg (orgx-capture-shell--execute-slash-command "/help")))
        (should (string-match-p "Available slash commands" msg)))
      ;; Query target
      (let ((msg (orgx-capture-shell--execute-slash-command "/target")))
        (should (string-match-p "Current target:" msg))))))


;;; Helpers

(defmacro orgx-capture-shell-test--with-file (content &rest body)
  "Run BODY with `file' bound to a temp Org file containing CONTENT."
  (declare (indent 1))
  `(let* ((file (make-temp-file "orgx-test-" nil ".org" ,content))
          (orgx-capture-shell-undo-ring nil)
          (orgx-capture-shell-headline-separator "/"))
     (unwind-protect (progn ,@body)
       (when-let* ((b (get-file-buffer file)))
         (with-current-buffer b (set-buffer-modified-p nil))
         (kill-buffer b))
       (delete-file file))))

(defun orgx-capture-shell-test--contents (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defmacro orgx-capture-shell-test--with-shell (input &rest body)
  "Run BODY in a temp shell-mode buffer whose line is a prompt followed by INPUT."
  (declare (indent 1))
  `(with-temp-buffer
     (orgx-capture-shell-mode)
     (insert (orgx-capture-shell--prompt-string) ,input)
     ,@body))

(defun orgx-capture-shell-test--capf-candidates ()
  "Return (START-OFFSET . CANDIDATES) from the capf at point."
  (when-let* ((r (orgx-capture-shell-completion-at-point))
              (table (nth 2 r)))
    (cons (nth 0 r)
          (all-completions (buffer-substring-no-properties (nth 0 r) (nth 1 r))
                           table))))

;;; Nesting

(ert-deftest orgx-capture-shell-relevel-test ()
  (should (equal (orgx-capture-shell--relevel "* TODO a" 3) "*** TODO a"))
  (should (equal (orgx-capture-shell--relevel "*** a" 1) "* a"))
  (should (equal (orgx-capture-shell--relevel "** a\nbody\n*** b" 3)
                 "*** a\nbody\n**** b"))
  (should (equal (orgx-capture-shell--relevel "no heading" 2) "no heading"))
  (should (equal (orgx-capture-shell--relevel "** a" 2) "** a")))

(ert-deftest orgx-capture-shell-format-bold-text-test ()
  "Text starting with *bold* is not mistaken for a heading."
  (should (equal (orgx-capture-shell--format-entry "*bold* thing" 'task)
                 "* TODO *bold* thing"))
  (should (equal (orgx-capture-shell--format-entry "** deep" 'task) "** deep"))
  (should (equal (orgx-capture-shell--format-entry "x" 'plain) "* x")))

(ert-deftest orgx-capture-shell-insert-nested-child-level-test ()
  "Entries become children of the target, not top-level headings."
  (orgx-capture-shell-test--with-file "* Projects\n** Tasks\n*** TODO old\n* Other\n"
    (orgx-capture-shell--insert-entry file "Tasks" "* TODO new")
    (should (equal (orgx-capture-shell-test--contents file)
                   "* Projects\n** Tasks\n*** TODO old\n*** TODO new\n* Other\n"))))

(ert-deftest orgx-capture-shell-insert-top-level-target-test ()
  (orgx-capture-shell-test--with-file "* Tasks\n** a\n* Other\n"
    (orgx-capture-shell--insert-entry file "Tasks" "* TODO new")
    (should (equal (orgx-capture-shell-test--contents file)
                   "* Tasks\n** a\n** TODO new\n* Other\n"))))

(ert-deftest orgx-capture-shell-insert-path-test ()
  "Outline paths disambiguate same-named headings."
  (orgx-capture-shell-test--with-file "* A\n** Tasks\n* B\n** Tasks\n"
    (orgx-capture-shell--insert-entry file "B/Tasks" "* TODO x")
    (should (equal (orgx-capture-shell-test--contents file)
                   "* A\n** Tasks\n* B\n** Tasks\n*** TODO x\n"))))

(ert-deftest orgx-capture-shell-insert-creates-nested-path-test ()
  (orgx-capture-shell-test--with-file "* A\n"
    (orgx-capture-shell--insert-entry file "A/New/Deep" "* TODO x")
    (should (equal (orgx-capture-shell-test--contents file)
                   "* A\n** New\n*** Deep\n**** TODO x\n"))))

(ert-deftest orgx-capture-shell-insert-creates-missing-toplevel-test ()
  (orgx-capture-shell-test--with-file "* A\n"
    (orgx-capture-shell--insert-entry file "Inbox" "* TODO x")
    (should (equal (orgx-capture-shell-test--contents file)
                   "* A\n* Inbox\n** TODO x\n"))))

(ert-deftest orgx-capture-shell-insert-no-headline-test ()
  (orgx-capture-shell-test--with-file "* A\n"
    (orgx-capture-shell--insert-entry file nil "** TODO x")
    (should (equal (orgx-capture-shell-test--contents file) "* A\n* TODO x\n"))))

(ert-deftest orgx-capture-shell-insert-no-trailing-newline-test ()
  (orgx-capture-shell-test--with-file "* Tasks\nbody"
    (orgx-capture-shell--insert-entry file "Tasks" "* TODO x")
    (should (equal (orgx-capture-shell-test--contents file)
                   "* Tasks\nbody\n** TODO x\n"))))

(ert-deftest orgx-capture-shell-insert-multiline-entry-test ()
  (orgx-capture-shell-test--with-file "* Tasks\n"
    (orgx-capture-shell--insert-entry file "Tasks" "* TODO x\nbody line")
    (should (equal (orgx-capture-shell-test--contents file)
                   "* Tasks\n** TODO x\nbody line\n"))))

(ert-deftest orgx-capture-shell-insert-does-not-match-ancestors-sibling-test ()
  "A path segment is only searched within the previous segment's subtree."
  (orgx-capture-shell-test--with-file "* A\n* B\n** Tasks\n"
    (orgx-capture-shell--insert-entry file "A/Tasks" "* TODO x")
    (should (equal (orgx-capture-shell-test--contents file)
                   "* A\n** Tasks\n*** TODO x\n* B\n** Tasks\n"))))

(ert-deftest orgx-capture-shell-undo-nested-test ()
  (orgx-capture-shell-test--with-file "* Projects\n** Tasks\n"
    (orgx-capture-shell--insert-entry file "Tasks" "* TODO x")
    (orgx-capture-shell--undo-last)
    (should (equal (orgx-capture-shell-test--contents file) "* Projects\n** Tasks\n"))))

(ert-deftest orgx-capture-shell-headline-paths-test ()
  (orgx-capture-shell-test--with-file "* A\n** B\n*** C\n** D\n* E\n"
    (should (equal (orgx-capture-shell--headline-paths file)
                   '("A" "A/B" "A/B/C" "A/D" "E")))
    (should-not (orgx-capture-shell--headline-paths "/nonexistent/file.org"))))

(ert-deftest orgx-capture-shell-capture-body-and-return-test ()
  (orgx-capture-shell-test--with-file "* Tasks\n"
    (let ((msg (orgx-capture-shell--capture "hello" :template 'task :file file
                                            :headline "Tasks" :body "some body\n")))
      (should (string-match-p "Captured -> \\[.*:Tasks\\] \\* TODO hello" msg)))
    (should (equal (orgx-capture-shell-test--contents file)
                   "* Tasks\n** TODO hello\nsome body\n"))))

;;; Prompt

(ert-deftest orgx-capture-shell-prompt-includes-template-test ()
  (let ((orgx-capture-shell-current-file "/tmp/foo.org")
        (orgx-capture-shell-current-headline "Tasks")
        (orgx-capture-shell-current-template 'journal))
    (should (equal (orgx-capture-shell--prompt-string)
                   "● [foo.org : Tasks · journal] ❯ ")))
  (let ((orgx-capture-shell-current-file nil)
        (orgx-capture-shell-current-headline nil)
        (orgx-capture-shell-current-template 'task))
    (should (equal (orgx-capture-shell--prompt-string) "● [none · task] ❯ "))))

(ert-deftest orgx-capture-shell-prompt-matches-regexp-test ()
  (let ((orgx-capture-shell-current-file "/tmp/foo.org")
        (orgx-capture-shell-current-headline "A/B")
        (orgx-capture-shell-current-template 'note))
    (should (string-match orgx-capture-shell--prompt-regexp
                          (concat (orgx-capture-shell--prompt-string) "a > b")))
    (should (equal (match-string 0 (concat (orgx-capture-shell--prompt-string) "a > b"))
                   (orgx-capture-shell--prompt-string)))))

(ert-deftest orgx-capture-shell-status-indicators-test ()
  (let ((orgx-capture-shell-current-file "/tmp/a.org")
        (orgx-capture-shell-current-headline nil)
        (orgx-capture-shell-current-template 'task))
    (should (string-prefix-p "●" (orgx-capture-shell--prompt-string 'ready)))
    (should (string-prefix-p "✓" (orgx-capture-shell--prompt-string 'success)))
    (should (string-prefix-p "✗" (orgx-capture-shell--prompt-string 'error)))
    (dolist (status '(ready success error))
      (should (string-match-p orgx-capture-shell--prompt-regexp
                              (orgx-capture-shell--prompt-string status))))))

(ert-deftest orgx-capture-shell-sender-status-test ()
  (cl-letf (((symbol-function 'orgx-capture-shell--capture)
             (lambda (&rest _) (setq orgx-capture-shell--status 'success) "Captured -> test")))
    (should (string-match-p "✓ \\[" (orgx-capture-shell-test--sender "hello"))))
  (cl-letf (((symbol-function 'orgx-capture-shell--capture)
             (lambda (&rest _) (error "Write failed"))))
    (let ((output (orgx-capture-shell-test--sender "hello")))
      (should (string-match-p "Write failed" output))
      (should (string-match-p "✗ \\[" output))))
  (should (string-match-p "✗ \\[" (orgx-capture-shell-test--sender "/not-a-command")))
  (let ((orgx-capture-shell-undo-ring nil))
    (should (string-match-p "✗ \\[" (orgx-capture-shell-test--sender "/undo"))))
  (should (string-match-p "● \\[" (orgx-capture-shell-test--sender ""))))

(ert-deftest orgx-capture-shell-transient-status-preserves-input-test ()
  (with-temp-buffer
    (orgx-capture-shell-mode)
    (let ((proc (start-process "orgx-form-status" (current-buffer) "cat"))
          (orgx-capture-shell--form-origin (current-buffer))
          (orgx-capture-shell--form '(:title "hello")))
      (unwind-protect
          (progn
            (comint-output-filter proc (orgx-capture-shell--prompt-string))
            (setq orgx-capture-shell--form-prompt-marker (copy-marker (process-mark proc)))
            (insert "unfinished input")
            (cl-letf (((symbol-function 'orgx-capture-shell--form-commit)
                       (lambda (_) (setq orgx-capture-shell--status 'success) "ok")))
              (orgx-capture-shell-form-commit))
            (should (string-prefix-p "✓ [" (buffer-string)))
            (should (string-suffix-p "unfinished input" (buffer-string)))
            (setq orgx-capture-shell--form-origin (current-buffer))
            (setq orgx-capture-shell--form-prompt-marker (copy-marker (process-mark proc)))
            (cl-letf (((symbol-function 'orgx-capture-shell--form-commit)
                       (lambda (_) (error "write failed"))))
              (orgx-capture-shell-form-commit))
            (should (string-prefix-p "✗ [" (buffer-string)))
            (should (string-suffix-p "unfinished input" (buffer-string))))
        (delete-process proc)))))

(ert-deftest orgx-capture-shell-transient-status-targets-original-prompt-test ()
  (with-temp-buffer
    (orgx-capture-shell-mode)
    (let ((proc (start-process "orgx-form-earlier" (current-buffer) "cat"))
          (orgx-capture-shell--form-origin (current-buffer))
          (orgx-capture-shell--form '(:title "hello")))
      (unwind-protect
          (progn
            (comint-output-filter proc (orgx-capture-shell--prompt-string))
            (setq orgx-capture-shell--form-prompt-marker (copy-marker (process-mark proc)))
            (goto-char (point-max))
            (insert "another input\n")
            (set-marker (process-mark proc) (point))
            (orgx-capture-shell-input-sender proc "")
            (cl-letf (((symbol-function 'orgx-capture-shell--form-commit)
                       (lambda (_) (setq orgx-capture-shell--status 'success) "ok")))
              (orgx-capture-shell-form-commit))
            (should (string-prefix-p "✓ [" (buffer-string)))
            (should (string-match-p "\n● \\[" (buffer-string))))
        (delete-process proc)))))

;;; Slash commands

(ert-deftest orgx-capture-shell-template-slash-invalid-test ()
  (let ((orgx-capture-shell-current-template 'task))
    (should (string-match-p "Unknown template"
                            (orgx-capture-shell--execute-slash-command "/template bogus")))
    (should (eq orgx-capture-shell-current-template 'task))
    (should (string-match-p "Current template: task"
                            (orgx-capture-shell--execute-slash-command "/tpl")))))

(ert-deftest orgx-capture-shell-target-slash-headline-with-colon-test ()
  (orgx-capture-shell-test--with-file "* Projects\n** My Tasks: now\n"
    (let ((orgx-capture-shell-current-file nil)
          (orgx-capture-shell-current-headline nil))
      (orgx-capture-shell--execute-slash-command (format "/target %s:Projects/My Tasks: now" file))
      (should (equal orgx-capture-shell-current-file (expand-file-name file)))
      (should (equal orgx-capture-shell-current-headline "Projects/My Tasks: now")))))

(ert-deftest orgx-capture-shell-validation-test ()
  "Test validation on target file and template selection."
  ;; Slash /target validation: non-existent file
  (let ((msg (orgx-capture-shell--execute-slash-command "/target /tmp/nonexistent-xyz-987.org")))
    (should (string-match-p "Error: Target file does not exist" msg)))
  ;; Slash /target validation: non-org file
  (let* ((tmp (make-temp-file "test-" nil ".txt" "text file")))
    (unwind-protect
        (let ((msg (orgx-capture-shell--execute-slash-command (format "/target %s" tmp))))
          (should (string-match-p "Error: Target file must be an Org file" msg)))
      (delete-file tmp)))
  ;; Slash /template validation: invalid template
  (let ((msg (orgx-capture-shell--execute-slash-command "/template non-existent-template-foo")))
    (should (string-match-p "Unknown template: non-existent-template-foo" msg)))
  ;; orgx-capture-shell-set-target-file: invalid file errors
  (should-error (orgx-capture-shell-set-target-file "/tmp/nonexistent-xyz-987.org")
                :type 'user-error)
  (let* ((tmp (make-temp-file "test-" nil ".txt" "text file")))
    (unwind-protect
        (should-error (orgx-capture-shell-set-target-file tmp) :type 'user-error)
      (delete-file tmp)))
  ;; orgx-capture-shell-set-template: invalid template errors
  (should-error (orgx-capture-shell-set-template 'not-a-valid-template)
                :type 'user-error))

(ert-deftest orgx-capture-shell-prompt-read-only-test ()
  "Prompt area should have read-only property and comint-prompt-read-only enabled."
  (let ((p (orgx-capture-shell--prompt-string)))
    (should (get-text-property 0 'read-only p))
    (should (equal (get-text-property 0 'front-sticky p) '(read-only))))
  (with-temp-buffer
    (orgx-capture-shell-mode)
    (should (eq comint-prompt-read-only t))))

(ert-deftest orgx-capture-shell-welcome-option-test ()
  "Welcome message should be controlled by orgx-capture-shell-show-welcome."
  (let ((orgx-capture-shell-show-welcome nil))
    (let ((buf (get-buffer "*orgx-capture-shell-test*")))
      (when buf (kill-buffer buf)))
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'orgx-capture-shell-buffer-name)
               (lambda () "*orgx-capture-shell-test*")))
      (let ((buf (orgx-capture-shell)))
        (unwind-protect
            (with-current-buffer buf
              (should-not (string-match-p "Orgx Capture Shell (type /help for commands)" (buffer-string))))
          (kill-buffer buf)))))
  (let ((orgx-capture-shell-show-welcome t))
    (let ((buf (get-buffer "*orgx-capture-shell-test*")))
      (when buf (kill-buffer buf)))
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'orgx-capture-shell-buffer-name)
               (lambda () "*orgx-capture-shell-test*")))
      (let ((buf (orgx-capture-shell)))
        (unwind-protect
            (with-current-buffer buf
              (should (string-match-p "Orgx Capture Shell (type /help for commands)" (buffer-string))))
          (kill-buffer buf))))))

(ert-deftest orgx-capture-shell-hide-slash-commands-test ()
  "When slash commands are hidden, capf returns nil and execution is blocked."
  (orgx-capture-shell-test--with-shell "/ta"
    (let ((orgx-capture-shell-hide-slash-commands t))
      (should-not (orgx-capture-shell-completion-at-point))
      (should (string-match-p "Slash commands are disabled"
                              (orgx-capture-shell--execute-slash-command "/target"))))))

(ert-deftest orgx-capture-shell-unknown-slash-test ()
  (should (string-match-p "Unknown command: /zzz"
                          (orgx-capture-shell--execute-slash-command "/zzz")))
  (should (string-match-p "Unknown command"
                          (orgx-capture-shell--execute-slash-command "/"))))

(ert-deftest orgx-capture-shell-help-mentions-form-test ()
  (should (string-match-p "/form" (orgx-capture-shell--execute-slash-command "/help"))))

;;; Completion at point

(ert-deftest orgx-capture-shell-capf-commands-test ()
  (orgx-capture-shell-test--with-shell "/ta"
    (let ((res (orgx-capture-shell-test--capf-candidates)))
      (should (equal (cdr res) '("/target")))
      (should (equal (buffer-substring-no-properties (car res) (point)) "/ta"))))
  (orgx-capture-shell-test--with-shell "/"
    (should (equal (sort (cdr (orgx-capture-shell-test--capf-candidates)) #'string<)
                   (sort (copy-sequence orgx-capture-shell-commands) #'string<)))))

(ert-deftest orgx-capture-shell-capf-no-completion-for-plain-text-test ()
  (orgx-capture-shell-test--with-shell "just text"
    (should-not (orgx-capture-shell-completion-at-point)))
  (orgx-capture-shell-test--with-shell "text /ta"
    (should-not (orgx-capture-shell-completion-at-point))))

(ert-deftest orgx-capture-shell-capf-ignores-prompt-test ()
  "The prompt itself (which contains no slash) is not completed."
  (orgx-capture-shell-test--with-shell ""
    (should-not (orgx-capture-shell-completion-at-point))))

(ert-deftest orgx-capture-shell-capf-template-args-test ()
  (orgx-capture-shell-test--with-shell "/template j"
    (should (equal (cdr (orgx-capture-shell-test--capf-candidates)) '("journal"))))
  (orgx-capture-shell-test--with-shell "/tpl "
    (should (equal (sort (cdr (orgx-capture-shell-test--capf-candidates)) #'string<)
                   '("journal" "note" "plain" "task")))))

(ert-deftest orgx-capture-shell-capf-target-headline-test ()
  (orgx-capture-shell-test--with-file "* Projects\n** Tasks\n* Inbox\n"
    (orgx-capture-shell-test--with-shell (format "/target %s:Pro" file)
      (let ((res (orgx-capture-shell-test--capf-candidates)))
        (should (equal (cdr res) '("Projects" "Projects/Tasks")))
        (should (equal (buffer-substring-no-properties (car res) (point)) "Pro"))))
    (orgx-capture-shell-test--with-shell (format "/target %s:" file)
      (should (= 3 (length (cdr (orgx-capture-shell-test--capf-candidates))))))))

(ert-deftest orgx-capture-shell-capf-target-file-test ()
  (orgx-capture-shell-test--with-file "* A\n"
    (let ((prefix (substring file 0 (- (length file) 3))))
      (orgx-capture-shell-test--with-shell (concat "/target " prefix)
        (let ((res (orgx-capture-shell-test--capf-candidates)))
          (should (member (file-name-nondirectory file)
                          (mapcar #'file-name-nondirectory (cdr res)))))))))

(ert-deftest orgx-capture-shell-capf-registered-in-mode-test ()
  (with-temp-buffer
    (orgx-capture-shell-mode)
    (should (memq #'orgx-capture-shell-completion-at-point
                  completion-at-point-functions))))

(ert-deftest orgx-capture-shell-default-directory-follows-target-test ()
  "Buffer default-directory matches the directory of the target file."
  (let ((tmpdir (make-temp-file "orgx-cap-dir-" t)))
    (unwind-protect
        (let* ((file (expand-file-name "target.org" tmpdir))
               (orgx-capture-shell-default-file file))
          (write-region "* Tasks\n" nil file nil 'quiet)
          (with-temp-buffer
            (orgx-capture-shell-mode)
            (should (equal (file-name-as-directory (expand-file-name default-directory))
                           (file-name-as-directory (expand-file-name tmpdir))))
            ;; Switch to another target in different directory
            (let* ((other-dir (make-temp-file "orgx-other-dir-" t))
                   (other-file (expand-file-name "other.org" other-dir)))
              (unwind-protect
                  (progn
                    (write-region "* Inbox\n" nil other-file nil 'quiet)
                    (orgx-capture-shell--set-target other-file "Inbox")
                    (should (equal (file-name-as-directory (expand-file-name default-directory))
                                   (file-name-as-directory (expand-file-name other-dir)))))
                (delete-directory other-dir t)))))
      (delete-directory tmpdir t))))

(ert-deftest orgx-capture-shell-abbrev-mode-test ()
  "Abbrev mode is enabled by default in orgx-capture-shell-mode."
  (with-temp-buffer
    (let ((orgx-capture-shell-enable-abbrev t))
      (orgx-capture-shell-mode)
      (should abbrev-mode)))
  (with-temp-buffer
    (let ((orgx-capture-shell-enable-abbrev nil))
      (orgx-capture-shell-mode)
      (should-not abbrev-mode))))

(ert-deftest orgx-capture-shell-text-capfs-registered-test ()
  "Cape and tempel backends are included in completion-at-point-functions when available."
  (cl-letf (((symbol-function 'cape-dabbrev) (lambda () nil))
            ((symbol-function 'cape-dict) (lambda () nil))
            ((symbol-function 'tempel-complete) (lambda () nil))
            ((symbol-function 'cape-file) (lambda () nil)))
    (with-temp-buffer
      (orgx-capture-shell-mode)
      (should (memq #'cape-dabbrev completion-at-point-functions))
      (should (memq #'cape-dict completion-at-point-functions))
      (should (memq #'tempel-complete completion-at-point-functions))
      (should (memq #'cape-file completion-at-point-functions)))))

;;; Visual line mode

(ert-deftest orgx-capture-shell-visual-line-mode-test ()
  (with-temp-buffer
    (orgx-capture-shell-mode)
    (should visual-line-mode)))

;;; Capture form

(ert-deftest orgx-capture-shell-form-trigger-detection-test ()
  (let ((orgx-capture-shell-form-trigger ";"))
    (should (orgx-capture-shell--form-triggered-p "buy milk;"))
    (should-not (orgx-capture-shell--form-triggered-p "buy milk")))
  (let ((orgx-capture-shell-form-trigger "|"))
    (should (orgx-capture-shell--form-triggered-p "x |"))
    (should-not (orgx-capture-shell--form-triggered-p "x;")))
  (let ((orgx-capture-shell-form-trigger nil))
    (should-not (orgx-capture-shell--form-triggered-p "x;")))
  (let ((orgx-capture-shell-form-trigger ""))
    (should-not (orgx-capture-shell--form-triggered-p "x"))))

(defun orgx-capture-shell-test--sender (input &optional force)
  "Run the input sender on INPUT in a shell buffer; return the buffer text."
  (with-temp-buffer
    (orgx-capture-shell-mode)
    (let ((proc (start-process "orgx-test" (current-buffer) "cat"))
          (orgx-capture-shell--force-form force))
      (set-process-query-on-exit-flag proc nil)
      (unwind-protect
          (progn (orgx-capture-shell-input-sender proc input)
                 (buffer-string))
        (delete-process proc)))))

(ert-deftest orgx-capture-shell-sender-plain-capture-test ()
  (orgx-capture-shell-test--with-file "* Tasks\n"
    (let ((orgx-capture-shell-form-trigger ";")
          (orgx-capture-shell-current-file file)
          (orgx-capture-shell-current-headline "Tasks"))
      (cl-letf (((symbol-function 'orgx-capture-shell--run-form)
                 (lambda (&rest _) (error "form must not run"))))
        (let ((out (with-temp-buffer
                     (orgx-capture-shell-mode)
                     (setq orgx-capture-shell-current-file file
                           orgx-capture-shell-current-headline "Tasks")
                     (let ((proc (start-process "orgx-test" (current-buffer) "cat")))
                       (set-process-query-on-exit-flag proc nil)
                       (orgx-capture-shell-input-sender proc "buy milk")
                       (delete-process proc)
                       (buffer-string)))))
          (should (string-match-p "Captured -> \\[.*:Tasks\\] \\* TODO buy milk" out))
          (should (string-match-p "✓ \\[.*· task\\] ❯ " out))))
      (should (equal (orgx-capture-shell-test--contents file)
                     "* Tasks\n** TODO buy milk\n")))))

(ert-deftest orgx-capture-shell-sender-trigger-opens-form-test ()
  (let ((orgx-capture-shell-form-trigger ";") seen)
    (cl-letf (((symbol-function 'orgx-capture-shell--run-form)
               (lambda (&optional initial) (setq seen (list initial)) "FORM-RAN")))
      (should (string-match-p "FORM-RAN" (orgx-capture-shell-test--sender "buy milk ;")))
      (should (equal seen '("buy milk"))))))

(ert-deftest orgx-capture-shell-sender-force-opens-form-test ()
  (let ((orgx-capture-shell-form-trigger ";") seen)
    (cl-letf (((symbol-function 'orgx-capture-shell--run-form)
               (lambda (&optional initial) (setq seen (list initial)) "FORM-RAN")))
      (orgx-capture-shell-test--sender "no trigger" t)
      (should (equal seen '("no trigger")))
      (setq seen nil)
      (orgx-capture-shell-test--sender "" t)
      (should (equal seen '(""))))))

(ert-deftest orgx-capture-shell-sender-slash-wins-over-trigger-test ()
  (let ((orgx-capture-shell-form-trigger ";"))
    (cl-letf (((symbol-function 'orgx-capture-shell--run-form)
               (lambda (&rest _) (error "form must not run"))))
      (should (string-match-p "Available slash commands"
                              (orgx-capture-shell-test--sender "/help ;"))))))

(ert-deftest orgx-capture-shell-sender-empty-input-test ()
  (cl-letf (((symbol-function 'orgx-capture-shell--run-form)
             (lambda (&rest _) (error "form must not run"))))
    (should-not (string-match-p "Captured" (orgx-capture-shell-test--sender "   ")))))

(ert-deftest orgx-capture-shell-send-form-binds-force-test ()
  (let (seen)
    (cl-letf (((symbol-function 'comint-send-input)
               (lambda (&rest _) (setq seen orgx-capture-shell--force-form))))
      (orgx-capture-shell-send-form)
      (should seen)
      (should-not orgx-capture-shell--force-form))
    (should (eq (lookup-key orgx-capture-shell-mode-map (kbd "C-<return>"))
                #'orgx-capture-shell-send-form))))

(ert-deftest orgx-capture-shell-form-prompt-style-test ()
  (orgx-capture-shell-test--with-file "* Projects\n** Tasks\n"
    (let ((orgx-capture-shell-form-style 'prompt)
          (strings (list "Write docs" "details here"))
          (choices (list "note" "Projects/Tasks"))
          prompts)
      (cl-letf (((symbol-function 'orgx-capture-shell--form-defaults)
                 (lambda () (list :file file :headline "Projects" :template 'task)))
                ((symbol-function 'read-string)
                 (lambda (prompt &rest _) (push prompt prompts) (pop strings)))
                ((symbol-function 'completing-read)
                 (lambda (prompt &rest _) (push prompt prompts) (pop choices)))
                ((symbol-function 'read-file-name)
                 (lambda (&rest _) file)))
        (should (string-match-p "Captured" (orgx-capture-shell--run-form "seed")))
        (should (equal (mapcar (lambda (p) (car (split-string p " "))) (nreverse prompts))
                       '("Title:" "Template:" "Headline" "Body"))))
      (should (equal (orgx-capture-shell-test--contents file)
                     "* Projects\n** Tasks\n*** Write docs\ndetails here\n")))))

(ert-deftest orgx-capture-shell-form-prompt-empty-title-cancels-test ()
  (orgx-capture-shell-test--with-file "* Tasks\n"
    (let ((orgx-capture-shell-form-style 'prompt))
      (cl-letf (((symbol-function 'orgx-capture-shell--form-defaults)
                 (lambda () (list :file file :headline "Tasks" :template 'task)))
                ((symbol-function 'read-string) (lambda (&rest _) "  ")))
        (should (string-match-p "cancelled" (orgx-capture-shell--run-form "")))
        (should (equal (orgx-capture-shell-test--contents file) "* Tasks\n"))))))

(ert-deftest orgx-capture-shell-form-prompt-quit-cancels-test ()
  (let ((orgx-capture-shell-form-style 'prompt))
    (cl-letf (((symbol-function 'orgx-capture-shell--form-defaults)
               (lambda () (list :file "/tmp/x.org" :headline "T" :template 'task)))
              ((symbol-function 'read-string) (lambda (&rest _) (signal 'quit nil))))
      (should (string-match-p "cancelled" (orgx-capture-shell--run-form "x"))))))

(ert-deftest orgx-capture-shell-form-transient-style-test ()
  (let ((orgx-capture-shell-form-style 'transient)
        (orgx-capture-shell--form nil)
        opened)
    (cl-letf (((symbol-function 'orgx-capture-shell--form-defaults)
               (lambda () (list :file "/tmp/x.org" :headline "T" :template 'note)))
              ((symbol-function 'orgx-capture-shell-form-transient)
               (lambda () (setq opened t))))
      (should (string-match-p "opened" (orgx-capture-shell--run-form "seed")))
      (should opened)
      (should (equal (plist-get orgx-capture-shell--form :title) "seed"))
      (should (eq (plist-get orgx-capture-shell--form :template) 'note))
      (should (equal (plist-get orgx-capture-shell--form :headline) "T")))))

(ert-deftest orgx-capture-shell-form-transient-commit-test ()
  (orgx-capture-shell-test--with-file "* Tasks\n"
    (let ((orgx-capture-shell--form
           (list :title "From form" :template 'task :file file
                 :headline "Tasks" :body "b")))
      (orgx-capture-shell-form-commit)
      (should-not orgx-capture-shell--form)
      (should (equal (orgx-capture-shell-test--contents file)
                     "* Tasks\n** TODO From form\nb\n")))))

(ert-deftest orgx-capture-shell-form-transient-setters-test ()
  (let ((orgx-capture-shell--form (list :title "" :file "/tmp/a.org"
                                        :headline "H" :template 'task)))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "typed"))
              ((symbol-function 'completing-read) (lambda (&rest _) "journal"))
              ((symbol-function 'read-file-name) (lambda (&rest _) "/tmp/b.org")))
      (orgx-capture-shell-form-set-title)
      (orgx-capture-shell-form-set-body)
      (orgx-capture-shell-form-set-template)
      (orgx-capture-shell-form-set-file)
      (should (equal (plist-get orgx-capture-shell--form :title) "typed"))
      (should (equal (plist-get orgx-capture-shell--form :body) "typed"))
      (should (eq (plist-get orgx-capture-shell--form :template) 'journal))
      (should (equal (plist-get orgx-capture-shell--form :file) "/tmp/b.org")))))

(ert-deftest orgx-capture-shell-form-commit-empty-title-test ()
  (should (string-match-p "cancelled"
                          (orgx-capture-shell--form-commit (list :title " ")))))

(ert-deftest orgx-capture-shell-form-slash-test ()
  (let (seen)
    (cl-letf (((symbol-function 'orgx-capture-shell--run-form)
               (lambda (&optional i) (setq seen i) "ok")))
      (should (equal (orgx-capture-shell--execute-slash-command "/form write the docs") "ok"))
      (should (equal seen "write the docs")))))

(ert-deftest orgx-capture-shell-form-transient-defined-test ()
  (should (commandp 'orgx-capture-shell-form-transient))
  (should (commandp 'orgx-capture-shell-form))
  (should (transient-get-suffix 'orgx-capture-shell-form-transient "c"))
  (should (transient-get-suffix 'orgx-capture-shell-transient "F")))

(ert-deftest orgx-capture-shell-read-headline-allows-top-level-test ()
  "The default is offered as initial input so it can be cleared to pick top level."
  (let (args)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest a) (setq args a) "")))
      (orgx-capture-shell-test--with-file "* Tasks\n"
        (should (equal (orgx-capture-shell--read-headline file "Tasks") ""))
        (should (equal (nth 4 args) "Tasks"))
        (should-not (nth 6 args))))))

(ert-deftest orgx-capture-shell-capture-empty-headline-uses-default-test ()
  (orgx-capture-shell-test--with-file "* Tasks\n"
    (let ((orgx-capture-shell-default-headline "Tasks"))
      (cl-letf (((symbol-function 'orgx-capture-shell--resolve-target)
                 (lambda (&optional _) (cons file "Tasks"))))
        (orgx-capture-shell--capture "x" :file nil :headline "" :template 'task)
        (should (equal (orgx-capture-shell-test--contents file)
                       "* Tasks\n** TODO x\n"))))))

(ert-deftest orgx-capture-shell-form-trigger-escape-test ()
  (let ((orgx-capture-shell-form-trigger ";"))
    (should-not (orgx-capture-shell--form-triggered-p "x;;"))
    (should (orgx-capture-shell--form-triggered-p "x;"))
    (should (equal (orgx-capture-shell--unescape-trigger "x;;") "x;"))
    (should (equal (orgx-capture-shell--unescape-trigger "x;") "x;"))
    (should (equal (orgx-capture-shell--unescape-trigger "x") "x")))
  (let ((orgx-capture-shell-form-trigger nil))
    (should (equal (orgx-capture-shell--unescape-trigger "x;;") "x;;"))))

(ert-deftest orgx-capture-shell-sender-escaped-trigger-captures-test ()
  (orgx-capture-shell-test--with-file "* Tasks\n"
    (let ((orgx-capture-shell-form-trigger ";"))
      (cl-letf (((symbol-function 'orgx-capture-shell--run-form)
                 (lambda (&rest _) (error "form must not run"))))
        (with-temp-buffer
          (orgx-capture-shell-mode)
          (setq orgx-capture-shell-current-file file
                orgx-capture-shell-current-headline "Tasks")
          (let ((proc (start-process "orgx-test" (current-buffer) "cat")))
            (set-process-query-on-exit-flag proc nil)
            (orgx-capture-shell-input-sender proc "int x = 1;;")
            (delete-process proc))))
      (should (equal (orgx-capture-shell-test--contents file)
                     "* Tasks\n** TODO int x = 1;\n")))))


;;; Prepend and Archive handling

(defun orgx-capture-shell-test--ins (content headline prepend &optional entry)
  "Insert ENTRY under HEADLINE in a file holding CONTENT; return the result."
  (orgx-capture-shell-test--with-file content
    (orgx-capture-shell--insert-entry file headline (or entry "* TODO new") prepend)
    (orgx-capture-shell-test--contents file)))

(ert-deftest orgx-capture-shell-prepend-default-test ()
  (should (eq (default-value 'orgx-capture-shell-prepend) 'follow)))

(ert-deftest orgx-capture-shell-prepend-before-first-child-test ()
  (should (equal (orgx-capture-shell-test--ins "* Tasks\nbody\n** a\n** b\n* Other\n" "Tasks" t)
                 "* Tasks\nbody\n** TODO new\n** a\n** b\n* Other\n")))

(ert-deftest orgx-capture-shell-prepend-no-children-test ()
  (should (equal (orgx-capture-shell-test--ins "* Tasks\nbody\n* Other\n" "Tasks" t)
                 "* Tasks\nbody\n** TODO new\n* Other\n"))
  (should (equal (orgx-capture-shell-test--ins "* Tasks\nbody" "Tasks" t)
                 "* Tasks\nbody\n** TODO new\n")))

(ert-deftest orgx-capture-shell-prepend-never-after-archive-test ()
  (should (equal (orgx-capture-shell-test--ins
                  "* Tasks\n** a\n** Archive :ARCHIVE:\n*** old\n" "Tasks" t)
                 "* Tasks\n** TODO new\n** a\n** Archive :ARCHIVE:\n*** old\n")))

(ert-deftest orgx-capture-shell-prepend-only-archive-child-test ()
  (should (equal (orgx-capture-shell-test--ins
                  "* Tasks\n** Archive :ARCHIVE:\n*** old\n" "Tasks" t)
                 "* Tasks\n** TODO new\n** Archive :ARCHIVE:\n*** old\n")))

(ert-deftest orgx-capture-shell-prepend-top-level-test ()
  (should (equal (orgx-capture-shell-test--ins "#+title: x\n\n* A\n* B\n" nil t "** TODO new")
                 "#+title: x\n\n* TODO new\n* A\n* B\n"))
  (should (equal (orgx-capture-shell-test--ins "#+title: x\n" nil t)
                 "#+title: x\n* TODO new\n")))

(ert-deftest orgx-capture-shell-prepend-order-newest-first-test ()
  (orgx-capture-shell-test--with-file "* Tasks\n** old\n"
    (orgx-capture-shell--insert-entry file "Tasks" "* TODO one" t)
    (orgx-capture-shell--insert-entry file "Tasks" "* TODO two" t)
    (should (equal (orgx-capture-shell-test--contents file)
                   "* Tasks\n** TODO two\n** TODO one\n** old\n"))))

(ert-deftest orgx-capture-shell-prepend-nested-path-test ()
  (should (equal (orgx-capture-shell-test--ins "* P\n** Tasks\n*** a\n* Q\n" "P/Tasks" t)
                 "* P\n** Tasks\n*** TODO new\n*** a\n* Q\n")))

(ert-deftest orgx-capture-shell-prepend-created-path-test ()
  (should (equal (orgx-capture-shell-test--ins "* A\n" "A/New" t)
                 "* A\n** New\n*** TODO new\n")))

(ert-deftest orgx-capture-shell-prepend-undo-test ()
  (orgx-capture-shell-test--with-file "* Tasks\n** a\n"
    (orgx-capture-shell--insert-entry file "Tasks" "* TODO x" t)
    (orgx-capture-shell--undo-last)
    (should (equal (orgx-capture-shell-test--contents file) "* Tasks\n** a\n"))))

(ert-deftest orgx-capture-shell-append-before-trailing-archive-test ()
  (should (equal (orgx-capture-shell-test--ins
                  "* Tasks\n** a\n** Archive :ARCHIVE:\n*** old\n" "Tasks" nil)
                 "* Tasks\n** a\n** TODO new\n** Archive :ARCHIVE:\n*** old\n")))

(ert-deftest orgx-capture-shell-append-before-multiple-trailing-archives-test ()
  (should (equal (orgx-capture-shell-test--ins
                  "* T\n** a\n** A1 :ARCHIVE:\n** A2 :ARCHIVE:\n" "T" nil)
                 "* T\n** a\n** TODO new\n** A1 :ARCHIVE:\n** A2 :ARCHIVE:\n")))

(ert-deftest orgx-capture-shell-append-archive-in-middle-not-skipped-test ()
  (should (equal (orgx-capture-shell-test--ins
                  "* T\n** A :ARCHIVE:\n** b\n" "T" nil)
                 "* T\n** A :ARCHIVE:\n** b\n** TODO new\n")))

(ert-deftest orgx-capture-shell-append-only-archive-test ()
  (should (equal (orgx-capture-shell-test--ins "* T\n** A :ARCHIVE:\n" "T" nil)
                 "* T\n** TODO new\n** A :ARCHIVE:\n")))

(ert-deftest orgx-capture-shell-append-deep-archive-not-direct-child-test ()
  "Only direct children are considered; a deeper tagged heading doesn't move the entry."
  (should (equal (orgx-capture-shell-test--ins "* T\n** a\n*** x :ARCHIVE:\n" "T" nil)
                 "* T\n** a\n*** x :ARCHIVE:\n** TODO new\n")))

(ert-deftest orgx-capture-shell-append-top-level-archive-test ()
  (should (equal (orgx-capture-shell-test--ins "* A\n* Archive :ARCHIVE:\n" nil nil)
                 "* A\n* TODO new\n* Archive :ARCHIVE:\n")))

(ert-deftest orgx-capture-shell-append-custom-archive-tag-test ()
  (let ((org-archive-tag "OLD"))
    (should (equal (orgx-capture-shell-test--ins "* T\n** a\n** Z :OLD:\n" "T" nil)
                   "* T\n** a\n** TODO new\n** Z :OLD:\n"))))


;;; Placement follows the capture template

(defun orgx-capture-shell-test--tpl (kind path prepend &optional headline)
  "Return a flat capture template of KIND (a description word) for PATH."
  (list "xx" (format "%s (proj; q)" kind) 'entry (list (quote file+headline) path (or headline "Tasks"))
        "* %?" :prepend prepend))

(ert-deftest orgx-capture-shell-resolve-prepend-builtin-defaults-test ()
  (let ((orgx-capture-shell-prepend 'follow) (org-capture-templates nil))
    (should (eq (orgx-capture-shell--resolve-prepend 'task) t))
    (should (eq (orgx-capture-shell--resolve-prepend 'note) t))
    (should (eq (orgx-capture-shell--resolve-prepend 'journal) nil))
    (should (eq (orgx-capture-shell--resolve-prepend 'plain) t))))

(ert-deftest orgx-capture-shell-resolve-prepend-forced-test ()
  (let ((org-capture-templates (list (orgx-capture-shell-test--tpl "tasks" "/tmp/a.org" nil))))
    (let ((orgx-capture-shell-prepend t))
      (should (eq (orgx-capture-shell--resolve-prepend 'journal) t))
      (should (eq (orgx-capture-shell--resolve-prepend 'task "/tmp/a.org") t)))
    (let ((orgx-capture-shell-prepend nil))
      (should (eq (orgx-capture-shell--resolve-prepend 'task) nil)))))

(ert-deftest orgx-capture-shell-resolve-prepend-follows-template-test ()
  (let ((orgx-capture-shell-prepend 'follow)
        (org-capture-templates
         (list (orgx-capture-shell-test--tpl "tasks" "/tmp/a.org" nil)
               (orgx-capture-shell-test--tpl "journal" "/tmp/a.org" t))))
    (should (eq (orgx-capture-shell--resolve-prepend 'task) nil))
    (should (eq (orgx-capture-shell--resolve-prepend 'journal) t))
    ;; no note template: builtin default
    (should (eq (orgx-capture-shell--resolve-prepend 'note) t))))

(ert-deftest orgx-capture-shell-resolve-prepend-prefers-same-file-test ()
  (let ((orgx-capture-shell-prepend 'follow)
        (org-capture-templates
         (list (orgx-capture-shell-test--tpl "tasks" "/tmp/a.org" nil)
               (orgx-capture-shell-test--tpl "tasks" "/tmp/b.org" t))))
    (should (eq (orgx-capture-shell--resolve-prepend 'task "/tmp/b.org") t))
    (should (eq (orgx-capture-shell--resolve-prepend 'task "/tmp/a.org") nil))
    ;; unknown file: first template of that kind
    (should (eq (orgx-capture-shell--resolve-prepend 'task "/tmp/zzz.org") nil))))

(ert-deftest orgx-capture-shell-resolve-prepend-relative-path-test ()
  (let ((orgx-capture-shell-prepend 'follow)
        (org-directory "/tmp/orgdir")
        (org-capture-templates
         (list (orgx-capture-shell-test--tpl "tasks" "x.org" nil)
               (orgx-capture-shell-test--tpl "tasks" "y.org" t))))
    (should (eq (orgx-capture-shell--resolve-prepend 'task "/tmp/orgdir/y.org") t))))

(ert-deftest orgx-capture-shell-resolve-prepend-ignores-non-flat-entries-test ()
  "Group entries and templates without :prepend don't confuse the lookup."
  (let ((orgx-capture-shell-prepend 'follow)
        (org-capture-templates
         (list '("x" "tasks group")
               (list "xt" "tasks (p; q)" 'entry '(file "/tmp/a.org") "* %?"))))
    ;; the :prepend-less template means org's default: append
    (should (eq (orgx-capture-shell--resolve-prepend 'task "/tmp/a.org") nil))))

(ert-deftest orgx-capture-shell-capture-follows-template-end-to-end-test ()
  "Tasks prepend and journal entries append, per the capture templates."
  (let ((orgx-capture-shell-prepend 'follow)
        (org-capture-templates nil))
    (orgx-capture-shell-test--with-file "* Tasks\n** old\n"
      (orgx-capture-shell--capture "new" :template 'task :file file :headline "Tasks")
      (should (equal (orgx-capture-shell-test--contents file)
                     (concat "* Tasks\n** TODO new\n** old\n"))))
    (orgx-capture-shell-test--with-file "* Log\n** old\n"
      (orgx-capture-shell--capture "new" :template 'journal :file file :headline "Log")
      (should (string-match-p "\\`\\* Log\n\\*\\* old\n\\*\\* new <" (orgx-capture-shell-test--contents file))))))

(ert-deftest orgx-capture-shell-capture-follows-configured-template-test ()
  "A configured :prepend nil makes tasks append, ahead of an Archive child."
  (orgx-capture-shell-test--with-file "* Tasks\n** old\n** Archive :ARCHIVE:\n"
    (let ((orgx-capture-shell-prepend 'follow)
          (org-capture-templates (list (orgx-capture-shell-test--tpl "tasks" file nil))))
      (orgx-capture-shell--capture "new" :template 'task :file file :headline "Tasks")
      (should (equal (orgx-capture-shell-test--contents file)
                     "* Tasks\n** old\n** TODO new\n** Archive :ARCHIVE:\n")))))

(ert-deftest orgx-capture-shell-recent-buttons-follow-moved-entries-test ()
  "Recent links visit their entries even after another capture is prepended."
  (orgx-capture-shell-test--with-file "* Tasks\n"
    (orgx-capture-shell--insert-entry file "Tasks" "* TODO first" t)
    (orgx-capture-shell--insert-entry file "Tasks" "* TODO second" t)
    (let* ((output (orgx-capture-shell--execute-slash-command "/recent"))
           (buffer (get-file-buffer file)))
      (with-temp-buffer
        (insert output)
        (goto-char (point-min))
        (search-forward "first")
        (let ((button (button-at (1- (point)))))
          (should button)
          (save-window-excursion
            (button-activate button)
            (should (eq (current-buffer) buffer))
            (should (looking-at "\*\* TODO first"))))))))

(ert-deftest orgx-capture-shell-recent-deleted-entry-test ()
  "Following a deleted capture reports it missing."
  (orgx-capture-shell-test--with-file "* Tasks\n"
    (orgx-capture-shell--insert-entry file "Tasks" "* TODO first" t)
    (let* ((output (orgx-capture-shell--execute-slash-command "/recent"))
           (buffer (get-file-buffer file)))
      (with-temp-buffer
        (insert output)
        (let ((button (button-at 4)))
          (orgx-capture-shell--undo-last)
          (save-window-excursion
            (should-error (button-activate button) :type 'user-error)))))))

(ert-deftest orgx-capture-shell-recent-preserves-accessible-narrowing-test ()
  "Following an accessible capture preserves the target buffer narrowing."
  (orgx-capture-shell-test--with-file "* Tasks\n"
    (orgx-capture-shell--insert-entry file "Tasks" "* TODO first" t)
    (let ((output (orgx-capture-shell--execute-slash-command "/recent"))
          (buffer (get-file-buffer file)))
      (with-current-buffer buffer
        (narrow-to-region (progn (goto-char (point-min)) (forward-line 1) (point))
                          (point-max)))
      (with-temp-buffer
        (insert output)
        (save-window-excursion
          (button-activate (button-at 4))
          (should (buffer-narrowed-p))
          (should (looking-at "\\*\\* TODO first")))))))

(ert-deftest orgx-capture-shell-body-separators-test ()
  "A semicolon or newline separates title from body."
  (orgx-capture-shell-test--with-file "* Tasks\n"
    (cl-letf (((symbol-function 'orgx-capture-shell--resolve-target)
               (lambda (&optional _) (cons file "Tasks"))))
      (orgx-capture-shell-test--sender "First; details")
      (orgx-capture-shell-test--sender "Second\nmore details")
      (let ((contents (orgx-capture-shell-test--contents file)))
        (should (string-match-p "TODO First\ndetails" contents))
        (should (string-match-p "TODO Second\nmore details" contents)))))
  (should (eq (keymap-lookup orgx-capture-shell-mode-map "C-j") #'newline))
  (should (string-match-p "\nbody\\'"
                          (orgx-capture-shell--format-entry "title\nbody" 'journal))))

(ert-deftest orgx-capture-shell-recent-multiline-summary-test ()
  "Multi-line captures show only their first line in /recent."
  (orgx-capture-shell-test--with-file "* Tasks\n"
    (orgx-capture-shell--insert-entry file "Tasks" "* TODO First Line\nSecond Line\nThird Line" t)
    (let ((output (orgx-capture-shell--execute-slash-command "/recent")))
      (should (string-match-p "\\* TODO First Line\\.\\.\\." output))
      (should-not (string-match-p "Second Line" output)))))

(ert-deftest orgx-capture-shell-flyspell-corfu-setup-test ()
  "Flyspell and corfu setup is configured for text mode with short command support."
  (with-temp-buffer
    (orgx-capture-shell-mode)
    (should (eq (get 'orgx-capture-shell-mode 'flyspell-mode-predicate)
                #'orgx-capture-shell--flyspell-verify))
    (insert "hello /target world")
    (goto-char (point-min))
    (search-forward "hello")
    (should (orgx-capture-shell--flyspell-verify))
    (search-forward "target")
    (should-not (orgx-capture-shell--flyspell-verify))))

(ert-deftest orgx-capture-shell-capture-echo-truncated-test ()
  "Post-capture message truncates multi-line content to first line."
  (orgx-capture-shell-test--with-file "* Tasks\n"
    (let ((msg (orgx-capture-shell--capture
                "First line of multi-line task"
                :body "Second line of body\nThird line of body"
                :template 'task
                :file file
                :headline "Tasks")))
      (should (string-match-p "Captured -> \\[.*:Tasks\\] \\* TODO First line of multi-line task\\.\\.\\." msg))
      (should-not (string-match-p "Second line" msg)))))


(ert-deftest orgx-capture-shell-submit-binding-test ()
  "C-c C-c submits capture input instead of interrupting the helper."
  (should (eq (lookup-key orgx-capture-shell-mode-map (kbd "C-c C-c"))
              #'comint-send-input)))

(ert-deftest orgx-capture-shell-restart-test ()
  "Restarting a dead helper leaves the shell usable."
  (let ((orgx-capture-shell-enable-flyspell nil)
        (orgx-capture-shell-enable-corfu nil))
    (with-temp-buffer
      (orgx-capture-shell-mode)
      (orgx-capture-shell--ensure-process)
      (delete-process (get-buffer-process (current-buffer)))
      (orgx-capture-shell-restart)
      (should (comint-check-proc (current-buffer)))
      (should (= (process-mark (get-buffer-process (current-buffer))) (point-max)))
      (delete-process (get-buffer-process (current-buffer))))))

(ert-deftest orgx-capture-shell-restart-preserves-input-test ()
  "Restart preserves multiline input and does not duplicate an empty prompt."
  (let ((orgx-capture-shell-enable-flyspell nil)
        (orgx-capture-shell-enable-corfu nil)
        received)
    (with-temp-buffer
      (orgx-capture-shell-mode)
      (orgx-capture-shell--ensure-process)
      (comint-output-filter (get-buffer-process (current-buffer))
                            (orgx-capture-shell--prompt-string))
      (let ((before (buffer-string)))
        (orgx-capture-shell-restart)
        (should (equal before (buffer-string))))
      (goto-char (point-max))
      (insert "first line\nsecond line")
      (delete-process (get-buffer-process (current-buffer)))
      (orgx-capture-shell-restart)
      (let ((comint-input-sender (lambda (_process input) (setq received input))))
        (goto-char (point-max))
        (comint-send-input))
      (should (equal received "first line\nsecond line"))
      (delete-process (get-buffer-process (current-buffer))))))

(ert-deftest orgx-capture-shell-context-name-test ()
  "Context changes update the name while preserving shell lookup."
  (let ((orgx-capture-shell-enable-flyspell nil)
        (orgx-capture-shell-enable-corfu nil)
        (orgx-capture-shell-buffer-name "*test-capture-shell*")
        (orgx-capture-shell--buffer nil))
    (with-temp-buffer
      (orgx-capture-shell-mode)
      (let ((shell (current-buffer)))
        (setq orgx-capture-shell--buffer shell)
        (setq orgx-capture-shell-current-file "/tmp/notes.org")
        (orgx-capture-shell-set-template 'note)
        (should (string-match-p (regexp-quote "notes.org · note") (buffer-name)))
        (should (eq shell (orgx-capture-shell--find-buffer)))
        (orgx-capture-shell--execute-slash-command "/template journal")
        ;; Sender refreshes names after slash command execution.
        (orgx-capture-shell--rename-buffer)
        (should (string-match-p "journal" (buffer-name)))))))

(ert-deftest orgx-capture-shell-template-acr-test ()
  "The interactive template picker uses annotated completion."
  (let (table)
    (cl-letf (((symbol-function 'annotated-completing-read)
               (lambda (candidates &rest _args)
                 (setq table candidates)
                 "journal")))
      (with-temp-buffer
        (orgx-capture-shell-mode)
        (call-interactively #'orgx-capture-shell-set-template)
        (should (eq orgx-capture-shell-current-template 'journal))
        (should (assoc "journal" table))))))

(ert-deftest orgx-capture-shell-display-below-other-test ()
  "Only split the other side when it contains an agenda."
  (save-window-excursion
    (delete-other-windows)
    (let ((left (selected-window))
          (agenda (generate-new-buffer " *test-agenda*"))
          (shell (generate-new-buffer " *test-shell*")))
      (unwind-protect
          (let ((right (split-window-right)))
            (set-window-buffer right agenda)
            (with-current-buffer agenda (org-agenda-mode))
            (select-window left)
            (let ((result (orgx-capture-shell--display-below-other shell nil)))
              (should (window-live-p result))
              (should (eq shell (window-buffer result)))
              (should (eq agenda (window-buffer right)))
              (should-not (orgx-capture-shell--display-below-other shell nil))))
        (kill-buffer agenda)
        (kill-buffer shell)))))

(ert-deftest orgx-capture-shell-display-does-not-split-unrelated-test ()
  "An unrelated other window keeps the regular bottom display behavior."
  (save-window-excursion
    (delete-other-windows)
    (let ((other (split-window-right)))
      (set-window-buffer other (get-buffer-create " *test-normal*"))
      (should-not (orgx-capture-shell--display-below-other (current-buffer) nil))
      (kill-buffer (window-buffer other)))))

(ert-deftest orgx-capture-shell-reopen-after-context-change-test ()
  "A submitted template switch preserves shell identity and form defaults."
  (let ((orgx-capture-shell--buffer nil)
        (orgx-capture-shell-buffer-name "*test-capture-shell-reopen*")
        (orgx-capture-shell-window-rule nil)
        (orgx-capture-shell-enable-flyspell nil)
        (orgx-capture-shell-enable-corfu nil)
        shell)
    (cl-letf (((symbol-function 'pop-to-buffer) #'identity))
      (unwind-protect
          (progn
            (setq shell (orgx-capture-shell))
            (with-current-buffer shell
              (goto-char (point-max))
              (insert "/template journal")
              (comint-send-input)
              (should (string-match-p "journal" (buffer-name))))
            (should (eq shell (orgx-capture-shell)))
            (should (eq 'journal (plist-get (orgx-capture-shell--form-defaults)
                                            :template))))
        (when (buffer-live-p shell)
          (when-let* ((process (get-buffer-process shell)))
            (delete-process process))
          (kill-buffer shell))))))

(ert-deftest orgx-capture-shell-file-acr-test ()
  "The file picker offers the current target through annotated completion."
  (orgx-capture-shell-test--with-file "* Tasks\n"
    (let ((orgx-capture-shell-enable-flyspell nil)
          (orgx-capture-shell-enable-corfu nil)
          offered)
      (with-temp-buffer
        (orgx-capture-shell-mode)
        (setq orgx-capture-shell-current-file file)
        (cl-letf (((symbol-function 'annotated-completing-read)
                   (lambda (table &rest _args)
                     (setq offered table)
                     file))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "Tasks")))
          (call-interactively #'orgx-capture-shell-set-target-file)
          (should (assoc file offered))
          (should (equal orgx-capture-shell-current-headline "Tasks")))))))

(ert-deftest orgx-capture-shell-window-rule-updates-test ()
  "Registering replaces an old shell rule and displays below the agenda."
  (save-window-excursion
    (delete-other-windows)
    (let* ((orgx-capture-shell-buffer-name "*test-display-shell*")
           (display-buffer-alist
            `((,(regexp-quote orgx-capture-shell-buffer-name) display-buffer-at-bottom)))
           (left (selected-window))
           (right (split-window-right))
           (agenda (generate-new-buffer " *test-display-agenda*"))
           (shell (generate-new-buffer orgx-capture-shell-buffer-name)))
      (unwind-protect
          (progn
            (set-window-buffer right agenda)
            (with-current-buffer agenda (org-agenda-mode))
            (select-window left)
            (orgx-capture-shell-register-window-rule)
            (let ((window (display-buffer shell)))
              (should (eq shell (window-buffer window)))
              (should (= (car (window-edges right)) (car (window-edges window))))
              (should (= (window-total-height window) orgx-capture-shell-popup-height))))
        (kill-buffer agenda)
        (kill-buffer shell)))))

(ert-deftest orgx-capture-shell-org-completion-target-test ()
  "Complete target-specific TODO keywords in multiline shell input."
  (let* ((dir (make-temp-file "orgx-completion-" t))
         (file (expand-file-name "target.org" dir))
         (orgx-capture-shell-enable-flyspell nil)
         (orgx-capture-shell-enable-corfu nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert "#+TODO: OPEN WIP | DONE\n"))
          (with-temp-buffer
            (orgx-capture-shell-mode)
            (setq orgx-capture-shell-current-file file)
            (orgx-capture-shell--ensure-process)
            (unwind-protect
                (progn
                  (comint-output-filter (get-buffer-process (current-buffer))
                                        (orgx-capture-shell--prompt-string))
                  (insert "first line\n* WI")
                  (let ((capf (orgx-capture-shell-org-completion-at-point)))
                    (should (equal (buffer-substring-no-properties (nth 0 capf) (nth 1 capf)) "WI"))
                    (should (member "WIP" (all-completions "WI" (nth 2 capf))))))
              (delete-process (get-buffer-process (current-buffer))))))
      (delete-directory dir t))))

(ert-deftest orgx-capture-shell-org-completion-link-test ()
  "Complete file links relative to the target directory."
  (let* ((dir (make-temp-file "orgx-completion-" t))
         (file (expand-file-name "target.org" dir))
         (orgx-capture-shell-enable-flyspell nil)
         (orgx-capture-shell-enable-corfu nil))
    (unwind-protect
        (progn
          (with-temp-file file (insert "* Target\n"))
          (with-temp-file (expand-file-name "another.txt" dir))
          (with-temp-buffer
            (orgx-capture-shell-mode)
            (setq orgx-capture-shell-current-file file)
            (orgx-capture-shell--ensure-process)
            (unwind-protect
                (progn
                  (comint-output-filter (get-buffer-process (current-buffer))
                                        (orgx-capture-shell--prompt-string))
                  (insert "[[file:ano")
                  (let ((capf (orgx-capture-shell-org-completion-at-point)))
                    (should capf)
                    (should (member "another.txt" (all-completions "ano" (nth 2 capf))))))
              (delete-process (get-buffer-process (current-buffer))))))
      (delete-directory dir t))))

(ert-deftest orgx-capture-shell-org-completion-end-to-end-test ()
  "Org completion edits shell input and releases its private buffer on kill."
  (let* ((dir (make-temp-file "orgx-completion-" t))
         (file (expand-file-name "target.org" dir))
         (shell (generate-new-buffer " *shell-completion-test*"))
         (orgx-capture-shell-enable-flyspell nil)
         (orgx-capture-shell-enable-corfu nil)
         shadow)
    (unwind-protect
        (progn
          (with-temp-file file (insert "#+TODO: OPEN WIP | DONE\n"))
          (with-current-buffer shell
            (orgx-capture-shell-mode)
            (setq orgx-capture-shell-current-file file)
            (orgx-capture-shell--ensure-process)
            (comint-output-filter (get-buffer-process shell) (orgx-capture-shell--prompt-string))
            (insert "* WI")
            (completion-at-point)
            (should (string-match-p "\\* WIP" (buffer-string)))
            (setq shadow orgx-capture-shell--completion-buffer)
            (should (buffer-live-p shadow))
            (delete-process (get-buffer-process shell)))
          (kill-buffer shell)
          (should-not (buffer-live-p shadow)))
      (when (buffer-live-p shell) (kill-buffer shell))
      (delete-directory dir t))))

(provide 'orgx-capture-shell-test)
;;; orgx-capture-shell-test.el ends here
