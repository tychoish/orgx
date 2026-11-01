;;; orgx-capture-shell-test.el --- Tests for orgx-capture-shell -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'org)
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
          ;; Insert item
          (let ((rec (orgx-capture-shell--insert-entry
                      tmp-file "Tasks" "* TODO Test item 1")))
            (push rec orgx-capture-shell-undo-ring))
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
  (let ((orgx-capture-shell-current-file "/tmp/initial.org")
        (orgx-capture-shell-current-headline "Tasks")
        (orgx-capture-shell-current-template 'task)
        (orgx-capture-shell-undo-ring nil))
    ;; Switch target
    (let ((msg (orgx-capture-shell--execute-slash-command "/target /tmp/new.org:Backlog")))
      (should (string-match-p "Target set to new.org \\[Backlog\\]" msg))
      (should (string-equal orgx-capture-shell-current-file "/tmp/new.org"))
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
      (should (string-match-p "Current target:" msg)))))

(provide 'orgx-capture-shell-test)
;;; orgx-capture-shell-test.el ends here
