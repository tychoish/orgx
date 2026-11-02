;;; orgx-capture-test.el --- Tests for orgx-capture.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'org)
(require 'org-capture)
(require 'orgx-capture)
(require 'orgx)

(ert-deftest orgx-standard-capture-templates-test ()
  "Test generating standard capture template categories."
  (let ((org-capture-templates nil)
        (org-directory "/tmp/org")
        (orgx-capture-default-tasks-file "planner.org")
        (orgx-capture-default-notes-file "records.org")
        (orgx-capture-default-journal-file "journal.org"))
    (orgx-capture-reset-templates)
    (should (assoc "t" org-capture-templates))
    (should (assoc "j" org-capture-templates))
    (should (assoc "n" org-capture-templates))
    (should (assoc "a" org-capture-templates))
    (should (string-equal (nth 1 (assoc "t" org-capture-templates)) "tasks"))
    (should (string-equal (nth 1 (assoc "j" org-capture-templates)) "journal"))))

(ert-deftest orgx-capture-setup-standard-templates-test ()
  "Test installing full standard template hierarchy."
  (let ((org-capture-templates nil)
        (org-directory "/tmp/org")
        (orgx-capture-default-tasks-file "/tmp/org/planner.org")
        (orgx-capture-default-notes-file "/tmp/org/records.org")
        (orgx-capture-default-journal-file "/tmp/org/journal.org")
        (orgx-capture-default-agent-file "/tmp/org/agentic.org"))
    (orgx-capture-setup-standard-templates)
    ;; Check that note, journal, and task templates were registered
    (should (assoc "nn" org-capture-templates))
    (should (assoc "jj" org-capture-templates))
    (should (assoc "tt" org-capture-templates))
    ;; Group header placeholders must exist so org-capture can nest the
    ;; multi-character keys under their prefix in the selection menu.
    (should (equal (assoc "t" org-capture-templates) '("t" "tasks")))
    (should (equal (assoc "j" org-capture-templates) '("j" "journal")))
    (should (equal (assoc "n" org-capture-templates) '("n" "notes")))
    (should (equal (assoc "a" org-capture-templates) '("a" "agent")))))

(ert-deftest orgx-capture-add-project-file-templates-test ()
  "Test project file template registration macro and helper."
  (let ((org-capture-templates nil)
        (org-agenda-files nil)
        (org-directory "/tmp/org"))
    (orgx-capture-add-project-file-templates
     :name "myproject"
     :path "/tmp/org/myproject.org"
     :key "p")
    (should (assoc "p" org-capture-templates))
    (should (assoc "pn" org-capture-templates))
    (should (assoc "pj" org-capture-templates))
    (should (assoc "pt" org-capture-templates))))

(ert-deftest orgx-capture-populates-templates-test ()
  "Test that orgx-capture populates org-capture-templates if empty.
Passes an explicit KEYS so the interactive `annotated-completing-read'
template picker is bypassed, same as a scripted/non-interactive caller."
  (let ((org-capture-templates nil)
        (captured nil))
    (cl-letf (((symbol-function 'org-capture)
               (lambda (&rest _)
                 (setq captured org-capture-templates))))
      (orgx-capture nil "tt")
      (should (assoc "tt" captured)))))

(ert-deftest orgx-capture-prompts-for-template-when-keys-omitted-test ()
  "Test that orgx-capture selects a template via `annotated-completing-read'
when no KEYS are supplied, and dispatches to it."
  (let ((org-capture-templates nil)
        (captured nil))
    (cl-letf (((symbol-function 'org-capture)
               (lambda (&rest args) (setq captured args)))
              ;; Stand in for the real minibuffer prompt: return the
              ;; description `orgx-capture--select-template' would show
              ;; for the "tt" template, so its key-table lookup resolves.
              ((symbol-function 'annotated-completing-read)
               (lambda (&rest _) (nth 1 (assoc "tt" org-capture-templates)))))
      (orgx-capture)
      (should (equal captured '(nil "tt"))))))

(ert-deftest orgx-capture-quick-task-template-test ()
  "orgx-capture-add-task-templates adds tq template with :immediate-finish t."
  (let ((org-capture-templates nil))
    (orgx-capture-add-task-templates :name "test" :path "/tmp/test.org")
    (let ((tq-entry (assoc "tq" org-capture-templates)))
      (should tq-entry)
      (should (member :immediate-finish tq-entry))
      (should (eq t (cadr (member :immediate-finish tq-entry))))
      (should (equal "* TODO %(orgx-capture--read-title)%?" (nth 4 tq-entry))))))

(ert-deftest orgx-capture-quick-task-template-with-key-test ()
  "orgx-capture-add-task-templates with key generates <key>tq and t<key>q entries with :immediate-finish t."
  (let ((org-capture-templates nil))
    (orgx-capture-add-task-templates :name "test" :path "/tmp/test.org" :key "p")
    (let ((ptq-entry (assoc "ptq" org-capture-templates))
          (tpq-entry (assoc "tpq" org-capture-templates)))
      (should ptq-entry)
      (should tpq-entry)
      (should (member :immediate-finish ptq-entry))
      (should (member :immediate-finish tpq-entry)))))

(ert-deftest orgx-capture-quick-link-task-template-test ()
  "orgx-capture-add-task-templates adds tlq, <key>tlq, and t<key>lq templates with :immediate-finish t."
  (let ((org-capture-templates nil))
    (orgx-capture-add-task-templates :name "test" :path "/tmp/test.org" :key "p")
    (let ((ptlq-entry (assoc "ptlq" org-capture-templates))
          (tplq-entry (assoc "tplq" org-capture-templates)))
      (should ptlq-entry)
      (should tplq-entry)
      (should (member :immediate-finish ptlq-entry))
      (should (member :immediate-finish tplq-entry))
      (should (equal "* TODO %(orgx-capture--prompt-link)%?" (nth 4 ptlq-entry))))))

(ert-deftest orgx-capture-link-template-variants-test ()
  "Task, journal, and note capture templates support body, heading, and quick heading link variants."
  (let ((org-capture-templates nil))
    (orgx-capture-add-task-templates :name "test" :path "/tmp/test.org")
    (orgx-capture-add-journal-templates :name "test" :path "/tmp/test.org")
    (orgx-capture-add-note-templates :name "test" :path "/tmp/test.org")
    ;; body variant (tlb / jlb / nlb)
    (should (string-match-p "\n%(orgx-capture--get-link)" (nth 4 (assoc "tlb" org-capture-templates))))
    (should (string-match-p "\n%(orgx-capture--get-link)" (nth 4 (assoc "jlb" org-capture-templates))))
    (should (string-match-p "\n%(orgx-capture--get-link)" (nth 4 (assoc "nlb" org-capture-templates))))
    ;; heading variant (tlh / jlh / nlh)
    (should (equal "* TODO %(orgx-capture--get-link)%?" (nth 4 (assoc "tlh" org-capture-templates))))
    (should (string-match-p "\\* %(orgx-capture--get-link) <%" (nth 4 (assoc "jlh" org-capture-templates))))
    (should (equal "* %(orgx-capture--get-link)%?" (nth 4 (assoc "nlh" org-capture-templates))))
    ;; quick heading + immediate finish (tlq / jlq / nlq)
    (should (member :immediate-finish (assoc "tlq" org-capture-templates)))
    (should (member :immediate-finish (assoc "jlq" org-capture-templates)))
    (should (member :immediate-finish (assoc "nlq" org-capture-templates)))
    (should (equal "* TODO %(orgx-capture--prompt-link)%?" (nth 4 (assoc "tlq" org-capture-templates))))
    (should (string-match-p "\\* %(orgx-capture--prompt-link) <%" (nth 4 (assoc "jlq" org-capture-templates))))
    (should (equal "* %(orgx-capture--prompt-link)%?" (nth 4 (assoc "nlq" org-capture-templates))))))

(ert-deftest orgx-capture-get-link-helper-test ()
  "orgx-capture--get-link returns annotation when non-empty, else the most recent stored link."
  (let ((org-capture-plist '(:annotation "[[file:/foo.el][foo.el]]"))
        (org-stored-links '(("http://example.com" "Example"))))
    (should (equal "[[file:/foo.el][foo.el]]" (orgx-capture--get-link))))
  (let ((org-capture-plist '(:annotation ""))
        (org-stored-links '(("http://example.com" "Example"))))
    (should (equal "[[http://example.com][Example]]" (orgx-capture--get-link)))))

(ert-deftest orgx-capture-empty-lines-zero-test ()
  "orgx task capture templates specify :empty-lines 0."
  (let ((org-capture-templates nil))
    (orgx-capture-add-task-templates :name "test" :path "/tmp/test.org")
    (let ((tt-entry (assoc "tt" org-capture-templates)))
      (should (member :empty-lines tt-entry))
      (should (eq 0 (cadr (member :empty-lines tt-entry)))))))

(ert-deftest orgx-capture-sequential-no-blank-lines-test ()
  "Sequential quick-task captures place entries consecutively without blank lines."
  (let* ((temp-file (make-temp-file "orgx-capture-test-" nil ".org"))
         (org-capture-templates nil))
    (unwind-protect
        (progn
          (with-temp-file temp-file (insert "* Tasks\n"))
          (orgx-capture-add-task-templates :name "test" :path temp-file)
          (cl-letf (((symbol-function 'read-string)
                     (let ((count 0))
                       (lambda (&rest _)
                         (setq count (1+ count))
                         (format "Task %d" count)))))
            (org-capture nil "tq")
            (org-capture nil "tq")
            (org-capture nil "tq"))
          (let ((content (with-temp-buffer
                           (insert-file-contents temp-file)
                           (buffer-string))))
            (should (equal "* Tasks\n** TODO Task 3\n** TODO Task 2\n** TODO Task 1\n" content))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(provide 'orgx-capture-test)
;;; orgx-capture-test.el ends here
