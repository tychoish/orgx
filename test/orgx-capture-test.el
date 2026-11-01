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
    (should (assoc "tt" org-capture-templates))))

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

(provide 'orgx-capture-test)
;;; orgx-capture-test.el ends here
