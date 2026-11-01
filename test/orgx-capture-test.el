;;; orgx-capture-test.el --- Tests for orgx-capture.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'org)
(require 'org-capture)
(require 'orgx-capture)

(ert-deftest orgx-standard-capture-templates-test ()
  "Test generating standard capture template categories."
  (let ((org-capture-templates nil)
        (org-directory "/tmp/org")
        (orgx-capture-default-tasks-file "planner.org")
        (orgx-capture-default-notes-file "records.org")
        (orgx-capture-default-journal-file "journal.org"))
    (orgx-reset-capture-templates)
    (should (assoc "t" org-capture-templates))
    (should (assoc "j" org-capture-templates))
    (should (assoc "n" org-capture-templates))
    (should (assoc "a" org-capture-templates))
    (should (string-equal (nth 1 (assoc "t" org-capture-templates)) "tasks"))
    (should (string-equal (nth 1 (assoc "j" org-capture-templates)) "journal"))))

(provide 'orgx-capture-test)
;;; orgx-capture-test.el ends here
