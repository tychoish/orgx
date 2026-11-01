;;; orgx-test.el --- Tests for orgx.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'org)
(require 'orgx)

(ert-deftest orgx-date-now-test ()
  "Test formatting of current date timestamp."
  (let ((long-now (orgx-date-now))
        (short-now (orgx-date-now :short t)))
    (should (string-match-p "^<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\} [^>]+>$" long-now))
    (should (string-match-p "^<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}>$" short-now))))

(ert-deftest orgx-done-state-match-test ()
  "Test done state match generator."
  (let ((org-done-keywords '("DONE" "CANCELLED")))
    (let ((match (orgx-done-state-match)))
      (should (string-equal match "/DONE|CANCELLED")))))

(ert-deftest orgx-denote-plan-file-p-test ()
  "Test denote plan file identification."
  (let ((tmp-plan (make-temp-file "20261031T173720==7b4f--plan__agent_plan" nil ".org" "#+title: Test Plan\n"))
        (tmp-note (make-temp-file "20261031T173720--note__note" nil ".org" "#+title: Simple Note\n")))
    (unwind-protect
        (progn
          (should (orgx-denote-plan-file-p tmp-plan))
          (should-not (orgx-denote-plan-file-p tmp-note)))
      (when (file-exists-p tmp-plan) (delete-file tmp-plan))
      (when (file-exists-p tmp-note) (delete-file tmp-note)))))

(provide 'orgx-test)
;;; orgx-test.el ends here
