;;; orgx-denote-test.el --- Tests for orgx-denote.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'org)
(require 'orgx-denote)
(require 'orgx)

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

(ert-deftest orgx-denote-file-completed-p-test ()
  "Test denote completed file identification via tag and frontmatter."
  (let ((tmp-done (make-temp-file "denote-test-done" nil ".org" "#+title: Done Task\n#+filetags: :agent:x:\n#+execution_status: complete\n"))
        (tmp-open (make-temp-file "denote-test-open" nil ".org" "#+title: Open Task\n#+filetags: :agent:\n#+execution_status: in-progress\n")))
    (unwind-protect
        (progn
          (should (orgx-denote-file-completed-p tmp-done))
          (should-not (orgx-denote-file-completed-p tmp-open)))
      (when (file-exists-p tmp-done) (delete-file tmp-done))
      (when (file-exists-p tmp-open) (delete-file tmp-open)))))

(ert-deftest orgx-denote-signature-comparator-test ()
  "Test denote signature comparator function."
  (let ((line-a #("line a" 0 6 (org-category "/tmp/20261031T173720==01--note.org")))
        (line-b #("line b" 0 6 (org-category "/tmp/20261031T173720==02--note.org"))))
    ;; Since denote-retrieve-filename-signature may not be defined in batch without denote,
    ;; mock it for testing comparison logic
    (cl-letf (((symbol-function 'denote-retrieve-filename-signature)
               (lambda (path)
                 (when (string-match "==\\([0-9a-z]+\\)--" path)
                   (match-string 1 path)))))
      (should (= (orgx-denote-agenda-cmp-signature line-a line-b) -1))
      (should (= (orgx-denote-agenda-cmp-signature line-b line-a) 1))
      (should (= (orgx-denote-agenda-cmp-signature line-a line-a) 0)))))

(ert-deftest orgx-denote-aliases-test ()
  "Test that backward-compatible aliases exist."
  (should (fboundp 'orgx-migrate-subtree-to-denote))
  (should (fboundp 'orgx-refile-to-denote))
  (should (fboundp 'orgx-agenda-denote-todos))
  (should (fboundp 'orgx-agenda-denote-questions))
  (should (fboundp 'orgx-agenda-denote-composite))
  (should (fboundp 'orgx-agenda-denote-completed-plans)))

(provide 'orgx-denote-test)
;;; orgx-denote-test.el ends here
