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



(ert-deftest orgx-denote-setup-agenda-commands-test ()
  "Test registering denote commands into `org-agenda-custom-commands'."
  (let ((org-agenda-custom-commands nil))
    (orgx-denote-setup-agenda-commands)
    (should (assoc "d" org-agenda-custom-commands))
    (should (assoc "da" org-agenda-custom-commands))
    (should (assoc "dt" org-agenda-custom-commands))
    (should (assoc "dn" org-agenda-custom-commands))
    (should (assoc "dg" org-agenda-custom-commands))
    (should (assoc "dq" org-agenda-custom-commands))
    (should (assoc "dc" org-agenda-custom-commands))
    (should (assoc "dx" org-agenda-custom-commands))))

(ert-deftest orgx-denote-setup-capture-templates-test ()
  "Test registering denote capture templates."
  (let ((org-capture-templates nil))
    (cl-letf (((symbol-function 'denote-org-capture) #'ignore)
              ((symbol-function 'denote-journal-capture-entry-today) #'ignore)
              ((symbol-function 'denote-journal-capture-entry-for-date) #'ignore))
      (orgx-denote-setup-capture-templates)
      (should (assoc "dn" org-capture-templates))
      (should (assoc "dj" org-capture-templates))
      (should (assoc "dJ" org-capture-templates)))))

(ert-deftest orgx-denote-parse-heading-date-inactive-test ()
  "Parses an inactive timestamp in a heading string."
  (let ((result (orgx-denote--parse-heading-date "[2024-03-15 Fri] My note")))
    (should result)
    (let ((decoded (decode-time result)))
      (should (= 2024 (nth 5 decoded)))
      (should (= 3 (nth 4 decoded)))
      (should (= 15 (nth 3 decoded))))))

(ert-deftest orgx-denote-parse-heading-date-active-test ()
  "Parses an active timestamp in a heading string."
  (let ((result (orgx-denote--parse-heading-date "<2023-11-01 Wed 09:30> Meeting notes")))
    (should result)
    (let ((decoded (decode-time result)))
      (should (= 2023 (nth 5 decoded)))
      (should (= 11 (nth 4 decoded)))
      (should (= 1 (nth 3 decoded))))))

(ert-deftest orgx-denote-parse-heading-date-no-timestamp-test ()
  "Returns nil when the heading contains no timestamp."
  (should-not (orgx-denote--parse-heading-date "Plain heading with no date")))

(ert-deftest orgx-denote-parse-heading-date-empty-test ()
  "Returns nil for an empty heading string."
  (should-not (orgx-denote--parse-heading-date "")))

(ert-deftest orgx-denote-parse-heading-date-date-only-test ()
  "Parses a date-only inactive timestamp."
  (let ((result (orgx-denote--parse-heading-date "[2025-06-30 Mon]")))
    (should result)
    (let ((decoded (decode-time result)))
      (should (= 2025 (nth 5 decoded)))
      (should (= 6 (nth 4 decoded)))
      (should (= 30 (nth 3 decoded))))))

(ert-deftest orgx-denote-skip-unless-open-question-test ()
  "orgx-skip-unless-open-question skips answered, done, metadata, and
inherited subheadings; it does not itself filter non-question headings
(that is the job of the \"+question|TODO=\\\"QUESTION\\\"\" agenda match
it is paired with)."
  ;; `org-set-regexps-and-options' reads `(default-value 'org-todo-keywords)'
  ;; when the buffer has no #+TODO: lines, so the override must be a dynamic
  ;; `let', not `setq-local' (which only shadows the default, it does not
  ;; replace it).
  (let ((org-todo-keywords '((sequence "TODO" "QUESTION" "INPROGRESS" "|" "ANSWERED" "DONE"))))
    (with-temp-buffer
      (org-mode)
      (org-set-regexps-and-options)
      (insert "
* Open Question A :question:
* QUESTION Open Question B
* QUESTION Open Question C :question:
* INPROGRESS Open Question D :question:
*** Context                                       :question:
*** Response                                      :question:
* ANSWERED Closed Question E :question:
* DONE Closed Question F :question:
")
      (let (results)
        (goto-char (point-min))
        ;; Mirror how `org-agenda' drives a skip function: call it with point
        ;; saved (it may jump to a parent via `org-up-heading-safe'), then
        ;; decide ourselves where the scan resumes next.
        (while (re-search-forward "^\\(\\*+\\)\\s-+" nil t)
          (goto-char (match-beginning 0))
          (let ((skip (save-excursion (orgx-skip-unless-open-question))))
            (if skip
                (goto-char skip)
              (push (org-get-heading t t t t) results)
              (end-of-line))))
        (setq results (mapcar #'substring-no-properties (nreverse results)))
        (should (equal '("Open Question A" "Open Question B" "Open Question C" "Open Question D")
                       results))))))

(ert-deftest orgx-denote-agenda-settings-header-test ()
  "HEADER is threaded through to org-agenda-overriding-header."
  (let ((settings (orgx-denote-agenda-settings "My Header")))
    (should (equal "My Header" (cadr (assq 'org-agenda-overriding-header settings))))))

(ert-deftest orgx-denote-agenda-settings-prefix-format-test ()
  "Prefix format is set for both the todo and tags agenda line types."
  (let* ((settings (orgx-denote-agenda-settings "Header"))
         (prefix-form (cadr (assq 'org-agenda-prefix-format settings)))
         (prefix-alist (eval prefix-form)))
    (should (assq 'todo prefix-alist))
    (should (assq 'tags prefix-alist))))

(ert-deftest orgx-denote-composite-files-filtering-test ()
  "orgx-denote-composite-files respects orgx-denote-agenda-suppress-completed."
  (let ((dir (make-temp-file "orgx-denote-composite-files-" t)))
    (unwind-protect
        (let ((file-completed (expand-file-name "20260101T000000==1a--test-plan__agent_plan_x.org" dir))
              (file-active (expand-file-name "20260101T000001==1b--test-plan__agent_plan.org" dir)))
          (with-temp-file file-completed (insert "#+title: Closed Note\n"))
          (with-temp-file file-active (insert "#+title: Active Note\n"))
          (cl-letf (((symbol-function 'orgx-denote-files)
                     (lambda () (list file-completed file-active))))
            (let ((orgx-denote-agenda-suppress-completed t))
              (should (equal (list file-active) (orgx-denote-composite-files))))
            (let ((orgx-denote-agenda-suppress-completed nil))
              (should (equal (list file-completed file-active) (orgx-denote-composite-files))))))
      (delete-directory dir t))))

(ert-deftest orgx-denote-dc-command-structure-test ()
  "The 'dc' composite agenda command contains the expected blocks."
  (let ((org-agenda-custom-commands nil))
    (orgx-denote-setup-agenda-commands)
    (let ((dc-cmd (assoc "dc" org-agenda-custom-commands)))
      (should dc-cmd)
      (should (equal "Denote Composite Agenda" (nth 1 dc-cmd)))
      (let ((blocks (nth 2 dc-cmd)))
        (should (= 5 (length blocks)))
        (should (equal "Human questions" (cadr (assq 'org-agenda-overriding-header (nth 2 (nth 0 blocks))))))
        (should (equal "Pending Considerations" (cadr (assq 'org-agenda-overriding-header (nth 2 (nth 1 blocks))))))
        (should (equal "Active Tasks" (cadr (assq 'org-agenda-overriding-header (nth 2 (nth 2 blocks))))))
        (should (equal "Tasks" (cadr (assq 'org-agenda-overriding-header (nth 2 (nth 3 blocks))))))
        (should (equal "Agent tasks" (cadr (assq 'org-agenda-overriding-header (nth 2 (nth 4 blocks))))))))))

(ert-deftest orgx-denote-dx-command-structure-test ()
  "The 'dx' completed-plans agenda command scopes to completed plan files."
  (let ((org-agenda-custom-commands nil))
    (orgx-denote-setup-agenda-commands)
    (let ((dx-cmd (assoc "dx" org-agenda-custom-commands)))
      (should dx-cmd)
      (should (equal "Completed Plans" (nth 1 dx-cmd)))
      (should (equal '(org-agenda-files (orgx-denote-completed-plan-files))
                     (assq 'org-agenda-files (nth 3 dx-cmd)))))))

(provide 'orgx-denote-test)
;;; orgx-denote-test.el ends here
