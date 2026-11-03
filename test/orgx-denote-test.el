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

;;; Open-items capture

(defun orgx-denote-test--write (dir name content)
  "Write CONTENT to NAME in DIR and return the full path."
  (let ((path (expand-file-name name dir)))
    (write-region content nil path nil 'silent)
    path))

(ert-deftest orgx-denote-capture-helpers-test ()
  "File id, title and open-item counting read denote notes correctly."
  (let* ((dir (make-temp-file "orgx-denote-cap" t))
         (note (orgx-denote-test--write
                dir "20261101T100000--a-note__x.org"
                "#+title: A [Note] 100%\n* TODO one\n* DONE two\n** NEXT three\n")))
    (unwind-protect
        (progn
          (should (equal "20261101T100000" (orgx-denote--file-id note)))
          (should-not (orgx-denote--file-id "/tmp/plain.org"))
          (should (equal "A [Note] 100%" (orgx-denote--file-title note)))
          (should (orgx-denote--has-open-items-p note '("TODO")))
          (should (orgx-denote--has-open-items-p note '("NEXT")))
          (should-not (orgx-denote--has-open-items-p note '("WAIT")))
          (should-not (orgx-denote--has-open-items-p note '("todo"))))
      (delete-directory dir t))))

(ert-deftest orgx-denote-peer-linked-ids-test ()
  "Only direct children of the target heading count as linked peers."
  (with-temp-buffer
    (org-mode)
    (insert "* Target\n"
            "** [[denote:20261101T100000][One]]\n"
            "*** [[denote:20261101T110000][Nested]]\n"
            "** Plain\n"
            "* Other\n"
            "** [[denote:20261101T120000][Elsewhere]]\n")
    (goto-char (point-min))
    (should (equal '("20261101T100000") (orgx-denote--peer-linked-ids)))
    (goto-char (point-min))
    (insert "* [[denote:20261101T130000][Top]]\n")
    (goto-char (point-min))
    (insert "preamble\n")
    (goto-char (point-min))
    (should (equal '("20261101T130000") (orgx-denote--peer-linked-ids)))))

(ert-deftest orgx-denote-capture-open-items-test ()
  "Capture builds one link heading per note with open items, deduping peers."
  (let* ((dir (make-temp-file "orgx-denote-cap" t))
         (open (orgx-denote-test--write
                dir "20261101T100000--open__x.org" "#+title: Open\n* TODO a\n"))
         (linked (orgx-denote-test--write
                  dir "20261101T110000--linked__x.org" "#+title: Linked\n* TODO b\n"))
         (closed (orgx-denote-test--write
                  dir "20261101T120000--closed__x.org" "#+title: Closed\n* DONE c\n"))
         (target (orgx-denote-test--write
                  dir "target.org" "* Inbox\n** [[denote:20261101T110000][Linked]]\n"))
         (captured nil))
    (unwind-protect
        (cl-letf (((symbol-function 'denote-retrieve-filename-keywords-as-list)
                   (lambda (_) nil))
                  ((symbol-function 'completing-read-multiple)
                   (lambda (&rest _) nil))
                  ((symbol-function 'org-refile-get-location)
                   (lambda (&rest _) (list "Inbox" target nil 1)))
                  ((symbol-function 'org-capture)
                   (lambda (&rest _)
                     (setq captured (nth 4 (car org-capture-templates))))))
          (let ((orgx-denote-capture-dedupe 'skip))
            (orgx-denote-capture-open-items (list open linked closed)))
          (should (equal "* [[denote:20261101T100000][Open]]" captured))
          (let ((orgx-denote-capture-dedupe 'include))
            (orgx-denote-capture-open-items (list open linked closed)))
          (should (equal (concat "* [[denote:20261101T100000][Open]]\n"
                                 "* [[denote:20261101T110000][Linked]]")
                         captured)))
      (when (get-file-buffer target)
        (kill-buffer (get-file-buffer target)))
      (delete-directory dir t))))

(ert-deftest orgx-denote-open-note-files-test ()
  "Notes need an id, every requested tag, and a heading in the keywords."
  (let* ((dir (make-temp-file "orgx-denote-cap" t))
         (tagged (orgx-denote-test--write
                  dir "20261101T100000--tagged__work_x.org" "* TODO a\n"))
         (untagged (orgx-denote-test--write
                    dir "20261101T110000--untagged.org" "* TODO b\n"))
         (waiting (orgx-denote-test--write
                   dir "20261101T120000--waiting__work.org" "* WAIT c\n"))
         (no-id (orgx-denote-test--write dir "plain__work.org" "* TODO d\n"))
         (all (list tagged untagged waiting no-id)))
    (unwind-protect
        (cl-letf (((symbol-function 'denote-retrieve-filename-keywords-as-list)
                   (lambda (file)
                     (when (string-match "__\\([a-z_]+\\)\\.org\\'" file)
                       (split-string (match-string 1 file) "_")))))
          (should (equal (list tagged untagged) (orgx-denote--open-note-files all nil '("TODO"))))
          (should (equal (list tagged) (orgx-denote--open-note-files all '("work") '("TODO"))))
          (should (equal (list waiting) (orgx-denote--open-note-files all '("work") '("WAIT"))))
          (should-not (orgx-denote--open-note-files all '("work" "other") '("TODO"))))
      (delete-directory dir t))))

(ert-deftest orgx-denote-target-marker-test ()
  "A refile target with a position gives that heading; without, the file start."
  (let* ((dir (make-temp-file "orgx-denote-cap" t))
         (file (orgx-denote-test--write dir "t.org" "* One\n* Two\n")))
    (unwind-protect
        (progn
          (should (= 7 (marker-position (orgx-denote--target-marker (list "Two" file nil 7)))))
          (should (= 1 (marker-position (orgx-denote--target-marker (list "t.org" file nil nil))))))
      (when (get-file-buffer file)
        (kill-buffer (get-file-buffer file)))
      (delete-directory dir t))))

(ert-deftest orgx-denote-unlinked-files-test ()
  "Linked notes are dropped, kept, or chosen according to the dedupe setting."
  (let* ((a "/d/20261101T100000--a.org")
         (b "/d/20261101T110000--b.org")
         (files (list a b))
         (linked '("20261101T110000")))
    (let ((orgx-denote-capture-dedupe 'skip))
      (should (equal (list a) (orgx-denote--unlinked-files files linked))))
    (let ((orgx-denote-capture-dedupe 'include))
      (should (equal files (orgx-denote--unlinked-files files linked))))
    (let ((orgx-denote-capture-dedupe 'prompt))
      (should (equal files (orgx-denote--unlinked-files files nil)))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "skip")))
        (should (equal (list a) (orgx-denote--unlinked-files files linked))))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "include")))
        (should (equal files (orgx-denote--unlinked-files files linked))))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "choose"))
                ((symbol-function 'completing-read-multiple)
                 (lambda (&rest _) (list "20261101T110000--b.org"))))
        (should (equal files (orgx-denote--unlinked-files files linked))))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "choose"))
                ((symbol-function 'completing-read-multiple) (lambda (&rest _) nil)))
        (should (equal (list a) (orgx-denote--unlinked-files files linked)))))))

(ert-deftest orgx-denote-link-headings-test ()
  "Headings link by id, drop brackets from titles and escape percent signs."
  (let* ((dir (make-temp-file "orgx-denote-cap" t))
         (a (orgx-denote-test--write dir "20261101T100000--a.org" "#+title: A [x] 100%\n"))
         (b (orgx-denote-test--write dir "20261101T110000--b.org" "#+title: B\n")))
    (unwind-protect
        (should (equal "* [[denote:20261101T100000][A x 100\\%]]\n* [[denote:20261101T110000][B]]"
                       (orgx-denote--link-headings (list a b))))
      (delete-directory dir t))))

(ert-deftest orgx-denote-capture-open-items-prompts-test ()
  "Tags, keywords and the empty and fully-linked cases flow through the prompts."
  (let* ((dir (make-temp-file "orgx-denote-cap" t))
         (work (orgx-denote-test--write dir "20261101T100000--work__work.org" "#+title: Work\n* TODO a\n"))
         (home (orgx-denote-test--write dir "20261101T110000--home__home.org" "#+title: Home\n* WAIT b\n"))
         (target (orgx-denote-test--write dir "target.org" "* Inbox\n"))
         (answers nil)
         (captured nil))
    (unwind-protect
        (cl-letf (((symbol-function 'denote-retrieve-filename-keywords-as-list)
                   (lambda (file)
                     (when (string-match "__\\([a-z_]+\\)\\.org\\'" file)
                       (split-string (match-string 1 file) "_"))))
                  ((symbol-function 'orgx-denote-active-files) (lambda () (list work home)))
                  ((symbol-function 'completing-read-multiple)
                   (lambda (&rest _) (pop answers)))
                  ((symbol-function 'org-refile-get-location)
                   (lambda (&rest _) (list "Inbox" target nil 1)))
                  ((symbol-function 'org-capture)
                   (lambda (&rest _)
                     (setq captured (nth 4 (car org-capture-templates))))))
          ;; Tag prompt only (no files given): "home" with keyword prompt empty.
          (setq answers (list '("home") nil))
          (should-error (orgx-denote-capture-open-items) :type 'user-error)
          ;; Keyword prompt names WAIT, so the home note qualifies.
          (setq answers (list '("home") '("WAIT")))
          (orgx-denote-capture-open-items)
          (should (equal "* [[denote:20261101T110000][Home]]" captured))
          ;; Explicit files skip the tag prompt, so only the keyword answer is consumed.
          (setq answers (list '("TODO")))
          (orgx-denote-capture-open-items (list work home))
          (should (equal "* [[denote:20261101T100000][Work]]" captured))
          ;; Everything already linked under the target is a user-error.
          (with-current-buffer (find-file-noselect target)
            (goto-char (point-max))
            (insert "** [[denote:20261101T100000][Work]]\n"))
          (setq answers (list '("TODO")))
          (let ((orgx-denote-capture-dedupe 'skip))
            (should-error (orgx-denote-capture-open-items (list work)) :type 'user-error)))
      (when (get-file-buffer target)
        (with-current-buffer (get-file-buffer target)
          (set-buffer-modified-p nil))
        (kill-buffer (get-file-buffer target)))
      (delete-directory dir t))))

(provide 'orgx-denote-test)
;;; orgx-denote-test.el ends here
