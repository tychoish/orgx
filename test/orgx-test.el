;;; orgx-test.el --- Tests for orgx.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'org)
(require 'orgx)
(require 'loaddefs-gen)

(defvar denote-directory)

;; orgx-loaddefs.el is a generated, gitignored convenience file (same
;; purpose as magit-autoloads.el / org-loaddefs.el: using the package
;; straight from a source checkout without package.el). Regenerate it
;; fresh here rather than committing it, so it can never go stale.
(let* ((orgx-dir (file-name-directory (locate-library "orgx")))
       (loaddefs-file (expand-file-name "orgx-loaddefs.el" orgx-dir)))
  (loaddefs-generate orgx-dir loaddefs-file)
  (load loaddefs-file nil t))

(defun orgx-test--org-mode ()
  "Enter `org-mode' without running `org-mode-hook'.
Other tests may leave `orgx-mode-turn-on' on the hook."
  (let ((org-mode-hook nil))
    (org-mode)))

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

(ert-deftest orgx-archive-done-direct-children-test ()
  "Archive direct children, leaving nested tasks and other branches alone."
  (with-temp-buffer
    (orgx-test--org-mode)
    (insert "* Parent\n** DONE First\n** TODO Open\n*** DONE Nested\n** DONE Second\n** Archive :ARCHIVE:\n*** DONE Old\n* Other\n** DONE Outside\n")
    (goto-char (point-min))
    (orgx-archive-done-tasks-to-archive-sibling)
    (should (string-match-p "\\*\\* TODO Open\n\\*\\*\\* DONE Nested" (buffer-string)))
    (should (string-match-p "\\*\\*\\* DONE Old" (buffer-string)))
    (should (string-match-p "\\* Other\n\\*\\* DONE Outside" (buffer-string)))
    (should (string-match-p "\\*\\*\\* DONE First" (buffer-string)))
    (should (string-match-p "\\*\\*\\* DONE Second" (buffer-string)))))

(ert-deftest orgx-archive-done-direct-siblings-test ()
  "At a leaf, archive completed siblings without descending into them."
  (with-temp-buffer
    (orgx-test--org-mode)
    (insert "* Parent\n** TODO Here\n** DONE First\n** TODO Open\n*** DONE Nested\n** DONE Second\n* Other\n** DONE Outside\n")
    (goto-char (point-min))
    (forward-line 1)
    (orgx-archive-done-tasks-to-archive-sibling)
    (should (string-match-p "\\*\\* TODO Open\n\\*\\*\\* DONE Nested" (buffer-string)))
    (should (string-match-p "\\* Other\n\\*\\* DONE Outside" (buffer-string)))
    (should (string-match-p "\\*\\*\\* DONE First" (buffer-string)))
    (should (string-match-p "\\*\\*\\* DONE Second" (buffer-string)))))

(ert-deftest orgx-archive-done-top-level-test ()
  "At a top-level leaf, archive only completed top-level headings."
  (with-temp-buffer
    (orgx-test--org-mode)
    (insert "* TODO Here\n* TODO Open\n** DONE Nested\n* DONE Finished\n")
    (goto-char (point-min))
    (orgx-archive-done-tasks-to-archive-sibling)
    (should (string-match-p "\\* TODO Open\n\\*\\* DONE Nested" (buffer-string)))
    (should (string-match-p "\\*\\* DONE Finished" (buffer-string)))))

(ert-deftest orgx-archive-done-skipped-level-and-region-test ()
  "Use actual parentage and ignore an active region during archiving."
  (with-temp-buffer
    (orgx-test--org-mode)
    (insert "* Parent\n*** DONE Direct\n*** TODO Open\n***** DONE Nested\n* DONE Outside\n")
    (goto-char (point-min))
    (set-mark (point-max))
    (let ((transient-mark-mode t)
          (mark-active t))
      (orgx-archive-done-tasks-to-archive-sibling))
    (should (string-match-p "\\*\\*\\* TODO Open\n\\*\\*\\*\\*\\* DONE Nested" (buffer-string)))
    (should (string-match-p "\\* DONE Outside" (buffer-string)))
    (should (string-match-p "\\*\\*\\*\\* DONE Direct" (buffer-string)))))

(ert-deftest orgx-autoload-capture-test ()
  "Test that orgx-capture is autoloaded on demand."
  (should (fboundp 'orgx-capture))
  (should (fboundp 'orgx-capture-shell)))

(ert-deftest orgx-mode-visual-line-default-test ()
  "orgx-mode enables visual-line-mode and disabling it restores the buffer."
  (let ((orgx--setup-done t))
    (with-temp-buffer
      (orgx-test--org-mode)
      (should orgx-use-visual-line)
      (orgx-mode 1)
      (should visual-line-mode)
      (orgx-mode -1)
      (should-not visual-line-mode))))

(ert-deftest orgx-mode-visual-line-disabled-test ()
  "Setting `orgx-use-visual-line' to nil leaves visual-line-mode off."
  (let ((orgx--setup-done t)
        (orgx-use-visual-line nil))
    (with-temp-buffer
      (orgx-test--org-mode)
      (orgx-mode 1)
      (should-not visual-line-mode)
      (orgx-mode -1)
      (should-not visual-line-mode))))

(ert-deftest orgx-mode-visual-line-preserves-existing-test ()
  "A visual-line-mode the user enabled is not turned off by orgx-mode."
  (let ((orgx--setup-done t))
    (with-temp-buffer
      (orgx-test--org-mode)
      (visual-line-mode 1)
      (orgx-mode 1)
      (orgx-mode -1)
      (should visual-line-mode))))

;;; Table shrinking

(defconst orgx-test--wide-table
  (concat "| a | b | c |\n|---+---+---|\n"
          "| 12345678901234567890123456789012345678901234567890 | x | <5> |\n"
          "| y | z | 1234567890 |\n")
  "Table with a wide column 1, narrow column 2 and cookie'd column 3.")

(defmacro orgx-test--with-table (text &rest body)
  "Run BODY in an `org-mode' buffer with TEXT, shrink advice and width 10."
  (declare (indent 1))
  `(let ((orgx--setup-done t)
         (orgx-table-shrink-width 10)
         (orgx-shrink-tables t))
     (advice-add 'org-table--shrink-field :filter-args #'orgx--ad-table-shrink-field)
     (unwind-protect
         (with-temp-buffer
           (insert ,text)
           (orgx-test--org-mode)
           ,@body)
       (advice-remove 'org-table--shrink-field #'orgx--ad-table-shrink-field))))

(defun orgx-test--shrunk (&optional pos)
  "Return the shrunk columns of the table at POS (default buffer start)."
  (goto-char (or pos (point-min)))
  (org-table--list-shrunk-columns))

(defun orgx-test--hidden-chars ()
  "Return the number of characters under table-column-hide overlays."
  (cl-loop for o in (overlays-in (point-min) (point-max))
           when (eq (overlay-get o 'org-overlay-type) 'table-column-hide)
           sum (- (overlay-end o) (overlay-start o))))

(ert-deftest orgx-table-shrink-display-only-test ()
  (orgx-test--with-table orgx-test--wide-table
    (set-buffer-modified-p nil)
    (orgx-mode 1)
    (should (equal (orgx-test--shrunk) '(1 3)))
    (should (equal (buffer-string) orgx-test--wide-table))
    (should-not (buffer-modified-p))))

(ert-deftest orgx-table-shrink-leaves-narrow-columns-test ()
  (orgx-test--with-table "| a | b |\n|---+---|\n| 1 | 2 |\n"
    (orgx-mode 1)
    (should-not (orgx-test--shrunk))
    (should (= 0 (orgx-test--hidden-chars)))))

(ert-deftest orgx-table-wide-columns-test ()
  (orgx-test--with-table orgx-test--wide-table
    (goto-char (point-min))
    (should (equal (orgx--table-wide-columns 10) '(1)))
    (should (equal (orgx--table-wide-columns 0) '(1 2)))
    (should (equal (orgx--table-wide-columns 100) nil))))

(ert-deftest orgx-table-wide-columns-counts-display-width-test ()
  "Wide (CJK) characters count double."
  (orgx-test--with-table "| a |\n|---|\n| 你好你好你好 |\n"
    (goto-char (point-min))
    (should (equal (orgx--table-wide-columns 10) '(1)))
    (should (equal (orgx--table-wide-columns 12) nil))))

(ert-deftest orgx-table-shrink-uses-configured-width-test ()
  (orgx-test--with-table "| a |\n|---|\n| 12345678901234567890 |\n"
    (orgx-mode 1)
    (let ((hidden-at-10 (orgx-test--hidden-chars)))
      (orgx-mode -1)
      (let ((orgx-table-shrink-width 5))
        (orgx-mode 1)
        (should (> (orgx-test--hidden-chars) hidden-at-10))))))

;; Realigning changes cell padding, so compare against a table that was
;; aligned before shrinking.
(ert-deftest orgx-table-shrink-survives-realign-test ()
  "`org-table-align' must not collapse default-width columns to one character."
  (let (expected)
    (orgx-test--with-table orgx-test--wide-table
      (goto-char (point-min))
      (org-table-align)
      (orgx-mode 1)
      (setq expected (orgx-test--hidden-chars)))
    (orgx-test--with-table orgx-test--wide-table
      (orgx-mode 1)
      (goto-char (point-min))
      (org-table-align)
      (should (equal (orgx-test--shrunk) '(1 3)))
      (should (> expected 0))
      (should (= expected (orgx-test--hidden-chars))))))

(ert-deftest orgx-table-shrink-multiple-tables-test ()
  (orgx-test--with-table (concat orgx-test--wide-table "\ntext\n\n" orgx-test--wide-table)
    (orgx-mode 1)
    (should (equal (orgx-test--shrunk) '(1 3)))
    (should (equal (orgx-test--shrunk (save-excursion (goto-char (point-max))
                                                      (forward-line -2) (point)))
                   '(1 3)))))

(ert-deftest orgx-table-shrink-disabled-test ()
  (orgx-test--with-table orgx-test--wide-table
    (let ((orgx-shrink-tables nil))
      (orgx-mode 1)
      (should (= 0 (orgx-test--hidden-chars)))
      (should-not orgx--tables-shrunk))))

(ert-deftest orgx-table-shrink-teardown-expands-test ()
  (orgx-test--with-table orgx-test--wide-table
    (orgx-mode 1)
    (should (> (orgx-test--hidden-chars) 0))
    (orgx-mode -1)
    (should (= 0 (orgx-test--hidden-chars)))
    (should (equal (buffer-string) orgx-test--wide-table))))

(ert-deftest orgx-table-shrink-teardown-keeps-user-shrink-test ()
  "Shrinking the user did by hand survives turning `orgx-mode' off."
  (orgx-test--with-table orgx-test--wide-table
    (let ((orgx-shrink-tables nil))
      (goto-char (point-min))
      (org-table-shrink)
      (let ((hidden (orgx-test--hidden-chars)))
        (should (> hidden 0))
        (orgx-mode 1)
        (orgx-mode -1)
        (should (= hidden (orgx-test--hidden-chars)))))))

(ert-deftest orgx-table-shrink-command-idempotent-test ()
  (orgx-test--with-table orgx-test--wide-table
    (orgx-table-shrink-wide-columns)
    (let ((n (length (overlays-in (point-min) (point-max)))))
      (orgx-table-shrink-wide-columns)
      (should (= n (length (overlays-in (point-min) (point-max))))))))

(ert-deftest orgx-table-shrink-ignores-non-org-buffers-test ()
  (with-temp-buffer
    (insert orgx-test--wide-table)
    (fundamental-mode)
    (orgx-table-shrink-wide-columns)
    (should (null (overlays-in (point-min) (point-max))))))

(ert-deftest orgx-table-shrink-field-filter-test ()
  (let ((orgx-table-shrink-width 12))
    (with-temp-buffer
      (let ((orgx--setup-done t))
        (orgx-test--org-mode)
        (orgx-mode 1)
        ;; no cookie (width 0): default width
        (should (equal (orgx--ad-table-shrink-field (list 0 "l" 1 2 "x")) '(12 "l" 1 2 "x")))
        ;; explicit cookie width kept
        (should (equal (orgx--ad-table-shrink-field (list 7 "l" 1 2 "x")) '(7 "l" 1 2 "x")))
        ;; disabled values
        (let ((orgx-table-shrink-width 0))
          (should (equal (orgx--ad-table-shrink-field (list 0 "l")) '(0 "l"))))
        (let ((orgx-table-shrink-width nil))
          (should (equal (orgx--ad-table-shrink-field (list 0 "l")) '(0 "l"))))
        (orgx-mode -1)
        ;; outside orgx-mode: untouched
        (should (equal (orgx--ad-table-shrink-field (list 0 "l")) '(0 "l")))))))

(ert-deftest orgx-table-shrink-advice-registered-test ()
  (orgx--enable-advice)
  (unwind-protect
      (should (advice-member-p #'orgx--ad-table-shrink-field 'org-table--shrink-field))
    (orgx--disable-advice))
  (should-not (advice-member-p #'orgx--ad-table-shrink-field 'org-table--shrink-field)))

(ert-deftest orgx-mode-setup-test ()
  "Test that activating orgx-mode applies local settings."
  (with-temp-buffer
    (orgx-test--org-mode)
    (orgx-mode 1)
    (should orgx-mode)
    (should org-return-follows-link)
    (should (eq org-use-speed-commands #'orgx--use-speed-commands))
    (should (memq #'orgx-set-weekday-of-timestamp org-ctrl-c-ctrl-c-hook))
    (should (equal org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))))
    (should org-log-into-drawer)
    (should org-reverse-note-order)
    (should org-fontify-done-headline)
    (should org-provide-todo-statistics)
    (should org-enforce-todo-dependencies)
    (should org-enforce-todo-checkbox-dependencies)
    (should org-fast-tag-selection-include-todo)
    (should (eq org-use-fast-todo-selection 'auto))
    (should (eq org-use-fast-tag-selection 'auto))
    (should (= org-tags-column -70))
    (should (equal org-tags-exclude-from-inheritance '("project")))
    (should org-track-ordered-property-with-tag)
    (orgx-mode -1)
    (should-not orgx-mode)
    (should-not (memq #'orgx-set-weekday-of-timestamp org-ctrl-c-ctrl-c-hook))))

(ert-deftest orgx-mode-managed-settings-revert-test ()
  "Test that orgx-mode cleanly reverts buffer-local settings."
  (with-temp-buffer
    (orgx-test--org-mode)
    (setq-local fill-column 80)
    (setq-local org-tags-column -60)
    (orgx-mode 1)
    (should (= fill-column 72))
    (should (= org-tags-column -70))
    (orgx-mode -1)
    ;; Restored prior local values
    (should (= fill-column 80))
    (should (= org-tags-column -60))
    ;; Variables not local prior to orgx-mode should have local binding removed
    (should-not (local-variable-p 'org-return-follows-link))))

(ert-deftest orgx-gist-keymap-test ()
  "Test that orgx-gist-map is mounted under C-c o g."
  (should (keymapp orgx-gist-map))
  (should (eq orgx-gist-map (keymap-lookup orgx-command-map "g")))
  (should (commandp (keymap-lookup orgx-gist-map "p")))
  (should (commandp (keymap-lookup orgx-gist-map "g"))))

(ert-deftest orgx-link-keymap-test ()
  "Test that orgx-link-map is mounted under C-c o l."
  (should (keymapp orgx-link-map))
  (should (eq orgx-link-map (keymap-lookup orgx-command-map "l")))
  (should (commandp (keymap-lookup orgx-link-map "s")))
  (should (commandp (keymap-lookup orgx-link-map "a")))
  (should (commandp (keymap-lookup orgx-link-map "i")))
  (should (commandp (keymap-lookup orgx-link-map "n"))))

(ert-deftest orgx-mode-keymaps-test ()
  "Test that orgx-mode-map and orgx-global-mode-map mount orgx-command-map on C-c o."
  (should (keymapp orgx-command-map))
  (should (keymapp orgx-mode-map))
  (should (keymapp orgx-global-mode-map))
  (should (eq orgx-command-map (keymap-lookup orgx-mode-map "C-c o")))
  (should (eq orgx-command-map (keymap-lookup orgx-global-mode-map "C-c o"))))

(ert-deftest orgx-command-map-keys-test ()
  "Test that all canonical command keys exist in orgx-command-map."
  (should (eq #'org-agenda (keymap-lookup orgx-command-map "4")))
  (should (eq #'org-capture (keymap-lookup orgx-command-map "k")))
  (should (eq #'org-save-all-org-buffers (keymap-lookup orgx-command-map "s")))
  (should (eq #'orgx-agenda-view (keymap-lookup orgx-command-map "a")))
  (should (eq #'orgx-agenda-files-reload (keymap-lookup orgx-command-map "r")))
  (should (eq #'orgx-agenda-files-open (keymap-lookup orgx-command-map "f")))
  (should (eq #'orgx-agenda-files-open (keymap-lookup orgx-command-map "o")))
  (should (eq #'orgx-capture (keymap-lookup orgx-command-map "j")))
  (should (eq #'orgx-agenda-for-file (keymap-lookup orgx-command-map "/")))
  (should (eq #'orgx-capture-shell-toggle (keymap-lookup orgx-command-map "C-s")))
  (should (eq #'orgx-capture-shell-transient (keymap-lookup orgx-command-map "C-t"))))

(ert-deftest orgx-global-minor-mode-test ()
  "Test that activating orgx-global-minor-mode runs setup and sets hooks."
  (let ((hook-run nil)
        (orgx--setup-done nil))
    (add-hook 'orgx-setup-hook (lambda () (setq hook-run t)))
    (unwind-protect
        (progn
          (orgx-global-minor-mode 1)
          (should orgx-global-mode)
          (should orgx--setup-done)
          (should hook-run)
          (should (memq #'orgx-mode-turn-on org-mode-hook))
          (should (memq #'orgx-agenda-minor-mode-turn-on org-agenda-mode-hook))
          (should (memq #'orgx-mode-turn-on org-capture-mode-hook))
          (orgx-global-minor-mode -1)
          (should-not orgx-global-mode)
          (should-not (memq #'orgx-mode-turn-on org-mode-hook)))
      (setq orgx-setup-hook nil)
      (orgx-global-minor-mode -1))))

(ert-deftest orgx-mode-triggers-setup-test ()
  "Test that activating orgx-mode triggers orgx-setup if not already done."
  (let ((orgx--setup-done nil)
        (hook-called nil))
    (add-hook 'orgx-setup-hook (lambda () (setq hook-called t)))
    (unwind-protect
        (with-temp-buffer
          (orgx-test--org-mode)
          (orgx-mode 1)
          (should orgx--setup-done)
          (should hook-called)
          (orgx-mode -1))
      (setq orgx-setup-hook nil))))

(ert-deftest orgx-global-mode-revert-settings-test ()
  "Test that disabling orgx-global-mode reverts managed settings."
  (let ((org-agenda-sticky nil)
        (org-agenda-default-appointment-duration 30)
        (org-refile-targets nil))
    (orgx-global-mode 1)
    (should (eq org-agenda-sticky t))
    (should (= org-agenda-default-appointment-duration 60))
    (should (equal org-refile-targets '((org-agenda-files :maxlevel . 4))))
    (orgx-global-mode -1)
    (should (eq org-agenda-sticky nil))
    (should (= org-agenda-default-appointment-duration 30))
    (should (null org-refile-targets))))

(ert-deftest orgx-babel-setup-test ()
  "Test that orgx-babel-setup loads all requested languages and defines aliases."
  (let ((alias-features '(ob-golang ob-elisp ob-javascript))
        (saved-languages (default-value 'org-babel-load-languages)))
    (unwind-protect
        (progn
          (setq-default org-babel-load-languages
                        '((golang . t) (elisp . t) (javascript . t)))
          (dolist (feature alias-features)
            (should-not (featurep feature)))
          (orgx-babel-setup)
          (orgx-babel-setup)
          (dolist (feature alias-features)
            (should-not (featurep feature)))
          (dolist (alias '(golang elisp javascript))
            (should-not (assq alias org-babel-load-languages)))
          (dolist (language '(python emacs-lisp dot js go rust))
            (should (assq language org-babel-load-languages))))
      (setq-default org-babel-load-languages saved-languages)))
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src elisp :results value\n(+ 1 2)\n#+end_src\n")
    (goto-char (point-min))
    (let ((org-confirm-babel-evaluate nil))
      (should (equal (org-babel-execute-src-block) 3))))
  (dolist (mode '(("golang" . go) ("elisp" . emacs-lisp)
                  ("javascript" . js) ("rust" . rust)))
    (should (member mode org-src-lang-modes)))
  (should (fboundp 'org-babel-execute:golang))
  (should (fboundp 'org-babel-execute:elisp))
  (should (fboundp 'org-babel-execute:javascript)))

(ert-deftest orgx-set-notes-directory-test ()
  "Test setting the notes directory updates related paths."
  (let ((local-notes-directory nil)
        (user-org-directories '("/tmp/other-org"))
        (org-directory nil)
        (org-agenda-files '("/tmp/existing.org"))
        (org-annotate-file-storage-file nil)
        (org-default-notes-file nil)
        (org-archive-location nil)
        (deft-directory nil)
        (denote-directory nil))
    (orgx-set-notes-directory "/tmp/test-notes")
    (should (equal local-notes-directory (expand-file-name "/tmp/test-notes")))
    (should (equal org-directory (expand-file-name "/tmp/test-notes/org")))
    (should (equal org-agenda-files '("/tmp/existing.org")))
    (cl-letf (((symbol-function 'denote-directories)
               (lambda () (list denote-directory))))
      (orgx-add-notes-to-agenda)
      (orgx-add-notes-to-agenda))
    (should (equal org-agenda-files
                   '("/tmp/existing.org" "/tmp/test-notes/org"
                     "/tmp/other-org")))
    (should (equal org-annotate-file-storage-file (expand-file-name "/tmp/test-notes/org/records.org")))
    (should (equal org-default-notes-file (expand-file-name "/tmp/test-notes/org/records.org")))
    (should (equal org-archive-location (expand-file-name "/tmp/test-notes/org/archive/%s::datetree/")))
    (should (equal deft-directory (expand-file-name "/tmp/test-notes/deft")))
    (should (equal denote-directory (expand-file-name "/tmp/test-notes/denote")))))

(ert-deftest orgx-add-notes-to-agenda-list-and-file-test ()
  "Preserve agenda entries without adding Denote directories."
  (let ((org-directory "/tmp/org-a")
        (denote-directory "/tmp/denote-a")
        (user-org-directories '("/tmp/extra"))
        (org-agenda-files '("/tmp/existing")))
    (cl-letf (((symbol-function 'denote-directories)
               (lambda () '("/tmp/denote-a" "/tmp/silo"))))
      (orgx-add-notes-to-agenda)
      (orgx-add-notes-to-agenda))
    (should (equal org-agenda-files
                   '("/tmp/existing" "/tmp/org-a" "/tmp/extra"))))
  (let ((org-agenda-files "/tmp/agenda-list"))
    (cl-letf (((symbol-function 'display-warning) #'ignore))
      (orgx-add-notes-to-agenda))
    (should (equal org-agenda-files "/tmp/agenda-list"))))

(ert-deftest orgx-command-map-capture-submap-test ()
  "c in orgx-command-map leads to the capture submap."
  (should (eq orgx-capture-map (keymap-lookup orgx-command-map "c"))))

(ert-deftest orgx-command-map-no-capitals-test ()
  "No plain capital-letter keys are bound in orgx-command-map."
  (should-not (keymap-lookup orgx-command-map "W")))

(ert-deftest orgx-agenda-minor-mode-map-test ()
  "orgx-agenda-minor-mode-map is a keymap binding /, C-l, and C-e to commands."
  (should (keymapp orgx-agenda-minor-mode-map))
  (should (commandp (keymap-lookup orgx-agenda-minor-mode-map "/")))
  (should (commandp (keymap-lookup orgx-agenda-minor-mode-map "C-l")))
  (should (commandp (keymap-lookup orgx-agenda-minor-mode-map "C-e"))))

(ert-deftest orgx-agenda-view-candidate-test ()
  "Dispatch key is left-justified to the given width before the colon."
  (should (equal "da: Denote Agenda ALL"
                 (orgx--agenda-view-candidate '("da" "Denote Agenda ALL") 2)))
  (should (equal "a : Agenda (week/day)"
                 (orgx--agenda-view-candidate '("a" "Agenda (week/day)") 2))))

(ert-deftest orgx-enforce-question-answered-blocks-done-test ()
  "A :question: heading refuses to close as DONE while orgx-mode is active."
  ;; `org-set-regexps-and-options' reads `(default-value 'org-todo-keywords)'
  ;; when the buffer has no #+TODO: lines, so the override must be a dynamic
  ;; `let', not `setq-local' (which only shadows the default, it does not
  ;; replace it).
  (let ((org-todo-keywords '((sequence "TODO" "QUESTION" "|" "ANSWERED" "DONE"))))
    (with-temp-buffer
      (orgx-test--org-mode)
      (org-set-regexps-and-options)
      (orgx-mode 1)
      (insert "* TODO Sample question :question:\n")
      (goto-char (point-min))
      (org-back-to-heading)
      (should-error (org-todo "DONE") :type 'user-error)
      (should (equal "TODO" (org-get-todo-state))))))

(ert-deftest orgx-enforce-question-answered-allows-answered-test ()
  "A :question: heading may close as ANSWERED while orgx-mode is active."
  (let ((org-todo-keywords '((sequence "TODO" "QUESTION" "|" "ANSWERED" "DONE"))))
    (with-temp-buffer
      (orgx-test--org-mode)
      (org-set-regexps-and-options)
      (orgx-mode 1)
      (insert "* TODO Sample question :question:\n")
      (goto-char (point-min))
      (org-back-to-heading)
      (org-todo "ANSWERED")
      (should (equal "ANSWERED" (org-get-todo-state))))))

(ert-deftest orgx-enforce-question-answered-ignores-untagged-test ()
  "A heading without :question: closes normally with DONE while orgx-mode is active."
  (let ((org-todo-keywords '((sequence "TODO" "QUESTION" "|" "ANSWERED" "DONE"))))
    (with-temp-buffer
      (orgx-test--org-mode)
      (org-set-regexps-and-options)
      (orgx-mode 1)
      (insert "* TODO Ordinary task\n")
      (goto-char (point-min))
      (org-back-to-heading)
      (org-todo "DONE")
      (should (equal "DONE" (org-get-todo-state))))))

(ert-deftest orgx-link-github-line-support-rewrites-line-test ()
  "GitHub-style #L123 fragments are rewritten to ::123."
  (let ((captured nil))
    (orgx-link-github-line-support (lambda (path _in-emacs) (setq captured path))
                                   "path/to/foo.el#L42"
                                   nil)
    (should (equal "path/to/foo.el::42" captured))))

(ert-deftest orgx-link-github-line-support-rewrites-range-test ()
  "GitHub-style #L123-L145 fragments are rewritten to the first line ::123."
  (let ((captured nil))
    (orgx-link-github-line-support (lambda (path _in-emacs) (setq captured path))
                                   "path/to/foo.el#L123-L145"
                                   nil)
    (should (equal "path/to/foo.el::123" captured))))

(ert-deftest orgx-link-github-line-support-preserves-plain-path-test ()
  "Paths without GitHub line fragments are passed through unchanged."
  (let ((captured nil))
    (orgx-link-github-line-support (lambda (path _in-emacs) (setq captured path))
                                   "path/to/foo.el"
                                   nil)
    (should (equal "path/to/foo.el" captured))))

(ert-deftest orgx-open-custom-id-slug-fallback-opens-matching-custom-id-test ()
  "Slug fallback jumps to the heading with a matching CUSTOM_ID."
  (with-temp-buffer
    (orgx-test--org-mode)
    (insert "* Target Heading\n:PROPERTIES:\n:CUSTOM_ID: target-id\n:END:\n")
    (should (orgx-open-custom-id-slug-fallback "#target-id"))
    (should (looking-at "\\* Target Heading"))))

(ert-deftest orgx-open-custom-id-slug-fallback-ignores-non-id-paths-test ()
  "Paths without a leading # are left untouched."
  (with-temp-buffer
    (orgx-test--org-mode)
    (insert "* Target Heading\n:PROPERTIES:\n:CUSTOM_ID: target-id\n:END:\n")
    (should-not (orgx-open-custom-id-slug-fallback "target-id"))))

(ert-deftest orgx-auxiliary-packages-includes-ob-mermaid-test ()
  "orgx--auxiliary-packages includes ob-mermaid."
  (should (memq 'ob-mermaid orgx--auxiliary-packages)))

(ert-deftest orgx-skip-child-of-project-tag-case-insensitive-test ()
  "orgx-skip-child-of-project-tag matches project tags regardless of case."
  (with-temp-buffer
    (orgx-test--org-mode)
    (insert "* TODO Epic Thing :EPIC:\n** TODO Subtask A\n* TODO Standalone Task\n")
    (goto-char (point-min))
    (forward-line 1)
    (org-back-to-heading)
    (should (orgx-skip-child-of-project-tag))))

(ert-deftest orgx-skip-child-of-project-tag-keeps-tagged-heading-test ()
  "orgx-skip-child-of-project-tag does not skip the tagged heading itself."
  (with-temp-buffer
    (orgx-test--org-mode)
    (insert "* TODO Epic Thing :EPIC:\n** TODO Subtask A\n")
    (goto-char (point-min))
    (org-back-to-heading)
    (should-not (orgx-skip-child-of-project-tag))))

(ert-deftest orgx-agenda-switch-buffer-test ()
  "orgx-agenda-switch-buffer offers only org-mode/org-agenda-mode buffers
and switches to the one selected via `annotated-completing-read'."
  (let (org-buf other-buf captured-candidates switched-to)
    (unwind-protect
        (progn
          (setq org-buf (generate-new-buffer "orgx-switch-test-org"))
          (setq other-buf (generate-new-buffer "orgx-switch-test-other"))
          (with-current-buffer org-buf (orgx-test--org-mode))
          (cl-letf (((symbol-function 'annotated-completing-read)
                     (lambda (table &rest _)
                       (setq captured-candidates (mapcar #'car table))
                       (buffer-name org-buf)))
                    ((symbol-function 'switch-to-buffer)
                     (lambda (buf) (setq switched-to buf))))
            (orgx-agenda-switch-buffer))
          (should (member (buffer-name org-buf) captured-candidates))
          (should-not (member (buffer-name other-buf) captured-candidates))
          (should (equal (buffer-name org-buf) switched-to)))
      (when (buffer-live-p org-buf) (kill-buffer org-buf))
      (when (buffer-live-p other-buf) (kill-buffer other-buf)))))

(ert-deftest orgx-agenda-switch-buffer-errors-without-buffers-test ()
  "orgx-agenda-switch-buffer signals a user-error when none are open."
  (cl-letf (((symbol-function 'buffer-list) (lambda () nil)))
    (should-error (orgx-agenda-switch-buffer) :type 'user-error)))

(ert-deftest orgx-find-heading-test ()
  "orgx-find-heading jumps to the target chosen via `annotated-completing-read'."
  (let* ((temp-file (make-temp-file "orgx-find-heading-" nil ".org"))
         (targets (list (list "Target One" temp-file nil 1)
                        (list "Target Two" temp-file nil 20))))
    (unwind-protect
        (progn
          (with-temp-file temp-file (insert "* Target One\n* Target Two\n"))
          (cl-letf (((symbol-function 'org-refile-get-targets) (lambda (&rest _) targets))
                    ((symbol-function 'annotated-completing-read)
                     (lambda (&rest _) "Target Two"))
                    ((symbol-function 'org-fold-show-context) #'ignore))
            (orgx-find-heading)
            (should (file-equal-p temp-file (buffer-file-name)))
            (should (= 20 (point)))))
      (when (file-exists-p temp-file) (delete-file temp-file))
      (when-let* ((buf (find-buffer-visiting temp-file))) (kill-buffer buf)))))

(provide 'orgx-test)
;;; Entry utilities

(ert-deftest orgx-entry-primitives-test ()
  (with-temp-buffer
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-log-done nil))
      (orgx-test--org-mode)
      (insert "* TODO A\n:PROPERTIES:\n:CUSTOM_ID: a\n:END:\n* B\n:PROPERTIES:\n:FOO: bar\n:END:\nbody\n")
      (org-set-regexps-and-options)
      (should-not (orgx-entry-find-by-property "FOO" "zzz"))
      (let ((m (orgx-entry-find-by-property "FOO" "bar")))
        (should (markerp m))
        (goto-char (point-min))
        (orgx-entry-set-property "X" "1" m)
        (should (= (point) 1))
        (should (equal (org-entry-get m "X") "1"))
        (orgx-entry-set-todo "DONE" m)
        (should (equal (org-entry-get m "TODO") "DONE"))
        (should-error (orgx-entry-set-todo "NOPE" m))
        (orgx-entry-append-note "line1\nline2" m)
        (should (string-match-p "body\n- \\[[^]]+\\] line1\n  line2\n\\'"
                                (buffer-string)))))))

(ert-deftest orgx-entry-get-test ()
  (with-temp-buffer
    (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
      (orgx-test--org-mode)
      (insert "* TODO Task Title\n:PROPERTIES:\n:CUSTOM_ID: parent-1\n:PR: 42\n:END:\nParent body text\n** fix-ci\n:PROPERTIES:\n:ACTION: fix-ci\n:END:\nChild body\n** threads\n")
      (org-set-regexps-and-options)
      (let ((res (orgx-entry-get (point-min))))
        (should (equal "Task Title" (plist-get res :heading)))
        (should (equal "TODO" (plist-get res :todo)))
        (should (equal "Parent body text" (plist-get res :body)))
        (should (equal '(("CUSTOM_ID" . "parent-1") ("PR" . "42"))
                       (plist-get res :properties)))
        (let ((children (plist-get res :children)))
          (should (= 2 (length children)))
          (should (equal "fix-ci" (plist-get (car children) :heading)))
          (should (equal nil (plist-get (car children) :todo)))
          (should (equal "Child body" (plist-get (car children) :body)))
          (should (equal '(("ACTION" . "fix-ci")) (plist-get (car children) :properties)))
          (should (equal "threads" (plist-get (cadr children) :heading)))
          (should (equal "" (plist-get (cadr children) :body))))))))

;;; orgx-test.el ends here
