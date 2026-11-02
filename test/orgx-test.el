;;; orgx-test.el --- Tests for orgx.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'org)
(require 'orgx)
(require 'orgx-loaddefs)

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

(ert-deftest orgx-autoload-capture-test ()
  "Test that orgx-capture is autoloaded on demand."
  (should (fboundp 'orgx-capture))
  (should (fboundp 'orgx-capture-shell)))

(ert-deftest orgx-mode-setup-test ()
  "Test that activating orgx-mode applies local settings."
  (with-temp-buffer
    (org-mode)
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
    (org-mode)
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
          (org-mode)
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
  (orgx-babel-setup)
  (should (fboundp 'org-babel-execute:golang))
  (should (fboundp 'org-babel-execute:elisp))
  (should (fboundp 'org-babel-execute:javascript))
  (should (assq 'python org-babel-load-languages))
  (should (assq 'emacs-lisp org-babel-load-languages))
  (should (assq 'dot org-babel-load-languages))
  (should (assq 'js org-babel-load-languages))
  (should (assq 'go org-babel-load-languages))
  (should (assq 'rust org-babel-load-languages)))

(ert-deftest orgx-set-notes-directory-test ()
  "Test setting the notes directory updates related paths."
  (let ((local-notes-directory nil)
        (user-org-directories nil)
        (org-directory nil)
        (org-agenda-files nil)
        (org-annotate-file-storage-file nil)
        (deft-directory nil)
        (denote-directory nil))
    (orgx-set-notes-directory "/tmp/test-notes")
    (should (equal local-notes-directory (expand-file-name "/tmp/test-notes")))
    (should (equal org-directory (expand-file-name "/tmp/test-notes/org")))
    (should (member (expand-file-name "/tmp/test-notes/org") org-agenda-files))
    (should (equal org-annotate-file-storage-file (expand-file-name "/tmp/test-notes/org/records.org")))
    (should (equal org-default-notes-file (expand-file-name "/tmp/test-notes/org/records.org")))
    (should (equal org-archive-location (expand-file-name "/tmp/test-notes/org/archive/%s::datetree/")))
    (should (equal deft-directory (expand-file-name "/tmp/test-notes/deft")))
    (should (equal denote-directory (expand-file-name "/tmp/test-notes/denote")))
    ;; Also verify bootstrap-set-notes-directory alias works
    (bootstrap-set-notes-directory "/tmp/test-notes-2")
    (should (equal local-notes-directory (expand-file-name "/tmp/test-notes-2")))))

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
      (org-mode)
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
      (org-mode)
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
      (org-mode)
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
    (org-mode)
    (insert "* Target Heading\n:PROPERTIES:\n:CUSTOM_ID: target-id\n:END:\n")
    (should (orgx-open-custom-id-slug-fallback "#target-id"))
    (should (looking-at "\\* Target Heading"))))

(ert-deftest orgx-open-custom-id-slug-fallback-ignores-non-id-paths-test ()
  "Paths without a leading # are left untouched."
  (with-temp-buffer
    (org-mode)
    (insert "* Target Heading\n:PROPERTIES:\n:CUSTOM_ID: target-id\n:END:\n")
    (should-not (orgx-open-custom-id-slug-fallback "target-id"))))

(ert-deftest orgx-auxiliary-packages-includes-ob-mermaid-test ()
  "orgx--auxiliary-packages includes ob-mermaid."
  (should (memq 'ob-mermaid orgx--auxiliary-packages)))

(ert-deftest orgx-skip-child-of-project-tag-case-insensitive-test ()
  "orgx-skip-child-of-project-tag matches project tags regardless of case."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Epic Thing :EPIC:\n** TODO Subtask A\n* TODO Standalone Task\n")
    (goto-char (point-min))
    (forward-line 1)
    (org-back-to-heading)
    (should (orgx-skip-child-of-project-tag))))

(ert-deftest orgx-skip-child-of-project-tag-keeps-tagged-heading-test ()
  "orgx-skip-child-of-project-tag does not skip the tagged heading itself."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Epic Thing :EPIC:\n** TODO Subtask A\n")
    (goto-char (point-min))
    (org-back-to-heading)
    (should-not (orgx-skip-child-of-project-tag))))

(provide 'orgx-test)
;;; orgx-test.el ends here
