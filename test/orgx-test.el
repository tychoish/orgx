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
  (should (eq orgx-gist-map (keymap-lookup orgx-commands-map "g")))
  (should (commandp (keymap-lookup orgx-gist-map "p")))
  (should (commandp (keymap-lookup orgx-gist-map "g"))))

(ert-deftest orgx-minor-mode-aliases-test ()
  "Test compatibility aliases for orgx-minor-mode."
  (should (eq orgx-minor-mode-map orgx-mode-map))
  (should (eq orgx-minor-mode-commands-map orgx-commands-map))
  (should (eq orgx-minor-mode-archive-map orgx-archive-map))
  (should (eq orgx-minor-mode-capture-map orgx-capture-map)))

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
    (should (equal deft-directory (expand-file-name "/tmp/test-notes/deft")))
    (should (equal denote-directory (expand-file-name "/tmp/test-notes/denote")))
    ;; Also verify bootstrap-set-notes-directory alias works
    (bootstrap-set-notes-directory "/tmp/test-notes-2")
    (should (equal local-notes-directory (expand-file-name "/tmp/test-notes-2")))))

(provide 'orgx-test)
;;; orgx-test.el ends here
