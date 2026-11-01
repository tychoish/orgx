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

(provide 'orgx-test)
;;; orgx-test.el ends here

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
