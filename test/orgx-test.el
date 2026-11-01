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
  (should (fboundp 'orgx-reset-capture-templates))
  (should (fboundp 'orgx-capture-shell))
  (should (fboundp 'orgx-migrate-subtree-to-denote)))

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
    (orgx-mode -1)
    (should-not orgx-mode)
    (should-not (memq #'orgx-set-weekday-of-timestamp org-ctrl-c-ctrl-c-hook))))
