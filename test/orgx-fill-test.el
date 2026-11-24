;;; orgx-fill-test.el --- Tests for orgx-fill -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `orgx-fill.el'.

;;; Code:

(require 'ert)
(require 'org)
(require 'orgx-fill)

(defconst orgx-fill-test--long "foo bar baz qux quux corge grault garply waldo fred")

(ert-deftest orgx-fill-buffer-skips-org-structure ()
  (with-temp-buffer
    (org-mode)
    (insert "#+title: " orgx-fill-test--long "\n* " orgx-fill-test--long "\n"
            ":PROPERTIES:\n:FOO: " orgx-fill-test--long "\n:END:\n"
            "#+begin_src sh\n" orgx-fill-test--long "\n#+end_src\n"
            orgx-fill-test--long "\n")
    (should (orgx-fill-buffer 30))
    (should (string-match-p (concat "^#\\+title: " orgx-fill-test--long "$") (buffer-string)))
    (should (string-match-p (concat "^\\* " orgx-fill-test--long "$") (buffer-string)))
    (should (string-match-p (concat "^:FOO: " orgx-fill-test--long "$") (buffer-string)))
    (should (string-match-p (concat "^" orgx-fill-test--long "\n#\\+end_src") (buffer-string)))
    (should (string-match-p "^foo bar baz qux quux corge\ngrault" (buffer-string)))))

(ert-deftest orgx-fill-handler-runs-before-suppression ()
  (with-temp-buffer
    (insert "abc\n")
    (let ((orgx-fill-handlers (list (lambda () t)))
          (orgx-fill-suppress-functions (list (lambda () (error "not reached")))))
      (should (orgx-fill-paragraph-at-point)))))

(ert-deftest orgx-fill-with-prefix-indents-and-keeps-blank-lines ()
  (with-temp-buffer
    (insert "  " orgx-fill-test--long "\n\n  " orgx-fill-test--long "\n")
    (let ((fill-column 30))
      (orgx-fill-with-prefix (point-min) (point-max) 2))
    (should (string-match-p "^  foo bar baz qux quux corge\n  grault" (buffer-string)))
    (should (string-match-p "\n\n  foo bar" (buffer-string)))))

(ert-deftest orgx-fill-guard-mode-installs-and-removes-advice ()
  (let ((orgx-fill-advised-functions '(fill-paragraph)))
    (unwind-protect
        (progn
          (orgx-fill-guard-mode 1)
          (should (advice-member-p #'ad:orgx-fill-guard 'fill-paragraph))
          (orgx-fill-guard-mode -1)
          (should-not (advice-member-p #'ad:orgx-fill-guard 'fill-paragraph)))
      (orgx-fill-guard-mode -1))))

(provide 'orgx-fill-test)
;;; orgx-fill-test.el ends here
