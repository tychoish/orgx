;;; orgx-fill-test.el --- Tests for orgx-fill -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `orgx-fill.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'orgx-fill)

(defconst orgx-fill-test--long "foo bar baz qux quux corge grault garply waldo fred"
  "A line longer than the 30 column limit the tests fill to.")

(defconst orgx-fill-test--filled "foo bar baz qux quux corge\ngrault garply waldo fred"
  "`orgx-fill-test--long' filled to 30 columns.")

(defmacro orgx-fill-test--with-org (text &rest body)
  "Run BODY in an `org-mode' temp buffer holding TEXT, point at the start."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,text)
     (goto-char (point-min))
     ,@body))

(defun orgx-fill-test--suppressed-at (text pattern)
  "Return `orgx-fill-org-suppressed-p' with point on the line matching PATTERN in TEXT."
  (orgx-fill-test--with-org text
    (re-search-forward pattern)
    (goto-char (line-beginning-position))
    (and (orgx-fill-org-suppressed-p) t)))

;;; Org predicate

(ert-deftest orgx-fill-org-suppressed-p-structural-lines ()
  (dolist (case '(("* Heading\n" . "Heading")
                  ("#+title: x\n" . "title")
                  ("* H\n:PROPERTIES:\n:FOO: bar\n:END:\n" . "PROPERTIES")
                  ("* H\n:PROPERTIES:\n:FOO: bar\n:END:\n" . ":FOO:")
                  ("* H\n:LOGBOOK:\n- note\n:END:\n" . "LOGBOOK")
                  ("* H\nSCHEDULED: <2026-11-23 Mon>\n" . "SCHEDULED")
                  ("* H\n:LOGBOOK:\nCLOCK: [2026-11-23 Mon 10:00]\n:END:\n" . "CLOCK")
                  ("#+call: foo()\n" . "call")
                  ("#+begin_src sh\nls\n#+end_src\n" . "begin_src")
                  ("#+begin_src sh\nls\n#+end_src\n" . "^ls")
                  ("#+begin_example\nx\n#+end_example\n" . "^x")
                  ("#+begin_export html\nx\n#+end_export\n" . "^x")
                  ("#+begin_verse\nx\n#+end_verse\n" . "^x")
                  ("#+begin_comment\nx\n#+end_comment\n" . "^x")
                  ("\\begin{equation}\nx\n\\end{equation}\n" . "^x")
                  (": fixed\n" . "fixed")
                  ("| a | b |\n" . "| a")
                  ("#+begin_quote\nx\n#+end_quote\n" . "begin_quote")
                  ("#+begin_center\nx\n#+end_center\n" . "begin_center")
                  ("#+begin_foo\nx\n#+end_foo\n" . "begin_foo")))
    (should (orgx-fill-test--suppressed-at (car case) (cdr case)))))

(ert-deftest orgx-fill-org-suppressed-p-prose-is-fillable ()
  (dolist (case '(("plain paragraph\n" . "plain")
                  ("- list item\n" . "list")
                  ("1. numbered item\n" . "numbered")
                  ("#+begin_quote\nquoted text\n#+end_quote\n" . "quoted")
                  ("# a comment\n" . "comment")
                  ("[fn:1] footnote\n" . "footnote")))
    (should-not (orgx-fill-test--suppressed-at (car case) (cdr case)))))

(ert-deftest orgx-fill-org-suppressed-p-ignores-other-modes ()
  (with-temp-buffer
    (text-mode)
    (insert "* looks like a heading\n")
    (goto-char (point-min))
    (should-not (orgx-fill-org-suppressed-p))))

(ert-deftest orgx-fill-org-element-types-is-customizable ()
  (orgx-fill-test--with-org "* Heading\n"
    (should (orgx-fill-org-suppressed-p))
    (let ((orgx-fill-org-element-types nil))
      (should-not (orgx-fill-org-suppressed-p)))))

;;; Policy lists

(ert-deftest orgx-fill-suppressed-p-any-predicate ()
  (let ((orgx-fill-suppress-functions (list #'ignore (lambda () t))))
    (should (orgx-fill-suppressed-p)))
  (let ((orgx-fill-suppress-functions (list #'ignore #'ignore)))
    (should-not (orgx-fill-suppressed-p)))
  (let ((orgx-fill-suppress-functions nil))
    (should-not (orgx-fill-suppressed-p))))

(ert-deftest orgx-fill-handled-p-stops-at-first-handler ()
  (let* ((calls nil)
         (orgx-fill-handlers (list (lambda () (push 'a calls) nil)
                                   (lambda () (push 'b calls) t)
                                   (lambda () (push 'c calls) t))))
    (should (orgx-fill-handled-p))
    (should (equal (reverse calls) '(a b)))))

(ert-deftest orgx-fill-handled-p-no-handlers ()
  (let ((orgx-fill-handlers nil))
    (should-not (orgx-fill-handled-p))))

(ert-deftest orgx-fill-defaults-cover-org-and-markdown ()
  (should (memq #'orgx-fill-org-suppressed-p
                (default-value 'orgx-fill-suppress-functions)))
  (should (memq #'orgx-fill-markdown-suppressed-p
                (default-value 'orgx-fill-suppress-functions)))
  (should (memq #'orgx-fill-markdown-fill-folded-scalar
                (default-value 'orgx-fill-handlers)))
  (should (memq 'markdown-fill-paragraph
                (default-value 'orgx-fill-advised-functions))))

;;; Helpers

(ert-deftest orgx-fill-with-prefix-indents-and-keeps-blank-lines ()
  (with-temp-buffer
    (insert "  " orgx-fill-test--long "\n\n  " orgx-fill-test--long "\n")
    (let ((fill-column 30))
      (orgx-fill-with-prefix (point-min) (point-max) 2))
    (should (equal (buffer-string)
                   (concat "  foo bar baz qux quux corge\n  grault garply waldo fred\n\n"
                           "  foo bar baz qux quux corge\n  grault garply waldo fred\n")))))

(ert-deftest orgx-fill-with-prefix-ignores-mode-paragraph-rules ()
  (with-temp-buffer
    (let ((fill-forward-paragraph-function (lambda (&rest _) (error "mode rule used"))))
      (insert "  " orgx-fill-test--long "\n")
      (let ((fill-column 30))
        (orgx-fill-with-prefix (point-min) (point-max) 2))
      (should (string-match-p "^  grault" (buffer-string))))))

(ert-deftest orgx-fill-paragraph-at-point-results ()
  (let ((orgx-fill-handlers (list (lambda () t)))
        (orgx-fill-suppress-functions (list (lambda () (error "not reached")))))
    (should (eq t (orgx-fill-paragraph-at-point))))
  (let ((orgx-fill-handlers nil)
        (orgx-fill-suppress-functions (list (lambda () t))))
    (should-not (orgx-fill-paragraph-at-point)))
  (orgx-fill-test--with-org orgx-fill-test--long
    (let ((fill-column 30))
      (should (eq t (orgx-fill-paragraph-at-point)))
      (should (equal (buffer-string) orgx-fill-test--filled)))))

;;; orgx-fill-buffer

(ert-deftest orgx-fill-buffer-fills-prose-and-lists ()
  (orgx-fill-test--with-org (concat orgx-fill-test--long "\n\n- " orgx-fill-test--long "\n")
    (should (orgx-fill-buffer 30))
    (should (equal (buffer-string)
                   (concat orgx-fill-test--filled
                           "\n\n- foo bar baz qux quux corge\n  grault garply waldo fred\n")))))

(ert-deftest orgx-fill-buffer-skips-org-structure ()
  (let ((text (concat "#+title: " orgx-fill-test--long "\n"
                      "* " orgx-fill-test--long "\n"
                      "SCHEDULED: <2026-11-23 Mon>\n"
                      ":PROPERTIES:\n:FOO: " orgx-fill-test--long "\n:END:\n"
                      "#+begin_src sh\n" orgx-fill-test--long "\n#+end_src\n"
                      "#+begin_example\n" orgx-fill-test--long "\n#+end_example\n"
                      "#+begin_verse\n" orgx-fill-test--long "\n#+end_verse\n"
                      "#+begin_comment\n" orgx-fill-test--long "\n#+end_comment\n"
                      ": " orgx-fill-test--long "\n"
                      "| " orgx-fill-test--long " | x |\n")))
    (orgx-fill-test--with-org text
      (should-not (orgx-fill-buffer 30))
      (should (equal (buffer-string) text)))))

(ert-deftest orgx-fill-buffer-descends-into-greater-blocks ()
  (orgx-fill-test--with-org (concat "#+begin_quote\n" orgx-fill-test--long "\n#+end_quote\n"
                                    "#+begin_center\n" orgx-fill-test--long "\n#+end_center\n")
    (should (orgx-fill-buffer 30))
    (should (equal (buffer-string)
                   (concat "#+begin_quote\n" orgx-fill-test--filled "\n#+end_quote\n"
                           "#+begin_center\n" orgx-fill-test--filled "\n#+end_center\n")))))

(ert-deftest orgx-fill-buffer-fills-text-between-structure ()
  (orgx-fill-test--with-org (concat "* H\n" orgx-fill-test--long "\n"
                                    "#+begin_src sh\nls\n#+end_src\n\n"
                                    orgx-fill-test--long "\n")
    (should (orgx-fill-buffer 30))
    (should (equal (buffer-string)
                   (concat "* H\n" orgx-fill-test--filled "\n"
                           "#+begin_src sh\nls\n#+end_src\n\n"
                           orgx-fill-test--filled "\n")))))

(ert-deftest orgx-fill-buffer-is-idempotent ()
  (orgx-fill-test--with-org (concat "* H\n" orgx-fill-test--long "\n\n- " orgx-fill-test--long "\n")
    (should (orgx-fill-buffer 30))
    (let ((once (buffer-string)))
      (should-not (orgx-fill-buffer 30))
      (should (equal once (buffer-string))))))

(ert-deftest orgx-fill-buffer-column-default-and-override ()
  (let ((orgx-fill-column 40))
    (orgx-fill-test--with-org orgx-fill-test--long
      (orgx-fill-buffer)
      (should (equal (buffer-string) "foo bar baz qux quux corge grault garply\nwaldo fred")))
    (orgx-fill-test--with-org orgx-fill-test--long
      (orgx-fill-buffer 30)
      (should (equal (buffer-string) orgx-fill-test--filled)))))

(ert-deftest orgx-fill-buffer-preserves-point ()
  (orgx-fill-test--with-org (concat orgx-fill-test--long "\n")
    (goto-char 5)
    (orgx-fill-buffer 30)
    (should (= (point) 5))))

(ert-deftest orgx-fill-buffer-empty-and-blank-buffers ()
  (orgx-fill-test--with-org ""
    (should-not (orgx-fill-buffer 30)))
  (orgx-fill-test--with-org "\n\n   \n"
    (should-not (orgx-fill-buffer 30))
    (should (equal (buffer-string) "\n\n   \n"))))

(ert-deftest orgx-fill-buffer-always-terminates ()
  "Constructs `fill-paragraph' declines to move past must not hang the loop."
  (orgx-fill-test--with-org "-----\n\n#+call: foo()\n#+name: bar\n\\begin{equation}\nx\n\\end{equation}\n"
    (with-timeout (5 (ert-fail "orgx-fill-buffer did not terminate"))
      (orgx-fill-buffer 30))
    (should t)))

(ert-deftest orgx-fill-buffer-runs-handlers-before-suppression ()
  (let ((handled nil))
    (with-temp-buffer
      (insert orgx-fill-test--long "\n")
      (let ((orgx-fill-handlers (list (lambda () (push (line-number-at-pos) handled) t)))
            (orgx-fill-suppress-functions (list (lambda () (error "not reached")))))
        (orgx-fill-buffer 30))
      (should (equal handled '(1)))
      (should (equal (buffer-string) (concat orgx-fill-test--long "\n"))))))

(ert-deftest orgx-fill-buffer-honors-custom-suppress-functions ()
  (with-temp-buffer
    (insert "keep " orgx-fill-test--long "\n\n" orgx-fill-test--long "\n")
    (let ((orgx-fill-suppress-functions
           (list (lambda () (looking-at-p "keep")))))
      (should (orgx-fill-buffer 30)))
    (should (string-prefix-p (concat "keep " orgx-fill-test--long "\n\n") (buffer-string)))
    (should (string-suffix-p (concat orgx-fill-test--filled "\n") (buffer-string)))))

;;; Guard mode

(ert-deftest orgx-fill-guard-mode-installs-and-removes-advice ()
  (let ((orgx-fill-advised-functions '(fill-paragraph)))
    (unwind-protect
        (progn
          (orgx-fill-guard-mode 1)
          (should (advice-member-p #'orgx-fill--ad-guard 'fill-paragraph))
          (orgx-fill-guard-mode -1)
          (should-not (advice-member-p #'orgx-fill--ad-guard 'fill-paragraph)))
      (orgx-fill-guard-mode -1))))

(ert-deftest orgx-fill-guard-mode-blocks-fill-paragraph-on-structure ()
  (unwind-protect
      (progn
        (orgx-fill-guard-mode 1)
        (let ((fill-column 30))
          (orgx-fill-test--with-org (concat "#+begin_src sh\n" orgx-fill-test--long "\n#+end_src\n")
            (forward-line 1)
            (fill-paragraph)
            (should (equal (buffer-string)
                           (concat "#+begin_src sh\n" orgx-fill-test--long "\n#+end_src\n"))))
          (with-temp-buffer
            (text-mode)
            (insert orgx-fill-test--long)
            (fill-paragraph)
            (should (equal (buffer-string) orgx-fill-test--filled)))))
    (orgx-fill-guard-mode -1)))

(ert-deftest orgx-fill-guard-mode-runs-handlers ()
  (unwind-protect
      (progn
        (orgx-fill-guard-mode 1)
        (let ((orgx-fill-handlers (list (lambda () (insert "handled") t))))
          (with-temp-buffer
            (fill-paragraph)
            (should (equal (buffer-string) "handled")))))
    (orgx-fill-guard-mode -1)))

(ert-deftest orgx-fill-guard-mode-advises-extra-functions ()
  (let ((orgx-fill-advised-functions '(fill-paragraph fill-region)))
    (unwind-protect
        (progn
          (orgx-fill-guard-mode 1)
          (should (advice-member-p #'orgx-fill--ad-guard 'fill-region))
          (orgx-fill-guard-mode -1)
          (should-not (advice-member-p #'orgx-fill--ad-guard 'fill-region)))
      (orgx-fill-guard-mode -1))))

(ert-deftest orgx-fill-guard-mode-loading-installs-nothing ()
  (should-not (advice-member-p #'orgx-fill--ad-guard 'fill-paragraph)))

;;; Markdown

(defmacro orgx-fill-test--with-markdown (text &rest body)
  "Run BODY in a `markdown-mode' temp buffer holding TEXT, point at the start.
Skips the test when markdown-mode is not installed."
  (declare (indent 1))
  `(progn
     (skip-unless (locate-library "markdown-mode"))
     (require 'markdown-mode)
     (with-temp-buffer
       (markdown-mode)
       (insert ,text)
       (goto-char (point-min))
       ,@body)))

(ert-deftest orgx-fill-markdown-fills-prose-only ()
  (orgx-fill-test--with-markdown (concat "# " orgx-fill-test--long "\n\n```\n"
                                         orgx-fill-test--long "\n```\n\n"
                                         orgx-fill-test--long "\n")
    (should (orgx-fill-buffer 30))
    (should (string-match-p (concat "^# " orgx-fill-test--long "$") (buffer-string)))
    (should (string-match-p (concat "^" orgx-fill-test--long "\n```") (buffer-string)))
    (should (string-suffix-p (concat orgx-fill-test--filled "\n") (buffer-string)))))

(ert-deftest orgx-fill-markdown-suppressed-p-by-context ()
  (orgx-fill-test--with-markdown (concat "# Heading\n\n```\ncode\n```\n\nprose\n")
    (should (orgx-fill-markdown-suppressed-p))
    (re-search-forward "^code")
    (should (orgx-fill-markdown-suppressed-p))
    (re-search-forward "^prose")
    (should-not (orgx-fill-markdown-suppressed-p))))

(ert-deftest orgx-fill-markdown-suppressed-p-ignores-other-modes ()
  (with-temp-buffer
    (text-mode)
    (insert "# not markdown\n")
    (should-not (orgx-fill-markdown-suppressed-p))
    (should-not (orgx-fill-markdown-fill-folded-scalar))))

(ert-deftest orgx-fill-markdown-frontmatter-p-bounds ()
  (orgx-fill-test--with-markdown "---\ntitle: x\n---\n\nbody\n"
    (should (orgx-fill-markdown-frontmatter-p))
    (re-search-forward "^title")
    (should (orgx-fill-markdown-frontmatter-p))
    (re-search-forward "^body")
    (should-not (orgx-fill-markdown-frontmatter-p))))

(ert-deftest orgx-fill-markdown-frontmatter-leaves-plain-and-literal-values ()
  (let ((text (concat "---\ntitle: " orgx-fill-test--long "\nlit: |\n  "
                      orgx-fill-test--long "\n---\n")))
    (orgx-fill-test--with-markdown text
      (should-not (orgx-fill-buffer 30))
      (should (equal (buffer-string) text)))))

(ert-deftest orgx-fill-markdown-frontmatter-fills-folded-scalars ()
  (skip-unless (and (fboundp 'treesit-language-available-p) (treesit-language-available-p 'yaml)))
  (orgx-fill-test--with-markdown (concat "---\ndesc: >\n  " orgx-fill-test--long
                                         "\n\n  second paragraph " orgx-fill-test--long
                                         "\nafter: " orgx-fill-test--long "\n---\n")
    (should (orgx-fill-buffer 30))
    (should (string-match-p "^desc: >\n  foo bar baz qux quux corge\n  grault garply waldo fred\n\n  second paragraph"
                            (buffer-string)))
    (should (string-match-p (concat "^after: " orgx-fill-test--long "$") (buffer-string)))
    (should-not (orgx-fill-buffer 30))))

(ert-deftest orgx-fill-markdown-guard-mode-covers-markdown-fill-paragraph ()
  (skip-unless (and (fboundp 'treesit-language-available-p) (treesit-language-available-p 'yaml)))
  (unwind-protect
      (progn
        (orgx-fill-guard-mode 1)
        (orgx-fill-test--with-markdown (concat "# " orgx-fill-test--long "\n")
          (let ((fill-column 30))
            (fill-paragraph))
          (should (equal (buffer-string) (concat "# " orgx-fill-test--long "\n"))))
        (orgx-fill-test--with-markdown (concat "---\ndesc: >\n  " orgx-fill-test--long "\n---\n")
          (let ((fill-column 30))
            (goto-char (point-min))
            (forward-line 2)
            (fill-paragraph))
          (should (string-match-p "^  grault" (buffer-string)))))
    (orgx-fill-guard-mode -1)))

(ert-deftest orgx-fill-org-buffers-do-not-need-markdown-mode ()
  "Org filling must not call into markdown-mode."
  (cl-letf (((symbol-function 'markdown-text-property-at-point)
             (lambda (&rest _) (error "markdown-mode called from org buffer")))
            ((symbol-function 'markdown-code-block-at-point-p)
             (lambda (&rest _) (error "markdown-mode called from org buffer"))))
    (orgx-fill-test--with-org (concat "* H\n" orgx-fill-test--long "\n")
      (should (orgx-fill-buffer 30)))))

(provide 'orgx-fill-test)
;;; orgx-fill-test.el ends here
