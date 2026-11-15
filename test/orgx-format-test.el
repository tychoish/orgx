;;; orgx-format-test.el --- Tests for orgx-format -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for `orgx-format.el'.

;;; Code:

(require 'ert)
(require 'org)
(require 'orgx-format)

(ert-deftest orgx-format-paragraph-reflow-test ()
  "Reflows long paragraphs to configured line length while preserving src blocks."
  (with-temp-buffer
    (org-mode)
    (insert "* Section 1\nThis is a very long line of prose text that goes on and on and should be reflowed into multiple clean lines of text according to the line-length configuration.\n\n#+begin_src elisp\n(defun long-code-line-that-must-never-be-reflowed-by-the-paragraph-formatter ()\n  t)\n#+end_src\n")
    (let ((orgx-format-line-length 50))
      (orgx-format-buffer))
    (let ((content (buffer-string)))
      (should (string-match-p "This is a very long line of prose text that goes\non and on" content))
      (should (string-match-p "(defun long-code-line-that-must-never-be-reflowed-by-the-paragraph-formatter ()" content)))))

(ert-deftest orgx-format-table-align-test ()
  "Realigns malformed tables in the buffer."
  (with-temp-buffer
    (org-mode)
    (insert "* Table Section\n|a|bb|ccc|\n|1234|5|67890|\n")
    (orgx-format-buffer)
    (let ((content (buffer-string)))
      (should (string-match-p "|[ ]+a | bb |[ ]+ccc |" content))
      (should (string-match-p "| 1234 |  5 | 67890 |" content)))))

(ert-deftest orgx-format-trim-section-start-test ()
  "Trims blank lines immediately following headings."
  (with-temp-buffer
    (org-mode)
    (insert "* Heading 1\n\n\nFirst line of content.\n* Heading 2\n:PROPERTIES:\n:ID: test\n:END:\n\n\nFirst line after drawer.\n")
    (let ((orgx-format-trim-section-start t))
      (orgx-format-buffer))
    (let ((content (buffer-string)))
      (should (string-match-p "\\* Heading 1\nFirst line of content\\." content))
      (should (string-match-p ":END:\nFirst line after drawer\\." content)))))

(ert-deftest orgx-format-refuses-unsaved-non-interactive-test ()
  "Refuses non-interactively if buffer has unsaved changes."
  (let ((file (make-temp-file "orgx-format-" nil ".org")))
    (unwind-protect
        (let ((buf (find-file-noselect file)))
          (with-current-buffer buf
            (insert "* Dirty Heading\nSome text\n")
            (set-buffer-modified-p t)
            (should-error (orgx-format-buffer buf) :type 'user-error)))
      (when (get-file-buffer file)
        (kill-buffer (get-file-buffer file)))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest orgx-format-lint-test ()
  "Lints lines longer than max length and section-initial blank lines without changing buffer."
  (with-temp-buffer
    (org-mode)
    (let ((long-line (make-string 120 ?a)))
      (insert "* Heading\n\n" long-line "\n"))
    (let* ((initial (buffer-string))
           (orgx-format-max-line-length 100)
           (diags (orgx-format-lint)))
      (should (equal (buffer-string) initial))
      (should (seq-find (lambda (d) (eq (plist-get d :type) 'blank-line)) diags))
      (should (seq-find (lambda (d) (eq (plist-get d :type) 'line-length)) diags)))))

(ert-deftest orgx-format-batch-locking-and-cancel-test ()
  "Batch format locks buffers and cancel clears locks cleanly."
  (let ((file (make-temp-file "orgx-batch-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "* Batch Heading\n\nBody.\n"))
          (let ((buf (find-file-noselect file)))
            (with-current-buffer buf
              (orgx-format-lock-buffer buf)
              (should buffer-read-only)
              (should (assq buf orgx-format--locked-buffers))
              (orgx-format-batch-cancel)
              (should-not buffer-read-only)
              (should-not orgx-format--locked-buffers))))
      (when (get-file-buffer file)
        (kill-buffer (get-file-buffer file)))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest orgx-format-hardline-breaks-preserved-test ()
  "Hard line breaks with \\\\ are preserved and not joined."
  (with-temp-buffer
    (org-mode)
    (insert "* Hardlines\nLine 1 is quite long and goes on for a bit\\\\\nLine 2 is also long and continues for a bit\\\\\nLine 3 is the end\n")
    (let ((orgx-format-line-length 30))
      (orgx-format-buffer))
    (let ((content (buffer-string)))
      (should (string-match-p "Line 1 is quite long and goes\non for a bit\\\\" content))
      (should (string-match-p "Line 2 is also long and\ncontinues for a bit\\\\" content))
      (should (string-match-p "Line 3 is the end" content)))))

(ert-deftest orgx-format-plain-list-items-preserved-test ()
  "Plain list items are reflowed individually without merging items."
  (with-temp-buffer
    (org-mode)
    (insert "* List Section\n- item 1 with words that go on and on and should wrap onto the next line\n- item 2 with words that go on and on and should wrap onto the next line\n- item 3 with words that go on and on and should wrap onto the next line\n")
    (let ((orgx-format-line-length 40))
      (orgx-format-buffer))
    (let ((content (buffer-string)))
      (should (string-match-p "- item 1 with words that go on and on\n  and should wrap onto the next line" content))
      (should (string-match-p "- item 2 with words that go on and on\n  and should wrap onto the next line" content))
      (should (string-match-p "- item 3 with words that go on and on\n  and should wrap onto the next line" content)))))

(ert-deftest orgx-format-checkboxes-preserved-test ()
  "List items with checkboxes preserve checkboxes and indentation."
  (with-temp-buffer
    (org-mode)
    (insert "* Checkbox Section\n- [ ] todo item with a long description that needs to be reflowed cleanly\n- [X] done item with another long description that needs to be reflowed cleanly\n")
    (let ((orgx-format-line-length 40))
      (orgx-format-buffer))
    (let ((content (buffer-string)))
      (should (string-match-p "- \\[ \\] todo item with a long description\n  that needs to be reflowed cleanly" content))
      (should (string-match-p "- \\[X\\] done item with another long\n  description that needs to be reflowed\n  cleanly" content)))))

(ert-deftest orgx-format-verse-block-preserved-test ()
  "Verse blocks preserve newlines and are not reflowed."
  (with-temp-buffer
    (org-mode)
    (insert "* Verse Section\n#+begin_verse\nLine 1\nLine 2\nLine 3\n#+end_verse\n")
    (let ((orgx-format-line-length 40))
      (orgx-format-buffer))
    (let ((content (buffer-string)))
      (should (string-match-p "#\\+begin_verse\nLine 1\nLine 2\nLine 3\n#\\+end_verse" content)))))

(ert-deftest orgx-format-quote-block-test ()
  "Quote blocks reflow paragraph text while keeping block delimiters intact."
  (with-temp-buffer
    (org-mode)
    (insert "* Quote Section\n#+begin_quote\nThis is a long quote line that should be wrapped nicely within the quote block without breaking the quote syntax.\n#+end_quote\n")
    (let ((orgx-format-line-length 40))
      (orgx-format-buffer))
    (let ((content (buffer-string)))
      (should (string-match-p "#\\+begin_quote\nThis is a long quote line that should be\nwrapped nicely within the quote block\nwithout breaking the quote syntax\\.\n#\\+end_quote" content)))))

(ert-deftest orgx-format-description-list-test ()
  "Description lists preserve term syntax and wrap definition text."
  (with-temp-buffer
    (org-mode)
    (insert "* Description Section\n- Term :: Description of term with long description text that should wrap cleanly.\n")
    (let ((orgx-format-line-length 40))
      (orgx-format-buffer))
    (let ((content (buffer-string)))
      (should (string-match-p "- Term :: Description of term with long\n  description text that should wrap\n  cleanly\\." content)))))

(ert-deftest orgx-format-no-regexp-reformatting-rules-test ()
  "Assert that orgx-format.el contains no regexp-based reformatting rules."
  (let* ((format-el (or (locate-library "orgx-format.el")
                        (locate-library "orgx-format")
                        (expand-file-name "lisp/orgx-format.el" default-directory)))
         (forms nil)
         (forbidden '(re-search-forward
                      re-search-backward
                      looking-at
                      looking-at-p
                      string-match
                      string-match-p
                      replace-regexp-in-string)))
    (should (and format-el (file-exists-p format-el)))
    (with-temp-buffer
      (insert-file-contents format-el)
      (goto-char (point-min))
      (while (ignore-errors
               (let ((form (read (current-buffer))))
                 (push form forms)
                 t))))
    (let ((reformat-fns '(orgx-format--reflow-paragraphs
                          orgx-format--realign-tables
                          orgx-format--trim-section-blank-lines
                          orgx-format-region
                          orgx-format-buffer
                          orgx-format-section-at-point))
          (contains-forbidden
           (lambda (tree)
             (let (found)
               (letrec ((walk (lambda (x)
                                (cond
                                 ((memq x forbidden)
                                  (push x found))
                                 ((consp x)
                                  (funcall walk (car x))
                                  (funcall walk (cdr x)))))))
                 (funcall walk tree))
               found))))
      (dolist (form forms)
        (when (and (consp form)
                   (memq (car form) '(defun defsubst cl-defun))
                   (memq (cadr form) reformat-fns))
          (let ((bad-calls (funcall contains-forbidden (cddr form))))
            (should-not bad-calls)))))))

(provide 'orgx-format-test)
;;; orgx-format-test.el ends here
