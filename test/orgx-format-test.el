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

(provide 'orgx-format-test)
;;; orgx-format-test.el ends here
