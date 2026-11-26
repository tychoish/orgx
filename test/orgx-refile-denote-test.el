;;; orgx-refile-denote-test.el --- Tests for orgx-refile-denote -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-refile)
(require 'orgx-refile-denote)

(defvar src)
(defvar dst)
(defvar denote-note)

(defmacro orgx-refile-denote-test--with-env (&rest body)
  "Run BODY with `src', `dst' org files, a `denote-note', and advice on."
  (declare (indent 0))
  `(let* ((src (make-temp-file "orgx-rd-src-" nil ".org" "* Inbox\n** TODO write docs\nbody\n** other\n"))
          (dst (make-temp-file "orgx-rd-dst-" nil ".org" "* Dest\n"))
          (denote-dir (make-temp-file "orgx-rd-denote-" t))
          (denote-note (expand-file-name "20261126T120000--sample-note__project.org" denote-dir))
          (denote-done (expand-file-name "20261126T120001--done-note__project_done.org" denote-dir))
          (org-refile-targets `((,dst :maxlevel . 1)))
          (org-refile-use-outline-path 'file)
          (org-outline-path-complete-in-steps nil)
          (org-refile-use-cache nil)
          (org-refile-history nil)
          (orgx-refile-denote-target-mode 'both)
          (orgx-refile-denote-prompt-heading t))
     (with-temp-file denote-note
       (insert "#+title: Sample Note\n\n* Tasks\n** Existing task\n* Notes\n"))
     (with-temp-file denote-done
       (insert "#+title: Done Note\n\n* Done section\n"))
     (unwind-protect
         (cl-letf (((symbol-function 'denote-directories) (lambda () (list denote-dir)))
                   ((symbol-function 'denote-directory-files)
                    (lambda (&rest _) (list denote-note denote-done)))
                   ((symbol-function 'denote-retrieve-filename-keywords-as-list)
                    (lambda (f)
                      (cond
                       ((string-match-p "done" f) '("project" "done"))
                       (t '("project")))))
                   ((symbol-function 'denote-get-file-name-relative-to-denote-directory)
                    (lambda (f) (file-name-nondirectory f))))
           (orgx-refile-denote-enable)
           (unwind-protect (progn ,@body)
             (orgx-refile-denote-disable)))
       (dolist (f (list src dst denote-note denote-done))
         (when-let* ((b (get-file-buffer f)))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b))
         (when (file-exists-p f)
           (delete-file f)))
       (when (file-directory-p denote-dir)
         (delete-directory denote-dir t)))))

(defun orgx-refile-denote-test--chooser (pattern)
  "Return a `completing-read' stub choosing the first candidate matching PATTERN."
  (lambda (_prompt coll &rest _)
    (if (functionp coll)
        (funcall coll "" nil t)
      (let ((choice (seq-find (lambda (c)
                                (let ((str (if (consp c) (car c) c)))
                                  (string-match-p pattern str)))
                              coll)))
        (if (consp choice) (car choice) choice)))))

(defun orgx-refile-denote-test--refile-heading (target-pattern &optional heading-pattern arg)
  "Visit `src', go to TODO heading, and `org-refile'."
  (with-current-buffer (find-file-noselect src)
    (org-mode)
    (goto-char (point-min))
    (re-search-forward "TODO write docs")
    (let ((step 0))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt coll &rest rest)
                   (prog1
                       (cond
                        ((string-match-p "Heading in" prompt)
                         (if heading-pattern
                             (funcall (orgx-refile-denote-test--chooser heading-pattern) prompt coll rest)
                           "(top level)"))
                        (t
                         (funcall (orgx-refile-denote-test--chooser target-pattern) prompt coll rest)))
                     (setq step (1+ step))))))
        (org-refile arg)))))

(defun orgx-refile-denote-test--file (f)
  "Return contents of F."
  (if-let* ((b (get-file-buffer f)))
      (with-current-buffer b (buffer-string))
    (with-temp-buffer (insert-file-contents f) (buffer-string))))

(ert-deftest orgx-refile-denote-entry-alist-test ()
  (orgx-refile-denote-test--with-env
    (let ((entries (orgx-refile-denote--entry-alist)))
      (should (member "denote: (choose note)" (mapcar #'car entries)))
      (should (member "denote: (new note)" (mapcar #'car entries)))
      (should (member "denote: 20261126T120000--sample-note__project.org" (mapcar #'car entries)))
      ;; Done note should be excluded
      (should-not (member "denote: 20261126T120001--done-note__project_done.org" (mapcar #'car entries))))))

(ert-deftest orgx-refile-denote-entry-alist-prompt-only-test ()
  (orgx-refile-denote-test--with-env
    (let ((orgx-refile-denote-target-mode 'prompt-only))
      (should (equal (mapcar #'car (orgx-refile-denote--entry-alist))
                     '("denote: (choose note)" "denote: (new note)"))))))

(ert-deftest orgx-refile-denote-entry-alist-notes-only-test ()
  (orgx-refile-denote-test--with-env
    (let ((orgx-refile-denote-target-mode 'notes-only))
      (let ((cars (mapcar #'car (orgx-refile-denote--entry-alist))))
        (should-not (member "denote: (choose note)" cars))
        (should (member "denote: (new note)" cars))
        (should (member "denote: 20261126T120000--sample-note__project.org" cars))))))

(ert-deftest orgx-refile-denote-targets-only-while-refiling-test ()
  (orgx-refile-denote-test--with-env
    (let ((names (mapcar #'car (org-refile-get-targets))))
      (should-not (seq-some (lambda (n) (string-prefix-p "denote:" n)) names))
      (should (seq-some (lambda (n) (string-match-p "Dest" n)) names)))))

(ert-deftest orgx-refile-denote-targets-offered-when-active-test ()
  (orgx-refile-denote-test--with-env
    (let* ((orgx-refile-denote--active t)
           (names (mapcar #'car (org-refile-get-targets))))
      (should (member "denote: (choose note)" names))
      (should (member "denote: (new note)" names))
      (should (seq-some (lambda (n) (string-match-p "sample-note" n)) names)))))

(ert-deftest orgx-refile-denote-lookup-test ()
  (let ((orgx-refile-denote--entries '(("denote: sample" . "/path/to/note.org"))))
    (should (orgx-refile-denote--lookup "denote: sample"))
    (should (orgx-refile-denote--lookup "denote: sample/"))
    (should (orgx-refile-denote--lookup "denote: sample/ (denote)"))
    (should-not (orgx-refile-denote--lookup "other"))))

(ert-deftest orgx-refile-denote-refile-to-heading-test ()
  "Refiling to a specific heading places subtree as child and removes from src."
  (orgx-refile-denote-test--with-env
    (orgx-refile-denote-test--refile-heading "sample-note" "Tasks")
    (let ((dest-content (orgx-refile-denote-test--file denote-note))
          (src-content (orgx-refile-denote-test--file src)))
      (should (string-match-p "\\* Tasks" dest-content))
      (should (string-match-p "\\*\\* TODO write docs" dest-content))
      (should-not (string-match-p "TODO write docs" src-content)))))

(ert-deftest orgx-refile-denote-refile-to-top-level-test ()
  "Refiling with (top level) places subtree at top level."
  (orgx-refile-denote-test--with-env
    (orgx-refile-denote-test--refile-heading "sample-note" "(top level)")
    (let ((dest-content (orgx-refile-denote-test--file denote-note))
          (src-content (orgx-refile-denote-test--file src)))
      (should (string-match-p "\\* TODO write docs" dest-content))
      (should-not (string-match-p "TODO write docs" src-content)))))

(ert-deftest orgx-refile-denote-refile-via-choose-note-test ()
  "Refiling via `(choose note)' uses `denote-file-prompt'."
  (orgx-refile-denote-test--with-env
    (cl-letf (((symbol-function 'denote-file-prompt)
               (lambda (&rest _) denote-note)))
      (orgx-refile-denote-test--refile-heading "choose note" "Notes")
      (let ((dest-content (orgx-refile-denote-test--file denote-note)))
        (should (string-match-p "\\* Notes\n\\*\\* TODO write docs" dest-content))))))

(ert-deftest orgx-refile-denote-refile-to-new-note-test ()
  "Refiling with (new note) calls `denote-convert-subtree' or `denote-org-extract-org-subtree'."
  (orgx-refile-denote-test--with-env
    (let (new-note-called)
      (cl-letf (((symbol-function 'denote-convert-subtree)
                 (lambda () (setq new-note-called t))))
        (orgx-refile-denote-test--refile-heading "new note")
        (should new-note-called)))))

(ert-deftest orgx-refile-denote-standalone-command-test ()
  "Calling `orgx-denote-refile' directly refiles to chosen note."
  (orgx-refile-denote-test--with-env
    (cl-letf (((symbol-function 'denote-file-prompt)
               (lambda (&rest _) denote-note))
              ((symbol-function 'completing-read)
               (lambda (_p _coll &rest _) "Tasks")))
      (with-current-buffer (find-file-noselect src)
        (org-mode)
        (goto-char (point-min))
        (re-search-forward "TODO write docs")
        (orgx-denote-refile))
      (let ((dest-content (orgx-refile-denote-test--file denote-note)))
        (should (string-match-p "\\* Tasks" dest-content))
        (should (string-match-p "\\*\\* TODO write docs" dest-content))))))

(ert-deftest orgx-refile-denote-copy-test ()
  "Refiling with copy argument leaves original in src."
  (orgx-refile-denote-test--with-env
    (orgx-refile-denote-test--refile-heading "sample-note" "Tasks" 3)
    (let ((dest-content (orgx-refile-denote-test--file denote-note))
          (src-content (orgx-refile-denote-test--file src)))
      (should (string-match-p "\\* Tasks" dest-content))
      (should (string-match-p "\\*\\* TODO write docs" dest-content))
      (should (string-match-p "TODO write docs" src-content)))))

(provide 'orgx-refile-denote-test)
;;; orgx-refile-denote-test.el ends here
