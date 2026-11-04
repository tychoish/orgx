;;; orgx-refile-agent-test.el --- Tests for orgx-refile-agent -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-refile)
(require 'orgx-refile-agent)

(defvar src)
(defvar dst)

(defmacro orgx-refile-agent-test--with-env (&rest body)
  "Run BODY with `src', `dst' org files, agent buffers `shell-a'/`shell-b', and advice on.
`opened' collects (TARGET-BUFFER ORIGIN-BUFFER TEXT) from stubbed capture opens."
  (declare (indent 0))
  `(let* ((src (make-temp-file "orgx-ra-src-" nil ".org" "* Inbox\n** TODO write docs\nbody\n** other\n"))
          (dst (make-temp-file "orgx-ra-dst-" nil ".org" "* Dest\n"))
          (shell-a (generate-new-buffer "*agent-shell-a*"))
          (shell-b (generate-new-buffer "*agent-shell-b*"))
          (opened nil)
          (org-refile-targets `((,dst :maxlevel . 1)))
          (org-refile-use-outline-path 'file)
          (org-outline-path-complete-in-steps nil)
          (org-refile-use-cache nil)
          (org-refile-history nil)
          (orgx-refile-agent-original-action 'keep))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list shell-a shell-b)))
                   ((symbol-function 'agent-shell-queue--open-capture)
                    (lambda (target origin &optional text &rest _)
                      (push (list target origin text) opened))))
           (orgx-refile-agent-enable)
           (unwind-protect (progn ,@body)
             (orgx-refile-agent-disable)))
       (dolist (f (list src dst))
         (when-let* ((b (get-file-buffer f)))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b))
         (delete-file f))
       (kill-buffer shell-a)
       (kill-buffer shell-b))))

(defun orgx-refile-agent-test--chooser (pattern)
  "Return a `completing-read' stub choosing the first candidate matching PATTERN."
  (lambda (_prompt coll &rest _)
    (car (seq-find (lambda (c) (string-match-p pattern (car c))) coll))))

(defun orgx-refile-agent-test--refile-heading (pattern &optional arg)
  "Visit `src', go to the TODO heading, and `org-refile' choosing PATTERN."
  (with-current-buffer (find-file-noselect src)
    (org-mode)
    (goto-char (point-min))
    (re-search-forward "TODO write docs")
    (cl-letf (((symbol-function 'completing-read)
               (orgx-refile-agent-test--chooser pattern)))
      (org-refile arg))))

(defun orgx-refile-agent-test--file (f)
  "Return the contents of F, from its buffer when visited."
  (if-let* ((b (get-file-buffer f)))
      (with-current-buffer b (buffer-string))
    (with-temp-buffer (insert-file-contents f) (buffer-string))))

(ert-deftest orgx-refile-agent-entry-alist-test ()
  (orgx-refile-agent-test--with-env
    (let ((entries (orgx-refile-agent--entry-alist)))
      (should (equal (mapcar #'car entries)
                     '("agent-shell: (unassigned queue)"
                       "agent-shell: *agent-shell-a*"
                       "agent-shell: *agent-shell-b*")))
      (should-not (cdr (car entries)))
      (should (eq (cdr (nth 1 entries)) shell-a)))))

(ert-deftest orgx-refile-agent-entry-alist-without-agent-shell-test ()
  (cl-letf (((symbol-function 'agent-shell-buffers) nil))
    (fmakunbound 'agent-shell-buffers)
    (should (equal (mapcar #'car (orgx-refile-agent--entry-alist))
                   '("agent-shell: (unassigned queue)")))))

(ert-deftest orgx-refile-agent-targets-only-while-refiling-test ()
  "Outside `org-refile' (e.g. bulk refile) the targets are unchanged."
  (orgx-refile-agent-test--with-env
    (let ((names (mapcar #'car (org-refile-get-targets))))
      (should-not (seq-some (lambda (n) (string-prefix-p "agent-shell:" n)) names))
      (should (seq-some (lambda (n) (string-match-p "Dest" n)) names)))))

(ert-deftest orgx-refile-agent-targets-offered-when-active-test ()
  (orgx-refile-agent-test--with-env
    (let* ((orgx-refile-agent--active t)
           (names (mapcar #'car (org-refile-get-targets))))
      (should (member "agent-shell: *agent-shell-a*" names))
      (should (member "agent-shell: (unassigned queue)" names))
      (should (seq-some (lambda (n) (string-match-p "Dest" n)) names)))))

(ert-deftest orgx-refile-agent-lookup-strips-suffixes-test ()
  (let ((orgx-refile-agent--entries '(("agent-shell: x" . nil))))
    (should (orgx-refile-agent--lookup "agent-shell: x"))
    (should (orgx-refile-agent--lookup "agent-shell: x/"))
    (should (orgx-refile-agent--lookup "agent-shell: x/ (agent-shell)"))
    (should-not (orgx-refile-agent--lookup "other"))))

(ert-deftest orgx-refile-agent-refile-to-session-test ()
  "Choosing a session opens a seeded capture buffer and moves nothing."
  (orgx-refile-agent-test--with-env
    (orgx-refile-agent-test--refile-heading "agent-shell: \\*agent-shell-b\\*")
    (should (= 1 (length opened)))
    (pcase-let ((`(,target ,origin ,text) (car opened)))
      (should (eq target shell-b))
      (should (equal (buffer-file-name origin) src))
      (should (equal text "** TODO write docs\nbody")))
    (should (equal (orgx-refile-agent-test--file dst) "* Dest\n"))
    (should (string-match-p "TODO write docs" (orgx-refile-agent-test--file src)))))

(ert-deftest orgx-refile-agent-refile-to-unassigned-test ()
  (orgx-refile-agent-test--with-env
    (orgx-refile-agent-test--refile-heading "unassigned queue")
    (should (equal (mapcar #'car opened) '(nil)))))

(ert-deftest orgx-refile-agent-normal-refile-unaffected-test ()
  (orgx-refile-agent-test--with-env
    (orgx-refile-agent-test--refile-heading "Dest")
    (should-not opened)
    (should (string-match-p "\\*\\* TODO write docs" (orgx-refile-agent-test--file dst)))
    (should-not (string-match-p "write docs" (orgx-refile-agent-test--file src)))))

(ert-deftest orgx-refile-agent-jump-prefix-does-not-offer-agents-test ()
  "With a jump prefix, agent targets are not in the list."
  (orgx-refile-agent-test--with-env
    (let (seen)
      (with-current-buffer (find-file-noselect src)
        (org-mode)
        (goto-char (point-min))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p coll &rest _)
                     (setq seen (mapcar #'car coll))
                     (car (seq-find (lambda (c) (string-match-p "Dest" (car c))) coll)))))
          (ignore-errors (org-refile '(4)))))
      (should seen)
      (should-not (seq-some (lambda (n) (string-prefix-p "agent-shell:" n)) seen)))))

(ert-deftest orgx-refile-agent-explicit-rfloc-skips-agents-test ()
  (let (active)
    (cl-letf (((symbol-function 'org-refile)
               (lambda (&rest _) (setq active orgx-refile-agent--active))))
      (orgx-refile-agent--ad-refile (lambda (&rest _) (setq active orgx-refile-agent--active))
                                    nil nil '("x" "f" nil 1))
      (should-not active)
      (orgx-refile-agent--ad-refile (lambda (&rest _) (setq active orgx-refile-agent--active))
                                    nil nil nil)
      (should active))))

(ert-deftest orgx-refile-agent-action-remove-test ()
  (orgx-refile-agent-test--with-env
    (let ((orgx-refile-agent-original-action 'remove))
      (orgx-refile-agent-test--refile-heading "agent-shell: \\*agent-shell-a\\*")
      (let ((content (orgx-refile-agent-test--file src)))
        (with-current-buffer (get-file-buffer src)
          (save-buffer)
          (setq content (buffer-string)))
        (should-not (string-match-p "write docs" content))
        (should (string-match-p "\\*\\* other" content)))
      (should (string-match-p "write docs" (car kill-ring)))
      (should (= 1 (length opened))))))

(ert-deftest orgx-refile-agent-action-mark-test ()
  (orgx-refile-agent-test--with-env
    (let ((orgx-refile-agent-original-action 'mark)
          (org-todo-keywords '((sequence "TODO" "INPROGRESS" "|" "DONE"))))
      (orgx-refile-agent-test--refile-heading "agent-shell: \\*agent-shell-a\\*")
      (with-current-buffer (get-file-buffer src)
        (should (string-match-p "\\*\\* INPROGRESS write docs" (buffer-string)))))))

(ert-deftest orgx-refile-agent-action-mark-unknown-keyword-noop-test ()
  (orgx-refile-agent-test--with-env
    (let ((orgx-refile-agent-original-action 'mark)
          (orgx-refile-agent-mark-state "NOPE"))
      (orgx-refile-agent-test--refile-heading "agent-shell: \\*agent-shell-a\\*")
      (with-current-buffer (get-file-buffer src)
        (should (string-match-p "\\*\\* TODO write docs" (buffer-string)))))))

(ert-deftest orgx-refile-agent-action-ask-prompts-test ()
  (orgx-refile-agent-test--with-env
    (let ((orgx-refile-agent-original-action 'ask)
          prompts)
      (with-current-buffer (find-file-noselect src)
        (org-mode)
        (goto-char (point-min))
        (re-search-forward "TODO write docs")
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt coll &rest _)
                     (push prompt prompts)
                     (if (equal prompt "Original heading: ")
                         "remove"
                       (car (seq-find (lambda (c) (string-match-p "agent-shell-a" (car c))) coll))))))
          (org-refile)))
      (should (member "Original heading: " prompts))
      (with-current-buffer (get-file-buffer src)
        (should-not (string-match-p "write docs" (buffer-string)))))))

(ert-deftest orgx-refile-agent-action-keep-leaves-heading-test ()
  (orgx-refile-agent-test--with-env
    (orgx-refile-agent-test--refile-heading "agent-shell: \\*agent-shell-a\\*")
    (with-current-buffer (get-file-buffer src)
      (should (string-match-p "\\*\\* TODO write docs" (buffer-string))))))

(ert-deftest orgx-refile-agent-send-requires-queue-test ()
  (with-temp-buffer
    (org-mode)
    (insert "* x\n")
    (cl-letf (((symbol-function 'agent-shell-queue--open-capture) nil))
      (fmakunbound 'agent-shell-queue--open-capture)
      (should-error (orgx-refile-agent-send nil) :type 'user-error))))

(ert-deftest orgx-refile-agent-enable-disable-test ()
  (orgx-refile-agent-disable)
  (should-not (advice-member-p #'orgx-refile-agent--ad-refile 'org-refile))
  (orgx-refile-agent-enable)
  (unwind-protect
      (progn
        (should (advice-member-p #'orgx-refile-agent--ad-refile 'org-refile))
        (should (advice-member-p #'orgx-refile-agent--ad-agenda-refile 'org-agenda-refile))
        (should (advice-member-p #'orgx-refile-agent--ad-get-targets 'org-refile-get-targets))
        (should (advice-member-p #'orgx-refile-agent--ad-get-location 'org-refile-get-location)))
    (orgx-refile-agent-disable))
  (should-not (advice-member-p #'orgx-refile-agent--ad-get-location 'org-refile-get-location)))

(ert-deftest orgx-refile-agent-history-not-polluted-test ()
  "An agent choice must not become the default for the next refile."
  (orgx-refile-agent-test--with-env
    (setq org-refile-history (list "previous"))
    (orgx-refile-agent-test--refile-heading "agent-shell: \\*agent-shell-a\\*")
    (should (equal org-refile-history '("previous")))
    (should-not orgx-refile-agent--active)))

(ert-deftest orgx-refile-agent-normal-refile-after-agent-test ()
  "No state leaks after an agent refile: a normal refile still works."
  (orgx-refile-agent-test--with-env
    (orgx-refile-agent-test--refile-heading "agent-shell: \\*agent-shell-a\\*")
    (should-not orgx-refile-agent--active)
    (orgx-refile-agent-test--refile-heading "Dest")
    (should (= 1 (length opened)))
    (should (string-match-p "\\*\\* TODO write docs" (orgx-refile-agent-test--file dst)))))

(ert-deftest orgx-refile-agent-copy-prefix-keeps-original-test ()
  "With prefix 3 (copy) the `remove' action is overridden."
  (orgx-refile-agent-test--with-env
    (let ((orgx-refile-agent-original-action 'remove))
      (orgx-refile-agent-test--refile-heading "agent-shell: \\*agent-shell-a\\*" 3)
      (should (= 1 (length opened)))
      (should (string-match-p "write docs" (orgx-refile-agent-test--file src))))))

(ert-deftest orgx-refile-agent-keep-binding-skips-prompt-test ()
  (let ((orgx-refile-agent-original-action 'ask)
        (org-refile-keep t))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (error "must not prompt"))))
      (should (eq (orgx-refile-agent--read-action) 'keep)))))

(ert-deftest orgx-refile-agent-region-does-not-offer-agents-test ()
  (orgx-refile-agent-test--with-env
    (let (seen)
      (with-current-buffer (find-file-noselect src)
        (org-mode)
        (goto-char (point-min))
        (re-search-forward "^\\*\\* TODO write docs")
        (beginning-of-line)
        (push-mark (save-excursion (org-end-of-subtree t t) (point)) t t)
        (let ((transient-mark-mode t))
          (setq mark-active t)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_p coll &rest _)
                       (setq seen (mapcar #'car coll))
                       (car (seq-find (lambda (c) (string-match-p "Dest" (car c))) coll)))))
            (org-refile))))
      (should seen)
      (should-not (seq-some (lambda (n) (string-prefix-p "agent-shell:" n)) seen))
      (should-not opened))))

(ert-deftest orgx-refile-agent-cache-not-polluted-test ()
  (orgx-refile-agent-test--with-env
    (let ((org-refile-use-cache t))
      (org-refile-cache-clear)
      (let ((orgx-refile-agent--active t))
        (should (seq-some (lambda (n) (string-prefix-p "agent-shell:" (car n)))
                          (org-refile-get-targets))))
      (should-not (seq-some (lambda (n) (string-prefix-p "agent-shell:" (car n)))
                            (org-refile-get-targets)))
      (org-refile-cache-clear))))

(ert-deftest orgx-refile-agent-refile-without-outline-path-test ()
  "Names get a \" (agent-shell)\" suffix when outline paths are off."
  (orgx-refile-agent-test--with-env
    (let ((org-refile-use-outline-path nil))
      (orgx-refile-agent-test--refile-heading "agent-shell: \\*agent-shell-b\\*")
      (should (= 1 (length opened)))
      (should (eq (car (car opened)) shell-b)))))

(ert-deftest orgx-refile-agent-agenda-refile-test ()
  "`org-agenda-refile' offers agent targets and sends the marked heading."
  (orgx-refile-agent-test--with-env
    (let ((redone nil)
          (src-buf (find-file-noselect src)))
      (with-current-buffer src-buf
        (org-mode)
        (goto-char (point-min))
        (re-search-forward "^\\*\\* TODO write docs")
        (beginning-of-line))
      (let ((marker (with-current-buffer src-buf (point-marker))))
        (with-temp-buffer
          (insert (propertize "TODO write docs\n" 'org-hd-marker marker))
          (goto-char (point-min))
          (cl-letf (((symbol-function 'completing-read)
                     (orgx-refile-agent-test--chooser "agent-shell: \\*agent-shell-a\\*"))
                    ((symbol-function 'org-agenda-redo)
                     (lambda (&rest _) (setq redone t))))
            (org-agenda-refile))))
      (should redone)
      (should (= 1 (length opened)))
      (pcase-let ((`(,target ,origin ,text) (car opened)))
        (should (eq target shell-a))
        (should (eq origin src-buf))
        (should (equal text "** TODO write docs\nbody")))
      (should (string-match-p "write docs" (orgx-refile-agent-test--file src))))))

(ert-deftest orgx-refile-agent-agenda-refile-no-update-test ()
  (orgx-refile-agent-test--with-env
    (let ((redone nil)
          (src-buf (find-file-noselect src)))
      (with-current-buffer src-buf
        (org-mode)
        (goto-char (point-min))
        (re-search-forward "^\\*\\* TODO write docs")
        (beginning-of-line))
      (let ((marker (with-current-buffer src-buf (point-marker))))
        (with-temp-buffer
          (insert (propertize "x\n" 'org-hd-marker marker))
          (goto-char (point-min))
          (cl-letf (((symbol-function 'completing-read)
                     (orgx-refile-agent-test--chooser "unassigned queue"))
                    ((symbol-function 'org-agenda-redo)
                     (lambda (&rest _) (setq redone t))))
            (org-agenda-refile nil nil t))))
      (should-not redone)
      (should (= 1 (length opened))))))

(provide 'orgx-refile-agent-test)
;;; orgx-refile-agent-test.el ends here
