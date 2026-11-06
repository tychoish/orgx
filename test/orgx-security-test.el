;;; orgx-security-test.el --- Security regression tests for orgx MCP surface -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for the prompt-injection review of the plan_* / bulk_* /
;; org_capture_* MCP tools.  Each test names the finding it covers.  The
;; threat model: an external agent calls the tools; the user has approved
;; nothing unless `hitl-plan-agent-may-apply' is set; plans created over MCP
;; carry `:origin mcp' and are restricted.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'mcpkit)
(require 'orgx-bulk)
(require 'orgx-bulk-ops)
(require 'orgx-bulk-agenda)
(require 'orgx-bulk-buffer)
(require 'orgx-mcp)

(defvar denote-directory)
(defvar orgx-sec-test--flag nil "Set by injected code if it ever runs.")

(defvar orgx-sec-test--root nil)
(defvar orgx-sec-test--allowed nil "Allowed org file (in `org-agenda-files').")
(defvar orgx-sec-test--outside nil "Org file that is not allowed.")
(defvar orgx-sec-test--victim nil "Non-org file an attacker wants to modify.")

(defun orgx-sec-test--read (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defmacro orgx-sec-test--with-env (&rest body)
  "Run BODY with an allowed Org file and files outside the allow-list."
  (declare (indent 0))
  `(let* ((orgx-sec-test--root (make-temp-file "orgx-sec-" t))
          (orgx-sec-test--flag nil)
          (orgx-sec-test--allowed (expand-file-name "allowed.org" orgx-sec-test--root))
          (orgx-sec-test--outside (expand-file-name "outside.org" orgx-sec-test--root))
          (orgx-sec-test--victim (expand-file-name "victim.sh" orgx-sec-test--root))
          (orgx-bulk-directory (expand-file-name "plans" orgx-sec-test--root))
          (org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (hitl-plan--store (make-hash-table :test #'equal))
          (hitl-plan-agent-may-apply nil)
          (orgx-bulk-mcp-allowed-files nil)
          (orgx-bulk-log-notes 'ignore)
          (org-agenda-files nil)
          (before (buffer-list)))
     (write-region "* TODO alpha\n* TODO beta\n* DONE gamma\n" nil orgx-sec-test--allowed)
     (write-region "* TODO secret\n" nil orgx-sec-test--outside)
     (write-region "echo victim\n" nil orgx-sec-test--victim)
     (setq org-agenda-files (list orgx-sec-test--allowed))
     (unwind-protect
         (cl-letf (((symbol-function 'display-buffer) #'ignore))
           ,@body)
       (dolist (b (buffer-list))
         (unless (memq b before)
           (with-current-buffer b (set-buffer-modified-p nil))
           (let ((kill-buffer-query-functions nil)) (kill-buffer b))))
       (delete-directory orgx-sec-test--root t))))

(defun orgx-sec-test--create (targets ops &rest extra)
  "Create an MCP plan over TARGETS and OPS (plists); return the result plist."
  (apply #'hitl-plan-mcp-create :domain "org-agenda" :targets targets :ops ops
         :show :json-false extra))

(defun orgx-sec-test--target (file heading &optional pos)
  (list :file file :heading heading :pos (or pos 1)))

(defun orgx-sec-test--plan (res)
  (hitl-plan-get (plist-get res :plan_id)))

(defun orgx-sec-test--run (res)
  "Execute the plan in creation result RES; return its steps' (status . message)."
  (let ((plan (orgx-sec-test--plan res)))
    (orgx-bulk-execute plan)
    (mapcar (lambda (s) (cons (hitl-plan-step-status s) (hitl-plan-step-message s)))
            (hitl-plan-steps plan))))

(defun orgx-sec-test--call (tool args)
  (let ((outcome nil))
    (funcall (mcpkit-tool-handler (map-elt (mcpkit-service-tools orgx-mcp-service) tool))
             args (lambda (err res) (setq outcome (cons err res))))
    (when (car outcome) (error "%s" (car outcome)))
    (cdr outcome)))

;;; Finding 1: operation argument validation

(ert-deftest orgx-sec/f1-tag-injection-rejected ()
  (orgx-sec-test--with-env
    (dolist (tag '("ok\n# Local Variables:\n# eval: (setq orgx-sec-test--flag t)\n# End:"
                   "a b" "a\tb" "a:b" "" " " "ok\n" 7))
      (should (orgx-bulk-ops--validate-tag tag))
      (dolist (op '("tag-add" "tag-remove"))
        (let ((res (orgx-sec-test--create
                    (list (orgx-sec-test--target orgx-sec-test--allowed "alpha"))
                    (list (list :op op :args (list :tag tag))))))
          (should (stringp (plist-get res :error))))))
    (should-not (orgx-bulk-ops--validate-tag "work_1"))
    (let ((res (orgx-sec-test--create
                (list (orgx-sec-test--target orgx-sec-test--allowed "alpha"))
                '((:op "tag-add" :args (:tag "work"))))))
      (should-not (plist-get res :error))
      (should (equal '((done)) (mapcar (lambda (c) (list (car c))) (orgx-sec-test--run res))))
      (should (string-match-p "alpha.*:work:" (orgx-sec-test--read orgx-sec-test--allowed))))
    (should-not orgx-sec-test--flag)))

(ert-deftest orgx-sec/f1-property-syntax-and-denylist ()
  (orgx-sec-test--with-env
    (should (orgx-bulk-ops--validate-property "a b" "v"))
    (should (orgx-bulk-ops--validate-property "a:b" "v"))
    (should (orgx-bulk-ops--validate-property "X" "1\ntouch /tmp/PWNED"))
    (should (orgx-bulk-ops--validate-property "X" "1\r2"))
    (should-not (orgx-bulk-ops--validate-property "CATEGORY" "ok"))
    (should-not (orgx-bulk-ops--validate-property "NOTE" "fine"))
    ;; MCP-origin: denied names, any case, with the `+' suffix.
    (dolist (name '("ARCHIVE" "archive" "Archive+" "CATEGORY" "ID" "id" "TODO" "PRIORITY"
                    "SCHEDULED" "DEADLINE" "TAGS" "ALLTAGS" "ITEM" "FILE" "EXPORT_FILE_NAME"
                    "header-args" "header-args:sh" "COLUMNS" "LOGGING" "ARCHIVE_FILE"))
      (let ((res (orgx-sec-test--create
                  (list (orgx-sec-test--target orgx-sec-test--allowed "alpha"))
                  (list (list :op "property-set" :args (list :property name :value "x"))))))
        (should (stringp (plist-get res :error))))
      (let ((hitl-plan-restricted t))
        (should (orgx-bulk-ops--validate-property name "x"))))
    (let ((res (orgx-sec-test--create
                (list (orgx-sec-test--target orgx-sec-test--allowed "alpha"))
                '((:op "property-set" :args (:property "OWNER" :value "me"))))))
      (should-not (plist-get res :error))
      (orgx-sec-test--run res)
      (should (string-match-p ":OWNER: +me" (orgx-sec-test--read orgx-sec-test--allowed))))))

(ert-deftest orgx-sec/f1-interactive-property-not-restricted ()
  "A plan the user made (no mcp origin) may set ARCHIVE and CATEGORY."
  (orgx-sec-test--with-env
    (let ((plan (hitl-plan-create
                 :domain 'org-agenda
                 :targets (list (orgx-sec-test--target orgx-sec-test--allowed "alpha"))
                 :ops (list (orgx-bulk-op--create :name 'property-set
                                                  :args '("CATEGORY" "mine"))))))
      (orgx-bulk-execute plan)
      (should (eq 'done (hitl-plan-step-status (car (hitl-plan-steps plan)))))
      (should (string-match-p ":CATEGORY: +mine" (orgx-sec-test--read orgx-sec-test--allowed))))))

(ert-deftest orgx-sec/f1-file-allowed-predicate ()
  (orgx-sec-test--with-env
    (should (orgx-bulk-file-allowed-p orgx-sec-test--allowed))
    (should-not (orgx-bulk-file-allowed-p orgx-sec-test--outside))
    (should-not (orgx-bulk-file-allowed-p orgx-sec-test--victim))
    (should-not (orgx-bulk-file-allowed-p "/etc/passwd"))
    (should-not (orgx-bulk-file-allowed-p "/nonexistent/x.org"))
    (should-not (orgx-bulk-file-allowed-p "/ssh:nobody@host.invalid:/x.org"))
    (should-not (orgx-bulk-file-allowed-p "/-:host:/x.org"))
    (should-not (orgx-bulk-file-allowed-p nil))
    (should-not (orgx-bulk-file-allowed-p 7))
    (should-not (orgx-bulk-file-allowed-p "x\n.org"))
    ;; The allow-list: files and directories.
    (let ((orgx-bulk-mcp-allowed-files (list orgx-sec-test--outside)))
      (should (orgx-bulk-file-allowed-p orgx-sec-test--outside)))
    (let ((orgx-bulk-mcp-allowed-files (list orgx-sec-test--root)))
      (should (orgx-bulk-file-allowed-p orgx-sec-test--outside))
      ;; ... but only Org files.
      (should-not (orgx-bulk-file-allowed-p orgx-sec-test--victim)))
    ;; denote-directory
    (let ((denote-directory (file-name-as-directory orgx-sec-test--root)))
      (should (orgx-bulk-file-allowed-p orgx-sec-test--outside)))
    ;; A symlink inside an allowed directory cannot reach a disallowed file.
    (let ((link (expand-file-name "link.org" orgx-sec-test--root))
          (orgx-bulk-mcp-allowed-files nil))
      (make-symbolic-link orgx-sec-test--outside link)
      (setq org-agenda-files (list link))
      (should (orgx-bulk-file-allowed-p link))
      (setq org-agenda-files (list orgx-sec-test--allowed))
      (let ((orgx-bulk-mcp-allowed-files (list (expand-file-name "sub/" orgx-sec-test--root))))
        (make-directory (expand-file-name "sub" orgx-sec-test--root))
        (make-symbolic-link orgx-sec-test--outside
                            (expand-file-name "sub/l.org" orgx-sec-test--root))
        (should-not (orgx-bulk-file-allowed-p (expand-file-name "sub/l.org" orgx-sec-test--root)))))))

(ert-deftest orgx-sec/f1-refile-destination-restricted ()
  (orgx-sec-test--with-env
    (dolist (dest (list orgx-sec-test--victim orgx-sec-test--outside "/etc/passwd"
                        "/nonexistent/x.org" "/ssh:u@h.invalid:/x.org"))
      (let ((res (orgx-sec-test--create
                  (list (orgx-sec-test--target orgx-sec-test--allowed "alpha"))
                  (list (list :op "refile" :args (list :file dest))))))
        (should (stringp (plist-get res :error)))))
    (should (equal "echo victim\n" (orgx-sec-test--read orgx-sec-test--victim)))
    ;; An allowed destination works.
    (let ((orgx-bulk-mcp-allowed-files (list orgx-sec-test--outside)))
      (let ((res (orgx-sec-test--create
                  (list (orgx-sec-test--target orgx-sec-test--allowed "alpha"))
                  (list (list :op "refile" :args (list :file orgx-sec-test--outside))))))
        (should-not (plist-get res :error))
        (should (eq 'done (car (car (orgx-sec-test--run res)))))
        (should (string-match-p "alpha" (orgx-sec-test--read orgx-sec-test--outside)))))))

(ert-deftest orgx-sec/f1-refile-tampered-plan-refused-at-execute ()
  "A saved mcp-origin plan whose refile destination is outside fails at execute."
  (orgx-sec-test--with-env
    (let ((plan (hitl-plan-create
                 :domain 'org-agenda :metadata '(:origin mcp)
                 :targets (list (orgx-sec-test--target orgx-sec-test--allowed "alpha"))
                 :ops (list (orgx-bulk-op--create
                             :name 'refile :args (list (list :file orgx-sec-test--victim)))))))
      (orgx-bulk-execute plan)
      (should (eq 'failed (hitl-plan-step-status (car (hitl-plan-steps plan)))))
      (should (equal "echo victim\n" (orgx-sec-test--read orgx-sec-test--victim))))))

(ert-deftest orgx-sec/f1-interactive-refile-not-restricted ()
  (orgx-sec-test--with-env
    (let ((plan (hitl-plan-create
                 :domain 'org-agenda
                 :targets (list (orgx-sec-test--target orgx-sec-test--allowed "alpha"))
                 :ops (list (orgx-bulk-op--create
                             :name 'refile :args (list (list :file orgx-sec-test--outside)))))))
      (orgx-bulk-execute plan)
      (should (eq 'done (hitl-plan-step-status (car (hitl-plan-steps plan)))))
      (should (string-match-p "alpha" (orgx-sec-test--read orgx-sec-test--outside))))))

(ert-deftest orgx-sec/f1-archive-property-checked-at-execute ()
  "The ARCHIVE property in effect is checked when the archive op runs."
  (orgx-sec-test--with-env
    (write-region (format "* TODO beta\n:PROPERTIES:\n:ARCHIVE:  %s::\n:END:\n"
                          orgx-sec-test--victim)
                  nil orgx-sec-test--allowed)
    (let ((res (orgx-sec-test--create
                (list (orgx-sec-test--target orgx-sec-test--allowed "beta"))
                '((:op "archive")))))
      (should-not (plist-get res :error))
      (let ((steps (orgx-sec-test--run res)))
        (should (eq 'failed (car (car steps)))))
      (should (equal "echo victim\n" (orgx-sec-test--read orgx-sec-test--victim)))
      (should (string-match-p "beta" (orgx-sec-test--read orgx-sec-test--allowed))))
    ;; An archive target that is allowed (and exists) is fine.
    (write-region (format "* TODO beta\n:PROPERTIES:\n:ARCHIVE:  %s::\n:END:\n"
                          orgx-sec-test--outside)
                  nil orgx-sec-test--allowed)
    (when-let* ((b (find-buffer-visiting orgx-sec-test--allowed)))
      (with-current-buffer b (revert-buffer t t t)))
    (let ((orgx-bulk-mcp-allowed-files (list orgx-sec-test--outside))
          (res (orgx-sec-test--create
                (list (orgx-sec-test--target orgx-sec-test--allowed "beta"))
                '((:op "archive")))))
      (should (eq 'done (car (car (orgx-sec-test--run res)))))
      (should (string-match-p "beta" (orgx-sec-test--read orgx-sec-test--outside))))))

(ert-deftest orgx-sec/f1-archive-interactive-not-restricted ()
  (orgx-sec-test--with-env
    (write-region (format "* TODO beta\n:PROPERTIES:\n:ARCHIVE:  %s::\n:END:\n"
                          orgx-sec-test--victim)
                  nil orgx-sec-test--allowed)
    (let ((plan (hitl-plan-create
                 :domain 'org-agenda
                 :targets (list (orgx-sec-test--target orgx-sec-test--allowed "beta"))
                 :ops (list (orgx-bulk-op--create :name 'archive)))))
      (orgx-bulk-execute plan)
      (should (eq 'done (hitl-plan-step-status (car (hitl-plan-steps plan)))))
      (should (string-match-p "beta" (orgx-sec-test--read orgx-sec-test--victim))))))

(ert-deftest orgx-sec/f1-chained-property-archive-attack ()
  "The reviewer's t4 chain (property-set ARCHIVE + archive) is rejected."
  (orgx-sec-test--with-env
    (let ((res (orgx-sec-test--create
                (list (orgx-sec-test--target orgx-sec-test--allowed "alpha"))
                (list (list :op "property-set"
                            :args (list :property "X" :value "1\ntouch /tmp/PWNED"))
                      (list :op "property-set"
                            :args (list :property "ARCHIVE"
                                        :value (concat orgx-sec-test--victim "::")))
                      '(:op "archive")))))
      (should (stringp (plist-get res :error)))
      (should (null (hitl-plan-list))))))

;;; Finding 3: targets are re-captured and must agree

(ert-deftest orgx-sec/f3-target-mismatch-rejected ()
  (orgx-sec-test--with-env
    (write-region "* TODO harmless\n* DONE IMPORTANT\n:PROPERTIES:\n:ID: ID-IMPORTANT\n:END:\n"
                  nil orgx-sec-test--allowed)
    (let ((f orgx-sec-test--allowed)
          (ops '((:op "todo-state" :args (:state "DONE")))))
      ;; id resolves to IMPORTANT but the label claims `harmless'.
      (should (string-match-p "disagrees"
                              (plist-get (orgx-sec-test--create
                                          (list (list :file f :id "ID-IMPORTANT"
                                                      :heading "harmless" :pos 1))
                                          ops)
                                         :error)))
      ;; wrong position
      (should (string-match-p "disagrees"
                              (plist-get (orgx-sec-test--create
                                          (list (list :file f :heading "harmless" :pos 5))
                                          ops)
                                         :error)))
      ;; unresolvable: names the target
      (let ((err (plist-get (orgx-sec-test--create
                             (list (list :file f :heading "nope" :pos 1)) ops)
                            :error)))
        (should (string-match-p "not found" err))
        (should (string-match-p "nope" err)))
      ;; neither id nor heading
      (should (plist-get (orgx-sec-test--create (list (list :file f :pos 1)) ops) :error))
      ;; bad types
      (should (plist-get (orgx-sec-test--create (list (list :file f :heading 5)) ops) :error))
      (should (plist-get (orgx-sec-test--create (list (list :file 7 :heading "x")) ops) :error))
      (should (null (hitl-plan-list))))))

(ert-deftest orgx-sec/f3-target-replaced-by-fresh-capture ()
  (orgx-sec-test--with-env
    (let* ((res (orgx-sec-test--create
                 (list (list :file orgx-sec-test--allowed :heading "beta"
                             :todo-state "DONE" :extra "evil"))
                 '((:op "todo-state" :args (:state "DONE")))))
           (plan (orgx-sec-test--plan res))
           (target (hitl-plan-step-target (car (hitl-plan-steps plan)))))
      (should-not (plist-get res :error))
      (should (equal "TODO" (plist-get target :todo-state)))
      (should-not (plist-member target :extra))
      (should (= 14 (plist-get target :pos)))
      ;; Because the stored todo-state is real, the step is not skipped.
      (should (eq 'pending (hitl-plan-step-status (car (hitl-plan-steps plan)))))
      (orgx-bulk-execute plan)
      (should (string-match-p "DONE beta" (orgx-sec-test--read orgx-sec-test--allowed))))))

(ert-deftest orgx-sec/f3-target-file-must-be-allowed ()
  (orgx-sec-test--with-env
    (dolist (f (list orgx-sec-test--outside orgx-sec-test--victim "/nonexistent/x.org"
                     "/ssh:u@h.invalid:/x.org"))
      (should (string-match-p
               "File not allowed"
               (plist-get (orgx-sec-test--create
                           (list (orgx-sec-test--target f "secret"))
                           '((:op "tag-add" :args (:tag "x"))))
                          :error))))
    (should (null (hitl-plan-list)))))

;;; Finding 4: scope files and scope type

(ert-deftest orgx-sec/f4-scope-files-restricted-and-uniform-error ()
  (orgx-sec-test--with-env
    (let (msgs)
      (dolist (f (list orgx-sec-test--outside orgx-sec-test--victim "/nonexistent/x.org"
                       "/etc/passwd"))
        (let ((res (hitl-plan-mcp-create
                    :domain "org-agenda" :show :json-false
                    :scope (list :type "files" :files (list f))
                    :ops '((:op "tag-add" :args (:tag "x"))))))
          (should (stringp (plist-get res :error)))
          (push (replace-regexp-in-string (regexp-quote f) "F" (plist-get res :error)) msgs)))
      ;; Identical regardless of why: no existence oracle.
      (should (= 1 (length (delete-dups msgs))))
      (should (null (hitl-plan-list))))
    ;; Allowed files work, with the type given as a string.
    (let ((res (hitl-plan-mcp-create
                :domain "org-agenda" :show :json-false
                :scope (list :type "files" :files (list orgx-sec-test--allowed))
                :ops '((:op "tag-add" :args (:tag "x"))))))
      (should-not (plist-get res :error))
      (should (= 3 (plist-get (plist-get res :counts) :total))))))

(ert-deftest orgx-sec/f4-remote-scope-does-not-connect ()
  (orgx-sec-test--with-env
    (let* ((start (float-time))
           (res (hitl-plan-mcp-create
                 :domain "org-agenda" :show :json-false
                 :scope (list :files (list "/ssh:nobody@host.invalid:/x.org"))
                 :ops '((:op "tag-add" :args (:tag "x"))))))
      (should (stringp (plist-get res :error)))
      (should (< (- (float-time) start) 5)))))

(ert-deftest orgx-sec/f4-scope-type-strings ()
  (let ((hitl-plan-restricted nil))
    (should (eq 'agenda (orgx-bulk-agenda--scope-type '(:type "agenda"))))
    (should (eq 'agenda (orgx-bulk-agenda--scope-type '(:type agenda))))
    (should (eq 'files (orgx-bulk-agenda--scope-type '(:type "files"))))
    (should (eq 'files (orgx-bulk-agenda--scope-type nil)))
    (should (eq 'agenda-files (orgx-bulk-agenda--scope-type '(:type "agenda_files"))))
    (should (eq 'agenda-files (orgx-bulk-agenda--scope-type '(:type "agenda-files"))))
    (should (eq 'denote-files (orgx-bulk-agenda--scope-type '(:type "denote"))))
    (should (eq 'files (orgx-bulk-agenda--scope-type '(:type "bogus")))))
  (let ((hitl-plan-restricted t))
    (should-error (orgx-bulk-agenda--scope-type '(:type "bogus")))))

(ert-deftest orgx-sec/f4-agenda-type-string-does-not-read-files ()
  "The reviewer's t2: type \"agenda\" with files must not read those files."
  (orgx-sec-test--with-env
    (let ((res (hitl-plan-mcp-create
                :domain "org-agenda" :show :json-false
                :scope (list :type "agenda" :files (list orgx-sec-test--outside)
                             :match "LEVEL>=1")
                :ops '((:op "tag-add" :args (:tag "x"))))))
      (should (stringp (plist-get res :error)))
      (should-not (string-match-p "secret" (format "%S" res)))
      (should-not (find-buffer-visiting orgx-sec-test--outside)))))

(ert-deftest orgx-sec/f4-local-variables-not-processed-and-buffers-killed ()
  (orgx-sec-test--with-env
    (write-region (concat "* TODO alpha\n\n# Local Variables:\n"
                          "# eval: (setq orgx-sec-test--flag t)\n# End:\n")
                  nil orgx-sec-test--allowed)
    (let ((enable-local-variables :all)
          (enable-local-eval t)
          (res nil))
      (setq res (orgx-sec-test--create
                 (list (orgx-sec-test--target orgx-sec-test--allowed "alpha"))
                 '((:op "tag-add" :args (:tag "x")))))
      (should-not (plist-get res :error))
      (should-not orgx-sec-test--flag)
      (should-not (find-buffer-visiting orgx-sec-test--allowed))
      ;; scope-built targets: same
      (setq res (hitl-plan-mcp-create
                 :domain "org-agenda" :show :json-false
                 :scope (list :type "files" :files (list orgx-sec-test--allowed))
                 :ops '((:op "tag-add" :args (:tag "y")))))
      (should-not (plist-get res :error))
      (should-not orgx-sec-test--flag)
      (should-not (find-buffer-visiting orgx-sec-test--allowed))
      ;; execution as well
      (should (eq 'done (car (car (orgx-sec-test--run res)))))
      (should-not orgx-sec-test--flag))))

(ert-deftest orgx-sec/f4-interactive-visiting-keeps-local-variables ()
  "Outside restriction, files are opened as before (local variables honored)."
  (orgx-sec-test--with-env
    (write-region (concat "* TODO alpha\n\n# Local Variables:\n"
                          "# orgx-sec-test--lv: 42\n# End:\n")
                  nil orgx-sec-test--allowed)
    (let ((enable-local-variables :all))
      (with-current-buffer (orgx-bulk--visit orgx-sec-test--allowed)
        (should (equal 42 (bound-and-true-p orgx-sec-test--lv)))))))

;;; Finding 12: agenda scope

(ert-deftest orgx-sec/f12-agenda-scope-needs-existing-agenda-buffer ()
  (orgx-sec-test--with-env
    (let ((agenda (get-buffer-create "*Org Agenda(orgx-sec)*"))
          (plain (get-buffer-create "*orgx-sec-plain*")))
      (with-current-buffer agenda (org-agenda-mode))
      (dolist (scope (list (list :type 'agenda :buffer-name "*orgx-sec-plain*")
                           (list :type 'agenda :buffer-name "*does-not-exist*")
                           (list :type 'agenda)
                           (list :type "agenda" :buffer-name 7)))
        (let ((res (hitl-plan-mcp-create :domain "org-agenda" :show :json-false
                                         :scope scope
                                         :ops '((:op "tag-add" :args (:tag "x"))))))
          (should (stringp (plist-get res :error)))))
      ;; No fallback to the current buffer even when it is an agenda buffer.
      (with-current-buffer agenda
        (should (stringp (plist-get (hitl-plan-mcp-create
                                     :domain "org-agenda" :show :json-false
                                     :scope (list :type "agenda")
                                     :ops '((:op "tag-add" :args (:tag "x"))))
                                    :error))))
      ;; A real agenda buffer is accepted.
      (let ((res (hitl-plan-mcp-create :domain "org-agenda" :show :json-false
                                       :scope (list :type "agenda"
                                                    :buffer-name (buffer-name agenda))
                                       :ops '((:op "tag-add" :args (:tag "x"))))))
        (should-not (plist-get res :error)))
      (kill-buffer plain)
      (kill-buffer agenda))))

;;; Finding 13: orgx-mcp

(ert-deftest orgx-sec/f13-bulk-create-errors-are-error-plists ()
  (orgx-sec-test--with-env
    (dolist (args (list (list :ops (vector '(:op "tag-add" :args (:tag "x"))) :scope 7)
                        (list :ops (vector '(:op "tag-add" :args (:tag "x")))
                              :files "abc")
                        (list :ops (vector '(:op "tag-add" :args (:tag "x")))
                              :files [7])
                        (list :ops (vector '(:op "tag-add" :args (:tag "x")))
                              :scope "bogus")
                        (list :ops "x")
                        (list :ops (vector '(:op "tag-add" :args (:tag "x")))
                              :from_states "TODO")
                        (list :ops (vector '(:op "tag-add" :args (:tag "x")))
                              :match 5)))
      (let ((res (orgx-sec-test--call "bulk_plan_create" args)))
        (should (stringp (plist-get res :error)))))
    (should (null (orgx-bulk-list-plans)))))

(ert-deftest orgx-sec/f13-bulk-create-files-scope-restricted ()
  (orgx-sec-test--with-env
    (let ((res (orgx-sec-test--call
                "bulk_plan_create"
                (list :files (vector orgx-sec-test--outside)
                      :ops (vector '(:op "tag-add" :args (:tag "x")))))))
      (should (stringp (plist-get res :error))))
    (let ((res (orgx-sec-test--call
                "bulk_plan_create"
                (list :files (vector orgx-sec-test--allowed)
                      :ops (vector '(:op "tag-add" :args (:tag "x")))
                      :show :json-false))))
      (should-not (plist-get res :error))
      (should (hitl-plan-origin-mcp-p (orgx-sec-test--plan res))))))

(defun orgx-sec-test--capture (heading body)
  "Capture HEADING and BODY into a temp file through the real capture; return text."
  (let ((f (make-temp-file "orgx-sec-cap" nil ".org" "* Tasks\n")))
    (unwind-protect
        (progn (orgx-mcp-capture-run (list 'file f) heading body)
               (orgx-sec-test--read f))
      (when-let* ((b (find-buffer-visiting f)))
        (with-current-buffer b (set-buffer-modified-p nil))
        (kill-buffer b))
      (delete-file f))))

(ert-deftest orgx-sec/f13-capture-template-escapes-are-inert ()
  "`%(...)' in agent text must not evaluate (it did before the fix)."
  (let ((orgx-sec-test--flag nil))
    (let ((text (orgx-sec-test--capture
                 "TODO x %(setq orgx-sec-test--flag t) %<%Y>"
                 "body %(setq orgx-sec-test--flag t) 100% \\%(foo) %t %a %[/etc/hostname]")))
      (should-not orgx-sec-test--flag)
      (should (string-match-p (regexp-quote "TODO x %(setq orgx-sec-test--flag t) %<%Y>") text))
      (should (string-match-p (regexp-quote "body %(setq orgx-sec-test--flag t) 100% \\%(foo) %t %a %[/etc/hostname]")
                              text)))))

(ert-deftest orgx-sec/f13-capture-heading-newlines-neutralised ()
  (let* ((text (orgx-sec-test--capture "TODO a\n* injected\n:PROPERTIES:\r\n:X: 1" nil))
         (lines (split-string text "\n")))
    (should (= 1 (seq-count (lambda (l) (string-match-p "\\`\\*+ TODO a" l)) lines)))
    (should-not (seq-find (lambda (l) (string-match-p "\\`\\* injected" l)) lines))
    (should (string-match-p "TODO a \\* injected :PROPERTIES: :X: 1" text))))

(ert-deftest orgx-sec/f13-capture-rejects-bad-types-and-lines ()
  (dolist (heading '(nil 5 (:a 1) [1] ""))
    (should-error (orgx-mcp-capture-run '(file "/x") heading)))
  (should-error (orgx-mcp-capture-run '(file "/x") "h" 7))
  (should-error (orgx-mcp-capture-run '(file "/x") "h" '("a")))
  (dolist (body '("ok\n#+begin_src emacs-lisp\n(boom)\n#+end_src"
                  "ok\n#+BEGIN_SRC sh"
                  "ok\n  #+begin_src sh"
                  "ok\n:PROPERTIES:\n:X: 1\n:END:"
                  "ok\n# Local Variables:\n# eval: (boom)\n# End:"
                  "ok\n# local variables:\n"
                  "ok\n* new heading"
                  "ok\r* new heading"
                  "* first line heading"
                  "ok\n** deeper"))
    (should-error (orgx-mcp-capture-run '(file "/x") "h" body)))
  ;; Tools return (:error ...) instead of signalling.
  (dolist (args '((:heading 5) (:heading "ok" :body 5) (:heading "ok" :body "* x")
                  (:heading "ok" :body "#+begin_src x")))
    (dolist (tool '("org_capture_task" "org_capture_journal" "org_capture_clock_in"))
      (should (stringp (plist-get (orgx-sec-test--call tool args) :error))))))

(ert-deftest orgx-sec/f13-capture-legitimate-bodies-work ()
  (let ((text (orgx-sec-test--capture
               "TODO write it"
               "First paragraph.\n\n- a list item\n- another\n\n  indented text\n#+begin_quote\nquoted\n#+end_quote\n*bold* and a star * in the middle\nTime 10:30 not a drawer: yes")))
    (should (string-match-p "^\\*+ TODO write it$" text))
    (should (string-match-p "^First paragraph\\.$" text))
    (should (string-match-p "^- a list item$" text))
    (should (string-match-p "^#\\+begin_quote$" text))
    (should (string-match-p "^\\*bold\\* and a star \\* in the middle$" text))
    (should (string-match-p "^Time 10:30 not a drawer: yes$" text))))

(provide 'orgx-security-test)
;;; orgx-security-test.el ends here
