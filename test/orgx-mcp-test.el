;;; orgx-mcp-test.el --- Tests for orgx-mcp.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'org)
(require 'mcpkit)
(require 'orgx-bulk)
(require 'orgx-bulk-ops)
(require 'orgx-bulk-agenda)
(require 'orgx-bulk-buffer)
(require 'orgx-mcp)

(defun orgx-mcp-test--call (tool args)
  "Invoke registered TOOL's handler with ARGS and return its result.
Signal an error carrying the handler's error message on failure."
  (let ((tool-obj (map-elt (mcpkit-service-tools orgx-mcp-service) tool))
        (outcome nil))
    (funcall (mcpkit-tool-handler tool-obj) args
             (lambda (err res) (setq outcome (cons err res))))
    (when (car outcome)
      (error "%s" (car outcome)))
    (cdr outcome)))

(defmacro orgx-mcp-test--with-file (var content &rest body)
  "Bind VAR to a temp org file holding CONTENT and run BODY."
  (declare (indent 2))
  `(let* ((orgx-bulk-directory (make-temp-file "orgx-mcp-bulk-dir-" t))
          (,var (make-temp-file "orgx-mcp-" nil ".org" ,content))
          (org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELED")))
          (orgx-bulk--plans (make-hash-table :test #'equal))
          (orgx-bulk--counter 0)
          (orgx-bulk-log-notes 'ignore))
     (unwind-protect
         (cl-letf (((symbol-function 'display-buffer) #'ignore))
           (with-current-buffer (find-file-noselect ,var)
             (org-mode)
             (org-set-regexps-and-options))
           ,@body)
       (dolist (buf (buffer-list))
         (when (or (and (buffer-file-name buf)
                        (equal (expand-file-name (buffer-file-name buf))
                               (expand-file-name ,var)))
                   (string-prefix-p "*Org Agenda" (buffer-name buf))
                   (string-prefix-p "*orgx-bulk:" (buffer-name buf)))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (when (file-exists-p ,var)
         (delete-file ,var))
       (when (file-directory-p orgx-bulk-directory)
         (delete-directory orgx-bulk-directory t)))))

(ert-deftest orgx-mcp-capture-target-test ()
  (let ((org-capture-templates '(("t" "Task" entry (file "/default") "")
                                 ("at" "Agent task" entry (file "/agent-task") "")
                                 ("aj" "Agent journal" entry (file "/agent-j") ""))))
    (should (equal (orgx-mcp-capture-target '("att" "at")) '(file "/agent-task")))
    (should (equal (orgx-mcp-capture-target '("ajj" "aj")) '(file "/agent-j")))
    (should (equal (orgx-mcp-capture-target '("zz")) '(file "/default"))))
  (let ((org-capture-templates nil))
    (should (equal (orgx-mcp-capture-target '("at"))
                   (list 'file (expand-file-name "~/org/agentic.org"))))))

(ert-deftest orgx-mcp-capture-run-test ()
  (let ((f (make-temp-file "orgx-mcp-cap" nil ".org" "* Tasks\n")))
    (unwind-protect
        (progn
          (orgx-mcp-capture-run (list 'file f) "TODO write" "some body")
          (let ((text (with-temp-buffer (insert-file-contents f) (buffer-string))))
            (should (string-match-p "^\\*+ TODO write$" text))
            (should (string-match-p ":CREATED:  \\[[0-9-]+ [A-Za-z]+ [0-9:]+\\]" text))
            (should (string-match-p "^some body$" text)))
          (orgx-mcp-capture-run (list 'file f) "TODO bare")
          (should (string-match-p "^\\*+ TODO bare\n:PROPERTIES:\n:CREATED:.*\n:END:\n"
                                  (with-temp-buffer (insert-file-contents f) (buffer-string)))))
      (when-let* ((b (find-buffer-visiting f)))
        (with-current-buffer b (set-buffer-modified-p nil))
        (kill-buffer b))
      (delete-file f))))

(ert-deftest orgx-mcp-capture-run-clock-in-test ()
  (let ((captured nil))
    (cl-letf (((symbol-function 'org-capture)
               (lambda (_goto keys)
                 (setq captured (assoc keys org-capture-templates)))))
      (orgx-mcp-capture-run '(file "/x") "h" nil t)
      (should (plist-get (nthcdr 5 captured) :clock-in))
      (orgx-mcp-capture-run '(file "/x") "h")
      (should-not (plist-get (nthcdr 5 captured) :clock-in))
      (should (equal (nth 3 captured) '(file "/x"))))))

(ert-deftest orgx-mcp-tools-registered-test ()
  (should (seq-every-p (lambda (name) (map-elt (mcpkit-service-tools orgx-mcp-service) name))
                       '("org_capture_task" "org_capture_journal" "org_capture_clock_in"
                         "bulk_ops" "bulk_plan_create" "bulk_plan_list"
                         "bulk_plan_status" "bulk_plan_discard" "bulk_plan_execute"))))

(ert-deftest orgx-mcp-capture-tools-test ()
  (let ((calls nil))
    (cl-letf (((symbol-function 'orgx-mcp-capture-run)
               (lambda (&rest args) (push args calls)))
              ((symbol-function 'orgx-mcp-capture-target) (lambda (keys) keys)))
      (let ((r (orgx-mcp-test--call "org_capture_task" '(:heading "write" :body "b"))))
        (should (equal (plist-get r :heading) "TODO write"))
        (should (equal (plist-get r :target) "task"))
        (should (equal (car calls) '(("att" "at") "TODO write" "b"))))
      (should (equal (plist-get (orgx-mcp-test--call "org_capture_task" '(:heading "TODO x"))
                                :heading)
                     "TODO x"))
      (let ((r (orgx-mcp-test--call "org_capture_journal" '(:heading "dear diary"))))
        (should (equal (plist-get r :heading) "dear diary"))
        (should (equal (plist-get r :target) "journal"))
        (should (equal (car calls) '(("ajj" "aj") "dear diary" nil))))
      (let ((r (orgx-mcp-test--call "org_capture_clock_in" '(:heading "go"))))
        (should (equal (plist-get r :heading) "TODO go"))
        (should (equal (plist-get r :target) "clock-in"))
        (should (equal (car calls) '(("att" "at") "TODO go" nil t)))))))

(ert-deftest orgx-mcp-bulk-ops-test ()
  (let* ((res (orgx-mcp-test--call "bulk_ops" nil))
         (ops (append (plist-get res :ops) nil))
         (names (mapcar (lambda (o) (plist-get o :name)) ops)))
    (should (member "todo-state" names))
    (should (member "schedule" names))
    (should (member "archive-sibling" names))
    (should (member "property-set" names))))

(ert-deftest orgx-mcp-bulk-plan-create-and-list-test ()
  (orgx-mcp-test--with-file f "* TODO a\n* TODO b\n* DONE c\n"
    (let ((reviewed nil)
          (org-agenda-files (list f)))
      (cl-letf (((symbol-function 'orgx-bulk-review) (lambda (plan &rest _) (push plan reviewed))))
        ;; Reject `each' log_notes over MCP.
        (should-error
         (orgx-mcp-test--call
          "bulk_plan_create"
          (list :files (vector f)
                :ops (vector '(:op "todo-state" :args (:state "DONE")))
                :log_notes "each")))
        ;; Create plan with explicit files scope.
        (let ((s (orgx-mcp-test--call
                  "bulk_plan_create"
                  (list :files (vector f)
                        :ops (vector '(:op "todo-state" :args (:state "CANCELED")))
                        :log_notes "one"
                        :message "Canceled over MCP"))))
          (should (equal (plist-get s :status) "draft"))
          (should (equal (plist-get s :log_notes) "one"))
          (should (equal (plist-get s :note) "Canceled over MCP"))
          (should (= (length (plist-get s :steps)) 2))
          (should (= (length reviewed) 1))
          (let ((listed (orgx-mcp-test--call "bulk_plan_list" '(:status "draft"))))
            (should (= (length (plist-get listed :plans)) 1))))))))

(ert-deftest orgx-mcp-bulk-status-discard-and-execute-test ()
  (orgx-mcp-test--with-file f "* TODO a\n* TODO b\n* TODO c\n"
    (let* ((created (orgx-mcp-test--call
                     "bulk_plan_create"
                     (list :files (vector f)
                           :ops (vector '(:op "todo-state" :args (:state "DONE")))
                           :show :json-false)))
           (plan-id (plist-get created :plan_id))
           (status-res (orgx-mcp-test--call "bulk_plan_status" (list :plan_id plan-id))))
      (should (equal (plist-get status-res :status) "draft"))
      ;; Gate execute behind `orgx-mcp-agent-may-apply'.
      (let ((orgx-mcp-agent-may-apply nil))
        (let ((r (orgx-mcp-test--call "bulk_plan_execute" (list :plan_id plan-id))))
          (should (equal (plist-get r :status) "awaiting_user_confirmation"))
          (should (equal (plist-get r :plan_id) plan-id))))
      ;; Allow agent apply and exclude step 2.
      (let ((orgx-mcp-agent-may-apply t))
        (let ((r (orgx-mcp-test--call "bulk_plan_execute"
                                      (list :plan_id plan-id :exclude [2]))))
          (should (equal (plist-get r :status) "complete"))
          (with-current-buffer (find-file-noselect f)
            (should (equal (org-map-entries #'org-get-todo-state)
                           '("DONE" "TODO" "DONE"))))))
      ;; Discard a second plan.
      (let* ((created2 (orgx-mcp-test--call
                        "bulk_plan_create"
                        (list :files (vector f)
                              :ops (vector '(:op "todo-state" :args (:state "CANCELED"))))))
             (pid2 (plist-get created2 :plan_id))
             (disc (orgx-mcp-test--call "bulk_plan_discard" (list :plan_id pid2))))
        (should (equal (plist-get disc :status) "discarded"))))))

(ert-deftest orgx-mcp-bulk-plan-create-one-requires-message-test ()
  ;; `one' without a note would make execution prompt inside the handler.
  (orgx-mcp-test--with-file f "* TODO a\n"
    (should-error
     (orgx-mcp-test--call
      "bulk_plan_create"
      (list :files (vector f)
            :ops (vector '(:op "todo-state" :args (:state "DONE")))
            :log_notes "one"
            :show :json-false)))))

(provide 'orgx-mcp-test)
;;; orgx-mcp-test.el ends here
