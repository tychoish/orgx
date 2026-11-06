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
          (hitl-plan--store (make-hash-table :test #'equal))
          (orgx-bulk-mcp-allowed-files (list ,var))
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
                   (string-prefix-p "*hitl-plan:" (buffer-name buf)))
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
        ;; Reject `each' log_notes over MCP (hitl reports failures as (:error MSG)).
        (should (string-match-p
                 "each"
                 (plist-get
                  (orgx-mcp-test--call
                   "bulk_plan_create"
                   (list :files (vector f)
                         :ops (vector '(:op "todo-state" :args (:state "DONE")))
                         :log_notes "each"))
                  :error)))
        (should (null (orgx-bulk-list-plans)))
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

(ert-deftest orgx-mcp-bulk-plan-create-show-false-test ()
  ;; mcpkit parses JSON false as nil; explicit false must suppress review.
  (orgx-mcp-test--with-file f "* TODO a\n"
    (let ((reviewed nil)
          (org-agenda-files (list f)))
      (cl-letf (((symbol-function 'orgx-bulk-review) (lambda (plan &rest _) (push plan reviewed))))
        (dolist (show '(nil :json-false))
          (orgx-mcp-test--call
           "bulk_plan_create"
           (list :files (vector f)
                 :ops (vector '(:op "todo-state" :args (:state "DONE")))
                 :show show)))
        (should-not reviewed)
        (orgx-mcp-test--call
         "bulk_plan_create"
         (list :files (vector f)
               :ops (vector '(:op "todo-state" :args (:state "DONE")))))
        (should (= 1 (length reviewed)))))))

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
    (should (string-match-p
             "note"
             (plist-get
              (orgx-mcp-test--call
               "bulk_plan_create"
               (list :files (vector f)
                     :ops (vector '(:op "todo-state" :args (:state "DONE")))
                     :log_notes "one"
                     :show :json-false))
              :error)))
    (should (null (orgx-bulk-list-plans)))))

(ert-deftest orgx-mcp-bulk-tools-delegate-to-hitl-scoped-to-org-agenda-test ()
  (orgx-mcp-test--with-file f "* TODO a\n* DONE b\n"
    (let* ((created (orgx-mcp-test--call
                     "bulk_plan_create"
                     (list :files (vector f) :from_states ["TODO" "DONE"]
                           :ops (vector '(:op "todo-state" :args (:state "DONE")))
                           :show :json-false)))
           (id (plist-get created :plan_id))
           (other (hitl-plan-create :domain 'file-target :save nil
                                    :targets (list (list :file f :line 1))
                                    :ops '(todo-state))))
      (should (hitl-plan-p other))
      ;; Created through `plan_create' as an `org-agenda' plan stored in
      ;; `orgx-bulk-directory'; the already-DONE heading is skipped.
      (should (equal (plist-get created :domain) "org-agenda"))
      (should (file-exists-p (hitl-plan-file id orgx-bulk-directory)))
      (should (equal (mapcar (lambda (s) (plist-get s :status))
                             (plist-get created :steps))
                     '("pending" "skipped")))
      ;; `bulk_plan_list' shows only `org-agenda' plans, even after a restart.
      (clrhash hitl-plan--store)
      (should (equal (mapcar (lambda (p) (plist-get p :plan_id))
                             (plist-get (orgx-mcp-test--call "bulk_plan_list" nil) :plans))
                     (list id)))
      ;; Unknown plans are reported in hitl's (:error MSG) shape.
      (should (plist-get (orgx-mcp-test--call "bulk_plan_status" '(:plan_id "nope")) :error))
      (should (equal (plist-get (orgx-mcp-test--call "bulk_ops" nil) :domain) "org-agenda")))))

;;; Review follow-ups

(defun orgx-mcp-test--agenda-buffers ()
  "Return the live agenda buffers."
  (seq-filter (lambda (b) (string-prefix-p "*Org Agenda" (buffer-name b))) (buffer-list)))

(ert-deftest orgx-mcp-bulk-plan-list-reports-errors-test ()
  "`bulk_plan_list' returns hitl's result, including its (:error MSG)."
  (orgx-mcp-test--with-file f "* TODO a\n"
    (should (string-match-p
             "not allowed"
             (plist-get (orgx-mcp-test--call "bulk_plan_list" '(:directory "/definitely/not/a/store"))
                        :error)))
    (should (equal [] (plist-get (orgx-mcp-test--call "bulk_plan_list" nil) :plans)))))

(ert-deftest orgx-mcp-bulk-plan-create-leaves-no-agenda-buffers-test ()
  "Building targets from a scope does not leave sticky agenda buffers behind."
  (orgx-mcp-test--with-file f "* TODO a\n* TODO b\n"
    (let ((before (orgx-mcp-test--agenda-buffers)))
      (let ((res (orgx-mcp-test--call
                  "bulk_plan_create"
                  (list :files (vector f)
                        :ops (vector '(:op "todo-state" :args (:state "DONE")))
                        :show :json-false))))
        (should-not (plist-get res :error))
        (should (= 2 (length (plist-get res :steps)))))
      (should (equal before (orgx-mcp-test--agenda-buffers)))
      ;; The plan still executes: it rebuilds its own batch.
      (let ((hitl-plan-agent-may-apply t)
            (id (plist-get (car (append (plist-get (orgx-mcp-test--call "bulk_plan_list" nil)
                                                   :plans)
                                        nil))
                           :plan_id)))
        (should (equal "complete"
                       (plist-get (orgx-mcp-test--call "bulk_plan_execute" (list :plan_id id))
                                  :status)))
        (with-current-buffer (find-file-noselect f)
          (should (equal '("DONE" "DONE") (org-map-entries #'org-get-todo-state))))))))

(ert-deftest orgx-mcp-bulk-plan-create-null-args-test ()
  (orgx-mcp-test--with-file f "* TODO a\n"
    (let ((res (orgx-mcp-test--call
                "bulk_plan_create"
                (list :files (vector f) :scope :null :match :null :from_states :null
                      :buffer_name :null :log_notes :null :message :null :description :null
                      :ops (vector '(:op "todo-state" :args (:state "DONE")))
                      :show :json-false))))
      (should-not (plist-get res :error))
      (should (equal "draft" (plist-get res :status))))))

(ert-deftest orgx-mcp-bulk-plan-execute-through-function-api-test ()
  "With `hitl-plan-agent-may-apply' the function API executes and excludes."
  (orgx-mcp-test--with-file f "* TODO a\n* TODO b\n"
    (let* ((hitl-plan-agent-may-apply t)
           (res (hitl-plan-mcp-create
                 :domain "org-agenda" :show :json-false
                 :scope (orgx-mcp--build-scope (list :files (vector f)))
                 :ops (vector '(:op "todo-state" :args (:state "DONE")))))
           (id (plist-get res :plan_id)))
      (should-not (plist-get res :error))
      (let ((done (hitl-plan-mcp-execute :domain "org-agenda" :plan_id id :exclude [2])))
        (should (equal "complete" (plist-get done :status)))
        (should (= 1 (plist-get (plist-get done :counts) :skipped))))
      (with-current-buffer (find-file-noselect f)
        (should (equal '("DONE" "TODO") (org-map-entries #'org-get-todo-state))))
      (should (equal "excluded over MCP"
                     (hitl-plan-step-message (nth 1 (hitl-plan-steps (hitl-plan-get id)))))))))

(provide 'orgx-mcp-test)
;;; orgx-mcp-test.el ends here
