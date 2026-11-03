;;; orgx-mcp-test.el --- Tests for orgx-mcp.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'org)
(require 'mcpkit)
(require 'orgx-bulk)
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
  `(let ((,var (make-temp-file "orgx-mcp" nil ".org" ,content))
         (org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELED")))
         (orgx-bulk--plans (make-hash-table :test #'equal)))
     (unwind-protect
         (cl-letf (((symbol-function 'display-buffer) #'ignore))
           ,@body)
       (when-let* ((b (find-buffer-visiting ,var)))
         (with-current-buffer b (set-buffer-modified-p nil))
         (kill-buffer b))
       (delete-file ,var))))

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
                         "bulk_state_preview" "bulk_state_status"
                         "bulk_state_apply" "bulk_state_discard"))))

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

(ert-deftest orgx-mcp-bulk-preview-test ()
  (orgx-mcp-test--with-file f "* TODO a\n* TODO b\n* DONE c\n"
    (let ((reviewed nil)
          (org-agenda-files (list f)))
      (cl-letf (((symbol-function 'orgx-bulk-review) (lambda (plan &rest _) (push plan reviewed))))
        ;; Explicit files scope, default :show.
        (let ((s (orgx-mcp-test--call "bulk_state_preview"
                                      (list :target_state "CANCELED" :files (vector f)))))
          (should (equal (plist-get s :target_state) "CANCELED"))
          (should (= (length (plist-get s :items)) 2))
          (should (= (length reviewed) 1))
          (should (orgx-bulk-get-plan (plist-get s :plan_id))))
        ;; Default scope without files is the agenda files; :show false skips review.
        (let ((s (orgx-mcp-test--call "bulk_state_preview"
                                      '(:target_state "CANCELED" :show :json-false))))
          (should (= (length (plist-get s :items)) 2))
          (should (= (length reviewed) 1)))
        ;; from_states and match.
        (let ((s (orgx-mcp-test--call "bulk_state_preview"
                                      (list :target_state "TODO" :files (vector f)
                                            :from_states ["DONE"] :show :json-false))))
          (should (equal (seq-map (lambda (i) (plist-get i :heading)) (plist-get s :items))
                         '("c"))))
        ;; denote scope.
        (cl-letf (((symbol-function 'orgx-denote-files) (lambda () (list f))))
          (let ((s (orgx-mcp-test--call "bulk_state_preview"
                                        '(:target_state "DONE" :scope "denote" :show :json-false))))
            (should (= (length (plist-get s :items)) 2))))))))

(ert-deftest orgx-mcp-bulk-status-and-unknown-test ()
  (orgx-mcp-test--with-file f "* TODO a\n"
    (let* ((plan (orgx-bulk-plan "DONE" (orgx-bulk-collect-files :files (list f))))
           (s (orgx-mcp-test--call "bulk_state_status" (list :plan_id (orgx-bulk-plan-id plan)))))
      (should (equal (plist-get s :status) "pending"))
      (should (equal (plist-get s :plan_id) (orgx-bulk-plan-id plan))))
    (should-error (orgx-mcp-test--call "bulk_state_status" '(:plan_id "bulk-nope"))
                  :type 'error)
    (should (string-match-p "Unknown plan: bulk-nope"
                            (condition-case err
                                (orgx-mcp-test--call "bulk_state_apply" '(:plan_id "bulk-nope"))
                              (error (cadr err)))))
    (should-error (orgx-mcp-test--call "bulk_state_discard" '(:plan_id "bulk-nope")))))

(ert-deftest orgx-mcp-bulk-apply-needs-confirmation-test ()
  (orgx-mcp-test--with-file f "* TODO a\n"
    (let* ((plan (orgx-bulk-plan "DONE" (orgx-bulk-collect-files :files (list f))))
           (orgx-mcp-agent-may-apply nil)
           (r (orgx-mcp-test--call "bulk_state_apply" (list :plan_id (orgx-bulk-plan-id plan)))))
      (should (equal (plist-get r :status) "awaiting_user_confirmation"))
      (should (equal (plist-get r :review_buffer) (orgx-bulk-review-buffer-name plan)))
      (should (eq (orgx-bulk-plan-status plan) 'pending))
      (should (equal (with-current-buffer (find-file-noselect f)
                       (org-map-entries #'org-get-todo-state))
                     '("TODO"))))))

(ert-deftest orgx-mcp-bulk-apply-allowed-test ()
  (orgx-mcp-test--with-file f "* TODO a\n* TODO b\n* TODO c\n"
    (let* ((plan (orgx-bulk-plan "DONE" (orgx-bulk-collect-files :files (list f))))
           (orgx-mcp-agent-may-apply t)
           (r (orgx-mcp-test--call "bulk_state_apply"
                                   (list :plan_id (orgx-bulk-plan-id plan) :exclude [2]))))
      (should (equal (plist-get r :status) "applied"))
      (should (equal (with-current-buffer (find-file-noselect f)
                       (org-map-entries #'org-get-todo-state))
                     '("DONE" "TODO" "DONE")))
      ;; Applying again just reports the non-pending plan.
      (should (equal (plist-get (orgx-mcp-test--call "bulk_state_apply"
                                                     (list :plan_id (orgx-bulk-plan-id plan)))
                                :status)
                     "applied")))))

(ert-deftest orgx-mcp-bulk-discard-test ()
  (orgx-mcp-test--with-file f "* TODO a\n"
    (let* ((plan (orgx-bulk-plan "DONE" (orgx-bulk-collect-files :files (list f))))
           (buf (orgx-bulk-review plan))
           (r (orgx-mcp-test--call "bulk_state_discard" (list :plan_id (orgx-bulk-plan-id plan)))))
      (should (equal (plist-get r :status) "discarded"))
      (should-not (buffer-live-p buf)))))

(provide 'orgx-mcp-test)
;;; orgx-mcp-test.el ends here
