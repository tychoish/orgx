;;; orgx-bulk-agenda-test.el --- Tests for orgx-bulk-agenda.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-agenda)
(require 'orgx-bulk)
(require 'orgx-bulk-ops)
(require 'orgx-bulk-agenda)
(require 'orgx-bulk-buffer)

(defmacro orgx-bulk-agenda-test--with-files (f1-var c1 f2-var c2 &rest body)
  "Create two temp Org files F1-VAR and F2-VAR and a temp `orgx-bulk-directory'."
  (declare (indent 4))
  `(let* ((orgx-bulk-directory (make-temp-file "orgx-bulk-ag-dir-" t))
          (,f1-var (make-temp-file "orgx-ag-1-" nil ".org" ,c1))
          (,f2-var (make-temp-file "orgx-ag-2-" nil ".org" ,c2))
          (org-todo-keywords '((sequence "TODO" "INPROGRESS" "|" "DONE" "CANCELED")))
          (orgx-bulk--plans (make-hash-table :test #'equal))
          (orgx-bulk--counter 0)
          (org-agenda-bulk-marked-entries nil))
     (unwind-protect
         (progn
           (dolist (f (list ,f1-var ,f2-var))
             (with-current-buffer (find-file-noselect f)
               (org-mode)
               (org-set-regexps-and-options)))
           ,@body)
       (dolist (buf (buffer-list))
         (when (or (and (buffer-file-name buf)
                        (member (expand-file-name (buffer-file-name buf))
                                (list (expand-file-name ,f1-var)
                                      (expand-file-name ,f2-var))))
                   (string-prefix-p "*Org Agenda" (buffer-name buf))
                   (string-prefix-p "*orgx-bulk:" (buffer-name buf)))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (dolist (f (list ,f1-var ,f2-var))
         (when (file-exists-p f)
           (delete-file f)))
       (when (file-directory-p orgx-bulk-directory)
         (delete-directory orgx-bulk-directory t)))))

(ert-deftest orgx-bulk-agenda-batch-for-scope-and-sticky-isolation-test ()
  (orgx-bulk-agenda-test--with-files
      f1 "* TODO Alpha :work:\n* DONE DoneOne :work:\n"
      f2 "* TODO Beta :home:\n* TODO Gamma :home:\n"
    (let* ((b1 (orgx-bulk-batch-for-scope
                (list :type 'files :files (list f1) :match "+work" :only-open t)))
           (buf1 (car b1))
           (targets1 (cdr b1))
           (b2 (orgx-bulk-batch-for-scope
                (list :type 'files :files (list f2) :match "+home" :only-open t)))
           (buf2 (car b2))
           (targets2 (cdr b2)))
      (should (buffer-live-p buf1))
      (should (buffer-live-p buf2))
      (should-not (eq buf1 buf2))
      (should (equal (mapcar (lambda (tgt) (plist-get tgt :heading)) targets1)
                     '("Alpha")))
      (should (equal (mapcar (lambda (tgt) (plist-get tgt :heading)) targets2)
                     '("Beta" "Gamma")))
      ;; Each sticky agenda buffer keeps its own `orgx-bulk-batch-markers' list.
      (should (= (length (buffer-local-value 'orgx-bulk-batch-markers buf1)) 1))
      (should (= (length (buffer-local-value 'orgx-bulk-batch-markers buf2)) 2)))))

(ert-deftest orgx-bulk-agenda-rebuild-marks-missing-and-survivors-test ()
  (orgx-bulk-agenda-test--with-files
      f1 "* TODO Survives\n* TODO Deleted\n"
      _f2 ""
    (pcase-let* ((`(,buf . ,targets)
                  (orgx-bulk-batch-for-scope
                   (list :type 'files :files (list f1) :only-open t)))
                 (plan (orgx-bulk-create-plan
                        :scope (list :type 'files
                                     :files (list f1)
                                     :buffer-name (buffer-name buf))
                        :targets targets
                        :ops '((todo-state "DONE")))))
      ;; Kill the agenda buffer and delete the second heading from f1.
      (kill-buffer buf)
      (with-current-buffer (find-file-noselect f1)
        (goto-char (point-min))
        (re-search-forward "^\\* TODO Deleted")
        (delete-region (line-beginning-position) (point-max))
        (save-buffer))
      (let ((rebuilt-buf (orgx-bulk-batch-rebuild plan)))
        (should (buffer-live-p rebuilt-buf))
        (should (= (length (buffer-local-value 'orgx-bulk-batch-markers rebuilt-buf)) 1))
        ;; Survivors stay marked, with matching overlays, after rebuild.
        (let ((marked (buffer-local-value 'orgx-bulk-batch-markers rebuilt-buf)))
          (should (seq-set-equal-p org-agenda-bulk-marked-entries marked))
          (with-current-buffer rebuilt-buf
            (should (seq-some (lambda (o) (eq (overlay-get o 'type) 'org-marked-entry-overlay))
                              (overlays-in (point-min) (point-max))))))
        (should (equal (mapcar #'orgx-bulk-step-status (orgx-bulk-plan-steps plan))
                       '(pending missing)))))))

(ert-deftest orgx-bulk-agenda-mark-lock-test ()
  (orgx-bulk-agenda-test--with-files
      f1 "* TODO One\n* TODO Two\n"
      _f2 ""
    (pcase-let ((`(,buf . _)
                 (orgx-bulk-batch-for-scope
                  (list :type 'files :files (list f1) :only-open t))))
      (with-current-buffer buf
        (goto-char (point-min))
        (orgx-bulk-batch-lock buf "bulk-lock-test")
        (should-error (org-agenda-bulk-mark) :type 'user-error)
        (should-error (org-agenda-bulk-unmark) :type 'user-error)
        (should-error (org-agenda-bulk-unmark-all) :type 'user-error)
        (orgx-bulk-batch-unlock buf)
        (org-agenda-bulk-unmark-all)))))

(ert-deftest orgx-bulk-agenda-driver-B-test ()
  (orgx-bulk-agenda-test--with-files
      f1 "* TODO ItemA\n* TODO ItemB\n"
      _f2 ""
    (pcase-let ((`(,buf . _)
                 (orgx-bulk-batch-for-scope
                  (list :type 'files :files (list f1) :only-open t))))
      ;; 1. Default `B' (no prefix) with numeric prefix `1' (`one' log-notes)
      ;; creates a saved plan and opens its plan buffer without modifying f1 yet.
      (with-current-buffer buf
        (let ((org-agenda-bulk-marked-entries orgx-bulk-batch-markers))
          (cl-letf (((symbol-function 'read-char-exclusive) (lambda (&rest _) ?+))
                    ((symbol-function 'read-string) (lambda (&rest _) "tagged"))
                    ((symbol-function 'pop-to-buffer) #'set-buffer))
            (orgx-agenda-bulk-action 0))))
      (let ((plan (orgx-bulk-get-plan
                   (buffer-local-value 'orgx-bulk-batch-plan-id buf))))
        (should plan)
        (should (eq (orgx-bulk-plan-status plan) 'draft))
        (should (eq (orgx-bulk-plan-log-notes plan) 'ignore))
        (should (get-buffer (orgx-bulk-plan-buffer-name plan))))
      ;; 2. `C-u B' (`arg' = '(4)) executes the plan immediately.
      (with-current-buffer buf
        (let ((org-agenda-bulk-marked-entries orgx-bulk-batch-markers))
          (cl-letf (((symbol-function 'read-char-exclusive) (lambda (&rest _) ?t))
                    ((symbol-function 'completing-read) (lambda (&rest _) "DONE")))
            (orgx-agenda-bulk-action '(4)))))
      (let ((plan (orgx-bulk-get-plan
                   (buffer-local-value 'orgx-bulk-batch-plan-id buf))))
        (should (eq (orgx-bulk-plan-status plan) 'complete))
        (with-current-buffer (find-file-noselect f1)
          (should (equal (org-map-entries #'org-get-todo-state) '("DONE" "DONE"))))))))

(provide 'orgx-bulk-agenda-test)
;;; orgx-bulk-agenda-test.el ends here
