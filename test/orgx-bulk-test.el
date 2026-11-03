;;; orgx-bulk-test.el --- Tests for orgx-bulk.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'org)
(require 'orgx-bulk)

(defmacro orgx-bulk-test--with-file (var content &rest body)
  "Bind VAR to a temp org file holding CONTENT and run BODY."
  (declare (indent 2))
  `(let ((,var (make-temp-file "orgx-bulk" nil ".org" ,content))
         (org-todo-keywords '((sequence "TODO" "INPROGRESS" "|" "DONE" "CANCELED")))
         (orgx-bulk--plans (make-hash-table :test #'equal)))
     (unwind-protect
         (progn ,@body)
       (when-let* ((b (find-buffer-visiting ,var)))
         (with-current-buffer b (set-buffer-modified-p nil))
         (kill-buffer b))
       (delete-file ,var))))

(defun orgx-bulk-test--states (file)
  (with-current-buffer (find-file-noselect file)
    (org-map-entries #'org-get-todo-state)))

(ert-deftest orgx-bulk-collect-skips-done-test ()
  (orgx-bulk-test--with-file f "* TODO a\n* DONE b\n* INPROGRESS c\n* plain\n"
    (should (equal (seq-map #'orgx-bulk-item-heading
                            (orgx-bulk-collect-files :files (list f)))
                   '("a" "c")))
    (should (equal (seq-map #'orgx-bulk-item-heading
                            (orgx-bulk-collect-files :files (list f) :from '("DONE")))
                   '("b")))))

(ert-deftest orgx-bulk-apply-selected-only-test ()
  (orgx-bulk-test--with-file f "* TODO a\n* TODO b\n* TODO c\n"
    (let ((plan (orgx-bulk-plan "CANCELED" (orgx-bulk-collect-files :files (list f)))))
      (orgx-bulk-plan-set-selection plan '(1 3))
      (orgx-bulk-apply plan)
      (should (equal (orgx-bulk-test--states f) '("CANCELED" "TODO" "CANCELED")))
      (should (eq (orgx-bulk-plan-status plan) 'applied))
      (should-error (orgx-bulk-apply plan) :type 'user-error))))

(ert-deftest orgx-bulk-invalid-target-deselected-test ()
  (orgx-bulk-test--with-file f "* TODO a\n"
    (let ((plan (orgx-bulk-plan "BOGUS" (orgx-bulk-collect-files :files (list f)))))
      (should-not (orgx-bulk-plan-selected plan))
      (should (orgx-bulk-item-problem (car (orgx-bulk-plan-items plan)))))))

(ert-deftest orgx-bulk-hooks-test ()
  (orgx-bulk-test--with-file f "* TODO a\n* TODO b\n"
    (let* ((per-item nil)
           (per-file nil)
           (orgx-bulk-item-functions (list (lambda (item _p) (push (orgx-bulk-item-heading item) per-item))))
           (orgx-bulk-after-apply-functions (list (lambda (file items _p) (push (cons file (length items)) per-file))))
           (plan (orgx-bulk-plan "DONE" (orgx-bulk-collect-files :files (list f)))))
      (orgx-bulk-apply plan)
      (should (equal (sort per-item #'string<) '("a" "b")))
      (should (equal per-file (list (cons f 2)))))))

(ert-deftest orgx-bulk-saves-buffer-test ()
  (orgx-bulk-test--with-file f "* TODO a\n"
    (orgx-bulk-apply (orgx-bulk-plan "DONE" (orgx-bulk-collect-files :files (list f))))
    (should (string-match-p "^\\* DONE a" (with-temp-buffer (insert-file-contents f) (buffer-string))))))

(ert-deftest orgx-bulk-denote-status-sync-test ()
  (orgx-bulk-test--with-file f "#+execution_status: in-progress\n* TODO a\n* TODO b\n"
    (cl-letf (((symbol-function 'orgx-denote-plan-file-p) (lambda (_) t)))
      (let ((plan (orgx-bulk-plan "DONE" (orgx-bulk-collect-files :files (list f)))))
        (orgx-bulk-plan-set-selection plan '(1))
        (orgx-bulk-apply plan)
        (should (string-match-p "execution_status: in-progress"
                                (with-temp-buffer (insert-file-contents f) (buffer-string))))
        (orgx-bulk-apply (orgx-bulk-plan "DONE" (orgx-bulk-collect-files :files (list f))))
        (should (string-match-p "execution_status: complete"
                                (with-temp-buffer (insert-file-contents f) (buffer-string))))))))

(ert-deftest orgx-bulk-review-buffer-test ()
  (orgx-bulk-test--with-file f "* TODO a\n* TODO b\n"
    (let* ((plan (orgx-bulk-plan "CANCELED" (orgx-bulk-collect-files :files (list f))))
           (buf (orgx-bulk-review plan)))
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (search-forward "TODO")
            (orgx-bulk-review-toggle)
            (should-not (orgx-bulk-item-selected (car (orgx-bulk-plan-items plan))))
            (orgx-bulk-review-confirm)
            (should (equal (orgx-bulk-test--states f) '("TODO" "CANCELED"))))
        (kill-buffer buf)))))

(ert-deftest orgx-bulk-summary-json-ready-test ()
  (orgx-bulk-test--with-file f "* TODO a\n"
    (let ((s (orgx-bulk-plan-summary (orgx-bulk-plan "DONE" (orgx-bulk-collect-files :files (list f))))))
      (should (equal (plist-get s :status) "pending"))
      (should (vectorp (plist-get s :items))))))

(defun orgx-bulk-test--plan (f target &optional from)
  (orgx-bulk-plan target (orgx-bulk-collect-files :files (list f) :from from)))

(defmacro orgx-bulk-test--without-ui (&rest body)
  "Run BODY with window-showing functions stubbed."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'display-buffer) #'ignore)
             ((symbol-function 'pop-to-buffer) (lambda (b &rest _) (set-buffer b)))
             ((symbol-function 'quit-window) #'ignore))
     ,@body))

(ert-deftest orgx-bulk-collect-agenda-test ()
  (orgx-bulk-test--with-file f "* TODO a\n* DONE b\n* plain\n* TODO c\n"
    (let* ((base (find-file-noselect f))
           (markers (with-current-buffer base
                      (org-map-entries #'point-marker))))
      (with-temp-buffer
        (seq-do (lambda (m)
                  (insert (propertize "line\n" 'org-hd-marker m)))
                markers)
        (insert "no marker line\n")
        (let ((org-agenda-bulk-marked-entries nil))
          ;; Fallback: every line with a marker that is a todo (not plain).
          (should (equal (seq-map #'orgx-bulk-item-heading (orgx-bulk-collect-agenda))
                         '("a" "b" "c"))))
        (let ((org-agenda-bulk-marked-entries (list (nth 3 markers) (nth 2 markers))))
          ;; Marked entries win; non-todo headings are dropped.
          (should (equal (seq-map #'orgx-bulk-item-heading (orgx-bulk-collect-agenda))
                         '("c"))))))))

(ert-deftest orgx-bulk-collect-agenda-dead-marker-test ()
  (let ((org-agenda-bulk-marked-entries (list (make-marker))))
    (should-not (orgx-bulk-collect-agenda))))

(ert-deftest orgx-bulk-already-target-state-test ()
  (orgx-bulk-test--with-file f "* TODO a\n* DONE b\n"
    (let* ((plan (orgx-bulk-test--plan f "DONE" '("TODO" "DONE")))
           (a (nth 0 (orgx-bulk-plan-items plan)))
           (b (nth 1 (orgx-bulk-plan-items plan))))
      (should (orgx-bulk-item-selected a))
      (should-not (orgx-bulk-item-problem a))
      (should (equal (orgx-bulk-item-problem b) "already DONE"))
      (should-not (orgx-bulk-item-selected b))
      (should (equal (seq-map #'orgx-bulk-item-id (orgx-bulk-plan-selected plan)) '(1))))))

(ert-deftest orgx-bulk-blocked-state-test ()
  (orgx-bulk-test--with-file f "* TODO a\n"
    (let ((plan (orgx-bulk-test--plan f "DONE"))
          (org-blocker-hook (list #'ignore)))
      (orgx-bulk-apply plan)
      (let ((item (car (orgx-bulk-plan-items plan))))
        (should (eq (orgx-bulk-item-result item) 'failed))
        (should (string-match-p "unchanged" (orgx-bulk-item-problem item))))
      (should (equal (orgx-bulk-test--states f) '("TODO")))
      (should (eq (orgx-bulk-plan-status plan) 'applied)))))

(ert-deftest orgx-bulk-org-todo-error-test ()
  (orgx-bulk-test--with-file f "* TODO a\n"
    (let ((plan (orgx-bulk-test--plan f "DONE")))
      (cl-letf (((symbol-function 'org-todo) (lambda (&rest _) (error "Boom"))))
        (orgx-bulk-apply plan))
      (let ((item (car (orgx-bulk-plan-items plan))))
        (should (eq (orgx-bulk-item-result item) 'failed))
        (should (equal (orgx-bulk-item-problem item) "Boom"))))))

(ert-deftest orgx-bulk-set-selection-test ()
  (orgx-bulk-test--with-file f "* TODO a\n* TODO b\n"
    (let ((plan (orgx-bulk-test--plan f "DONE")))
      (orgx-bulk-plan-set-selection plan '(2))
      (should (equal (seq-map #'orgx-bulk-item-selected (orgx-bulk-plan-items plan)) '(nil t)))
      (orgx-bulk-plan-set-selection plan nil)
      (should-not (orgx-bulk-plan-selected plan)))))

(ert-deftest orgx-bulk-plan-ids-increment-test ()
  (orgx-bulk-test--with-file f "* TODO a\n"
    (let ((p1 (orgx-bulk-test--plan f "DONE"))
          (p2 (orgx-bulk-test--plan f "DONE")))
      (should-not (equal (orgx-bulk-plan-id p1) (orgx-bulk-plan-id p2)))
      (should (eq (orgx-bulk-get-plan (orgx-bulk-plan-id p2)) p2))
      (should-not (orgx-bulk-get-plan "nope")))))

(ert-deftest orgx-bulk-review-select-all-none-test ()
  (orgx-bulk-test--with-file f "* TODO a\n* DONE b\n* TODO c\n"
    (orgx-bulk-test--without-ui
      (let* ((plan (orgx-bulk-test--plan f "DONE" '("TODO" "DONE")))
             (buf (orgx-bulk-review plan)))
        (unwind-protect
            (with-current-buffer buf
              (orgx-bulk-review-select-none)
              (should-not (orgx-bulk-plan-selected plan))
              (should (string-match-p "0 of 3 selected" (buffer-string)))
              (orgx-bulk-review-select-all)
              ;; b is already DONE, so it keeps its problem and stays unselected.
              (should (equal (seq-map #'orgx-bulk-item-id (orgx-bulk-plan-selected plan)) '(1 3)))
              (should (string-match-p "-- already DONE" (buffer-string)))
              (orgx-bulk-review-refresh))
          (kill-buffer buf))))))

(ert-deftest orgx-bulk-review-toggle-problem-test ()
  (orgx-bulk-test--with-file f "* DONE a\n"
    (orgx-bulk-test--without-ui
      (let* ((plan (orgx-bulk-test--plan f "DONE" '("DONE")))
             (buf (orgx-bulk-review plan)))
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-min))
              (search-forward "already")
              (should-error (orgx-bulk-review-toggle) :type 'user-error)
              (goto-char (point-min))
              (should-error (orgx-bulk-review-toggle) :type 'user-error))
          (kill-buffer buf))))))

(ert-deftest orgx-bulk-review-confirm-nothing-and-after-apply-test ()
  (orgx-bulk-test--with-file f "* TODO a\n"
    (orgx-bulk-test--without-ui
      (let* ((plan (orgx-bulk-test--plan f "DONE"))
             (buf (orgx-bulk-review plan)))
        (unwind-protect
            (with-current-buffer buf
              (orgx-bulk-review-select-none)
              (should-error (orgx-bulk-review-confirm) :type 'user-error)
              (orgx-bulk-review-select-all)
              (orgx-bulk-review-confirm)
              (should (string-match-p "^\\[✓\\]" (buffer-string)))
              (should-error (orgx-bulk-review-toggle) :type 'user-error)
              (should-error (orgx-bulk-review-select-all) :type 'user-error))
          (kill-buffer buf))))))

(ert-deftest orgx-bulk-review-failed-marker-test ()
  (orgx-bulk-test--with-file f "* TODO a\n"
    (orgx-bulk-test--without-ui
      (let ((plan (orgx-bulk-test--plan f "DONE"))
            (org-blocker-hook (list #'ignore)))
        (orgx-bulk-apply plan)
        (let ((buf (orgx-bulk-review plan)))
          (unwind-protect
              (should (string-match-p "^\\[!\\]" (with-current-buffer buf (buffer-string))))
            (kill-buffer buf)))))))

(ert-deftest orgx-bulk-review-visit-test ()
  (orgx-bulk-test--with-file f "* TODO a\n* TODO b\n"
    (orgx-bulk-test--without-ui
      (let* ((plan (orgx-bulk-test--plan f "DONE"))
             (buf (orgx-bulk-review plan)))
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-min))
              (should-error (orgx-bulk-review-visit) :type 'user-error)
              (search-forward "TODO")
              (search-forward "TODO")
              (orgx-bulk-review-visit)
              (should (eq (current-buffer) (find-buffer-visiting f)))
              (should (looking-at "\\* TODO b")))
          (kill-buffer buf))))))

(ert-deftest orgx-bulk-review-quit-test ()
  (orgx-bulk-test--with-file f "* TODO a\n"
    (orgx-bulk-test--without-ui
      (let* ((plan (orgx-bulk-test--plan f "DONE"))
             (buf (orgx-bulk-review plan)))
        (with-current-buffer buf
          (orgx-bulk-review-quit))
        (should (eq (orgx-bulk-plan-status plan) 'discarded))
        (should-not (buffer-live-p buf))))))

(ert-deftest orgx-bulk-review-quit-applied-keeps-status-test ()
  (orgx-bulk-test--with-file f "* TODO a\n"
    (orgx-bulk-test--without-ui
      (let* ((plan (orgx-bulk-test--plan f "DONE"))
             (buf (orgx-bulk-review plan)))
        (orgx-bulk-apply plan)
        (with-current-buffer buf
          (orgx-bulk-review-quit))
        (should (eq (orgx-bulk-plan-status plan) 'applied))))))

(ert-deftest orgx-bulk-discard-test ()
  (orgx-bulk-test--with-file f "* TODO a\n"
    (orgx-bulk-test--without-ui
      (let* ((plan (orgx-bulk-test--plan f "DONE"))
             (buf (orgx-bulk-review plan)))
        (should (eq (orgx-bulk-discard plan) plan))
        (should (eq (orgx-bulk-plan-status plan) 'discarded))
        (should-not (buffer-live-p buf))
        ;; Without a review buffer it still works.
        (should (eq (orgx-bulk-discard plan) plan))))))

(ert-deftest orgx-bulk-read-scope-test ()
  (orgx-bulk-test--with-file f "* TODO a :x:\n* TODO b\n"
    (let ((scope nil)
          (match "")
          (org-agenda-files (list f)))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) scope))
                ((symbol-function 'read-string) (lambda (&rest _) match))
                ((symbol-function 'orgx-denote-files) (lambda () (list f))))
        (with-current-buffer (find-file-noselect f)
          (setq scope "buffer")
          (pcase-let ((`(,items . ,desc) (orgx-bulk--read-scope)))
            (should (= (length items) 2))
            (should (string-match-p "^Buffer " desc)))
          (setq match "x")
          (should (equal (seq-map #'orgx-bulk-item-heading (car (orgx-bulk--read-scope)))
                         '("a")))
          (setq scope "agenda-files")
          (pcase-let ((`(,items . ,desc) (orgx-bulk--read-scope)))
            (should (= (length items) 1))
            (should (equal desc "Agenda files")))
          (setq scope "denote" match "")
          (pcase-let ((`(,items . ,desc) (orgx-bulk--read-scope)))
            (should (= (length items) 2))
            (should (equal desc "Denote files"))))
        (with-temp-buffer
          (setq major-mode 'org-agenda-mode)
          (let ((org-agenda-bulk-marked-entries nil))
            (pcase-let ((`(,items . ,desc) (orgx-bulk--read-scope)))
              (should-not items)
              (should (equal desc "Agenda entries")))))))))

(ert-deftest orgx-bulk-todo-state-test ()
  (orgx-bulk-test--with-file f "* TODO a\n"
    (orgx-bulk-test--without-ui
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "buffer"))
                ((symbol-function 'read-string) (lambda (&rest _) "")))
        (with-current-buffer (find-file-noselect f)
          (let ((buf (orgx-bulk-todo-state "DONE")))
            (unwind-protect
                (let ((plan (buffer-local-value 'orgx-bulk--review-plan buf)))
                  (should (equal (orgx-bulk-plan-target plan) "DONE"))
                  (should (eq (orgx-bulk-plan-status plan) 'pending)))
              (kill-buffer buf))))))))

(ert-deftest orgx-bulk-todo-state-no-match-test ()
  (orgx-bulk-test--with-file f "* plain\n"
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "buffer"))
              ((symbol-function 'read-string) (lambda (&rest _) "")))
      (with-current-buffer (find-file-noselect f)
        (should-error (orgx-bulk-todo-state "DONE") :type 'user-error)))))

(ert-deftest orgx-bulk-plan-summary-fields-test ()
  (orgx-bulk-test--with-file f "* TODO a\n* TODO b\n"
    (let* ((plan (orgx-bulk-test--plan f "DONE"))
           (_ (orgx-bulk-plan-set-selection plan '(1)))
           (_ (setf (orgx-bulk-item-problem (nth 1 (orgx-bulk-plan-items plan))) "oops"))
           (_ (setf (orgx-bulk-item-file (nth 1 (orgx-bulk-plan-items plan))) nil))
           (s (orgx-bulk-plan-summary plan))
           (items (plist-get s :items))
           (a (aref items 0))
           (b (aref items 1)))
      (should (equal (plist-get s :target_state) "DONE"))
      (should (equal (plist-get s :plan_id) (orgx-bulk-plan-id plan)))
      (should (= (plist-get s :selected) 1))
      (should (= (length items) 2))
      (should (equal (plist-get a :file) f))
      (should (equal (plist-get a :heading) "a"))
      (should (equal (plist-get a :from_state) "TODO"))
      (should (eq (plist-get a :selected) t))
      (should (eq (plist-get a :result) :null))
      (should (eq (plist-get a :problem) :null))
      (should (equal (plist-get a :path) []))
      (should (eq (plist-get b :selected) :json-false))
      (should (eq (plist-get b :file) :null))
      (should (equal (plist-get b :problem) "oops"))
      (orgx-bulk-apply plan)
      (should (equal (plist-get (aref (plist-get (orgx-bulk-plan-summary plan) :items) 0) :result)
                     "changed")))))

(provide 'orgx-bulk-test)
;;; orgx-bulk-test.el ends here
