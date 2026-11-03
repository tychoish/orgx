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

(provide 'orgx-bulk-test)
;;; orgx-bulk-test.el ends here
