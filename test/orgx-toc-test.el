;;; orgx-toc-test.el --- Tests for orgx-toc -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'orgx-toc)

(ert-deftest orgx-toc-update-directory-only-touches-toc-files ()
  (let ((dir (make-temp-file "orgx-toc" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "docs" dir))
          (with-temp-file (expand-file-name "README.org" dir)
            (insert "* Contents :TOC:\n* One\n* Two\n"))
          (with-temp-file (expand-file-name "docs/plain.org" dir)
            (insert "* No toc here\n"))
          (cl-letf (((symbol-function 'orgx-toc-update-file) #'ignore))
            (should (equal (mapcar #'file-name-nondirectory (orgx-toc-update-directory dir))
                           '("README.org")))))
      (delete-directory dir t))))

(provide 'orgx-toc-test)
;;; orgx-toc-test.el ends here
