#!/usr/bin/env -S emacs --script
;;; test.el --- Batch test runner for orgx -*- lexical-binding: t; -*-

(require 'ert)
(require 'seq)

(let* ((script-dir (file-name-directory (or load-file-name buffer-file-name (buffer-file-name))))
       (root-dir (expand-file-name ".." script-dir))
       (lisp-dir (expand-file-name "lisp" root-dir))
       (test-dir (expand-file-name "test" root-dir)))

  (add-to-list 'load-path lisp-dir)
  (add-to-list 'load-path test-dir)

  (let ((loaddefs (expand-file-name "orgx-loaddefs.el" lisp-dir)))
    (when (file-exists-p loaddefs)
      (load-file loaddefs)))

  (let ((test-files (directory-files test-dir t "\\`-?.*-test\\.el\\'")))
    (message "==> Running ERT tests (%d test files)..." (length test-files))
    (dolist (tf test-files)
      (message "Loading %s..." (file-name-nondirectory tf))
      (load-file tf))
    (ert-run-tests-batch-and-exit)))
