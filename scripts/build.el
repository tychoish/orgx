#!/usr/bin/env -S emacs --script
;;; build.el --- Batch byte-compilation and autoload generation -*- lexical-binding: t; -*-

(require 'bytecomp)
(require 'seq)

(let* ((script-dir (file-name-directory (or load-file-name buffer-file-name (buffer-file-name))))
       (root-dir (expand-file-name ".." script-dir))
       (lisp-dir (expand-file-name "lisp" root-dir))
       (loaddefs-file (expand-file-name "orgx-loaddefs.el" lisp-dir)))

  (add-to-list 'load-path lisp-dir)
  (setq byte-compile-error-on-warn t)

  (message "==> Generating autoloads in %s..." lisp-dir)
  (if (fboundp 'loaddefs-generate)
      (loaddefs-generate lisp-dir loaddefs-file)
    (with-no-warnings
      (make-directory-autoloads lisp-dir loaddefs-file)))

  (message "==> Byte-compiling lisp files in %s..." lisp-dir)
  (let ((files (directory-files lisp-dir t "\\`orgx.*\\.el\\'")))
    (dolist (file files)
      (unless (string-suffix-p "orgx-loaddefs.el" file)
        (message "Compiling %s..." (file-name-nondirectory file))
        (unless (byte-compile-file file)
          (message "Compilation failed for %s" file)
          (kill-emacs 1)))))

  (message "==> Build completed successfully!"))
