#!/usr/bin/env -S emacs --script
;;; rust.el --- Rust automation via builder.el -*- lexical-binding: t; -*-

(require 'package)
(package-initialize)

(add-to-list 'load-path "/home/tychoish/src/builder")
(require 'builder-rust)

(let* ((script-dir (file-name-directory (or load-file-name buffer-file-name (buffer-file-name))))
       (root-dir (expand-file-name ".." script-dir))
       (args (or command-line-args-left '("test")))
       (action-str (pop args)))

  (message "==> Rust runner (powered by builder.el) for %s" root-dir)
  (let ((action (pcase action-str
                  ("test" :test)
                  ("build" :build)
                  ("release" :build-release)
                  ("check" :check)
                  ("clippy" :clippy)
                  (_ (intern (concat ":" action-str))))))
    (message "==> Executing %s in %s..." action root-dir)
    (let ((res (builder-rust-batch action root-dir)))
      (if (zerop res)
          (message "==> Rust operation %s succeeded!" action)
        (message "==> Rust operation %s failed with exit code %d" action res)
        (kill-emacs res)))))
