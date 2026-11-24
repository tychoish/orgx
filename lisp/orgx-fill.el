;;; orgx-fill.el --- Fill policy for headings, blocks and other no-fill regions -*- lexical-binding: t; -*-

;; Author: tychoish <tychoish@tychoish.com>
;; URL: https://github.com/tychoish/orgx
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: org, text, convenience

;;; Commentary:
;; Decides where `fill-paragraph' must not act, and fills whole buffers
;; with that policy applied.
;;
;; Org structure is covered here: headings, keyword lines, drawers,
;; planning and clock lines, and source/example/export/fixed-width
;; blocks.  Other modes plug in through two lists:
;;
;; - `orgx-fill-suppress-functions': predicates of no arguments; any
;;   non-nil result means "do not fill at point".
;; - `orgx-fill-handlers': functions of no arguments that fill a special
;;   region themselves (see `orgx-fill-with-prefix') and return non-nil
;;   when they did.  Handlers run before the suppress predicates.
;;
;; `orgx-fill-guard-mode' installs the policy as `:around' advice on
;; `orgx-fill-advised-functions'.  Loading this file installs nothing.

;;; Code:

(require 'org-element)
(require 'seq)
(require 'subr-x)

(defgroup orgx-fill nil
  "Fill policy for headings, blocks and other no-fill regions."
  :group 'orgx
  :prefix "orgx-fill-")

(defcustom orgx-fill-org-element-types
  '(headline keyword property-drawer node-property drawer planning clock
    babel-call src-block example-block export-block fixed-width verse-block
    comment-block latex-environment table table-row quote-block special-block
    center-block dynamic-block)
  "Org element types that `fill-paragraph' must leave untouched.
Greater blocks (quote, special, center, dynamic) are listed so that
their opening line is skipped on its own and `orgx-fill-buffer' then
descends into their contents instead of skipping the whole block."
  :type '(repeat symbol)
  :group 'orgx-fill)

(defcustom orgx-fill-column 72
  "Default fill column for `orgx-fill-buffer'."
  :type 'integer
  :group 'orgx-fill)

(defun orgx-fill-org-suppressed-p ()
  "Return non-nil when point is in an Org element that must not be filled."
  (and (derived-mode-p 'org-mode)
       (memq (org-element-type (org-element-at-point))
             orgx-fill-org-element-types)))

(defvar orgx-fill-suppress-functions '(orgx-fill-org-suppressed-p)
  "Predicates of no arguments; any non-nil result suppresses filling at point.")

(defvar orgx-fill-handlers nil
  "Functions of no arguments that fill a special region at point.
Each returns non-nil when it handled filling.")

(defvar orgx-fill-advised-functions '(fill-paragraph)
  "Functions `orgx-fill-guard-mode' wraps with the fill policy.")

(defun orgx-fill-suppressed-p ()
  "Return non-nil when any of `orgx-fill-suppress-functions' says to skip."
  (seq-some #'funcall orgx-fill-suppress-functions))

(defun orgx-fill-handled-p ()
  "Return non-nil when one of `orgx-fill-handlers' filled at point."
  (seq-some #'funcall orgx-fill-handlers))

(defun orgx-fill-with-prefix (beg end indent)
  "Fill the text between BEG and END, indenting continuation lines by INDENT.
Paragraphs are separated by blank lines, independent of the major mode's
own paragraph rules."
  (let ((fill-prefix (make-string indent ?\s))
        (fill-forward-paragraph-function #'forward-paragraph))
    (fill-region beg end)))

(defun orgx-fill-paragraph-at-point ()
  "Fill at point under the policy; return non-nil unless suppressed."
  (cond
   ((orgx-fill-handled-p) t)
   ((orgx-fill-suppressed-p) nil)
   (t (fill-paragraph) t)))

(defun orgx-fill-buffer (&optional column)
  "Fill every paragraph in the current buffer to COLUMN under the policy.
COLUMN defaults to `orgx-fill-column'.  Point is preserved.  Return
non-nil when the buffer changed."
  (let ((fill-column (or column orgx-fill-column))
        (before (buffer-hash)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((start (point)))
          (cond
           ((looking-at-p "[ \t]*$")
            (forward-line 1))
           ((or (orgx-fill-handled-p) (orgx-fill-suppressed-p))
            (forward-line 1))
           (t
            (fill-paragraph)
            (forward-paragraph)
            (unless (bolp)
              (forward-line 1))))
          (when (<= (point) start)
            (goto-char start)
            (forward-line 1)))))
    (not (equal before (buffer-hash)))))

(defun ad:orgx-fill-guard (fn &rest args)
  "Around advice for FN: apply the fill policy before filling with ARGS."
  (or (orgx-fill-handled-p)
      (orgx-fill-suppressed-p)
      (apply fn args)))

;;;###autoload
(define-minor-mode orgx-fill-guard-mode
  "Apply the orgx fill policy to `orgx-fill-advised-functions'."
  :global t
  :group 'orgx-fill
  (seq-do (lambda (fn)
            (if orgx-fill-guard-mode
                (advice-add fn :around #'ad:orgx-fill-guard)
              (advice-remove fn #'ad:orgx-fill-guard)))
          orgx-fill-advised-functions))

(provide 'orgx-fill)
;;; orgx-fill.el ends here
