#!/usr/bin/env bash
set -euo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

emacs -Q --batch \
  -L "${REPO_ROOT}/lisp" \
  -L "${REPO_ROOT}/test" \
  -l ert \
  --eval "(dolist (f (directory-files \"${REPO_ROOT}/test\" t \"test-.*\\\\.el$\")) (load-file f))" \
  -f ert-run-tests-batch-and-exit
