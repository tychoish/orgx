#!/usr/bin/env bash
set -euo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

emacs -Q --batch \
  -L "${REPO_ROOT}/lisp" \
  --eval '(setq byte-compile-error-on-warn t)' \
  -f batch-byte-compile "${REPO_ROOT}"/lisp/*.el
