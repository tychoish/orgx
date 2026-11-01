#!/usr/bin/env bash
set -euo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

echo "=== Running Elisp ERT tests ==="
"${REPO_ROOT}/scripts/test-elisp.sh"

echo "=== Running Rust CLI tests ==="
"${REPO_ROOT}/scripts/test-cli.sh"
