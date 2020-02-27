#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o xtrace

cd "$(dirname "${BASH_SOURCE[0]}")"

trap 'rm -f esc.elc' EXIT

readonly init_package_el="(progn
  (require 'package)
  (push '(\"melpa\" . \"http://melpa.org/packages/\") package-archives)
  (package-initialize)
  (unless package-archive-contents
     (package-refresh-contents))
  (dolist (pkg '(package-lint))
    (unless (package-installed-p pkg)
      (package-install pkg))))"

emacs \
  -Q \
  -batch \
  --eval "(setq byte-compile-error-on-warn t)" \
  -f batch-byte-compile \
  esc.el

# emacs \
#   -Q \
#   -batch \
#   --eval "$init_package_el" \
#   -l package-lint.el \
#   -f package-lint-batch-and-exit \
#   esc.el

emacs \
  -Q \
  -batch \
  -l esc.el \
  -l esc-test.el \
  -f ert-run-tests-batch-and-exit
