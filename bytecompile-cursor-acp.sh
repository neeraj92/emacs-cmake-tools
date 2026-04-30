#!/usr/bin/env sh
set -eu

repo_root="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
cd "$repo_root"

rm -f -- ./*.elc

emacs -Q --batch -L . -f batch-byte-compile \
  cursor-acp.el \
  cursor-acp-core.el \
  cursor-acp-transport.el \
  cursor-acp-ui.el \
  cursor-acp-commands.el

rm -f -- ./*.elc

