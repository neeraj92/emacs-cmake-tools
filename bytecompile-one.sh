#!/usr/bin/env sh
set -eu

repo_root="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
cd "$repo_root"

if [ "$#" -lt 1 ] || [ -z "${1:-}" ]; then
  echo "usage: $0 FILE.el" >&2
  exit 1
fi

f="$1"
case "$f" in
  /* | ./*) ;;
  *) f="./$f" ;;
esac

if [ ! -f "$f" ]; then
  echo "$0: not a file: $f" >&2
  exit 1
fi

case "$f" in
  *.el) ;;
  *) echo "$0: expected .el file: $f" >&2
     exit 1 ;;
esac

base="${f%.el}"
elc="${base}.elc"

rm -f -- "$elc"

emacs -Q --batch -L . -f batch-byte-compile "$f"

rm -f -- "$elc"
