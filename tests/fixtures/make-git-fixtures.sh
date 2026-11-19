#!/usr/bin/env bash
# Regenerate tests/fixtures/git/<case>.diff from the committed before/ and after/
# trees with real git (and plain.diff with diff -u). Run once after changing a
# tree; commit the .diff files. Not run by the test suite.
#
# Modes: renames/after/run.sh is made executable in the index (a mode change),
# so the trees themselves carry no mode bits.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
git_dir="$here/git"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

gitq() {
  git -c core.autocrlf=false -c core.safecrlf=false -c core.quotePath=true \
      -c diff.noprefix=false -c diff.mnemonicPrefix=false -c diff.renames=true \
      -c user.name=fixture -c user.email=fixture@example.invalid \
      -c init.defaultBranch=main "$@"
}

make_case() {
  local name="$1"
  local repo="$work/$name"
  mkdir -p "$repo"
  gitq -C "$repo" init -q
  cp -R "$git_dir/$name/before/." "$repo/"
  gitq -C "$repo" add -A
  gitq -C "$repo" commit -q -m before
  find "$repo" -mindepth 1 -maxdepth 1 ! -name .git -exec rm -rf {} +
  cp -R "$git_dir/$name/after/." "$repo/"
  gitq -C "$repo" add -A
  if [[ "$name" == renames ]]; then
    gitq -C "$repo" update-index --chmod=+x run.sh
  fi
  gitq -C "$repo" diff --cached -M --no-color --no-ext-diff --no-textconv > "$git_dir/$name.diff"
}

make_case basic
make_case renames
make_case binary

# plain diff -u of one file (headers "--- before/f.txt <time>" / "+++ after/f.txt <time>")
( cd "$git_dir/plain" && diff -u before/f.txt after/f.txt > "$git_dir/plain.diff" ) || [[ $? -eq 1 ]]

echo "wrote: $(cd "$git_dir" && ls ./*.diff | tr '\n' ' ')"
