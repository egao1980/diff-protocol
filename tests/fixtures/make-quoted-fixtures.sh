#!/usr/bin/env bash
# Regenerate tests/fixtures/git/quoted*.diff with real git. Run once after
# changing a case below; commit the .diff files. Not run by the test suite.
#
# Unlike make-git-fixtures.sh the before / after trees are not committed: their
# file names hold a TAB, a newline, '"' and '\', which a Windows checkout cannot
# create. The same trees are spelled out in tests/git-quoted-test.lisp.
#
#   quoted.diff       core.quotePath=true  (git's default: non-ASCII as \ooo)
#   quoted-off.diff   core.quotePath=off   (non-ASCII raw UTF-8; control
#                                           characters, '"' and '\' still quoted)
#   copies.diff       -C --find-copies-harder: "copy from" / "copy to"
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
git_dir="$here/git"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

gitq() {
  git -c core.autocrlf=false -c core.safecrlf=false \
      -c diff.noprefix=false -c diff.mnemonicPrefix=false -c diff.renames=true \
      -c user.name=fixture -c user.email=fixture@example.invalid \
      -c init.defaultBranch=main "$@"
}

repo="$work/quoted"
mkdir -p "$repo"
gitq -C "$repo" init -q
(
  cd "$repo"
  printf 'one\ntwo\n' > $'tab\there.txt'
  printf '1\n2\n3\n4\n5\n' > 'quote"d.txt'
  printf 'a\nb\nc\nd\ne\n' > 'plain.txt'
  printf 'gone\n' > 'back\slash.txt'
  printf 'x\n' > $'caf\xc3\xa9.txt'
  printf 'echo\n' > $'mode\tx.sh'
)
gitq -C "$repo" add -A
gitq -C "$repo" commit -q -m before
(
  cd "$repo"
  printf 'one\nTWO\n' > $'tab\there.txt'
  rm 'quote"d.txt'
  printf '1\n2\n3\n4\nFIVE\n' > $'renamed\ttoo.txt'
  mv 'plain.txt' 'new"name.txt'
  rm 'back\slash.txt'
  printf 'y\n' > $'caf\xc3\xa9.txt'
  printf 'new\n' > $'nl\nname.txt'
  printf '\000\001\002' > $'bin\tfile.bin'
)
gitq -C "$repo" add -A
gitq -C "$repo" update-index --chmod=+x $'mode\tx.sh'
for qp in true off; do
  out="$git_dir/quoted.diff"
  [[ "$qp" == off ]] && out="$git_dir/quoted-off.diff"
  gitq -C "$repo" -c core.quotePath="$qp" \
       diff --cached -M --no-color --no-ext-diff --no-textconv > "$out"
done

copies="$work/copies"
mkdir -p "$copies"
gitq -C "$copies" init -q
printf 'same\ncontent\n' > "$copies/"$'src\tfile.txt'
gitq -C "$copies" add -A
gitq -C "$copies" commit -q -m before
cp "$copies/"$'src\tfile.txt' "$copies/"$'copy\tfile.txt'
gitq -C "$copies" add -A
gitq -C "$copies" diff --cached -C --find-copies-harder --no-color --no-ext-diff > "$git_dir/copies.diff"

echo "wrote: quoted.diff quoted-off.diff copies.diff"
