#!/bin/sh
# Does the snapshot in this folder still match the renderer that actually runs?
# The originals live in ../../_syllabus-template/ and are shared across courses,
# so they change without this book knowing. A drifted snapshot is worse than no
# snapshot: it answers questions wrongly. Run this, and if it reports drift,
# either refresh the copy or find out what changed and why.
cd "$(dirname "$0")" || exit 1
TPL="../../../_syllabus-template"
[ -d "$TPL" ] || { echo "no renderer at $TPL"; exit 1; }
drift=0
while read -r want file; do
  case "$want" in \#*|"") continue;; esac
  have=$(shasum -a 256 "$TPL/$file" 2>/dev/null | cut -d' ' -f1)
  if [ "$want" != "$have" ]; then echo "DRIFTED  $file"; drift=1; fi
done < MANIFEST.txt
[ "$drift" -eq 0 ] && echo "snapshot matches the live renderer"
exit $drift
