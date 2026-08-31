#!/bin/sh
# ---------------------------------------------------------------------------
# Run every corpus check. From anywhere:
#
#     sh book/labs/_lib/check-all.sh
#
# Each check answers a different question, and they are ordered by how cheap
# they are to run and how loudly they fail:
#
#   layout    is every file where the convention says, and does every path
#             in every brief and build script point at something that exists
#   sources   can a reader get the data -- does every chapter that reads data
#             give its publisher's address, and hand over its own tables
#   stamps    does every chapter's BUILD-STAMP.tsv still describe its data --
#             the right files, and each one still hashing to what it recorded
#   vacuous   is any check in the corpus unable to fail
#   tables    is anything wrong inside the derived tables themselves --
#             truncation, an undeclared top-N, precision nobody measured
#   figures   is anything wrong in the RENDER that is invisible in the source --
#             the same figure emitted twice, a figure with no static fallback
#   captions   is any figure captioned twice, once by the figure and once by a
#             bold Figure N paragraph saying the same thing. Advisory: it
#             reports a backlog, and gating on it would stop every render
#   template  does each document end the way its type's 3rd-edition skeleton
#             ends, and does its title name the data. Advisory for the same
#             reason as captions; DD_STRICT_TEMPLATE=1 turns it into a gate
#   contrast  is any figure text unreadable on the dark page, measured against
#             what is actually painted behind it. OPT-IN (--contrast): it needs
#             a browser and takes minutes, so it is the one check here that
#             does not obey the "seconds" rule below.
#
# None of them opens the network or runs a build. Together they take seconds,
# which is the point: the expensive verification is rendering the corpus, and
# these are what you run before bothering. `stamps` reads every byte of the
# corpus's data -- about 2 GB -- and still comes in around a second and a half,
# because it is all on local disk.
#
# Exit status is 0 only if all of them are clean, so this can gate a commit.
#
# `figures` reads the rendered HTML rather than the source, so it reports on the
# LAST render rather than on the working tree. It is advisory here for that
# reason: a brief edited and not yet re-rendered would otherwise fail a check
# about a file the edit has not reached.
# ---------------------------------------------------------------------------
LIB=$(cd "$(dirname "$0")" && pwd)
fail=0

echo "=== layout ==="
python3 "$LIB/check-layout.py" --gate-only || fail=1

echo
echo "=== chunk pairing (brief <-> code.R) ==="
python3 "$LIB/check-chunks.py"    || fail=1

echo
echo "=== sources (address, and the tables themselves) ==="
python3 "$LIB/check-sources.py"   || fail=1

echo
echo "=== build stamps ==="
python3 "$LIB/check-stamps.py"    || fail=1

echo
echo "=== checks that cannot fail ==="
Rscript "$LIB/check-vacuous.R"    || fail=1

echo
echo "=== language (labs/STYLE.md) ==="
python3 "$LIB/check-language.py" || true
echo

echo "=== AI prompt boxes ==="
python3 "$LIB/check-ai-prompt.py" || fail=1

echo
echo "=== inside the tables ==="
python3 "$LIB/check-tables.py"    || fail=1

echo
echo "=== inside the renders ==="
python3 "$LIB/check-figures.py"  || true

echo
echo "=== one figure, one caption (STYLE.md rule 4) ==="
python3 "$LIB/check-captions.py" || true

# Advisory like captions, and for the same reason: the corpus is mid-rewrite
# toward the 3rd-edition template, and gating today would stop every render.
# DD_STRICT_TEMPLATE=1 makes check-layout.py gate on these once it is done.
echo
echo "=== 3rd-edition template and titles (STYLE.md Part Three) ==="
python3 "$LIB/check-layout.py" --template || true

# Opt-in, because it breaks the rule the rest of this file keeps. It drives a
# browser over every render and takes minutes, and render-brief.R runs this
# script before EVERY render -- making it default would put a browser in the
# path of each build. Advisory when it does run, for the same reason `figures`
# is: it reads the last render, not the working tree.
echo
if [ "$1" = "--contrast" ] || [ -n "$DD_CHECK_CONTRAST" ]; then
  echo "=== figure text on the dark page ==="
  node "$LIB/check-contrast.js" || true
else
  echo "(figure contrast on the dark page not checked: sh _lib/check-all.sh --contrast)"
fi

echo
if [ "$fail" -eq 0 ]; then
  echo "all checks clean"
else
  echo "something above needs reading"
fi
exit $fail
