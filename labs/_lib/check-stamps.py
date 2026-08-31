#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# Does every chapter's BUILD-STAMP.tsv still describe the directory it sits in?
#
#     python3 _lib/check-stamps.py            # from labs/, or from anywhere
#
# Four questions, and the last is the reason this is a separate script:
#
#   1. Does the stamp exist at all?
#   2. Is anything in derived/ or raw/ that it does not name?
#   3. Does it name anything that is no longer there?
#   4. Does every file still hash to what the stamp says it did?
#
# WHY 4 NEEDS ITS OWN CHECK. Questions 1-3 compare file NAMES, which is a
# listing check: it catches a file appearing, a file vanishing, and a chapter
# nobody ever stamped. It cannot catch the case where the name is right and the
# CONTENT is not -- a derived table edited by hand to fix a typo, a raw file
# replaced with a newer download under the same name, a build re-run whose
# output moved. In every one of those the stamp still lists exactly the right
# files and quietly attests to bytes that are gone. That is the same failure
# the whole provenance apparatus exists to catch, one level down, and a listing
# check reports clean through all of it.
#
# WHAT A MISMATCH MEANS, AND WHY IT IS NOT AUTOMATICALLY A BUG. A file whose
# content changed is a file somebody changed, deliberately or not. The check
# does not judge which; it says the stamp no longer matches and leaves the
# reading to a human. If the change was intended, restamp. If it was not, the
# check just found what it is for.
#
# COST. About 1.5s for the whole corpus -- roughly 2 GB across 1,900 files,
# read at local disk speed. Cheap enough to run every time, which is the only
# reason it is worth having: a check nobody runs catches nothing.
# ---------------------------------------------------------------------------
import hashlib
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
LABS = os.path.dirname(HERE)

STAMP = "BUILD-STAMP.tsv"
FIX_LIST = "run: python3 _lib/stamp-builds.py --sync"
FIX_HASH = "if the change was intended: python3 _lib/stamp-builds.py --force"

# Chapters with no data/ and nothing to stamp. Mirrors check-layout.py's
# NO_DATA_OK: an allowlist, so a chapter that quietly loses its data/ is a
# finding rather than a chapter this script silently stops looking at.
NO_DATA_OK = {"introduction"}


def chapters():
    """(slug, data dir) for every chapter that has one.

    A chapter is a folder holding <slug>-brief.Rmd. They sit at
    labs/<NN-part>/<slug>/, and one not yet assigned to a part sits at
    labs/<slug>/ -- both depths, or `gotv` goes unchecked while this script
    reports that it covered the corpus.
    """
    out, seen = [], []
    for part in sorted(os.listdir(LABS)):
        pdir = os.path.join(LABS, part)
        if part.startswith("_") or not os.path.isdir(pdir):
            continue
        cands = [(part, pdir)]
        cands += [(s, os.path.join(pdir, s)) for s in sorted(os.listdir(pdir))
                  if os.path.isdir(os.path.join(pdir, s))]
        for slug, d in cands:
            if not os.path.exists(os.path.join(d, f"{slug}-brief.Rmd")):
                continue
            seen.append(slug)
            data = os.path.join(d, "data")
            if os.path.isdir(data):
                out.append((slug, data))
    return out, seen


def sha(path):
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return "sha256:" + h.hexdigest()


def read_stamp(path):
    """{file: sha256} as the stamp records it, or None if unreadable."""
    try:
        with open(path, encoding="utf-8") as fh:
            head = fh.readline().rstrip("\n").split("\t")
            if "file" not in head or "sha256" not in head:
                return None
            fi, hi = head.index("file"), head.index("sha256")
            out = {}
            for line in fh:
                v = line.rstrip("\n").split("\t")
                if len(v) == len(head):
                    out[v[fi]] = v[hi]
            return out
    except OSError:
        return None


def data_files(d):
    out = set()
    for sub in ("derived", "raw"):
        p = os.path.join(d, sub)
        if not os.path.isdir(p):
            continue
        for root, _, files in os.walk(p):
            for f in files:
                if f.startswith("."):
                    continue
                out.add(os.path.relpath(os.path.join(root, f), d))
    return out


problems = []
chaps, seen = chapters()
n_files = n_chap = 0

for slug, d in chaps:
    on_disk = data_files(d)
    if not on_disk:
        continue                       # nothing built, nothing to claim
    sp = os.path.join(d, STAMP)
    if not os.path.exists(sp):
        problems.append(("no build stamp", f"{slug}/data/", FIX_LIST))
        continue
    listed = read_stamp(sp)
    if listed is None:
        problems.append(("bad build stamp", f"{slug}/data/{STAMP}",
                         "unreadable, or missing a `file` / `sha256` column"))
        continue

    n_chap += 1
    missing = on_disk - set(listed)
    if missing:
        problems.append(("unstamped file(s)", f"{slug}/data/",
                         f"{len(missing)} not in {STAMP} "
                         f"(e.g. {sorted(missing)[0]}); {FIX_LIST}"))
    gone = set(listed) - on_disk
    if gone:
        problems.append(("stamped, not on disk", f"{slug}/data/",
                         f"{len(gone)} listed but absent "
                         f"(e.g. {sorted(gone)[0]}); {FIX_LIST}"))

    # 4. the contents themselves
    moved = []
    for f in sorted(on_disk & set(listed)):
        p = os.path.join(d, f)
        try:
            actual = sha(p)
        except OSError as e:
            problems.append(("unreadable", f"{slug}/data/{f}", str(e)))
            continue
        n_files += 1
        if actual != listed[f]:
            moved.append(f)
    if moved:
        problems.append(("content changed", f"{slug}/data/",
                         f"{len(moved)} file(s) no longer hash to their stamp "
                         f"(e.g. {moved[0]}); {FIX_HASH}"))

# A chapter absent from both the checked set and the allowlist means the
# traversal missed it -- the blind spot check-layout.py records having had.
unaccounted = set(seen) - {s for s, _ in chaps} - NO_DATA_OK
for slug in sorted(unaccounted):
    problems.append(("no data/", slug, "not in this script's allowlist"))

if problems:
    w = max(len(k) for k, _, _ in problems)
    for kind, where, what in problems:
        print(f"{kind:{w}}  {where:34s}  {what}")
    print(f"\n{len(problems)} problem(s)")
else:
    print(f"{n_chap} stamped chapters, {n_files:,} files, "
          f"every one matching its recorded hash")
sys.exit(1 if problems else 0)
