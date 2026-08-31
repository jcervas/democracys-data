#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# stamp-builds.py -- give every chapter a BUILD-STAMP.tsv without rebuilding it.
#
# WHY THIS EXISTS
#
# provenance.py's stamp() records what a build produced, at the moment it
# produces it. That is the right way to get a stamp and the only way to get an
# honest build date -- but it only helps a chapter that is about to be rebuilt,
# and rebuilding the corpus means re-fetching from several dozen sources, some
# of which no longer answer. The introduction claims that every chapter's data
# sits beside a record of what produced it; waiting for 96 rebuilds to make
# that true would mean waiting indefinitely.
#
# So this walks the corpus and writes the stamp from the files already on disk.
# Every row it writes is marked `stamp_source = disk`, which says exactly what
# happened: the sizes, hashes and row counts are real and were measured just
# now, the modification date is the best available evidence of when the file
# was written, and the build date is NOT known because nothing was built. A
# chapter promotes itself to `stamp_source = build` the next time its own
# script runs, and this tool then leaves it alone.
#
# USE
#
#     python3 book/labs/_lib/stamp-builds.py            # stamp what has none
#     python3 book/labs/_lib/stamp-builds.py --sync     # + reconcile the rest
#     python3 book/labs/_lib/stamp-builds.py --force    # restamp everything
#     python3 book/labs/_lib/stamp-builds.py --dry-run
#
# NEVER overwrites a `build` stamp unless --force is given: a real build date
# is worth more than a fresh hash, and this tool cannot produce one.
#
# --sync is the everyday one, and what check-layout.py tells you to run. It
# leaves every existing row alone and only reconciles the difference: a row is
# added for a file the stamp does not mention, and dropped for a file that is
# no longer there. A chapter that was rebuilt keeps its real `build` dates and
# picks up whatever the rebuild added.
# ---------------------------------------------------------------------------

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
LABS = os.path.dirname(HERE)
sys.path.insert(0, HERE)

import provenance as prov  # noqa: E402


def entry_script(data):
    """The script a reader would say built this directory."""
    fs = sorted(f for f in os.listdir(data) if f.endswith((".R", ".py", ".sh")))
    for pref in ("build-data.R", "build-data.py", "build-data.sh"):
        if pref in fs:
            return pref
    if len(fs) == 1:
        return fs[0]
    b = [f for f in fs if f.startswith("build")]
    # More than one build script and no canonical name: name them all rather
    # than picking one, so the stamp does not quietly credit the wrong file.
    return ", ".join(b or fs) if (b or fs) else ""


def existing_kinds(path):
    """The stamp_source values already in a BUILD-STAMP.tsv."""
    if not os.path.exists(path):
        return set()
    out = set()
    with open(path, encoding="utf-8") as fh:
        head = fh.readline().rstrip("\n").split("\t")
        if "stamp_source" not in head:
            return {"?"}
        i = head.index("stamp_source")
        for line in fh:
            v = line.rstrip("\n").split("\t")
            if len(v) == len(head):
                out.add(v[i])
    return out


def listed_files(path):
    """Files a BUILD-STAMP.tsv already names."""
    if not os.path.exists(path):
        return set()
    with open(path, encoding="utf-8") as fh:
        head = fh.readline().rstrip("\n").split("\t")
        if "file" not in head:
            return set()
        i = head.index("file")
        return {v[i] for v in (l.rstrip("\n").split("\t") for l in fh)
                if len(v) == len(head)}


def main():
    force = "--force" in sys.argv
    sync = "--sync" in sys.argv
    dry = "--dry-run" in sys.argv

    # Chapters sit at labs/<NN-part>/<slug>/, and a chapter that has not been
    # assigned to a part sits at labs/<slug>/. Both depths count -- `gotv` is
    # currently the second kind, and a walk that only looked one level down
    # would leave it unstamped while reporting that it had done every chapter.
    chapters = []
    for part in sorted(os.listdir(LABS)):
        pdir = os.path.join(LABS, part)
        if part.startswith("_") or not os.path.isdir(pdir):
            continue
        if os.path.isdir(os.path.join(pdir, "data")):
            chapters.append((part, os.path.join(pdir, "data")))
            continue
        for slug in sorted(os.listdir(pdir)):
            data = os.path.join(pdir, slug, "data")
            if os.path.isdir(data):
                chapters.append((f"{part}/{slug}", data))

    wrote = kept = empty = synced = 0
    for name, data in chapters:
        stamp = os.path.join(data, prov.STAMP_FILE)
        kinds = existing_kinds(stamp)

        if kinds and not force:
            if not sync:
                kept += 1
                continue
            # Reconcile only. Existing rows -- including real `build` rows with
            # dates this tool cannot reproduce -- are left exactly as they are.
            cwd = os.getcwd()
            os.chdir(data)
            try:
                on_disk = set(prov._stamp_outputs())
                already = listed_files(stamp)
                add, gone = sorted(on_disk - already), already - on_disk
                if not add and not gone:
                    kept += 1
                    continue
                bits = []
                if add:
                    bits.append(f"+{len(add)}")
                if gone:
                    bits.append(f"-{len(gone)}")
                if dry:
                    print(f"  would sync {','.join(bits):8}  {name}")
                else:
                    # stamp() drops rows for files that have gone, and an empty
                    # `outputs` still rewrites the pruned table.
                    prov.stamp(outputs=add, script=entry_script(data),
                               source_kind="disk")
                    print(f"  synced {','.join(bits):8}  {name}")
                synced += 1
            finally:
                os.chdir(cwd)
            continue

        cwd = os.getcwd()
        os.chdir(data)
        try:
            outs = prov._stamp_outputs()
            if not outs:
                # A data/ directory with no derived/ or raw/ content. Nothing
                # to stamp and nothing to claim; say so rather than writing a
                # header with no rows, which would read as a build that made
                # nothing.
                empty += 1
                print(f"  (no data files) {name}")
                continue
            if dry:
                print(f"  would stamp {len(outs):4} file(s)  {name}")
            else:
                prov.stamp(outputs="all",
                           script=entry_script(data),
                           source_kind="disk")
                print(f"  stamped {len(outs):4} file(s)  {name}")
            wrote += 1
        finally:
            os.chdir(cwd)

    print()
    print(f"chapters with data/          : {len(chapters)}")
    print(f"{'would stamp' if dry else 'stamped':29}: {wrote}")
    if sync:
        print(f"{'would sync' if dry else 'reconciled':29}: {synced}")
    print(f"left alone (already stamped) : {kept}")
    print(f"no data files to stamp       : {empty}")


main()
