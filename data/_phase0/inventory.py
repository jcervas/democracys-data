#!/usr/bin/env python3
"""
Phase 0, step 1 -- read every data file in the corpus and write down what it is.

READS NOTHING BUT DATA AND WRITES NOTHING BUT REPORTS. This script does not
touch a single file under labs/. That is the whole point of Phase 0: establish
what is there, and a baseline to prove later phases broke nothing, before any
phase is allowed to change anything.

Outputs, all into this folder:

  inventory.tsv     one row per data file: size, hash, rows, cols, column names
  columns.tsv       one row per COLUMN of every file -- the draft dictionary
  duplicates.md     exact-hash duplicates, and files sharing a column schema

Column level inference mirrors the ten levels in labs/_lib/structure.R, so the
draft dictionary speaks the vocabulary the corpus already uses. It is coarse on
purpose; a human corrects it, which is why it is a draft.
"""

import csv, hashlib, io, os, re, sys
from collections import defaultdict, Counter

HERE = os.path.dirname(os.path.abspath(__file__))
F26  = os.path.abspath(os.path.join(HERE, "..", ".."))
LABS = os.path.join(F26, "labs")

# Mirrors DD_CODE_RX / DD_ID_RX in labs/_lib/structure.R.
CODE_RX = re.compile(
    r"(^|[^a-z])(geoid|fips|zcta|ansi|district|precinct|ward|tract|block|blkgrp|"
    r"cd|sldu|sldl|statefp|countyfp|zip|zipcode|id|code|no|num|number)([^a-z]|$)", re.I)
ID_RX   = re.compile(r"(^|[^a-z])(id|uuid|key|registration|voter ?reg)([^a-z]|$)", re.I)
NUM_RX  = re.compile(r"^-?\d{1,3}(,\d{3})*(\.\d+)?$|^-?\d*\.?\d+([eE][-+]?\d+)?$")
INT_RX  = re.compile(r"^-?\d+$")
DATE_RX = re.compile(r"^\d{4}-\d{2}-\d{2}|^\d{1,2}/\d{1,2}/\d{2,4}")

SAMPLE = 400          # data rows sampled for type inference
SKIP_DIRS = {"_archive", "_phase0", "raw", "tiger", "holc", "blocks",
             "shp2020", "shp2024"}


def sha256(path, cap=None):
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        while True:
            b = fh.read(1 << 20)
            if not b:
                break
            h.update(b)
    return h.hexdigest()


def classify(name, values):
    """Return (level, storage) for one column, from its name and sampled values."""
    vals = [v for v in values if v not in ("", "NA", "NaN", "NULL", "N/A")]
    if not vals:
        return "empty", "character"
    uniq = set(vals)
    n_u  = len(uniq)

    all_int = all(INT_RX.match(v) for v in vals)
    all_num = all(NUM_RX.match(v) for v in vals)
    storage = "integer" if all_int else ("numeric" if all_num else "character")

    if n_u == 1:
        return "constant", storage
    if any(DATE_RX.match(v) for v in vals[:20]):
        return "date", "date"
    if ID_RX.search(name) and n_u == len(vals):
        return "identifier", storage
    if CODE_RX.search(name):
        return "code", storage
    if n_u == 2:
        return "dichotomous", storage
    if all_int:
        neg = any(v.startswith("-") for v in vals)
        return ("continuous" if neg else "count"), storage
    if all_num:
        return "continuous", storage
    if n_u <= max(12, len(vals) // 20):
        return "categorical", "character"
    if sum(len(v) for v in vals) / len(vals) > 40:
        return "text", "character"
    return "categorical", "character"


def sniff(path):
    """Return (nrows, header, sampled column values). Tolerant of junk."""
    try:
        with open(path, "r", encoding="utf-8", errors="replace", newline="") as fh:
            head = fh.read(64 * 1024)
            fh.seek(0)
            try:
                dialect = csv.Sniffer().sniff(head[:8192], delimiters=",\t;|")
            except Exception:
                dialect = csv.excel
            rdr = csv.reader(fh, dialect)
            try:
                header = next(rdr)
            except StopIteration:
                return 0, [], []
            cols = [[] for _ in header]
            n = 0
            for row in rdr:
                n += 1
                if n <= SAMPLE:
                    for i in range(min(len(row), len(cols))):
                        cols[i].append(row[i].strip())
            return n, [h.strip().lstrip("﻿") for h in header], cols
    except Exception as e:
        return -1, ["<<unreadable: %s>>" % type(e).__name__], []


def main():
    files = []
    for lab in sorted(os.listdir(LABS)):
        labdir = os.path.join(LABS, lab)
        if not os.path.isdir(labdir) or lab in SKIP_DIRS:
            continue
        for root, dirs, names in os.walk(labdir):
            dirs[:] = [d for d in dirs if d not in SKIP_DIRS]
            for nm in names:
                if nm.lower().endswith((".csv", ".tsv")):
                    files.append((lab, os.path.join(root, nm)))

    inv_rows, col_rows = [], []
    by_hash   = defaultdict(list)
    by_schema = defaultdict(list)

    for lab, path in sorted(files):
        rel  = os.path.relpath(path, F26)
        size = os.path.getsize(path)
        dig  = sha256(path)
        nrow, header, cols = sniff(path)
        schema = "|".join(header)

        inv_rows.append(dict(
            lab=lab, file=os.path.basename(path), path=rel, bytes=size,
            sha256=dig[:16], rows=nrow, cols=len(header), schema=schema))
        by_hash[dig].append(rel)
        if header and nrow > 0:
            by_schema[schema].append(rel)

        for i, cname in enumerate(header):
            vals = cols[i] if i < len(cols) else []
            level, storage = classify(cname, vals)
            ex = next((v for v in vals if v not in ("", "NA")), "")
            col_rows.append(dict(
                table=rel, column=cname, storage=storage, level=level,
                n_distinct=len(set(v for v in vals if v != "")),
                pct_missing=round(100 * sum(1 for v in vals if v in ("", "NA"))
                                  / max(len(vals), 1), 1),
                example=ex[:40]))

    def write_tsv(name, rows, fields):
        with open(os.path.join(HERE, name), "w", newline="", encoding="utf-8") as fh:
            w = csv.DictWriter(fh, fieldnames=fields, delimiter="\t",
                               extrasaction="ignore", quoting=csv.QUOTE_MINIMAL)
            w.writeheader()
            w.writerows(rows)

    write_tsv("inventory.tsv", inv_rows,
              ["lab", "file", "path", "bytes", "rows", "cols", "sha256", "schema"])
    write_tsv("columns.tsv", col_rows,
              ["table", "column", "storage", "level", "n_distinct",
               "pct_missing", "example"])

    # ---- duplicate report ---------------------------------------------------
    dup_hash   = {h: p for h, p in by_hash.items() if len(p) > 1}
    dup_schema = {s: p for s, p in by_schema.items()
                  if len(p) > 1 and len(set(os.path.dirname(x) for x in p)) > 1}

    wasted = 0
    for h, paths in dup_hash.items():
        sz = os.path.getsize(os.path.join(F26, paths[0]))
        wasted += sz * (len(paths) - 1)

    out = [
        "# Phase 0 — duplicate report",
        "",
        "Generated by `inventory.py`. No files were modified.",
        "",
        "## Summary",
        "",
        "| | |",
        "|---|---:|",
        "| Data files scanned | %d |" % len(inv_rows),
        "| Distinct column schemas | %d |" % len(by_schema),
        "| **Exact-duplicate file groups** | **%d** |" % len(dup_hash),
        "| Redundant bytes from exact duplicates | %s |" % f"{wasted:,}",
        "| Schemas shared across ≥2 labs | %d |" % len(dup_schema),
        "",
        "## Exact duplicates (identical sha256)",
        "",
    ]
    if dup_hash:
        for h, paths in sorted(dup_hash.items(), key=lambda kv: -len(kv[1])):
            sz = os.path.getsize(os.path.join(F26, paths[0]))
            out.append("**%d copies** · %s bytes each · `%s…`" % (len(paths), f"{sz:,}", h[:12]))
            out += ["- `%s`" % p for p in sorted(paths)] + [""]
    else:
        out += ["None.", ""]

    out += ["## Same schema, different labs",
            "",
            "Candidates for a single master table. Column list, then the files.",
            ""]
    for s, paths in sorted(dup_schema.items(), key=lambda kv: -len(kv[1]))[:40]:
        ncol = len(s.split("|"))
        out.append("**%d files** · %d columns · `%s`" %
                   (len(paths), ncol, s[:110] + ("…" if len(s) > 110 else "")))
        out += ["- `%s`" % p for p in sorted(paths)] + [""]

    with open(os.path.join(HERE, "duplicates.md"), "w", encoding="utf-8") as fh:
        fh.write("\n".join(out))

    print("files scanned      : %d" % len(inv_rows))
    print("columns catalogued : %d" % len(col_rows))
    print("exact dup groups   : %d  (%s redundant bytes)" % (len(dup_hash), f"{wasted:,}"))
    print("cross-lab schemas  : %d" % len(dup_schema))


if __name__ == "__main__":
    main()
