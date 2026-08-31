#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# check-tables.py — what is wrong *inside* the derived tables.
#
#     python3 _lib/check-tables.py
#
# Three defects, all of which pass every other check in this folder: the files
# are in the right place, the paths resolve, the builds run, the briefs render,
# and the numbers are still wrong.
#
#   1. FIXED-WIDTH CUTS. lobbying/filings.csv used to cut client names at
#      exactly 70 characters, mid-word, on 34 of 1,500 rows. Nothing in the
#      chapter showed it. The signature is a spike of values at exactly the
#      maximum length where a real distribution would taper, breaking mid-word.
#
#   2. UNDECLARED TOP-N. A ranked table cut to a round number of rows without
#      saying so reads as complete. lobbying/entities.csv was a top 40.
#
#   3. SPURIOUS PRECISION. A vote share to thirteen decimal places, where the
#      last nine digits are binary floating point rather than a measurement.
#      house-competition's dpres was a 32-bit float widened to 64 and printed
#      whole; 12,623 of 15,647 values claimed precision nobody measured.
#
# Every hit needs reading. A column of genuine full-width names, a table of
# exactly 50 states, and a probability of 2.7e-3 all look like these and are
# all fine.
# ---------------------------------------------------------------------------
import os, csv, re, sys, collections

HERE = os.path.dirname(os.path.abspath(__file__))
LABS = os.path.dirname(HERE)

MIN_LEN = 16          # below this, a "cut" is more likely a genuine short code
MIN_AT_MAX = 3        # how many values must sit exactly at the maximum

# A LENGTH SPIKE NEEDS A TABLE BIG ENOUGH FOR "SPIKE" TO MEAN ANYTHING.
#
# rank-size/zipf.csv has 22 rows, three of which are the same Wikipedia article
# title with different dates -- 52 characters each because a fixed title and a
# fixed-width date happen to add up, not because anything was cut. In a table
# that small, three values sharing a length is a coincidence rather than
# evidence. The case this scan exists for looked nothing like it: lobbying had
# 30 values at the cut against 15 just below, out of 1,500 rows.
#
# This bound applies ONLY to the width scan. The top-N check below deliberately
# looks at small tables -- a ranking cut to 25 or 30 rows is exactly what it is
# for -- so it keeps its own, lower, threshold.
MIN_ROWS = 50

# The top-N check keeps the threshold it has always had: a body of at least 20
# rows. Named here rather than left implicit in a shared guard, because raising
# the width bound above would otherwise have quietly LOWERED this one and
# started reporting ten-row tables that were never in scope.
MIN_ROWS_TOPN = 20


MISSING = {"na", "n/a", "nan", "-", ".", "null", "inf", "-inf"}


def is_number(s):
    """A numeric field, counting the usual spellings of missing as numeric --
    otherwise a column of numbers with NAs in it looks like free text."""
    s = s.strip()
    if s.lower() in MISSING:
        return True
    try:
        float(s.replace(",", "").replace("%", "").replace("$", ""))
        return True
    except ValueError:
        return False


def midword(s):
    """Does this string stop in the middle of something?"""
    if not s:
        return False
    unbalanced = (s.count("(") > s.count(")") or s.count("[") > s.count("]")
                  or s.count('"') % 2 == 1)
    # ends on a word character with no terminal punctuation
    open_end = bool(re.search(r"[A-Za-z0-9]$", s)) and not re.search(r"[.!?)\]]$", s)
    return unbalanced or open_end


def scan_column(values):
    vals = [v for v in values if v.strip()]
    if len(vals) < 20:
        return None
    if sum(is_number(v) for v in vals) > 0.9 * len(vals):
        return None
    lens = [len(v) for v in vals]
    L = max(lens)
    if L < MIN_LEN or len(set(lens)) < 5:
        return None
    at_max = [v for v in vals if len(v) == L]
    distinct_at_max = len(set(at_max))
    if distinct_at_max < MIN_AT_MAX:
        return None
    just_below = sum(1 for n in lens if L - 5 <= n <= L - 1)
    if len(at_max) <= just_below:          # tapering, not a spike
        return None
    cut = sum(1 for v in set(at_max) if midword(v))
    return dict(maxlen=L, n_at_max=len(at_max), distinct=distinct_at_max,
                just_below=just_below, midword=cut,
                sample=sorted(set(at_max))[:2])


ROUND_N = {10, 12, 15, 20, 24, 25, 30, 40, 50, 60, 75, 100, 150, 200, 250, 500}


# ---------------------------------------------------------------- traversal
#
# Chapters live one level deeper than they used to: labs/NN-part/<slug>/.
# A chapter with no part (gotv) still sits at the labs root, so both depths
# are walked and the caller gets the same (slug, path) pairs either way.
def chapter_dirs(labs):
    out = []
    for d in sorted(os.listdir(labs)):
        p = os.path.join(labs, d)
        if not os.path.isdir(p) or d.startswith((".", "_")):
            continue
        if re.match(r"^\d\d-", d):                 # a part directory
            for c in sorted(os.listdir(p)):
                q = os.path.join(p, c)
                if os.path.isdir(q) and not c.startswith((".", "_")):
                    out.append((c, q))
        else:
            out.append((d, p))
    return out

width_hits, row_hits = [], []

for lab, _labdir in chapter_dirs(LABS):
    d = os.path.join(_labdir, "data", "derived")
    if not os.path.isdir(d):
        continue
    for dirpath, _, files in os.walk(d):
        for fn in sorted(files):
            if not fn.lower().endswith((".csv", ".tsv")):
                continue
            p = os.path.join(dirpath, fn)
            rel = os.path.relpath(p, LABS)
            try:
                with open(p, newline="", encoding="utf-8", errors="replace") as fh:
                    delim = "\t" if fn.lower().endswith(".tsv") else ","
                    rows = list(csv.reader(fh, delimiter=delim))
            except Exception:
                continue
            if len(rows) < 11 or not rows[0]:
                continue
            header, body = rows[0], rows[1:]

            if len(body) >= MIN_ROWS:
                for j, col in enumerate(header):
                    vals = [r[j] for r in body if j < len(r)]
                    hit = scan_column(vals)
                    if hit:
                        hit.update(file=rel, column=col)
                        width_hits.append(hit)

            # a ranked table cut to a round number of rows
            n = len(body)
            if n in ROUND_N and n >= MIN_ROWS_TOPN:
                for j, col in enumerate(header):
                    vals = [r[j] for r in body if j < len(r)]
                    if len(vals) == n and all(is_number(v) for v in vals if v.strip()):
                        try:
                            nums = [float(v.replace(",", "").replace("%", "").replace("$", ""))
                                    for v in vals if v.strip()]
                        except ValueError:
                            continue        # a column with NAs cannot be a clean ranking
                        if len(nums) == n and nums == sorted(nums, reverse=True) \
                           and len(set(nums)) > n / 2:
                            row_hits.append((rel, n, col))
                            break


# --- 3. spurious precision --------------------------------------------------
LONG = re.compile(r"^-?\d+\.\d{9,}$")
prec = collections.Counter()
for lab, _labdir in chapter_dirs(LABS):
    d = os.path.join(_labdir, "data", "derived")
    if not os.path.isdir(d):
        continue
    for dp, _, fs in os.walk(d):
        for fn in fs:
            if not fn.endswith(".csv"):
                continue
            try:
                rows = list(csv.DictReader(open(os.path.join(dp, fn), encoding="utf-8",
                                                errors="replace")))
            except Exception:
                continue
            for r in rows:
                for v in r.values():
                    # below 0.001 a value needs many decimals to carry six
                    # significant digits, and those decimals are real
                    if v and LONG.match(v.strip()) and abs(float(v)) >= 0.001:
                        prec[os.path.relpath(os.path.join(dp, fn), LABS)] += 1

# --- findings already read and judged fine ---------------------------------
REVIEWED = os.path.join(HERE, "check-tables-reviewed.tsv")
reviewed = set()
if os.path.exists(REVIEWED):
    for line in open(REVIEWED, encoding="utf-8"):
        if line.startswith("#") or not line.strip():
            continue
        f = line.rstrip("\n").split("\t")
        if len(f) >= 3:
            reviewed.add((f[0], f[1], f[2]))

def new(kind, file, key):
    return (kind, file, str(key)) not in reviewed

width_hits = [h for h in width_hits if new("width", h["file"], h["column"])]
row_hits   = [r for r in row_hits if new("topn", r[0], r[1])]
prec       = collections.Counter({f: n for f, n in prec.items()
                                  if new("precision", f, n)})
nseen = len(reviewed)
problems = len(width_hits) + len(row_hits) + len(prec)

print("1. fixed-width cuts")
if width_hits:
    for h in sorted(width_hits, key=lambda x: -x["n_at_max"]):
        print(f"   {h['file']}  column `{h['column']}`")
        print(f"      max length {h['maxlen']} · {h['n_at_max']} values there "
              f"({h['distinct']} distinct) vs {h['just_below']} just below "
              f"· {h['midword']} break mid-word")
        print(f"      e.g. {h['sample'][0]!r}")
else:
    print("   clean")

print("\n2. round row counts on a ranking (possible undeclared top-N)")
if row_hits:
    for rel, n, col in row_hits:
        print(f"   {rel:56s} {n} rows, ranked on `{col}`")
else:
    print("   clean")

print("\n3. values printed past six significant digits")
if prec:
    for f, n in prec.most_common():
        print(f"   {f:58s} {n}")
else:
    print("   clean")

print()
print(f"({nseen} finding(s) previously read and recorded in "
      f"{os.path.basename(REVIEWED)})")
print("no new findings" if problems == 0 else f"{problems} NEW thing(s) to read")
sys.exit(1 if problems else 0)
