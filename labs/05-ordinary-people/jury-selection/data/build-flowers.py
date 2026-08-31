#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# Build flowers.csv: the six trials of Curtis Flowers, one row each.
#
# WHY THIS IS A SEPARATE FILE. trials.csv keeps only trials with at least ten
# eligible jurors whose race the reporters could code. That filter drops
# Flowers II, which has only eight (5 Black, 3 white) — 22 of its 30 eligible
# jurors have race "Unknown". Flowers II is exactly the trial you cannot leave
# out of a figure about Flowers, because FLOWERS v. MISSISSIPPI (2019) turned
# on the pattern across all six. So this file is built without the size filter
# and carries the Unknown count openly, per trial, next to the coded counts.
#
# Same eligibility rule as build-data.py: a juror counts as eligible only if
# `strike_eligibility` says the STATE could have struck them ("State" or "Both
# State and Defense"). A juror already removed for cause, or never reached,
# was available to nobody.
#
# `batson_claim` and `verdict` come from the trial-level file, not the juror
# file. A Batson claim was raised by the defence in four of the six trials and
# none of them produced relief at trial — which is the point the brief makes
# about where the rule is enforced.
#
# SOURCE. APM Reports, "In the Dark" season 2, jury data.
#   https://github.com/APM-Reports/jury-data
#   jurors.csv  (14,874 rows)   trials.csv  (305 rows)
#   Fetched 2026-08-10. Output: flowers.csv, 6 rows.
#
# Run from this directory:  python3 build-flowers.py
# ---------------------------------------------------------------------------

import collections, csv, io, sys, urllib.request

# raw/ holds the sources as they arrive; derived/ is what this script writes.
import os
os.makedirs("derived", exist_ok=True)

# Fetches go through provenance.py, which records url, bytes, hash and row
# count in PROVENANCE.tsv and prints a banner when a source moves under us --
# a URL that still returns 200 and no longer means what it meant.
sys.path.insert(0, os.path.join("..", "..", "..", "_lib"))
import provenance as prov

BASE = "https://raw.githubusercontent.com/APM-Reports/jury-data/master/"

def get(name):
    raw = prov.fetch_bytes(BASE + name, label=name, timeout=300)
    rows = list(csv.DictReader(io.StringIO(raw.decode("utf-8", "replace"))))
    print(f"{name}: {len(rows):,} rows")
    return rows

jurors = get("jurors.csv")
trials = get("derived/trials.csv")

ORDER = ["I", "II", "III", "IV", "V", "VI"]
STATE_ELIGIBLE = ("Both State and Defense", "State")

meta = {}
for r in trials:
    if r["defendant_name"].startswith("Curtis Flowers "):
        meta[r["defendant_name"].split()[-1]] = r

per = collections.defaultdict(collections.Counter)
for r in jurors:
    if "Curtis Flowers " not in r["trial"]:
        continue
    if r["strike_eligibility"] not in STATE_ELIGIBLE:
        continue
    numeral = r["trial"].split()[-1]
    d = per[numeral]
    race = r["race"] if r["race"] in ("Black", "White") else "Unknown"
    d[race + "_elig"] += 1
    if r["struck_by"] == "Struck by the state":
        d[race + "_struck"] += 1

with open("derived/flowers.csv", "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["trial", "black_eligible", "black_struck", "white_eligible",
                "white_struck", "unknown_eligible", "batson_claim", "verdict"])
    for numeral in ORDER:
        d, m = per[numeral], meta[numeral]
        w.writerow(["Flowers " + numeral,
                    d["Black_elig"], d["Black_struck"],
                    d["White_elig"], d["White_struck"],
                    d["Unknown_elig"],
                    "TRUE" if m["batson_claim_by_defense"] == "TRUE" else "FALSE",
                    m["verdict"]])
        print(f"  Flowers {numeral:<3} Black {d['Black_struck']}/{d['Black_elig']}"
              f"  white {d['White_struck']}/{d['White_elig']}"
              f"  unknown {d['Unknown_elig']}")
print("wrote flowers.csv, 6 rows")

# Anything fetched above is now in PROVENANCE.tsv; say so if a source moved.
prov.report()
