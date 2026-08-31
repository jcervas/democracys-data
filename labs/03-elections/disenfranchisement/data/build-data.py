#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# Build the felony-disenfranchisement dataset, 2024.
#
#   derived/states.csv    One row per state: people disenfranchised by correctional
#                 status, the total, the voting-eligible population, and the
#                 rate.
#   derived/national.csv  The same for the country.
#
# THE SOURCE IS A PDF. The Sentencing Project publishes "Locked Out 2024" as a
# report, not a dataset. The state-by-state numbers exist only as Table 2 on
# page 18. This script extracts them with `pdftotext -layout` and parses the
# fixed-width columns.
#
# WHY FIXED-WIDTH AND NOT A REGEX. Cells are BLANK where a category does not
# apply -- California disenfranchises only people in prison, Maine nobody at
# all -- so a pattern expecting eight numbers per row silently drops the most
# interesting states. Column positions handle blanks correctly.
#
# THE PARSE IS CHECKED, NOT TRUSTED. Every row must satisfy
#     prison + parole + probation + jail + post_sentence == total
# and all 50 states pass. If a future edition changes the layout, the check
# fails loudly rather than producing plausible nonsense.
#
# WHAT IT SHOWS
#   * 4,049,978 Americans cannot vote because of a felony conviction -- 1.70%
#     of the voting-eligible population.
#   * 1,616,437 of them -- 40% -- have completed their sentences entirely.
#     They are not in prison, on parole, or on probation.
#   * Tennessee 7.68%, Florida 6.13%, Alabama 5.95%. Maine and Vermont: zero,
#     because they disenfranchise nobody, including people in prison.
#
# WHY IT BELONGS IN THIS COURSE. Every turnout rate in every lab has a
# denominator, and every one of those denominators either includes or excludes
# these four million people. Almost nobody says which.
#
# THE DISTRICT OF COLUMBIA IS NOT IN TABLE 2 and therefore not here.
#
# SOURCE. Uggen, Larson, Shannon & Stewart, "Locked Out 2024: Four Million
# Denied Voting Rights Due to a Felony Conviction", The Sentencing Project,
# October 2024. https://www.sentencingproject.org/reports/locked-out-2024-four-million-denied-voting-rights-due-to-a-felony-conviction/
#
# Requires `pdftotext` (poppler). Run from this directory:  python3 build-data.py
# ---------------------------------------------------------------------------

import csv, os, re, subprocess, sys, tempfile, urllib.request

# Fetches go through provenance.py, which records url, bytes, hash and row
# count in PROVENANCE.tsv and prints a banner when a source moves under us --
# a URL that still returns 200 and no longer means what it meant.
sys.path.insert(0, os.path.join("..", "..", "..", "_lib"))
import provenance as prov

# raw/ holds the sources as they arrive; derived/ is what this script writes.
os.makedirs("derived", exist_ok=True)

PDF = ("https://www.sentencingproject.org/app/uploads/2024/10/"
       "Locked-Out-2024-Four-Million-Denied-Voting-Rights-Due-to-a-Felony-Conviction.pdf")

tmp = tempfile.mkdtemp()
pdf = os.path.join(tmp, "lockedout.pdf")
txt = os.path.join(tmp, "lockedout.txt")
prov.fetch(PDF, pdf)
subprocess.run(["pdftotext", "-layout", pdf, txt], check=True)
lines = open(txt, encoding="utf-8", errors="replace").read().split("\n")

# locate Table 2 by its caption, then take the rows after the header block
start = next(i for i, L in enumerate(lines)
             if L.strip().startswith("Table 2.") and "Disenfranchised" in L)
head = next(i for i in range(start, start + 8) if lines[i].strip().startswith("State"))
# stop at the national Total row -- anything after it belongs to Table 3
end = next(i for i in range(head, head + 80) if lines[i].strip().startswith("Total"))
body = lines[head + 2 : end + 1]

# Fixed-width columns. The boundary between prison and parole sits at 28, not
# the 30 the header suggests: Texas reports 139,631 and 100,600, wide enough to
# run past a 30-character cut and be sliced in half. At 28 all 50 states pass
# the sum check below; at 30, Texas silently fails.
COLS = [(18, 28, "prison"), (28, 40, "parole"), (40, 52, "probation"),
        (52, 63, "jail"), (63, 76, "post_sentence"), (76, 91, "total"),
        (91, 106, "voting_eligible_pop"), (106, 120, "pct")]
FIELDS = [c[2] for c in COLS]

def cell(s):
    s = s.strip().replace(",", "")
    return float(s) if s else 0.0      # blank means the category does not apply

rows, national = [], None
for L in body:
    if not L.strip():
        continue
    name = L[0:18].strip().rstrip("0123456789 ").strip()
    if not name:
        continue
    if name.lower().startswith("total"):
        parts = L.split()
        vals = [float(p.replace(",", "")) for p in parts[1:]]
        if len(vals) == 8:
            national = dict(zip(FIELDS, vals))
        continue
    try:
        rec = {k: cell(L[a:b]) for a, b, k in COLS}
    except ValueError:
        print(f"  ! could not parse: {name}")
        continue
    rec["state"] = name
    rows.append(rec)

# the check that makes the parse trustworthy
bad = [r for r in rows
       if abs(r["prison"] + r["parole"] + r["probation"] + r["jail"]
              + r["post_sentence"] - r["total"]) > 1]
if bad:
    raise SystemExit(f"parse failed: {len(bad)} rows do not sum to their total")

flds = ["state"] + FIELDS
with open("derived/states.csv", "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=flds)
    w.writeheader()
    for r in sorted(rows, key=lambda r: -r["pct"]):
        w.writerow(r)

if national:
    with open("derived/national.csv", "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=flds[1:])
        w.writeheader(); w.writerow(national)

print(f"states parsed: {len(rows)}  (all rows sum to their totals)")
if national:
    print(f"national total: {national['total']:,.0f}"
          f"  post-sentence: {national['post_sentence']:,.0f}"
          f"  ({100*national['post_sentence']/national['total']:.0f}%)"
          f"  rate: {national['pct']}%")

# --- state abbreviations and tile-grid positions, for the brief's map -------
# A tile cartogram needs no boundary files: each state is one square in an
# approximate geographic layout. Columns and rows are the standard US tile grid.
GRID = {
 "Alabama":("AL",7,6),"Alaska":("AK",0,0),"Arizona":("AZ",2,5),"Arkansas":("AR",5,5),
 "California":("CA",1,4),"Colorado":("CO",3,4),"Connecticut":("CT",10,3),
 "Delaware":("DE",10,4),"Florida":("FL",9,7),"Georgia":("GA",8,6),"Hawaii":("HI",0,7),
 "Idaho":("ID",2,2),"Illinois":("IL",6,3),"Indiana":("IN",7,3),"Iowa":("IA",5,3),
 "Kansas":("KS",4,5),"Kentucky":("KY",6,4),"Louisiana":("LA",5,6),"Maine":("ME",11,0),
 "Maryland":("MD",9,4),"Massachusetts":("MA",10,2),"Michigan":("MI",7,2),
 "Minnesota":("MN",5,2),"Mississippi":("MS",6,6),"Missouri":("MO",5,4),
 "Montana":("MT",3,2),"Nebraska":("NE",4,4),"Nevada":("NV",2,4),
 "New Hampshire":("NH",10,1),"New Jersey":("NJ",10,4) if False else ("NJ",9,3),
 "New Mexico":("NM",3,5),"New York":("NY",9,2),"North Carolina":("NC",8,5),
 "North Dakota":("ND",4,2),"Ohio":("OH",8,3),"Oklahoma":("OK",4,6),"Oregon":("OR",1,3),
 "Pennsylvania":("PA",9,3) if False else ("PA",8,2),"Rhode Island":("RI",11,2),
 "South Carolina":("SC",9,5),"South Dakota":("SD",4,3),"Tennessee":("TN",7,5),
 "Texas":("TX",4,7),"Utah":("UT",2,3) if False else ("UT",3,3),"Vermont":("VT",10,0),
 "Virginia":("VA",9,4) if False else ("VA",8,4),"Washington":("WA",1,2),
 "West Virginia":("WV",7,4),"Wisconsin":("WI",6,2),"Wyoming":("WY",3,3) if False else ("WY",3,2),
}
with open("derived/grid.csv", "w", newline="") as f:
    w = csv.writer(f); w.writerow(["state", "abbr", "col", "row"])
    for st, (ab, c, r) in sorted(GRID.items()):
        w.writerow([st, ab, c, r])
print(f"grid positions written for {len(GRID)} states")

# --- build stamp -----------------------------------------------------------
# Records which script produced what is now in this directory into
# BUILD-STAMP.tsv beside the data. See ../../../_lib/provenance.py. Guarded,
# because a missing helper must not fail a build that was otherwise fine.
try:
    import os as _os, sys as _sys
    _sys.path.insert(0, _os.path.join("..", "..", "..", "_lib"))
    import provenance as _prov
    _prov.report()
    _prov.stamp("all")
except Exception as _e:
    print("  [stamp] skipped:", _e)
