#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# Build the jury-selection dataset: peremptory strikes by race.
#
#   derived/strikes.csv   Strike rates by race, for the STATE and for the DEFENSE.
#   derived/trials.csv    Per trial: Black and white jurors eligible, and struck by
#                 the state. 211 trials with at least 10 eligible jurors.
#
# THE CASE. Curtis Flowers was tried six times for the same crime by the same
# prosecutor, Doug Evans, in Mississippi's Fifth Circuit. In FLOWERS v.
# MISSISSIPPI (2019) the Supreme Court threw out his conviction, and the
# majority opinion rested substantially on the pattern of whom Evans's office
# struck from juries. APM Reports built the underlying dataset by reading court
# records from 225 trials over 26 years.
#
# THE INFERENCE, AND WHY THIS DATASET IS UNUSUALLY STRONG. A prosecutor may
# strike a juror for almost any reason except race. Reasons are rarely
# recorded, so a disparity alone invites the reply that struck jurors differed
# in some legitimate way. **This file contains its own control group.** The
# defense strikes from the same pool, in the same trials, under the same rules,
# with opposite incentives:
#
#     STATE     struck 49.8% of eligible Black jurors, 11.2% of white  (4.4x)
#     DEFENSE   struck 15.0% of eligible Black jurors, 47.3% of white
#
# The mirror image is the finding. Whatever made Black jurors strike-worthy to
# the state made white jurors strike-worthy to the defense at almost exactly
# the same rate. That is very hard to explain by anything except race, and it
# is why the comparison is worth more than the disparity.
#
# ELIGIBILITY MATTERS. A juror already removed for cause, or never reached,
# could not be struck by anyone. `strike_eligibility` records who could have
# struck each juror, and the rates below use only jurors the relevant side
# could actually have struck. Using all jurors as the denominator would understate
# both sides.
#
# RACE IS MISSING FOR 4,752 OF 14,873 JURORS. APM coded race from court records
# and, where those were silent, from other sources; the rest are "Unknown" and
# are excluded here. That is a third of the file, and the lab says so.
#
# SOURCE. APM Reports, "In the Dark" season 2, jury data.
#   https://github.com/APM-Reports/jury-data
#   jurors.csv, trials.csv, voir_dire_answers.csv
#
# Run from this directory:  python3 build-data.py
# ---------------------------------------------------------------------------

import collections, csv, io, urllib.request

# raw/ holds the sources as they arrive; derived/ is what this script writes.
import os

# Fetches go through provenance.py, which records url, bytes, hash and row
# count in PROVENANCE.tsv and prints a banner when a source moves under us --
# a URL that still returns 200 and no longer means what it meant.
import sys as _sys
_sys.path.insert(0, os.path.join("..", "..", "..", "_lib"))
import provenance as prov
os.makedirs("derived", exist_ok=True)

BASE = "https://raw.githubusercontent.com/APM-Reports/jury-data/master/"
rows = list(csv.DictReader(io.StringIO(
    prov.fetch_bytes(BASE + "jurors.csv", label="jurors.csv", timeout=300).decode("utf-8", "replace"))))
print(f"jurors: {len(rows):,}")

def tally(eligible_for, struck_label):
    t = collections.defaultdict(lambda: [0, 0])
    for r in rows:
        if r["strike_eligibility"] in eligible_for and r["race"] in ("Black", "White"):
            t[r["race"]][1] += 1
            if r["struck_by"] == struck_label:
                t[r["race"]][0] += 1
    return t

state   = tally(("Both State and Defense", "State"),   "Struck by the state")
defense = tally(("Both State and Defense", "Defense"), "Struck by the defense")

with open("derived/strikes.csv", "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["side", "race", "eligible", "struck", "pct"])
    for side, t in (("state", state), ("defense", defense)):
        for race in ("Black", "White"):
            struck, n = t[race]
            w.writerow([side, race, n, struck, round(100 * struck / n, 1)])

per = collections.defaultdict(collections.Counter)
for r in rows:
    if r["strike_eligibility"] in ("Both State and Defense", "State") \
       and r["race"] in ("Black", "White"):
        d = per[r["trial"]]
        d[r["race"] + "_elig"] += 1
        if r["struck_by"] == "Struck by the state":
            d[r["race"] + "_struck"] += 1

with open("derived/trials.csv", "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["trial", "black_eligible", "black_struck", "white_eligible", "white_struck"])
    n = 0
    for t, d in sorted(per.items()):
        if d["Black_elig"] + d["White_elig"] >= 10:
            w.writerow([t, d["Black_elig"], d["Black_struck"],
                        d["White_elig"], d["White_struck"]]); n += 1
print(f"trials with >=10 eligible jurors: {n}")

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
