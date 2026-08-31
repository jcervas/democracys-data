#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# Build the policing dataset: the outcome test.
#
# PYTHON, not R, because the source is a 177 MB CSV of 905,070 individual
# stops. The lab is base R and reads only the two small files below.
#
#   derived/by_race.csv   One row per race: stops, searches, contraband found, arrests.
#   derived/by_year.csv   The same, split by year, for the trend option.
#
# THE INFERENCE THIS FILE SUPPORTS -- the "outcome test", also called the
# hit-rate test. You cannot observe the standard of suspicion an officer
# applies before searching somebody. You CAN observe how often searches turn
# something up. If the same evidentiary bar were applied to everyone, the share
# of searches that find contraband would be roughly equal across groups.
#
# It is not:
#
#   white   3.14% of stops searched, contraband found in 24.2% of searches
#   Black  15.52% of stops searched, contraband found in  9.2% of searches
#
# Searched nearly five times as often, and found with contraband less than half
# as often. Under the outcome test that pattern implies a LOWER bar for
# searching Black drivers -- more searches on weaker evidence.
#
# WHY THE OUTCOME TEST AND NOT A RATE COMPARISON. "Black drivers are searched
# more" invites the reply that the stopped populations differ. The hit rate
# answers that reply without needing to know anything about who was stopped:
# it conditions on a search having happened, and asks what the officer found.
# Its own weakness -- infra-marginality -- is stated in the lab.
#
# SOURCE. Stanford Open Policing Project, San Francisco, CA, 2020-04-01
# release. https://openpolicing.stanford.edu/data/
#   https://stacks.stanford.edu/file/druid:yg821jf8611/
#     yg821jf8611_ca_san_francisco_2020_04_01.csv.zip     (25 MB zipped)
#
# NOTE ON RACE. The race field is the officer's perception as recorded, not
# self-report. That is a real limitation and it is also the right variable:
# what matters for a claim about the officer's behaviour is what the officer
# believed.
#
# Run from this directory:  python3 build-data.py
# ---------------------------------------------------------------------------

import collections, csv, io, urllib.request, zipfile

# raw/ holds the sources as they arrive; derived/ is what this script writes.
import os

# Fetches go through provenance.py, which records url, bytes, hash and row
# count in PROVENANCE.tsv and prints a banner when a source moves under us --
# a URL that still returns 200 and no longer means what it meant.
import sys as _sys
_sys.path.insert(0, os.path.join("..", "..", "..", "_lib"))
import provenance as prov
os.makedirs("derived", exist_ok=True)

URL = ("https://stacks.stanford.edu/file/druid:yg821jf8611/"
       "yg821jf8611_ca_san_francisco_2020_04_01.csv.zip")

print("downloading (25 MB) ...")
raw = prov.fetch_bytes(URL, timeout=900)
z = zipfile.ZipFile(io.BytesIO(raw))
name = z.namelist()[0]

is_true = lambda v: v.strip().upper() == "TRUE"
by_race = collections.defaultdict(collections.Counter)
by_year = collections.defaultdict(collections.Counter)

with z.open(name) as fh:
    for row in csv.DictReader(io.TextIOWrapper(fh, encoding="utf-8",
                                               errors="replace")):
        race = row["subject_race"].strip()
        if not race or race == "NA":
            continue
        for store, key in ((by_race, race), (by_year, (row["date"][:4], race))):
            d = store[key]
            d["stops"] += 1
            if is_true(row["search_conducted"]):
                d["searched"] += 1
                if is_true(row["contraband_found"]):
                    d["hit"] += 1
            if is_true(row["arrest_made"]):
                d["arrest"] += 1

with open("derived/by_race.csv", "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["race", "stops", "searched", "contraband_found", "arrests"])
    for race, d in sorted(by_race.items(), key=lambda kv: -kv[1]["stops"]):
        w.writerow([race, d["stops"], d["searched"], d["hit"], d["arrest"]])

with open("derived/by_year.csv", "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["year", "race", "stops", "searched", "contraband_found"])
    for (yr, race), d in sorted(by_year.items()):
        if d["stops"] >= 200:                # suppress unusable cells
            w.writerow([yr, race, d["stops"], d["searched"], d["hit"]])

total = sum(d["stops"] for d in by_race.values())
print(f"{total:,} stops, {len(by_race)} race categories")

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
