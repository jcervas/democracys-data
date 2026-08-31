#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# Build the census-geography dataset: what nests, and what does not.
#
#   derived/levels.csv    One row per geography type: how many exist, median land area,
#                 what its ID encodes, and what it nests into.
#   derived/nesting.csv   The nesting tests, run and recorded.
#
# WHY THIS LAB EXISTS. Five labs in this course fail in the same place, for the
# same reason, and none of them explains it: precincts do not nest into census
# blocks; HOLC areas do not nest into tracts; "jurisdiction" means a township in
# Wisconsin and a county in Texas. Every one of those is the same fact --
# **American geography is two systems, and only one of them nests.**
#
# THE SPINE THAT NESTS. block -> block group -> tract -> county -> state, and
# each is encoded in the identifier by prefix. An 11-digit tract GEOID is
# state(2) + county(3) + tract(6), so the first five digits ARE the county.
# Verified here: all 85,396 tracts match a county by prefix. Zero orphans.
#
# THE ONES THAT DO NOT. Places, ZCTAs, school districts, congressional
# districts. These are drawn for other purposes and cut across the spine.
#
# THE WORST OFFENDER IS THE ONE EVERYBODY USES. A ZIP code is not an area at
# all -- it is a set of mail delivery stops, maintained by the Postal Service
# for routing, changed whenever routing changes. The Census approximates them
# with ZCTAs, and the gazetteer file for ZCTAs has **no state column, and no
# state in the identifier.** You cannot say what state a ZCTA is in from its
# ID, because some of them are in two.
#
# THAT MATTERS HERE SPECIFICALLY: the census replication survey students take
# on the first day of class asks for their ZIP code.
#
# SOURCE. Census Bureau Gazetteer Files, 2024.
#   https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2024_Gazetteer/
# All keyless. Seven geography types are pulled.
#
# THE SHAPES ARE NEXT DOOR. This script has no geometry in it -- it counts the
# containers and reads their identifiers. `build-maps.R` in this folder draws
# them, for one county (Houston County, Georgia), from TIGER/Line 2020, and
# counts how many census tracts a city limit and a ZCTA boundary cut. Run both;
# the three .Rmd documents read the CSVs from each.
#
# Run from this directory:  python3 build-data.py
# ---------------------------------------------------------------------------

import csv, io, statistics, urllib.request, zipfile

# raw/ holds the sources as they arrive; derived/ is what this script writes.
import os

# Fetches go through provenance.py, which records url, bytes, hash and row
# count in PROVENANCE.tsv and prints a banner when a source moves under us --
# a URL that still returns 200 and no longer means what it meant.
import sys as _sys
_sys.path.insert(0, os.path.join("..", "..", "..", "_lib"))
import provenance as prov
os.makedirs("derived", exist_ok=True)

BASE = ("https://www2.census.gov/geo/docs/maps-data/data/gazetteer/"
        "2024_Gazetteer/2024_Gaz_{}_national.zip")

def load(kind):
    raw = prov.fetch_bytes(BASE.format(kind), label=kind, timeout=600)
    z = zipfile.ZipFile(io.BytesIO(raw))
    with z.open(z.namelist()[0]) as fh:
        return [{k.strip(): (v.strip() if isinstance(v, str) else v)
                 for k, v in row.items()}
                for row in csv.DictReader(io.TextIOWrapper(fh, encoding="latin-1"),
                                          delimiter="\t")]

KINDS = ["counties", "tracts", "place", "zcta", "cousubs", "elsd", "unsd"]
data = {k: load(k) for k in KINDS}
for k, v in data.items():
    print(f"  {k:<10} {len(v):>8,}")

LABEL = {"counties": "county", "tracts": "census tract", "place": "place",
         "zcta": "ZCTA (ZIP approximation)", "cousubs": "county subdivision",
         "elsd": "elementary school district", "unsd": "unified school district"}
ENCODES = {"counties": "state(2) + county(3)",
           "tracts": "state(2) + county(3) + tract(6)",
           "place": "state(2) + place(5)",
           "zcta": "five digits, nothing else",
           "cousubs": "state(2) + county(3) + subdivision(5)",
           "elsd": "state(2) + district(5)",
           "unsd": "state(2) + district(5)"}
NESTS = {"counties": "state", "tracts": "county", "place": "state only",
         "zcta": "NOTHING", "cousubs": "county",
         "elsd": "state only", "unsd": "state only"}

def median_area(rows):
    v = [float(r["ALAND_SQMI"]) for r in rows
         if r.get("ALAND_SQMI") and r["ALAND_SQMI"] not in ("", "0")]
    return round(statistics.median(v), 2) if v else ""

with open("derived/levels.csv", "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["geography", "count", "id_length", "id_encodes",
                "nests_into", "median_sq_mi"])
    for k in KINDS:
        rows = data[k]
        w.writerow([LABEL[k], len(rows), len(rows[0]["GEOID"]), ENCODES[k],
                    NESTS[k], median_area(rows)])

# --- the nesting tests ------------------------------------------------------
counties = {c["GEOID"] for c in data["counties"]}
tract_ok = sum(1 for t in data["tracts"] if t["GEOID"][:5] in counties)
sub_ok   = sum(1 for s in data["cousubs"] if s["GEOID"][:5] in counties)
zcta_has_state = "USPS" in data["zcta"][0]
place_has_county = any("COUNTY" in c.upper() for c in data["place"][0])

with open("derived/nesting.csv", "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["test", "result", "n_pass", "n_total"])
    w.writerow(["tract GEOID[:5] is a real county", tract_ok == len(data["tracts"]),
                tract_ok, len(data["tracts"])])
    w.writerow(["county subdivision GEOID[:5] is a real county",
                sub_ok == len(data["cousubs"]), sub_ok, len(data["cousubs"])])
    w.writerow(["ZCTA file records a state", zcta_has_state, "", ""])
    w.writerow(["place file records a county", place_has_county, "", ""])

print(f"\ntracts nesting into counties: {tract_ok:,}/{len(data['tracts']):,}")
print(f"ZCTA file has a state column: {zcta_has_state}")

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
