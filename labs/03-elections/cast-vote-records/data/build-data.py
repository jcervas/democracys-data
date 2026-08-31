#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# Build the cast-vote-records dataset: Alaska's cast vote record.
#
# THIS BUILDER IS PYTHON, not R, which breaks the pattern in every other lab.
# The reason: the source is 2,043 JSON files totalling 1.58 GB, and base R has
# no JSON parser. The lab itself is base R and reads only the small CSVs below.
#
# Three files end up in this folder:
#
#   derived/sequences.csv          One row per DISTINCT ranking sequence, with how many
#                          ballots cast it. 926 rows standing in for 340,778
#                          ballots. Nothing here identifies anybody.
#   derived/official_rounds.csv    Alaska's own round-by-round result, transcribed from
#                          RCV-USRep.pdf, so students can check their own count.
#   derived/official_transfers.csv Where each eliminated candidate's ballots went.
#
# SOURCE
#   CVR    https://www.elections.alaska.gov/results/24GENR/CVR_Export_20241130154411.zip
#   RCV    https://www.elections.alaska.gov/results/24GENR/RCV-USRep.pdf
#   2024 General Election, State of Alaska, U.S. Representative (contest Id 7).
#
# WHAT THE CVR IS. A Dominion export: one JSON "session" per scanned ballot,
# with Cards -> Contests -> Marks, each mark carrying a CandidateId and a Rank.
# It is the closest thing to a public record of individual votes that exists in
# the United States, and it exists because Alaska chose to publish it.
#
# WHAT IT LEAVES OUT, in Alaska's own words: ballots counted by hand are not
# included, and the names voters wrote in are not shown. So this file does NOT
# reproduce the certified totals, and it is not supposed to.
#
# HOW CLOSE IT GETS. Re-running the count from these ballots reproduces the
# official percentages exactly -- 51.22% / 48.78% in the final round -- while
# the raw counts differ by about 150 ballots in 322,000 (0.05%). The gap is not
# error: the official tabulator applies rules this reconstruction does not,
# including "exhausted on two or more ranks skipped" and its own overvote
# handling. Both facts belong in the lab.
#
# PRIVACY. A CVR is public and carries no name -- but it is individual-level,
# and in a small precinct a distinctive ranking can be identifying. That is why
# the committed file is COLLAPSED TO DISTINCT SEQUENCES WITH COUNTS, and why
# precinct and tabulator identifiers are dropped here rather than published.
#
# Run from this directory:  python3 build-data.py
# (Downloads ~44 MB; nothing large is written to disk.)
# ---------------------------------------------------------------------------

import collections, csv, io, json, urllib.request, zipfile

# raw/ holds the sources as they arrive; derived/ is what this script writes.
import os

# Fetches go through provenance.py, which records url, bytes, hash and row
# count in PROVENANCE.tsv and prints a banner when a source moves under us --
# a URL that still returns 200 and no longer means what it meant.
import sys as _sys
_sys.path.insert(0, os.path.join("..", "..", "..", "_lib"))
import provenance as prov
os.makedirs("derived", exist_ok=True)

URL = ("https://www.elections.alaska.gov/results/24GENR/"
       "CVR_Export_20241130154411.zip")
CONTEST = 7          # U.S. Representative

print("downloading CVR ...")
raw = prov.fetch_bytes(URL, timeout=600)
z = zipfile.ZipFile(io.BytesIO(raw))

cand = {c["Id"]: c["Description"]
        for c in json.load(z.open("CandidateManifest.json"))["List"]}

seqs = collections.Counter()
ballots = 0
for name in z.namelist():
    if not name.startswith("CvrExport"):
        continue
    for s in json.load(z.open(name)).get("Sessions", []):
        for card in (s.get("Original") or {}).get("Cards", []):
            for con in card.get("Contests", []):
                if con.get("Id") != CONTEST:
                    continue
                ballots += 1
                by_rank = collections.defaultdict(list)
                for m in con.get("Marks", []):
                    if m.get("IsVote"):
                        by_rank[m["Rank"]].append(m["CandidateId"])
                seq = []
                for r in sorted(by_rank):
                    # two marks at one rank is an overvote at that rank
                    seq.append(by_rank[r][0] if len(by_rank[r]) == 1 else -1)
                seqs[tuple(seq)] += 1

print(f"{ballots:,} ballots, {len(seqs):,} distinct ranking sequences")

K = max(len(s) for s in seqs)
rows = sorted(seqs.items(), key=lambda kv: -kv[1])
with open("derived/sequences.csv", "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["ballots"] + [f"rank{i+1}" for i in range(K)])
    for seq, n in rows:
        cells = [("OVERVOTE" if c == -1 else cand.get(c, f"id{c}")) for c in seq]
        w.writerow([n] + cells + [""] * (K - len(cells)))

print("wrote sequences.csv")
print("official_rounds.csv and official_transfers.csv are transcribed by hand "
      "from RCV-USRep.pdf and are not regenerated here.")

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
