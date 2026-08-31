#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# Build the models-markets dataset: do market prices mean what they say?
#
# PYTHON, not R, because this reads two JSON APIs. The lab is base R and reads
# only the CSV this produces.
#
# One file ends up in this folder:
#
#   derived/polymarket_resolved.csv   746 resolved Polymarket markets, each with the
#                             price 7 and 30 days before it closed, and what
#                             actually happened.
#
# WHY THIS SHAPE. You cannot evaluate a single probabilistic forecast. "70%"
# is not refuted by the thing failing to happen. The only way to check a
# forecaster is to collect MANY forecasts and ask whether the things they put
# at 70% happened about 70% of the time. That is calibration, and it needs a
# few hundred resolved questions -- which is exactly what a prediction market
# leaves behind.
#
# WHAT WAS FOUND (see the lab for the full table):
#   * Calibration above 0.3 is very good. Markets priced 0.8-0.9 happened
#     84.6% of the time; priced 0.9-1.0, 97.8%.
#   * Brier score 0.064, against 0.25 for always saying 50% and 0.157 for
#     always saying the base rate.
#   * Below 0.3 the market looks like it underprices: the 0.1-0.3 band expected
#     8.8 and got 15 (p = 0.037). **But that is one marginal result out of ten
#     bins tested**, which is roughly what chance produces. The lab says so.
#   * 70.6% of these markets were priced under 0.10. Most questions people
#     trade are about things that do not happen.
#
# TWO CAVEATS BUILT INTO THE DATA
#   * These are the highest-VOLUME resolved markets, not a random sample, and
#     not only political ones -- sport, crypto and Fed decisions are in here.
#     Calibration of a market mechanism is the question; American politics is
#     not the population.
#   * The price 7 days out is one arbitrary moment. A market that was wrong for
#     months and right at the end looks good here. p30 is included so the lab
#     can test that.
#
# ENDPOINTS (both public, no key; both reject requests without a User-Agent)
#   https://gamma-api.polymarket.com/markets?closed=true&order=volumeNum
#   https://clob.polymarket.com/prices-history?market=<clobTokenId>&interval=max
#
# Run from this directory:  python3 build-data.py     (takes ~15 minutes)
# ---------------------------------------------------------------------------

import csv, datetime as dt, json, time, urllib.request

# raw/ holds the sources as they arrive; derived/ is what this script writes.
import os

# Fetches go through provenance.py, which records url, bytes, hash and row
# count in PROVENANCE.tsv and prints a banner when a source moves under us --
# a URL that still returns 200 and no longer means what it meant.
import sys as _sys
_sys.path.insert(0, os.path.join("..", "..", "..", "_lib"))
import provenance as prov
os.makedirs("derived", exist_ok=True)

UA = {"User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
                    "AppleWebKit/537.36 Chrome/120.0 Safari/537.36"}

def get(url, tries=3):
    for i in range(tries):
        try:
            # UA passed through: these endpoints refuse a bare request.
            return json.loads(prov.fetch_bytes(url, headers=UA, timeout=60))
        except Exception:
            if i == tries - 1:
                return None
            time.sleep(2)

markets = []
for offset in range(0, 900, 100):
    batch = get("https://gamma-api.polymarket.com/markets"
                f"?closed=true&limit=100&order=volumeNum&ascending=false&offset={offset}")
    if not batch:
        break
    markets += batch
print(f"markets fetched: {len(markets)}")

rows = []
for i, m in enumerate(markets):
    try:
        prices = json.loads(m["outcomePrices"])
        tokens = json.loads(m["clobTokenIds"])
        # settled markets resolve to exactly "1" or "0"; anything else is
        # unresolved, void, or scalar, and cannot be scored
        if prices[0] not in ("1", "0"):
            continue
        end = dt.datetime.fromisoformat(m["endDate"].replace("Z", "+00:00"))
        hist = get(f"https://clob.polymarket.com/prices-history"
                   f"?market={tokens[0]}&interval=max&fidelity=1440")
        if not hist:
            continue
        hist = hist.get("history", [])
        if len(hist) < 10:
            continue

        snap = {}
        for label, days in (("p7", 7), ("p30", 30)):
            cutoff = (end - dt.timedelta(days=days)).timestamp()
            before = [p for p in hist if p["t"] <= cutoff]
            snap[label] = round(before[-1]["p"], 4) if before else ""
        if snap["p7"] == "":
            continue

        rows.append({"question": m["question"][:120],
                     "p7": snap["p7"], "p30": snap["p30"],
                     "outcome": int(prices[0]),
                     "volume": round(float(m.get("volumeNum") or 0)),
                     "end_date": m["endDate"][:10]})
    except Exception:
        pass
    time.sleep(0.08)          # be polite; there is no documented rate limit

with open("derived/polymarket_resolved.csv", "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=["question", "p7", "p30", "outcome",
                                      "volume", "end_date"])
    w.writeheader()
    w.writerows(rows)

yes = sum(r["outcome"] for r in rows)
brier = sum((r["p7"] - r["outcome"]) ** 2 for r in rows) / len(rows)
print(f"usable: {len(rows)}  resolved yes: {yes}")
print(f"Brier score: {brier:.4f}")

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
