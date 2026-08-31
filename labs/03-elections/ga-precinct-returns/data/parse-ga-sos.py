#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# Georgia precinct returns, all 159 counties, from the Secretary of State.
#
#   derived/precincts.csv      one row per precinct: votes for each major candidate,
#                      the two-party share, and turnout where reported
#   derived/vote_methods.csv   precinct x candidate x HOW THE BALLOT WAS CAST
#   derived/counties.csv       county totals, precinct counts, size spread
#   derived/structure.csv      what the source contains, for the lab's first section
#
# WHY THIS REPLACES VEST FOR GEORGIA
#
# VEST gives precinct x candidate. This gives **precinct x candidate x vote
# method** -- Election Day, Advanced Voting, Absentee by Mail, Provisional --
# because that is how Georgia's counties tabulate and report. In 2020 the split
# between those four *is* the story, and VEST cannot answer it at all.
#
# It is also the primary source: these files are the counties' own tabulation
# exports, published by the state, not a third-party reconciliation of them.
#
# ---------------------------------------------------------------------------
# TWO SOURCE FORMATS, ONE SHAPE
#
# The SoS has published the same data two ways, and this script reads both.
#
# A. XML  (2012 - June 2024)   https://sos.ga.gov/page/historical-elections-results
#    One ZIP per election. Inside: detail/xml/<County>_*.zip, 159 of them, each
#    holding one detail.xml:
#
#      ElectionResult
#        VoterTurnout/Precincts/Precinct   name, totalVoters, ballotsCast
#        Contest        text="President of the United States"
#          Choice       text="Joseph R. Biden (Dem)" totalVotes=...
#            VoteType   name="Absentee by Mail Votes"
#              Precinct name, votes
#
#    **Cloudflare blocks scripts on sos.ga.gov**, so the outer ZIP is a manual
#    browser download. Everything after that is automatic. See README.md.
#
# B. JSON (2024 general)
#    https://results.sos.ga.gov/cdn/results/Georgia/export-2024NovGen.json
#    ~34 MB, and **this one IS script-fetchable** -- the CDN path is not behind
#    the bot check. Same shape, different names:
#
#      localResults[]            one per county
#        ballotItems[]           contest
#          ballotOptions[]       candidate, voteCount, politicalParty
#            groupResults[]      vote method
#            precinctResults[]   precinct, voteCount, groupResults[]
#
# ---------------------------------------------------------------------------
# IS THE JSON TRUSTWORTHY? CHECKED, AND MOSTLY YES.
#
# It carries no "certified" flag, so it was tested rather than trusted:
#
#   * Statewide presidential totals come to **Trump 2,663,117 / Harris
#     2,548,017**, which are Georgia's certified 2024 figures.
#   * Summing the 159 counties independently reproduces the top-level totals
#     exactly, for every presidential candidate.
#   * Vote-method sums equal each candidate's total in **all 5,009**
#     county-contest-candidate rows. No exceptions.
#   * `createdAt` is 2025-01-30, well after certification.
#
# Two things to know before relying on it:
#
#   * **84 rows have precinct sums that fall a few votes short of the
#     candidate's total** -- 2 to 4 votes, in 23 counties, Fulton worst with 32.
#     Every one is a down-ballot local contest (Board of Education, County
#     Commission, a Sunday sales referendum). **No statewide contest is
#     affected.** This script reports the count rather than hiding it, and the
#     lab uses the presidential race, which is clean.
#   * **Two candidates appear in county data but not in the statewide roll-up**
#     -- Cornel West and Claudia De la Cruz, who were removed from Georgia's
#     ballot. Their county rows exist with votes; the state total omits them.
#
# ---------------------------------------------------------------------------
# Usage
#
#   python3 parse-ga-sos.py --xml "raw/November 3,2020-General Election"
#   python3 parse-ga-sos.py --json raw/export-2024NovGen.json
#   python3 parse-ga-sos.py --json-url          # fetches 2024 directly
#
# Optional:  --contest "President"   (default; a case-insensitive substring)
#            --out-prefix ""         (prefix for the four output CSVs,
#                                     which are always written to derived/)
# ---------------------------------------------------------------------------

import argparse, csv, io, json, os, re, sys, urllib.request, zipfile
import xml.etree.ElementTree as ET
from collections import defaultdict

# Fetches go through provenance.py, which records url, bytes, hash and row
# count in PROVENANCE.tsv and prints a banner when a source moves under us --
# a URL that still returns 200 and no longer means what it meant.
sys.path.insert(0, os.path.join("..", "..", "..", "_lib"))
import provenance as prov

JSON_2024 = "https://results.sos.ga.gov/cdn/results/Georgia/export-2024NovGen.json"

PARTY = re.compile(r"\((Rep|Dem|Lib|Grn|Ind|I)\)\s*$", re.I)


def party_of(name, explicit=None):
    """Major-party label. The XML puts it in the candidate string, e.g.
    'Joseph R. Biden (Dem)'; the JSON has a politicalParty field. '(I)' means
    incumbent, not Independent, and is stripped before the party is read."""
    if explicit:
        p = explicit.strip().lower()
        if p.startswith("rep"): return "REP"
        if p.startswith("dem"): return "DEM"
        return "OTH"
    s = re.sub(r"\(I\)", "", name or "")
    m = re.findall(r"\((Rep|Dem|Lib|Grn|Ind)\)", s, re.I)
    if not m: return "OTH"
    t = m[-1].lower()
    return "REP" if t == "rep" else "DEM" if t == "dem" else "OTH"


def clean_name(name):
    return re.sub(r"\s+", " ", re.sub(r"\((I|Rep|Dem|Lib|Grn|Ind)\)", "", name or "")).strip()


# --------------------------------------------------------------------------
# Source A: the XML archives
# --------------------------------------------------------------------------

def read_xml_folder(folder, contest_pat):
    """Walk detail/xml/*.zip -> one detail.xml per county."""
    xml_dir = os.path.join(folder, "detail", "xml")
    if not os.path.isdir(xml_dir):
        sys.exit(f"no detail/xml/ under {folder!r} -- is this an unzipped SoS election folder?")
    zips = sorted(f for f in os.listdir(xml_dir) if f.lower().endswith(".zip"))
    print(f"  {len(zips)} county archives in detail/xml/")
    rows, turnout, meta = [], [], []
    xml_short = []
    for zf in zips:
        county = re.sub(r"_\d+_\d+-detailxml\.zip$", "", zf).replace("_", " ")
        with zipfile.ZipFile(os.path.join(xml_dir, zf)) as z:
            inner = [n for n in z.namelist() if n.lower().endswith(".xml")]
            if not inner:
                print(f"     ! {county}: no xml inside"); continue
            root = ET.fromstring(z.read(inner[0]))
        county = (root.get("Region") or root.findtext("Region") or county)
        reg = root.find(".//VoterTurnout")
        if reg is not None:
            for p in reg.findall(".//Precinct"):
                turnout.append(dict(county=county, precinct=p.get("name"),
                                    registered=p.get("totalVoters"),
                                    ballots_cast=p.get("ballotsCast")))
        contests = root.findall(".//Contest")
        meta.append(dict(county=county, contests=len(contests)))
        for c in contests:
            cname = c.get("text") or ""
            if not contest_pat.search(cname):
                continue
            for ch in c.findall("Choice"):
                cand = ch.get("text") or ""
                seen = {}
                for vt in ch.findall(".//VoteType"):
                    method = vt.get("name") or ""
                    for p in vt.findall("Precinct"):
                        v = p.get("votes")
                        if v is None or v == "":
                            continue
                        rows.append(dict(
                            county=county, precinct=p.get("name"), contest=cname,
                            candidate=clean_name(cand), party=party_of(cand),
                            method=method, votes=int(v)))
                        seen[p.get("name")] = True
                # reconcile against the candidate's stated total for this contest
                stated = ch.get("totalVotes")
                if stated is not None:
                    got = sum(r["votes"] for r in rows
                              if r["county"] == county and r["contest"] == cname
                              and r["candidate"] == clean_name(cand))
                    if int(stated) != got:
                        xml_short.append((county, cname[:28], clean_name(cand),
                                          int(stated), got))
    if xml_short:
        print(f"  ! {len(xml_short)} candidate totals not reproduced by their "
              f"precinct x method rows; first: {xml_short[0]}")
    else:
        print("  every candidate total is reproduced exactly by its "
              "precinct x method rows")
    return rows, turnout, meta


# --------------------------------------------------------------------------
# Source B: the JSON export
# --------------------------------------------------------------------------

def read_json(path_or_url, contest_pat):
    if path_or_url.startswith("http"):
        print("  downloading (~34 MB) ...", flush=True)
        raw = prov.fetch_bytes(path_or_url, timeout=600)
    else:
        raw = open(path_or_url, "rb").read()
    d = json.loads(raw)
    print(f"  {d.get('electionName')} {d.get('electionDate')} "
          f"| snapshot {d.get('createdAt','?')[:10]} | {len(d['localResults'])} counties")
    rows, meta = [], []
    short = method_ok = method_missing = 0
    for c in d["localResults"]:
        county = re.sub(r"\s+County$", "", c["name"])
        meta.append(dict(county=county, contests=len(c["ballotItems"])))
        for b in c["ballotItems"]:
            for o in b["ballotOptions"]:
                pr = o.get("precinctResults") or []
                if sum(p["voteCount"] for p in pr) != o["voteCount"]:
                    short += 1
                if not contest_pat.search(b.get("name") or ""):
                    continue
                for p in pr:
                    # p["voteCount"] is authoritative. The method breakdown is
                    # OFTEN ABSENT -- see the coverage note below -- so build
                    # from it only where every group carries a number, and book
                    # the remainder to "(not reported)" so the total still
                    # reconciles. Never silently drop votes.
                    ptot = int(p.get("voteCount") or 0)
                    gs = [g for g in (p.get("groupResults") or [])
                          if g.get("voteCount") is not None]
                    allg = p.get("groupResults") or []
                    usable = bool(allg) and len(gs) == len(allg)
                    base = dict(county=county, precinct=p.get("name"),
                                contest=b["name"], candidate=clean_name(o["name"]),
                                party=party_of(o["name"], o.get("politicalParty")))
                    if usable:
                        got = 0
                        for g in gs:
                            v = int(g["voteCount"])
                            got += v
                            rows.append(dict(base, method=g["groupName"], votes=v))
                        if got != ptot:
                            rows.append(dict(base, method="(not reported)", votes=ptot - got))
                        method_ok += 1
                    else:
                        rows.append(dict(base, method="(not reported)", votes=ptot))
                        method_missing += 1
    print(f"  rows whose precinct sums fall short of the candidate total: {short} "
          f"(all down-ballot; see header)")
    tot = method_ok + method_missing
    if tot:
        print(f"  vote-method breakdown present for {method_ok:,} of {tot:,} "
              f"precinct-candidate rows ({100*method_ok/tot:.0f}%)")
    return rows, [], meta


# --------------------------------------------------------------------------
# Source C: the summary/ CSVs -- county totals only, but ALWAYS COMPLETE
# --------------------------------------------------------------------------

def read_summary_folder(folder, contest_pat):
    """summary/<County>_*.zip -> summary.csv, one row per contest x choice.

    WHY THIS EXISTS. The 2020 RECOUNT archive carries 159 county summaries and
    the precinct detail for exactly ONE county (Lanier). So the recount cannot
    be compared to the original at precinct level -- only at county level. That
    is a fact about what Georgia published, not a gap in this script.
    """
    sdir = os.path.join(folder, "summary")
    if not os.path.isdir(sdir):
        sys.exit(f"no summary/ under {folder!r}")
    zips = sorted(f for f in os.listdir(sdir) if f.lower().endswith(".zip"))
    print(f"  {len(zips)} county summaries")
    out = []
    for zf in zips:
        # "Ben_Hill_107248_272701-summary.zip" -> "Ben Hill". Splitting on the
        # FIRST underscore silently truncated Ben Hill and Jeff Davis.
        county = re.sub(r"_\d+_\d+-summary\.zip$", "", zf).replace("_", " ")
        with zipfile.ZipFile(os.path.join(sdir, zf)) as z:
            inner = [n for n in z.namelist() if n.lower().endswith(".csv")]
            if not inner:
                continue
            txt = z.read(inner[0]).decode("utf-8-sig", "replace")
        for r in csv.DictReader(io.StringIO(txt)):
            if not contest_pat.search(r.get("contest name") or ""):
                continue
            out.append(dict(county=county,
                            contest=r["contest name"],
                            candidate=clean_name(r["choice name"]),
                            party=party_of(r["choice name"]),
                            votes=int(r["total votes"] or 0),
                            registered=r.get("registered voters", ""),
                            ballots_cast=r.get("ballots cast", "")))
    return out


# --------------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--xml", help="unzipped SoS election folder")
    ap.add_argument("--json", help="export-*.json file")
    ap.add_argument("--json-url", action="store_true", help="fetch the 2024 export")
    ap.add_argument("--summary", help="SoS election folder, county summaries only")
    ap.add_argument("--contest", default="President of the U")
    ap.add_argument("--out-prefix", default="")
    a = ap.parse_args()

    # Outputs are derived data and belong in derived/, whatever prefix the
    # caller passes; the SoS exports they are parsed from stay in raw/.
    os.makedirs("derived", exist_ok=True)
    a.out_prefix = os.path.join("derived", a.out_prefix)

    # ANCHOR THE CONTEST MATCH. An earlier version matched the bare substring
    # "President" and silently swept in
    #     "Co Commission Chair/Presidente de la Comisión del Condado"
    # -- County Commission Chair, in Spanish -- which put a Gwinnett commission
    # candidate into the presidential totals with 233,110 votes. Georgia's
    # bilingual counties label the real contest
    #     "President of the United States/Presidentede los Estados Unidos"
    # so the match has to be specific enough to keep that and drop the other.
    # The two sources also name the race differently -- the XML says "President
    # of the United States", the 2024 JSON says "President of the US" -- so the
    # default is the longest prefix common to both.
    pat = re.compile(re.escape(a.contest), re.I)
    if a.summary:
        print(f"reading county summaries, contest matching {a.contest!r}")
        rows = read_summary_folder(a.summary, pat)
        if not rows:
            sys.exit("no matching contest in the summaries")
        with open(f"{a.out_prefix}counties.csv", "w", newline="") as fh:
            w = csv.DictWriter(fh, list(rows[0].keys())); w.writeheader(); w.writerows(rows)
        agg = defaultdict(int)
        for r in rows: agg[r["candidate"]] += r["votes"]
        print(f"\n  counties {len({r['county'] for r in rows})}")
        for c, v in sorted(agg.items(), key=lambda x: -x[1])[:4]:
            print(f"     {c:28s} {v:>10,}")
        print(f"  wrote {a.out_prefix}counties.csv")
        return
    if a.xml:
        print(f"reading XML archives, contest matching {a.contest!r}")
        rows, turnout, meta = read_xml_folder(a.xml, pat)
    elif a.json or a.json_url:
        print(f"reading JSON export, contest matching {a.contest!r}")
        rows, turnout, meta = read_json(a.json or JSON_2024, pat)
    else:
        sys.exit("give --xml FOLDER, --json FILE, or --json-url")

    if not rows:
        sys.exit(f"no contest matched {a.contest!r}")

    labels = sorted({r["contest"] for r in rows})
    if len(labels) > 1:
        print(f"  {len(labels)} distinct contest labels matched -- check they are "
              f"all the same race:")
        for L in labels:
            print(f"     {L}")

    P = a.out_prefix
    # ---- vote_methods.csv : the thing VEST cannot give you -----------------
    with open(f"{P}vote_methods.csv", "w", newline="") as fh:
        w = csv.DictWriter(fh, ["county","precinct","contest","candidate","party","method","votes"])
        w.writeheader(); w.writerows(rows)

    # ---- precincts.csv : one row per precinct, candidates as columns -------
    by_pr = defaultdict(lambda: defaultdict(int))
    cands = defaultdict(int)
    for r in rows:
        by_pr[(r["county"], r["precinct"])][r["candidate"]] += r["votes"]
        cands[r["candidate"]] += r["votes"]
    top = [c for c, _ in sorted(cands.items(), key=lambda x: -x[1])]
    party_of_cand = {r["candidate"]: r["party"] for r in rows}
    dem = next((c for c in top if party_of_cand.get(c) == "DEM"), None)
    rep = next((c for c in top if party_of_cand.get(c) == "REP"), None)
    tmap = {(t["county"], t["precinct"]): t for t in turnout}

    with open(f"{P}precincts.csv", "w", newline="") as fh:
        cols = ["county","precinct","registered","ballots_cast"] + top + ["total","dem_two_party_pct"]
        w = csv.writer(fh); w.writerow(cols)
        for (co, pr), d in sorted(by_pr.items()):
            t = tmap.get((co, pr), {})
            votes = [d.get(c, 0) for c in top]
            tot = sum(votes)
            two = d.get(dem, 0) + d.get(rep, 0)
            pct = round(100 * d.get(dem, 0) / two, 2) if two else ""
            w.writerow([co, pr, t.get("registered",""), t.get("ballots_cast","")] + votes + [tot, pct])

    # ---- counties.csv -----------------------------------------------------
    by_co = defaultdict(lambda: defaultdict(int)); npr = defaultdict(set)
    for r in rows:
        by_co[r["county"]][r["candidate"]] += r["votes"]; npr[r["county"]].add(r["precinct"])
    with open(f"{P}counties.csv", "w", newline="") as fh:
        w = csv.writer(fh); w.writerow(["county","precincts"] + top + ["total","dem_two_party_pct"])
        for co, d in sorted(by_co.items()):
            votes = [d.get(c, 0) for c in top]
            two = d.get(dem, 0) + d.get(rep, 0)
            w.writerow([co, len(npr[co])] + votes + [sum(votes),
                       round(100 * d.get(dem,0)/two, 2) if two else ""])

    # ---- structure.csv ----------------------------------------------------
    methods = sorted({r["method"] for r in rows})
    with open(f"{P}structure.csv", "w", newline="") as fh:
        w = csv.writer(fh); w.writerow(["item","value"])
        for k, v in [("counties", len(by_co)),
                     ("precincts", len(by_pr)),
                     ("contest", rows[0]["contest"]),
                     ("candidates", len(top)),
                     ("vote methods", " | ".join(methods)),
                     ("contests in the source (median county)",
                      sorted(m["contests"] for m in meta)[len(meta)//2] if meta else ""),
                     ("rows in vote_methods.csv", len(rows))]:
            w.writerow([k, v])

    tot = sum(cands.values())
    print(f"\n  counties {len(by_co)}   precincts {len(by_pr):,}   rows {len(rows):,}")
    print(f"  methods: {', '.join(methods)}")
    print(f"  {rows[0]['contest']}:")
    for c in top[:4]:
        print(f"     {c:28s} {cands[c]:>10,}  ({100*cands[c]/tot:5.2f}%)")
    print(f"  wrote {P}precincts.csv, {P}vote_methods.csv, {P}counties.csv, {P}structure.csv")


if __name__ == "__main__":
    main()
    # Anything fetched above is now in PROVENANCE.tsv; say so if a
    # source moved. Inside the __main__ guard so importing this
    # module for its parser does not print a banner.
    prov.report()
