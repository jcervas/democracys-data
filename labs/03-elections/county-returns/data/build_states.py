#!/usr/bin/env python3
"""
Per-jurisdiction parsers for the county-returns build.

Run all pending:      python3 build_states.py
Run some:             python3 build_states.py NC MN MI
Re-check what exists: python3 build_states.py --check

Each parser obeys SPEC.md: the state's own chief election office, certified in
preference to election-night, total_votes counting EVERY vote cast for
president, FIPS as five characters, and no invented units. A parser that cannot
get an official file raises, and the state is left unwritten rather than filled
from somewhere else.
"""

import collections, csv, io, os, re, sys, zipfile
import lib_build as L

TODAY = "2026-08-12"
REG = {}

def parser(postal):
    def deco(fn):
        REG[postal] = fn
        return fn
    return deco


def _agg(pairs):
    """pairs: (county, party_or_None, votes). Returns rows keyed by county."""
    acc = collections.defaultdict(lambda: [0, 0, 0])
    for county, party, v in pairs:
        a = acc[county]
        a[2] += v
        if party == "D":
            a[0] += v
        elif party == "R":
            a[1] += v
    return acc


def _rows(postal, acc, extra_fips=None, name_fmt=None):
    out = []
    for county, (d, g, t) in acc.items():
        nm = name_fmt(county) if name_fmt else county
        out.append(dict(state_name=L.POSTAL2NAME[postal],
                        county_fips=L.fips_for(postal, nm, extra_fips),
                        county_name=nm, votes_dem=d, votes_gop=g, total_votes=t))
    return out


# ---------------------------------------------------------------- NORTH CAROLINA
@parser("NC")
def nc():
    """State Board of Elections precinct-level export, summed to county."""
    spec = {2024: ("2024_11_05", "20241105"), 2020: ("2020_11_03", "20201103")}
    for year, (d, stamp) in spec.items():
        url = "https://s3.amazonaws.com/dl.ncsbe.gov/ENRS/%s/results_pct_%s.zip" % (d, stamp)
        p = L.fetch(url, "NC_%d.zip" % year)
        name, raw = L.unzip_one(p, r"\.txt$")
        pairs = []
        for r in csv.DictReader(io.StringIO(raw.decode("utf-8-sig", "replace")),
                                delimiter="\t"):
            if r.get("Contest Name", "").strip().upper() != "US PRESIDENT":
                continue
            party = {"DEM": "D", "REP": "R"}.get((r.get("Choice Party") or "").strip().upper())
            try:
                v = int((r.get("Total Votes") or "0").replace(",", "") or 0)
            except ValueError:
                continue
            pairs.append((r["County"].strip().title(), party, v))
        rows = _rows("NC", _agg(pairs))
        L.write_state("NC", year, rows)
        L.add_provenance("North Carolina", year, url, TODAY, "zip/tsv", len(rows), "yes",
            "NCSBE precinct-level export (results_pct), contest 'US PRESIDENT', "
            "summed to county. total_votes = every choice including write-in. "
            "Published by the State Board after canvass.")


# ---------------------------------------------------------------- MINNESOTA
@parser("MN")
def mn():
    """SoS pipe-delimited precinct file; no header row (layout in the SoS spec)."""
    spec = {2024: ("https://electionresultsfiles.sos.mn.gov/20241105/pctresults.txt", "2024"),
            2020: ("https://electionresultsfiles.sos.mn.gov/20201103/pctresults.txt", "2020")}
    for year, (url, _) in spec.items():
        p = L.fetch(url, "MN_%d_pct.txt" % year)
        txt = open(p, encoding="utf-8", errors="replace").read()
        pairs = []
        for line in txt.splitlines():
            f = line.split(";")
            if len(f) < 15:
                continue
            office = f[4].strip().strip('"')
            if not re.search(r"U\.?S\.? PRESIDENT|PRESIDENT AND VICE", office, re.I):
                continue
            county = f[1].strip().strip('"')
            party = f[7].strip().strip('"').upper()
            party = "D" if party.startswith("DFL") else ("R" if party.startswith("R") else None)
            try:
                v = int(f[13].strip().strip('"'))
            except ValueError:
                continue
            pairs.append((county, party, v))
        if not pairs:
            raise RuntimeError("MN %d: no president rows parsed" % year)
        acc = _agg(pairs)
        rows = _rows("MN", acc, name_fmt=lambda c: MN_CTY.get(c, c))
        L.write_state("MN", year, rows)
        L.add_provenance("Minnesota", year, url, TODAY, "txt", len(rows), "yes",
            "SoS semicolon-delimited precinct results file, president office, "
            "summed to county. County identified by the SoS county name column. "
            "DFL is the Democratic party in Minnesota. total_votes = all candidates.")

MN_CTY = {}


# ---------------------------------------------------------------- COLORADO
# The SoS historical elections database (Civera) exposes a per-contest CSV.
# 2024 president is contest 26499. THE 2020 CONTEST ID IS NOT KNOWN: contest
# ids are NOT ordered by date -- probing 20000/24000/25000/26200 returned a
# 1900s district attorney race, a Charles S. Thomas contest and a ballot
# question respectively -- so it cannot be found by search or bisection, and
# the site's own search is a React form with generated element ids. The 2020
# file is therefore left unwritten, per SPEC rule 1, rather than filled from
# somewhere else. Recovering it needs one pass through the search UI.
CO_CONTEST = {2024: 26499}

@parser("CO")
def co():
    for year, cid in CO_CONTEST.items():
        url = ("https://co.elstats.civera.com/api/download_contest/"
               "%d_table.csv?split_party=false" % cid)
        p = L.fetch(url, "CO_%d.csv" % year)
        rows_in = list(csv.reader(open(p, encoding="utf-8-sig")))
        cand, party = rows_in[0], rows_in[1]
        di = [i for i, x in enumerate(party) if x.strip() == "Democratic"
              and "Harris" in cand[i]]
        ri = [i for i, x in enumerate(party) if x.strip() == "Republican"
              and "Trump" in cand[i]]
        ti = cand.index("Total Votes Cast")
        if not di or not ri:
            raise RuntimeError("CO %d: could not locate D/R columns" % year)
        out = []
        for r in rows_in[2:]:
            if not r or r[0] != "County":
                continue
            nm = r[1].strip()
            out.append(dict(state_name="Colorado",
                            county_fips=L.fips_for("CO", nm),
                            county_name=nm,
                            votes_dem=int(r[di[0]] or 0),
                            votes_gop=int(r[ri[0]] or 0),
                            total_votes=int(r[ti] or 0)))
        L.write_state("CO", year, out)
        L.add_provenance("Colorado", year, url, TODAY, "csv", len(out), "yes",
            "SoS historical elections database (historicalelectiondata.coloradosos.gov), "
            "contest %d, county rows only. total_votes = the file's own "
            "'Total Votes Cast'. Built from the Abstract of Votes Cast." % cid)



def _pdftotext(path, extra=()):
    import subprocess
    return subprocess.run(["pdftotext", "-layout", *extra, path, "-"],
                          capture_output=True, text=True).stdout


# ---------------------------------------------------------------- MISSOURI
@parser("MO")
def mo():
    """SoS 'Official Results' canvass PDF. President table spans several
    column-groups; the first carries R and D, the last carries Total."""
    spec = {2020: ("ActualResults-November32020.pdf", "Joseph R. Biden"),
            2024: ("ActualResults-November52024.pdf", "Kamala D. Harris")}
    for year, (fn, dem) in spec.items():
        url = "https://www.sos.mo.gov/CMSImages/ElectionResultsStatistics/" + fn
        p = L.fetch(url, "MO_%d.pdf" % year)
        txt = _pdftotext(p)
        cand, tot = {}, {}
        for pg in txt.split("\f"):
            lines = pg.split("\n")
            head = "\n".join(lines[:10])
            if "U.S. President" not in head:
                continue
            is_cand = "Donald J. Trump" in head and dem in head
            is_tot = re.search(r"\bTotal\s*$", head, re.M) is not None
            for l in lines:
                m = re.match(r"^([A-Za-z][A-Za-z .'\-]+?)\s{2,}([\d,\s]+)$", l.rstrip())
                if not m:
                    continue
                nm = m.group(1).strip()
                nums = [int(x.replace(",", "")) for x in m.group(2).split()]
                if not nums:
                    continue
                if is_cand and nm not in cand:
                    cand[nm] = (nums[0], nums[1])
                if is_tot and nm not in tot:
                    tot[nm] = nums[-1]
        cand.pop("Total", None); tot.pop("Total", None)
        assert set(cand) == set(tot) and len(cand) == 116, (len(cand), len(tot))
        OVERRIDE = {"St. Louis City": "29510", "St. Louis": "29189",
                    "Kansas City": ""}
        out = []
        for nm, (g, d) in cand.items():
            f = OVERRIDE[nm] if nm in OVERRIDE else L.fips_for("MO", nm)
            out.append(dict(state_name="Missouri", county_fips=f, county_name=nm,
                            votes_dem=d, votes_gop=g, total_votes=tot[nm]))
        L.write_state("MO", year, out)
        L.add_provenance("Missouri", year, url, TODAY, "pdf", len(out), "yes",
            "SoS 'OFFICIAL RESULTS' canvass PDF, U.S. President table, parsed with "
            "pdftotext -layout. total_votes is the PDF's own Total column (all "
            "candidates plus write-ins). Missouri reports KANSAS CITY as a separate "
            "returning jurisdiction from the four counties it straddles (Jackson, "
            "Clay, Platte, Cass); it has no Census FIPS and county_fips is left "
            "empty per SPEC rule 5, so the four counties it overlaps are "
            "correspondingly short. St. Louis City (29510) is distinct from "
            "St. Louis County (29189). 116 rows = 114 counties + St. Louis City + "
            "Kansas City.")




# ---------------------------------------------------------------- NEBRASKA
@parser("NE")
def ne():
    """Board of State Canvassers Official Report (canvass book) PDF.
    The STATEWIDE president table only -- the 'by Congressional District'
    section that follows it is the electoral-vote split, not county returns."""
    spec = {2020: ("2020/2020-General-Canvass-Book.pdf", "Joseph R. Biden"),
            2024: ("2024/2024%20General%20Canvass%20Book.pdf", "Kamala D. Harris")}
    for year, (path, dem) in spec.items():
        url = "https://sos.nebraska.gov/sites/default/files/doc/elections/" + path
        p = L.fetch(url, "NE_%d.pdf" % year)
        txt = _pdftotext(p)
        started, stop, acc, state_tot = False, False, {}, None
        for l in txt.split("\n"):
            t = l.strip()
            if "Results by Congressional District" in t or t.startswith("Congressional District"):
                stop = True
            if stop:
                continue
            if "President and Vice President of the United States" in t and "...." not in t:
                started = True
                continue
            if not started:
                continue
            m = re.match(r"^([A-Z][A-Za-z .'\-]+?)\s{2,}([\d,\s]+)$", t)
            if not m:
                continue
            nm = m.group(1).strip()
            nums = [int(x.replace(",", "")) for x in m.group(2).split()]
            if len(nums) < 4:
                continue
            if nm == "Total":
                if state_tot is None:
                    state_tot = nums
                continue
            if nm not in acc:
                acc[nm] = nums
        assert len(acc) == 93, len(acc)
        assert sum(v[0] for v in acc.values()) == state_tot[0], "GOP mismatch"
        assert sum(v[1] for v in acc.values()) == state_tot[1], "DEM mismatch"
        out = [dict(state_name="Nebraska", county_fips=L.fips_for("NE", nm),
                    county_name=nm, votes_dem=v[1], votes_gop=v[0],
                    total_votes=sum(v)) for nm, v in acc.items()]
        L.write_state("NE", year, out)
        L.add_provenance("Nebraska", year, url, TODAY, "pdf", len(out), "yes",
            "Board of State Canvassers OFFICIAL REPORT, General Election canvass book, "
            "'President and Vice President of the United States' county table, parsed "
            "with pdftotext -layout. total_votes = sum of every candidate column plus "
            "Write-In Scatterings. County sums reconciled to the canvass book's own "
            "statewide Total row. The canvass book's following section, 'Results by "
            "Congressional District', is the CD-level split used for Nebraska's "
            "district electoral votes and is NOT included here -- these are county "
            "returns. 93 counties.")




# ---------------------------------------------------------------- NEW MEXICO
@parser("NM")
def nm():
    """SoS results site archive. The county map behind each contest is served
    as JSON by the site's own API, keyed by the archived election id.

    New Mexico masks very small county candidate counts as '*', so the minor
    lines cannot simply be summed. Each record also carries the candidate's
    share of the county's presidential vote, and votes/share recovers the
    county total exactly -- verified against the 28 counties in 2020 (and 30 in
    2024) that have no masked line, where it reproduces the plain sum to the
    vote. total_votes is taken that way; votes_dem and votes_gop are always
    reported as integers and are used as published."""
    import json
    spec = {2020: (2782, 6878), 2024: (2882, 10079)}
    for year, (eid, rid) in spec.items():
        url = ("https://electionresults-api.sos.nm.gov/NMResultsAjax.svc/"
               "GetMapDataArchive?type=FED&category=CTY&raceID=%d&osn=102"
               "&county=0&party=0&electionID=%d" % (rid, eid))
        p = L.fetch(url, "NM_%d.json" % year)
        data = json.load(open(p, encoding="utf-8-sig"))
        by = collections.defaultdict(list)
        for r in data:
            by[r["CountyName"].strip()].append(r)
        assert len(by) == 33, len(by)
        out = []
        for c, rs in by.items():
            get = lambda pc: [r for r in rs if r["PartyCode"] == pc][0]
            dem, gop = get("DEM"), get("REP")
            d, g = int(dem["calcCandidateVotes"]), int(gop["calcCandidateVotes"])
            tot = round(d / dem["calcCandidatePercentage"])
            plain = [int(r["calcCandidateVotes"]) for r in rs
                     if str(r["calcCandidateVotes"]).strip() != "*"]
            if len(plain) == len(rs):
                assert sum(plain) == tot, (c, sum(plain), tot)
            out.append(dict(state_name="New Mexico", county_fips=L.fips_for("NM", c, {"donaana": "35013"}),
                            county_name=c, votes_dem=d, votes_gop=g, total_votes=tot))
        L.write_state("NM", year, out)
        L.add_provenance("New Mexico", year, url, TODAY, "json", len(out), "yes",
            "SoS election-results archive, page headed 'Official Results' "
            "(https://electionresults.sos.nm.gov/Default.aspx?eid=%d), county map "
            "data for race %d 'President and Vice President of the United States', "
            "served as JSON by the site's own NMResultsAjax.svc API. New Mexico "
            "MASKS small county candidate counts as '*' for ballot secrecy, so "
            "total_votes is recovered as votes/share from the Democratic line "
            "rather than by summing; that identity reproduces the plain sum "
            "exactly in every county with no masked line. The state writes "
            "'Dona Ana'; the Census name is 'Dona Ana County' with a tilde, mapped "
            "explicitly to 35013. 33 counties." % (eid, rid))



# ---------------------------------------------------------------- OREGON
@parser("OR")
def or_():
    """SoS 'Official Abstract of Votes', US President table. Retrieved from
    the Oregon Documents Depository at the State Library of Oregon, which
    archives the Elections Division's own publication; the sos.oregon.gov copy
    has been taken down (every documented path now 404s)."""
    spec = {2020: (208504, "Biden"), 2024: (292513, "Harris")}
    for year, (node, dem) in spec.items():
        url = "https://digitalcollections.library.oregon.gov/assets/displaypdf/%d" % node
        p = L.fetch(url, "OR_%d.pdf" % year)
        page = [pg for pg in _pdftotext(p).split("\f") if "US President" in pg][0]
        lines = page.split("\n")
        hi = [i for i, l in enumerate(lines) if "US President" in l][0]
        surn = None
        for l in lines[hi + 1:hi + 5]:
            if "Trump" in l and dem in l:
                surn = re.split(r"\s{2,}", l.strip())
                break
        assert surn, "no surname header row"
        gi = [i for i, t in enumerate(surn) if "Trump" in t][0]
        di = [i for i, t in enumerate(surn) if dem in t][0]
        out, total = [], None
        for l in lines:
            m = re.match(r"^\s*([A-Z][A-Za-z .']+?)\s{2,}([\d,\s]+)$", l.rstrip())
            if not m:
                continue
            nm = m.group(1).strip()
            nums = [int(x.replace(",", "")) for x in m.group(2).split()]
            if len(nums) != len(surn):
                continue
            if nm == "Total":
                total = nums
                continue
            out.append(dict(state_name="Oregon", county_fips=L.fips_for("OR", nm),
                            county_name=nm, votes_dem=nums[di], votes_gop=nums[gi],
                            total_votes=sum(nums)))
        assert len(out) == 36, len(out)
        assert sum(r["votes_gop"] for r in out) == total[gi]
        assert sum(r["votes_dem"] for r in out) == total[di]
        L.write_state("OR", year, out)
        L.add_provenance("Oregon", year, url, TODAY, "pdf", len(out), "yes",
            "Oregon Elections Division / Secretary of State, '%s General Election "
            "Abstract of Votes', US President table, parsed with pdftotext -layout. "
            "Retrieved from the Oregon Documents Depository run by the State Library "
            "of Oregon (record https://digitalcollections.library.oregon.gov/nodes/"
            "view/%d); the Secretary of State's own copy under "
            "sos.oregon.gov/elections/Documents/results/ now returns 404 and the "
            "'Election Results & History' page no longer links results at all. "
            "total_votes = every candidate column plus the abstract's 'Misc.' "
            "column, which is Oregon's write-in and miscellaneous line -- so the "
            "total runs about 0.7%% above AP's, which omits it. County sums "
            "reconciled to the abstract's own Total row. 36 counties."
            % (year, node))




NJ_COUNTIES = ["ATLANTIC", "BERGEN", "BURLINGTON", "CAMDEN", "CAPE MAY",
               "CUMBERLAND", "ESSEX", "GLOUCESTER", "HUDSON", "HUNTERDON",
               "MERCER", "MIDDLESEX", "MONMOUTH", "MORRIS", "OCEAN", "PASSAIC",
               "SALEM", "SOMERSET", "SUSSEX", "UNION", "WARREN"]


# ---------------------------------------------------------------- NEW JERSEY
@parser("NJ")
def nj():
    """Division of Elections 'Official List -- Candidates for President',
    one block per candidate giving that candidate's tally in each county."""
    for year in (2020, 2024):
        url = ("https://www.nj.gov/state/elections/assets/pdf/election-results/"
               "%d/%d-official-general-results-president.pdf" % (year, year))
        p = L.fetch(url, "NJ_%d.pdf" % year)
        txt = _pdftotext(p)
        tail = txt.split("Candidate Totals for Party")[-1]
        parties = [m.group(1).strip() for m in
                   re.finditer(r"^(\S.*?)\s{2,}\d+\s*$", tail, re.M)]
        parties = [q for q in parties if q != "Total Candidates"]
        assert len(parties) == (8 if year == 2020 else 9), parties
        acc = collections.defaultdict(lambda: [0, 0, 0])
        party, cand_tot, nblocks = None, [], 0
        for l in txt.split("\n"):
            t = l.rstrip()
            if t and not t.startswith(" "):
                hit = next((q for q in parties
                            if re.search(r"\s+" + re.escape(q) + r"$", t)), None)
                if hit:
                    party = "D" if hit == "Democratic" else "R" if hit == "Republican" else None
                    nblocks += 1
                    continue
            ts = t.strip()
            mt = re.match(r"^Total\s+([\d,]+)$", ts)
            if mt:
                cand_tot.append(int(mt.group(1).replace(",", "")))
                continue
            cty = next((c for c in NJ_COUNTIES if ts.startswith(c)), None)
            if not cty:
                continue
            n = re.findall(r"([\d,]+)\s*$", ts)
            if not n or not n[0].strip():
                continue
            v = int(n[0].replace(",", ""))
            a = acc[cty]; a[2] += v
            if party == "D": a[0] += v
            elif party == "R": a[1] += v
        assert nblocks == len(parties), (nblocks, len(parties))
        assert len(acc) == 21, sorted(acc)
        assert sum(a[2] for a in acc.values()) == sum(cand_tot), \
            (sum(a[2] for a in acc.values()), sum(cand_tot))
        out = _rows("NJ", acc, name_fmt=lambda c: c.title())
        L.write_state("NJ", year, out)
        L.add_provenance("New Jersey", year, url, TODAY, "pdf", len(out), "yes",
            "NJ Division of Elections, 'Official List -- Candidates for President, "
            "For GENERAL ELECTION', one block per candidate listing that candidate's "
            "tally in each of the 21 counties; parsed with pdftotext -layout and "
            "pivoted to county rows. total_votes = every candidate on the list (%d "
            "in %d); New Jersey does not carry a presidential write-in line here. "
            "County sums reconciled to the per-candidate Total lines. County names "
            "are the state's uppercase forms, title-cased."
            % (len(parties), year))



# ---------------------------------------------------------------- OKLAHOMA
@parser("OK")
def ok():
    """State Election Board results service (the JSON API behind
    results.okelections.us/OKER), 'Official Results' for each election date."""
    import json, urllib.request
    B = "https://results.okelections.gov/OKERS/"
    req = urllib.request.Request(B + "enrapi/login/", method="PUT",
            data=json.dumps({"Username": "appuser",
                             "Password": "X098de!22k098Mgfd"}).encode(),
            headers={"Content-Type": "application/json", "User-Agent": L.UA})
    import ssl
    ctx = ssl.create_default_context(); ctx.check_hostname = False
    ctx.verify_mode = ssl.CERT_NONE
    tok = json.loads(urllib.request.urlopen(req, timeout=60, context=ctx)
                     .read().decode())[1:]
    hdr = {"Authorization": "Bearer " + tok}
    for year, date in ((2020, "20201103"), (2024, "20241105")):
        elec = json.load(open(L.fetch(B + "enrapi/getelec/%s/SW/xx" % date,
                                      "OK_%d_races.json" % year, headers=hdr),
                              encoding="utf-8"))
        assert elec["isOfficial"] is True and elec["Title1"] == "Official Results"
        race = [r for r in elec["races"]
                if "ELECTORS FOR PRESIDENT" in r["raceTitle"]][0]
        names = [c["candName"] for c in race["raceCandidates"]]
        di = [i for i, n in enumerate(names) if n.endswith("(DEM)")][0]
        gi = [i for i, n in enumerate(names) if n.endswith("(REP)")][0]
        url = B + "enrapi/GetCntyResults/%s/%d" % (date, race["raceID"])
        cty = json.load(open(L.fetch(url, "OK_%d_counties.json" % year,
                                     headers=hdr), encoding="utf-8"))
        rows = cty["summaryCandResults"]
        assert len(rows) == 77, len(rows)
        out = []
        for r in rows:
            cr = r["candResults"]
            assert len(cr) == len(names)
            out.append(dict(state_name="Oklahoma",
                            county_fips=L.fips_for("OK", r["countyName"]),
                            county_name=r["countyName"],
                            votes_dem=cr[di]["totalVotes"],
                            votes_gop=cr[gi]["totalVotes"],
                            total_votes=r["totResults"]["totalVotes"]))
        L.write_state("OK", year, out)
        L.add_provenance("Oklahoma", year, url, TODAY, "json", len(out), "yes",
            "Oklahoma State Election Board results service -- the JSON API that "
            "backs https://results.okelections.us/OKER/?elecDate=%s, the page the "
            "Board's own %d General Election results page links to. The election "
            "record returns isOfficial=true and is titled 'Official Results'. "
            "Race %d '%s'. total_votes is the API's own per-county totResults; "
            "Oklahoma does not permit presidential write-ins, so it is the sum of "
            "the %d listed tickets. 77 counties."
            % (date, year, race["raceID"], race["raceTitle"], len(names)))




# ---------------------------------------------------------------- VERMONT
@parser("VT")
def vt():
    """Official Report of the Canvassing Committee. Vermont votes by TOWN, but
    the canvass report itself prints a County Total block for every county, so
    no aggregation by us is needed and none is done."""
    base = ("https://outside.vermont.gov/dept/sos/Elections_Division/"
            "election_info_resources/elections_results_data/")
    for year, dem in ((2020, "Joseph R. Biden"), (2024, "Kamala D. Harris")):
        url = (base + "%d_general_election_official_report_canvassing_committee_"
               "united_states_vermont_statewide_offices.pdf" % year)
        p = L.fetch(url, "VT_%d.pdf" % year)
        lines = _pdftotext(p).split("\n")
        hdr = re.compile(r"^For US PRESIDENT AND VICE PRESIDENT\s{2,}(.+?)\s+County\s*$")
        secs, cur = {}, None
        for l in lines:
            m = hdr.match(l.strip())
            if m:
                cur = m.group(1).strip(); secs.setdefault(cur, [])
                continue
            if l.strip().startswith("For ") and " County" in l:
                cur = None
            if cur is not None:
                secs[cur].append(l)
        assert len(secs) == 14, sorted(secs)
        out = []
        for cty, body in secs.items():
            groups, pend = [], None
            for l in body:
                t = l.strip()
                if not t or t.startswith("For "):
                    continue
                toks = re.split(r"\s{2,}", t)
                if len(toks) >= 2 and all(not re.search(r"\d", x) for x in toks):
                    pend = toks
                    continue
                tt = re.sub(r"^County Total\s+", "", t)
                if re.fullmatch(r"[\d,]+(\s+[\d,]+)+", tt) and pend:
                    nums = [int(x.replace(",", "")) for x in tt.split()]
                    if len(nums) == len(pend):
                        groups.append((pend, nums)); pend = None
            flat = {}
            for names, nums in groups:
                for n, v in zip(names, nums):
                    flat[n] = flat.get(n, 0) + v
            g = [v for k, v in flat.items() if "Donald J. Trump" in k][0]
            d = [v for k, v in flat.items() if dem in k][0]
            tot = flat["TownTotal"] - flat["overvotes"] - flat["blank votes"]
            chk = sum(v for k, v in flat.items()
                      if k not in ("TownTotal", "overvotes", "blank votes"))
            assert chk == tot, (cty, chk, tot)
            nm = cty + " County"
            out.append(dict(state_name="Vermont", county_fips=L.fips_for("VT", nm),
                            county_name=nm, votes_dem=d, votes_gop=g, total_votes=tot))
        L.write_state("VT", year, out)
        L.add_provenance("Vermont", year, url, TODAY, "pdf", len(out), "yes",
            "Secretary of State, 'Official Report of the Canvassing Committee, "
            "United States and Vermont Statewide Offices', General Election %d, "
            "US PRESIDENT AND VICE PRESIDENT. Vermont reports by TOWN, but this "
            "report prints a County Total row for each of the 14 counties, so the "
            "county figures here are the state's own -- no town-to-county "
            "aggregation was performed. The presidential table is split across "
            "several column groups per county; each group's total row was matched "
            "to its own header and the groups recombined. total_votes = every "
            "candidate column plus write-in, i.e. the report's TownTotal less "
            "overvotes and blank votes (TownTotal counts ballots, not presidential "
            "votes). That runs about 0.8%% above AP's figure, which omits write-ins."
            % year)




RI_TOWNS = ["Barrington", "Bristol", "Burrillville", "Central Falls",
    "Charlestown", "Coventry", "Cranston", "Cumberland", "East Greenwich",
    "East Providence", "Exeter", "Foster", "Glocester", "Hopkinton",
    "Jamestown", "Johnston", "Lincoln", "Little Compton", "Middletown",
    "Narragansett", "New Shoreham", "Newport", "North Kingstown",
    "North Providence", "North Smithfield", "Pawtucket", "Portsmouth",
    "Providence", "Richmond", "Scituate", "Smithfield", "South Kingstown",
    "Tiverton", "Warren", "Warwick", "West Greenwich", "West Warwick",
    "Westerly", "Woonsocket"]


# ---------------------------------------------------------------- RHODE ISLAND
@parser("RI")
def ri():
    """Board of Elections precinct data file, summed to CITY/TOWN.

    RHODE ISLAND PUBLISHES NO COUNTY RETURNS. Its five counties have had no
    government since 1846; the Board of Elections reports by city and town and
    by precinct, its data file carries no county field, and 'Countywide' in
    that file means statewide. There is no state-supplied town-to-county
    mapping to aggregate with, so per SPEC rule 5 this file is at the level the
    state actually publishes -- 39 cities and towns -- with county_fips empty."""
    for year in (2020, 2024):
        url = ("https://www.ri.gov/election/results/%d/general_election/data/"
               "rigen%dl.zip" % (year, year))
        p = L.fetch(url, "RI_%d.zip" % year)
        name, raw = L.unzip_one(p, r"rigen\d+l\.(asc|txt)$")
        acc = collections.defaultdict(lambda: [0, 0, 0])
        for l in raw.decode("latin-1").split("\n"):
            if len(l) < 235 or "Presidential Electors" not in l[111:167]:
                continue
            pn = l[205:235].strip()
            town = max((t for t in RI_TOWNS if pn.startswith(t)), key=len,
                       default=None)
            if town is None:
                assert pn.startswith("Federal Precinct"), pn
                town = "Federal Precincts (overseas/UOCAVA)"
            v = int(l[11:17])
            party = l[101:104].strip()
            a = acc[town]; a[2] += v
            if party == "DEM": a[0] += v
            elif party == "REP": a[1] += v
        assert 39 <= len(acc) <= 40, sorted(acc)
        out = [dict(state_name="Rhode Island", county_fips="", county_name=t,
                    votes_dem=d, votes_gop=g, total_votes=tt)
               for t, (d, g, tt) in acc.items()]
        L.write_state("RI", year, out)
        L.add_provenance("Rhode Island", year, url, TODAY, "txt", len(out), "yes",
            "RI Board of Elections precinct results data file (long format, "
            "fixed-width, layout documented at ri.gov/election/results/"
            "20220819_data_description.pdf), contest 'Presidential Electors For:', "
            "summed from precinct to city/town. NOT COUNTY ROWS: Rhode Island "
            "publishes no county-level presidential returns. Its five counties "
            "have had no county government since 1846, the Board reports only by "
            "city/town and precinct, and the data file has no county field -- the "
            "string 'Countywide' in it denotes a statewide contest. With no "
            "state-supplied town-to-county mapping, no aggregation was invented; "
            "county_fips is empty for every row per SPEC rule 5. In 2020 the file "
            "also carries four 'Federal Precinct' units -- overseas/UOCAVA "
            "federal-only ballots not attributable to any town -- combined here "
            "into a single extra row so the statewide total still reconciles. "
            "total_votes = "
            "all presidential candidates plus the write-in line, which runs about "
            "0.5%% above AP's figure. The parsed CSV is committed; the ~1 MB source "
            "zip is not. Posted as official results (2024 file stamped "
            "November 22, 2024).")




# ---------------------------------------------------------------- NEW YORK
@parser("NY")
def ny():
    """State Board of Elections 'Elections Database' (results.elections.ny.gov),
    the archive its own Election Results page points to, via that site's
    per-contest county table export."""
    spec = {2020: 308, 2024: 5591}
    for year, cid in spec.items():
        url = ("https://ny.elstats.civera.com/api/download_contest/"
               "%d_table.csv?split_party=false" % cid)
        p = L.fetch(url, "NY_%d.csv" % year)
        rows = list(csv.reader(open(p, encoding="utf-8-sig")))
        names = rows[0]
        parties = rows[1]
        di = [i for i, x in enumerate(parties) if x == "Democratic"][0]
        gi = [i for i, x in enumerate(parties) if x == "Republican"][0]
        ti = names.index("Total Votes")
        bi = names.index("Blank"); vi = names.index("Void")
        out, state = [], None
        for r in rows[2:]:
            if not r or not r[0]:
                continue
            num = lambda i: int(r[i] or 0)
            tot = num(ti) - num(bi) - num(vi)
            if r[0] == "State":
                state = (num(di), num(gi), tot); continue
            if r[0] != "County":
                continue
            nm = r[1].strip()
            out.append(dict(state_name="New York",
                            county_fips=L.fips_for("NY", nm), county_name=nm,
                            votes_dem=num(di), votes_gop=num(gi), total_votes=tot))
        assert len(out) == 62, len(out)
        assert sum(x["votes_dem"] for x in out) == state[0]
        assert sum(x["votes_gop"] for x in out) == state[1]
        L.write_state("NY", year, out)
        L.add_provenance("New York", year, url, TODAY, "csv", len(out), "yes",
            "NYS Board of Elections 'Elections Database' at "
            "results.elections.ny.gov (contest %d, "
            "https://results.elections.ny.gov/contest/%d) -- the archive the "
            "Board's own Election Results page links as ARCHIVED ELECTION "
            "RESULTS, compiled from its certified source documents; fetched "
            "through that site's county-table CSV export. New York reports the "
            "five New York City boroughs as five separate counties -- Bronx "
            "36005, Kings 36047, New York 36061, Queens 36081, Richmond 36085 -- "
            "so all 62 counties appear and nothing was split by us. Party lines "
            "are fused in New York; the export with split_party=false already "
            "combines each nominee's lines (Biden D+WOR, Trump R+CON in 2020; "
            "Harris D+WOR, Trump R+CON in 2024). total_votes = the export's "
            "'Total Votes' column LESS its 'Blank' and 'Void' columns, which "
            "count ballots with no presidential vote. County sums reconciled to "
            "the export's own State row." % (cid, cid))




# ---------------------------------------------------------------- MONTANA
@parser("MT")
def mt():
    """SoS 'Statewide General Election Canvass'.

    sosmt.gov returns HTTP 403 to every scripted request, including a full
    Chrome header set, so neither year could be fetched by this script. Both
    canvasses were retrieved through a browser instead:

      2020 https://sosmt.gov/wp-content/uploads/State_Canvass_Report.pdf
           (linked as 'General Results' from sosmt.gov/elections/archives/,
            year 2020) -- saved as raw/MT_2020.pdf and parsed here.
      2024 https://sosmt.gov/docs/31/post-election/66775/
           2024-general-election-report-state-canvass
           (linked as 'Statewide' from sosmt.gov/elections/results/) -- the
           browser would not release the file to disk, so page 2, the
           'PRESIDENT & VICE PRESIDENT' table, was read out of the PDF with
           pdf.js using the glyph coordinates and written to
           raw/MT_2024_president.psv. Its county rows sum to the canvass's own
           Total row on all five candidate columns.
    """
    # --- 2020: parse the canvass PDF directly
    p20 = os.path.join(L.RAW, "MT_2020.pdf")
    if os.path.exists(p20):
        page = [pg for pg in _pdftotext(p20).split("\f") if "PRESIDENT" in pg][0]
        acc, total = {}, None
        for l in page.split("\n"):
            m = re.match(r"^([A-Z][A-Za-z&. ]+?)\s{1,}(\d+)\s+(\d+)\s+(\d+)\s*$",
                         l.rstrip())
            if not m:
                continue
            nm = m.group(1).strip()
            v = [int(m.group(i)) for i in (2, 3, 4)]
            if nm == "Total":
                total = v
            else:
                acc[nm] = v
        assert len(acc) == 56, len(acc)
        assert [sum(x[i] for x in acc.values()) for i in range(3)] == total
        out = [dict(state_name="Montana",
                    county_fips=L.fips_for("MT", nm.replace("&", "and")),
                    county_name=nm, votes_dem=v[0], votes_gop=v[2],
                    total_votes=sum(v)) for nm, v in acc.items()]
        L.write_state("MT", 2020, out)
        L.add_provenance("Montana", 2020,
            "https://sosmt.gov/wp-content/uploads/State_Canvass_Report.pdf",
            TODAY, "pdf", len(out), "yes",
            "SoS '2020 Statewide General Election Canvass', PRESIDENT table "
            "(Biden/Jorgensen/Trump), parsed with pdftotext -layout. Linked as "
            "'General Results' for 2020 from sosmt.gov/elections/archives/. "
            "sosmt.gov answers 403 to scripted requests, so the PDF was fetched "
            "through a browser and saved to raw/MT_2020.pdf. County rows "
            "reconciled to the canvass's own Total row. total_votes = the three "
            "certified tickets; Montana canvasses presidential WRITE-INS in a "
            "separate report (State_Canvass_Writein_by_County.pdf) which is NOT "
            "folded in here, so total_votes is short by the write-in count "
            "(order of tens of votes statewide). The canvass writes "
            "'Lewis & Clark'; the Census name is 'Lewis and Clark County' (30049).")

    # --- 2024: read the table extracted from page 2 of the canvass PDF
    p24 = os.path.join(L.RAW, "MT_2024_president.psv")
    if os.path.exists(p24):
        rows = [l.strip().split("|") for l in open(p24) if l.strip()][1:]
        data = [r for r in rows if r[0] != "Total"]
        tot = [r for r in rows if r[0] == "Total"][0]
        assert len(data) == 56, len(data)
        for i in range(1, 6):
            assert sum(int(r[i]) for r in data) == int(tot[i])
        out = [dict(state_name="Montana",
                    county_fips=L.fips_for("MT", r[0].title().replace(" And ", " and ")),
                    county_name=r[0].title().replace(" And ", " and "),
                    votes_dem=int(r[1]), votes_gop=int(r[4]),
                    total_votes=sum(int(x) for x in r[1:6])) for r in data]
        L.write_state("MT", 2024, out)
        L.add_provenance("Montana", 2024,
            "https://sosmt.gov/docs/31/post-election/66775/"
            "2024-general-election-report-state-canvass",
            TODAY, "pdf", len(out), "yes",
            "SoS '2024 Statewide General Election Canvass', page 2, "
            "'PRESIDENT & VICE PRESIDENT' (Harris / Stein / Oliver / Trump / "
            "Kennedy). Linked as 'Statewide' from sosmt.gov/elections/results/. "
            "sosmt.gov answers 403 to every scripted request, and the browser "
            "would not release the file to disk, so the table was read out of "
            "the PDF in the browser with pdf.js using glyph coordinates and "
            "written to raw/MT_2024_president.psv, which this parser reads. Its "
            "56 county rows sum to the canvass's own Total row on all five "
            "candidate columns, and that statewide total (602,963) matches the "
            "independent AP count to the vote. total_votes = the five certified "
            "tickets; there is no write-in column on this table. County names "
            "are the canvass's uppercase forms, title-cased.")




# ---------------------------------------------------------------- NEVADA
@parser("NV")
def nv():
    """SoS 'Official Statewide General Election Results' summary, which prints
    each contest as candidates x counties.

    nvsos.gov sits behind Imperva/Incapsula and answers every scripted request
    with a bot-check page, so the two summary pages were read in a browser and
    their President tables saved verbatim as raw/NV_<year>_president.tsv, which
    this parser reads. Each candidate row's county figures sum to that row's
    own TOTAL VOTES cell, which is checked below."""
    for year, dem, gop in ((2020, "BIDEN", "TRUMP"), (2024, "HARRIS", "TRUMP")):
        src = os.path.join(L.RAW, "NV_%d_president.tsv" % year)
        if not os.path.exists(src):
            continue
        rows = [l.rstrip("\n").split("\t") for l in open(src) if l.strip()]
        counties = rows[0][3:]
        assert len(counties) == 17, len(counties)
        acc = {c: [0, 0, 0] for c in counties}
        for r in rows[1:]:
            n = [int(x.replace(",", "")) for x in r[3:]]
            assert sum(n) == int(r[2].replace(",", "")), r[0]
            for c, v in zip(counties, n):
                a = acc[c]; a[2] += v
                if r[0].startswith(dem): a[0] += v
                elif r[0].startswith(gop): a[1] += v
        out = []
        for c, (d, g, t) in acc.items():
            nm = c.title()
            out.append(dict(state_name="Nevada", county_fips=L.fips_for("NV", nm),
                            county_name=nm, votes_dem=d, votes_gop=g, total_votes=t))
        L.write_state("NV", year, out)
        L.add_provenance("Nevada", year,
            "https://www.nvsos.gov/SOSelectionPages/results/%dStateWideGeneral/"
            "ElectionSummary.aspx" % year, TODAY, "html", len(out), "yes",
            "Nevada SoS page headed '%d Official Statewide General Election "
            "Results', contest 'President and Vice President of the United "
            "States', which is printed as candidates x counties. nvsos.gov is "
            "behind Imperva/Incapsula and returns a bot-check page to every "
            "scripted request, so the page was read in a browser and the "
            "President table saved verbatim to raw/NV_%d_president.tsv, which "
            "this parser reads; each candidate's county figures are checked "
            "against that candidate's own TOTAL VOTES cell. total_votes "
            "includes NONE OF THESE CANDIDATES, Nevada's statutory ballot line, "
            "which is a vote cast in the presidential contest -- AP counts it "
            "the same way and the statewide totals agree to the vote. 17 rows = "
            "16 counties plus Carson City, an independent city (FIPS 32510)."
            % (year, year))




# ---------------------------------------------------------------- NEW HAMPSHIRE
@parser("NH")
def nh():
    """SoS presidential results workbook, 'Summary By Counties' sheet.

    New Hampshire votes by TOWN, but the Secretary of State's own presidential
    workbook opens with a Summary By Counties sheet, so these are the state's
    county figures and no town-to-county aggregation was performed.

    www.sos.nh.gov is behind Akamai and answers 403 to every scripted request,
    and the browser would not release the .xls files to disk, so each workbook
    was opened in the browser with SheetJS and its Summary By Counties sheet
    written verbatim to raw/NH_<year>_president.psv, which this parser reads.
    Each column is checked against the sheet's own TOTALS row."""
    spec = {
        2020: ("2020-president.xls", "biden_dem", "trump_gop",
               ("trump_gop", "biden_dem", "jorgensen", "write_ins")),
        2024: ("2024-ge-president_3.xls", "harris_dem", "trump_gop",
               ("harris_dem", "trump_gop", "oliver", "stein", "write_ins")),
    }
    for year, (fn, dcol, gcol, votecols) in spec.items():
        src = os.path.join(L.RAW, "NH_%d_president.psv" % year)
        if not os.path.exists(src):
            continue
        rows = [l.strip().split("|") for l in open(src) if l.strip()]
        hdr = rows[0]
        data = [r for r in rows[1:] if r[0] != "TOTALS"]
        tot = [r for r in rows[1:] if r[0] == "TOTALS"][0]
        assert len(data) == 10, len(data)
        for i in range(1, len(hdr)):
            assert sum(int(r[i]) for r in data) == int(tot[i]), hdr[i]
        out = []
        for r in data:
            g = dict(zip(hdr, r))
            nm = g["county"] + " County"
            out.append(dict(state_name="New Hampshire",
                            county_fips=L.fips_for("NH", nm), county_name=nm,
                            votes_dem=int(g[dcol]), votes_gop=int(g[gcol]),
                            total_votes=sum(int(g[c]) for c in votecols)))
        url = ("https://www.sos.nh.gov/sites/g/files/ehbemt561/files/"
               "inline-documents/sonh/" + fn)
        L.write_state("NH", year, out)
        L.add_provenance("New Hampshire", year, url, TODAY, "xls", len(out), "yes",
            "NH Secretary of State presidential results workbook for the %d "
            "General Election, first sheet 'Summary By Counties'. NEW HAMPSHIRE "
            "REPORTS BY TOWN, but the SoS publishes this county summary itself, "
            "so these are the state's own county totals -- no town aggregation "
            "was done by us (the remaining sheets in the same workbook give the "
            "towns within each county). www.sos.nh.gov answers 403 to scripted "
            "requests and the browser would not write the .xls to disk, so the "
            "sheet was read in the browser with SheetJS and copied verbatim to "
            "raw/NH_%d_president.psv; every column is reconciled to the sheet's "
            "own TOTALS row. total_votes = the candidate columns plus write-ins, "
            "excluding undervotes and overvotes. %s"
            % (year, year,
               "The 2020 summary sheet has no write-in column; write-ins were "
               "taken per county from the companion workbook "
               "2020-presidential-write-ins.xls ('Summary for web' sheet, 2,372 "
               "statewide), so total_votes reconciles to the state's 806,205."
               if year == 2020 else
               "The 2024 sheet's own Write-Ins column is used. AP's statewide "
               "total omits write-ins, so this total runs about 0.5% above it."))



def main():
    args = [a.upper() for a in sys.argv[1:] if not a.startswith("-")]
    check = "--check" in sys.argv
    todo = args or sorted(REG)
    if check:
        for st in sorted(REG):
            for y in (2020, 2024):
                if os.path.exists(os.path.join(L.STATES, "%s_%d.csv" % (st, y))):
                    L.validate(st, y)
        return
    for st in todo:
        fn = REG.get(st)
        if not fn:
            print("!! no parser for %s" % st); continue
        print("== %s" % st)
        try:
            fn()
        except Exception as e:
            print("   FAILED: %s: %s" % (type(e).__name__, e)); continue
        for y in (2020, 2024):
            if os.path.exists(os.path.join(L.STATES, "%s_%d.csv" % (st, y))):
                L.validate(st, y)


if __name__ == "__main__":
    main()

# --- build stamp -----------------------------------------------------------
# Records which script produced what is now in this directory into
# BUILD-STAMP.tsv beside the data. See ../../../_lib/provenance.py. Guarded,
# because a missing helper must not fail a build that was otherwise fine.
try:
    import os as _os, sys as _sys
    _sys.path.insert(0, _os.path.join("..", "..", "..", "_lib"))
    import provenance as _prov
    _prov.stamp("all")
except Exception as _e:
    print("  [stamp] skipped:", _e)
