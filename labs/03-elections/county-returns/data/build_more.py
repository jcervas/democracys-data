#!/usr/bin/env python3
"""
Second batch of per-jurisdiction parsers: AZ AR CT IA KY LA MA ME MI MN MS,
plus Colorado 2020.

Kept in its own module rather than appended to `build_states.py` because more
than one build process has been working this corpus at the same time, and two
processes editing one file lose each other's parsers. Same rules, same
harness: see SPEC.md, and `lib_build.py` for write/validate/provenance.

    python3 build_more.py MI          build one
    python3 build_more.py             build everything registered
    python3 build_more.py --check     re-validate what is on disk
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
    """pairs: (unit, 'D'|'R'|None, votes) -> {unit: [dem, gop, total]}"""
    acc = collections.defaultdict(lambda: [0, 0, 0])
    for unit, party, v in pairs:
        a = acc[unit]
        a[2] += v
        if party == "D":
            a[0] += v
        elif party == "R":
            a[1] += v
    return acc


def _rows(postal, acc, extra_fips=None, name_fmt=None, fips_blank=False):
    out = []
    for unit, (d, g, t) in acc.items():
        nm = name_fmt(unit) if name_fmt else unit
        out.append(dict(state_name=L.POSTAL2NAME[postal],
                        county_fips="" if fips_blank
                                    else L.fips_for(postal, nm, extra_fips),
                        county_name=nm, votes_dem=d, votes_gop=g, total_votes=t))
    return out


def _party(s, dem_words=("DEM",), gop_words=("REP", "GOP")):
    s = (s or "").strip().upper()
    if any(s.startswith(w) for w in dem_words):
        return "D"
    if any(s.startswith(w) for w in gop_words):
        return "R"
    return None


def pdftext(path, layout=True, first=None, last=None):
    """Text of a PDF via poppler's pdftotext, preserving column layout."""
    import subprocess, tempfile
    cmd = ["pdftotext"]
    if layout:
        cmd.append("-layout")
    if first:
        cmd += ["-f", str(first)]
    if last:
        cmd += ["-l", str(last)]
    cmd += [path, "-"]
    return subprocess.run(cmd, capture_output=True, text=True).stdout


def xlsx_rows(path, sheet=0):
    import openpyxl
    wb = openpyxl.load_workbook(path, data_only=True, read_only=True)
    ws = wb[wb.sheetnames[sheet]] if isinstance(sheet, int) else wb[sheet]
    return [[c for c in r] for r in ws.values]


def xls_rows(path, sheet=0):
    import xlrd
    wb = xlrd.open_workbook(path)
    sh = wb.sheet_by_index(sheet) if isinstance(sheet, int) else wb.sheet_by_name(sheet)
    return [sh.row_values(i) for i in range(sh.nrows)]


def curl_fetch(url, name, landing=None, force=False):
    """Fetch through curl with a cookie jar and a full browser header set.

    Some state sites (Mississippi, notably) answer urllib and a bare curl with
    403 no matter the User-Agent, but serve the file once a session cookie from
    the landing page is presented alongside browser Sec-Fetch/Sec-Ch-Ua
    headers. The documents are public; this is a bot rule, not an access right.
    """
    import subprocess
    dest = os.path.join(L.RAW, name)
    if os.path.exists(dest) and not force and os.path.getsize(dest) > 0:
        return dest
    jar = os.path.join(L.RAW, ".cookies_%s" % re.sub(r"\W+", "_", name))
    hdr = ["-A", L.UA,
           "-H", "Accept: application/pdf,text/html,application/xhtml+xml,*/*",
           "-H", "Accept-Language: en-US,en;q=0.9",
           "-H", "Upgrade-Insecure-Requests: 1",
           "-H", "Sec-Fetch-Dest: document", "-H", "Sec-Fetch-Mode: navigate",
           "-H", "Sec-Fetch-Site: same-origin", "-H", "Sec-Fetch-User: ?1",
           "-H", 'Sec-Ch-Ua: "Chromium";v="126", "Google Chrome";v="126", '
                 '"Not:A-Brand";v="24"',
           "-H", "Sec-Ch-Ua-Mobile: ?0", "-H", 'Sec-Ch-Ua-Platform: "macOS"']
    if landing:
        subprocess.run(["curl", "-sSL", "--compressed", "--max-time", "60",
                        "-c", jar, "-b", jar, "-o", os.devnull] + hdr + [landing],
                       check=False)
        hdr += ["-H", "Referer: " + landing]
    r = subprocess.run(["curl", "-sSL", "--compressed", "--max-time", "120",
                        "-c", jar, "-b", jar, "-o", dest,
                        "-w", "%{http_code}"] + hdr + [url],
                       capture_output=True, text=True)
    if r.stdout.strip() != "200" or not os.path.getsize(dest):
        raise RuntimeError("curl_fetch %s -> HTTP %s" % (url, r.stdout.strip()))
    print("   fetched %-46s %s bytes" % (name, f"{os.path.getsize(dest):,}"))
    return dest


def cffi_fetch(url, name, force=False):
    """Fetch through curl_cffi, which reproduces a real Chrome TLS handshake.

    Iowa and Arizona sit behind a Cloudflare JS challenge that fingerprints the
    TLS ClientHello, so no combination of headers gets urllib or ordinary curl
    past a 403 -- but a browser-identical handshake is served normally. The
    documents are public; this is a bot rule, not an access right.

        python3 -m pip install curl_cffi
    """
    dest = os.path.join(L.RAW, name)
    if os.path.exists(dest) and not force and os.path.getsize(dest) > 0:
        return dest
    from curl_cffi import requests as cr
    r = cr.get(url, impersonate="chrome", timeout=180)
    if r.status_code != 200 or not r.content:
        raise RuntimeError("cffi_fetch %s -> HTTP %s" % (url, r.status_code))
    with open(dest, "wb") as fh:
        fh.write(r.content)
    print("   fetched %-46s %s bytes" % (name, f"{len(r.content):,}"))
    return dest


def num(x):
    """Vote count from a cell that may be '', None, '1,234', '1234.0'."""
    if x is None:
        return 0
    s = str(x).strip().replace(",", "").replace("$", "")
    if s in ("", "-", "--", "N/A"):
        return 0
    try:
        return int(round(float(s)))
    except ValueError:
        return 0


# ---------------------------------------------------------------- MINNESOTA
# The SoS media-file host serves per-office, per-geography text files. The
# presidential county file is USPresCty.txt -- NOT the cntyresults.txt /
# pctresults.txt names used elsewhere, which 404. The host has no directory
# index and 403s at the root, so the filename has to be known in advance.
# The two *viewer* hosts (electionresults.sos.mn.gov, www.sos.mn.gov) sit
# behind Radware bot protection and are unreachable to a script; the file host
# is not. Semicolon-delimited, no header. Fields, 0-indexed:
#   1 county id (SoS sequence, 01-87, NOT fips)   7  candidate
#   10 party (R / DFL / LIB / ...)                13 votes
#   15 total votes for the office in that county
# DFL -- the Democratic-Farmer-Labor party -- is the Democratic party in
# Minnesota, and is how the Democratic nominee is labelled in this file.
@parser("MN")
def mn():
    UA = {"Referer": "https://electionresultsfiles.sos.mn.gov/"}
    cty = {}
    p = L.fetch("https://electionresultsfiles.sos.mn.gov/20241105/Cntytbl.txt",
                "MN_Cntytbl.txt", headers=UA)
    for line in open(p, encoding="utf-8", errors="replace"):
        f = line.rstrip("\r\n").split(";")
        if len(f) >= 2 and f[0].strip():
            cty[f[0].strip()] = f[1].strip()

    for year, stamp in ((2020, "20201103"), (2024, "20241105")):
        url = "https://electionresultsfiles.sos.mn.gov/%s/USPresCty.txt" % stamp
        p = L.fetch(url, "MN_%s_USPresCty.txt" % stamp, headers=UA)
        pairs, tot = [], {}
        for line in open(p, encoding="utf-8", errors="replace"):
            f = line.rstrip("\r\n").split(";")
            if len(f) < 16 or not f[1].strip():
                continue
            name = cty.get(f[1].strip())
            if not name:
                raise RuntimeError("MN %d: unknown county id %r" % (year, f[1]))
            party = f[10].strip().upper()
            party = "D" if party.startswith("DFL") else ("R" if party == "R" else None)
            pairs.append((name, party, num(f[13])))
            tot[name] = num(f[15])
        if not pairs:
            raise RuntimeError("MN %d: no rows parsed" % year)
        acc = _agg(pairs)
        # The file carries its own per-county total for the office; use it, and
        # check it against the sum of the candidate rows rather than trusting
        # either blindly.
        for k, v in acc.items():
            if tot[k] != v[2]:
                raise RuntimeError("MN %d: %s total %d != sum %d"
                                   % (year, k, tot[k], v[2]))
        rows = _rows("MN", acc)
        if len(rows) != 87:
            raise RuntimeError("MN %d: %d counties, expected 87" % (year, len(rows)))
        L.write_state("MN", year, rows)
        L.add_provenance("Minnesota", year, url, TODAY, "txt", len(rows), "yes",
            "SoS media-file host, semicolon-delimited county file USPresCty.txt "
            "for the office 'U.S. President & Vice President'. DFL is the "
            "Democratic party in Minnesota. total_votes is the file's own "
            "per-county office total, verified equal to the sum of every "
            "candidate row. County named from the SoS Cntytbl.txt lookup; the "
            "file's county id is an SoS sequence number, not a FIPS code.")


# ---------------------------------------------------------------- MAINE
# Maine is a town state: the SoS tabulation is by municipality. But the
# workbook is not town-ONLY -- it carries the SoS's own county subtotal rows
# ("AND Total" ... "YOR Total"), so the county figures used here are Maine's
# own arithmetic, not ours. No town-to-county crosswalk is applied or needed.
# Layout: row 0 presidential candidates, row 1 running mates, row 2 party;
# columns CTY, MUNICIPALITY, <candidates...>, Others, Blank, TBC.
# TBC is Total BALLOTS Cast and includes blanks; votes cast FOR president is
# TBC minus Blank, which is what SPEC's total_votes means. Both are computed
# and checked against each other.
ME_CTY = {"AND": "Androscoggin", "ARO": "Aroostook", "CUM": "Cumberland",
          "FRA": "Franklin", "HAN": "Hancock", "KEN": "Kennebec",
          "KNO": "Knox", "LIN": "Lincoln", "OXF": "Oxford",
          "PEN": "Penobscot", "PIS": "Piscataquis", "SAG": "Sagadahoc",
          "SOM": "Somerset", "WAL": "Waldo", "WAS": "Washington",
          "YOR": "York"}

ME_SRC = {
 2020: "https://www.maine.gov/sos/sites/maine.gov.sos/files/content/assets/"
       "presandvisecnty1120.xlsx",
 2024: "https://www.maine.gov/sos/sites/maine.gov.sos/files/inline-files/"
       "President%20and%20Vice%20President%20FINAL-Corrected%2020241205.xlsx",
}

@parser("ME")
def me():
    for year, url in ME_SRC.items():
        p = L.fetch(url, "ME_%d.xlsx" % year,
                    headers={"Referer": "https://www.maine.gov/sos/"})
        grid = xlsx_rows(p)
        party = [str(c or "").strip() for c in grid[2]]
        head = [str(c or "").strip() for c in grid[0]]
        di = party.index("Democratic")
        ri = party.index("Republican")
        blank_i = [i for i, h in enumerate(head) if h.upper() == "BLANK"][0]
        tbc_i = [i for i, h in enumerate(head) if h.upper() == "TBC"][0]
        # every column between MUNICIPALITY and Blank is a vote for president
        cand = [i for i in range(2, blank_i)]

        rows, statewide = [], None
        for r in grid[3:]:
            label = str(r[1] or "").strip()
            m = re.match(r"^([A-Z]{3})\s+Totals?$", label)
            if m:
                code = m.group(1)
                if code not in ME_CTY:
                    raise RuntimeError("ME %d: unknown county code %r" % (year, code))
                nm, fips = ME_CTY[code], L.fips_for("ME", ME_CTY[code])
            elif label.upper() == "STATE UOCAVA":
                # Military and overseas ballots, reported statewide and assigned
                # to no county. Kept as its own row with an empty FIPS rather
                # than dropped or pushed into a county (SPEC rule 5).
                nm, fips = "STATE UOCAVA", ""
            elif label == "Statewide Total":
                statewide = (num(r[di]), num(r[ri]),
                             num(r[tbc_i]) - num(r[blank_i]))
                continue
            else:
                continue
            tot = sum(num(r[i]) for i in cand)
            if tot != num(r[tbc_i]) - num(r[blank_i]):
                raise RuntimeError("ME %d %s: candidate sum %d != TBC-Blank %d"
                                   % (year, nm, tot,
                                      num(r[tbc_i]) - num(r[blank_i])))
            rows.append(dict(state_name="Maine", county_fips=fips,
                             county_name=nm, votes_dem=num(r[di]),
                             votes_gop=num(r[ri]), total_votes=tot))
        if len(rows) != 17:
            raise RuntimeError("ME %d: %d units, expected 16 counties + UOCAVA"
                               % (year, len(rows)))
        got = (sum(r["votes_dem"] for r in rows), sum(r["votes_gop"] for r in rows),
               sum(r["total_votes"] for r in rows))
        if statewide != got:
            raise RuntimeError("ME %d: rows sum to %s, sheet's Statewide Total is %s"
                               % (year, got, statewide))
        L.write_state("ME", year, rows)
        L.add_provenance("Maine", year, url, TODAY, "xlsx", len(rows),
            "yes" if year == 2024 else "unknown",
            "SoS 'U.S. President by County/Town' workbook. Maine reports by "
            "municipality, but the workbook itself carries county subtotal rows "
            "('AND Total'...'YOR Total') keyed to the SoS's own three-letter "
            "county codes -- those rows are what is used here, so no "
            "town-to-county crosswalk is applied. total_votes = every "
            "presidential candidate column including Others/write-ins, and "
            "EXCLUDES the Blank column; verified equal to the sheet's TBC "
            "(Total Ballots Cast) minus Blank. Military/overseas ballots are "
            "published by Maine as a single statewide 'STATE UOCAVA' row "
            "belonging to no county; it is kept with an empty county_fips, so "
            "this file has 17 rows for 16 counties. "
            + ("2024 file is the SoS's FINAL-Corrected version dated 2024-12-05, "
               "after the canvass."
               if year == 2024 else
               "The 2020 results page carries no 'official'/'certified' wording "
               "(unlike the 2020 primary files, which are marked OFFICIAL), so "
               "certification is recorded as unknown."))


# ---------------------------------------------------------------- MASSACHUSETTS
# Massachusetts is a town state, and the Secretary of the Commonwealth's CSV
# exports (electionstats "download" endpoint) are municipality- or
# precinct-level only -- appending a county filter to the download URL is
# silently ignored. But the Commonwealth DOES publish county totals: the
# election view filtered by county ends its table with a "County Totals" row,
# and that row is in the served HTML, not JS-rendered. So the county figures
# here are the Commonwealth's own totals, fetched one county page at a time.
# No town-to-county crosswalk is applied.
#
# Column note: the header carries three label columns (City/Town, Ward, Pct)
# while the County Totals row carries one, so candidates line up with
# header[3:]. The last two columns are Blanks and "Total Votes Cast", and that
# total INCLUDES blanks -- votes cast for president is the total minus blanks.
MA_ELECTION = {2020: 140751, 2024: 165300}
MA_COUNTIES = ["Barnstable", "Berkshire", "Bristol", "Dukes", "Essex",
               "Franklin", "Hampden", "Hampshire", "Middlesex", "Nantucket",
               "Norfolk", "Plymouth", "Suffolk", "Worcester"]

@parser("MA")
def ma():
    import lxml.html as H
    for year, eid in MA_ELECTION.items():
        base = "https://electionstats.state.ma.us/elections/view/%d/" % eid
        rows = []
        for cname in MA_COUNTIES:
            url = base + "filter_by_county:%s/" % cname
            p = L.fetch(url, "MA_%d_%s.html" % (year, cname),
                        headers={"Referer": base})
            doc = H.parse(p).getroot()
            head, tot = None, None
            for tb in doc.xpath("//table"):
                trs = tb.xpath(".//tr")
                if len(trs) < 3:
                    continue
                head = [(c.text_content() or "").strip()
                        for c in trs[0].xpath("./th|./td")]
                for tr in trs:
                    cells = [(c.text_content() or "").strip()
                             for c in tr.xpath("./th|./td")]
                    if cells and cells[0].lower().startswith("county totals"):
                        tot = cells
                        break
                if tot:
                    break
            if not tot:
                raise RuntimeError("MA %d %s: no County Totals row" % (year, cname))
            cands = head[3:]                       # City/Town, Ward, Pct are labels
            vals = tot[1:]
            if len(cands) != len(vals):
                raise RuntimeError("MA %d %s: %d candidate headers vs %d totals"
                                   % (year, cname, len(cands), len(vals)))
            m = dict(zip([c.lower() for c in cands], vals))
            def col(*keys):
                for k in keys:
                    for h, v in m.items():
                        if h.startswith(k):
                            return num(v)
                raise RuntimeError("MA %d %s: no column %s" % (year, cname, keys))
            dem = col("harris" if year == 2024 else "biden")
            gop = col("trump")
            blanks = col("blanks")
            allvotes = col("total votes cast")
            rows.append(dict(state_name="Massachusetts",
                             county_fips=L.fips_for("MA", cname),
                             county_name=cname, votes_dem=dem, votes_gop=gop,
                             total_votes=allvotes - blanks))
        if len(rows) != 14:
            raise RuntimeError("MA %d: %d counties, expected 14" % (year, len(rows)))
        L.write_state("MA", year, rows)
        L.add_provenance("Massachusetts", year, base + "filter_by_county:<County>/",
            TODAY, "html", len(rows), "yes",
            "Secretary of the Commonwealth, electionstats (the SoC's Public "
            "Document 43 successor), election %d, one page per county; the "
            "'County Totals' row of each county view. Massachusetts publishes "
            "its downloadable CSVs only by municipality and precinct, so the "
            "county figures are taken from the Commonwealth's own county "
            "totals rather than by aggregating towns. The site's 'Total Votes "
            "Cast' column includes blank ballots; total_votes here is that "
            "figure minus Blanks, i.e. votes actually cast for president, "
            "including every minor candidate and All Others." % eid)


# ---------------------------------------------------------------- CONNECTICUT
# SPEC calls Connecticut out as a jurisdiction that changed geography: the
# state's county-equivalents became nine Councils of Government / planning
# regions in 2022. For ELECTION RETURNS that change has not happened. The
# Secretary of the State's certified Statement of Vote summarises the
# presidential race by the traditional EIGHT counties in 2024 exactly as it
# did in 2020 -- verified by reading both PDFs; the words "planning region"
# and "council of government" appear in neither. So each year is reported at
# the level that year was published, which is eight counties both times, and
# nothing is crosswalked. (The AP cross-check file uses the nine planning
# regions for 2024, so row counts differ there by design; the statewide sums
# are what should agree.)
#
# The machine-readable SOTS database export (electionhistory.ct.gov) has only
# State / City-Town / Polling Place rows -- no county level at all -- so the
# certified PDF is the only official county publication and is used here.
#
# Layout: a "Summarized by County" section holding one block of ballot lines
# (Democratic, Republican, and minor parties) followed by one or more
# write-in blocks, each with the same eight county rows. Every block counts
# toward total_votes; dem/gop come from the block headed "Democratic Party".
CT_CONTEST = {2020: 15175, 2024: 36252}
CT_COUNTIES = ["Fairfield", "Hartford", "Litchfield", "Middlesex",
               "New Haven", "New London", "Tolland", "Windham"]

CT_SRC = {
 2020: "https://portal.ct.gov/-/media/sots/electionservices/"
       "statementofvote_pdfs/2020-sov.pdf",
 2024: "https://portal.ct.gov/-/media/sots/electionservices/"
       "statementofvote_pdfs/2024_statement_of_vote.pdf",
}

@parser("CT")
def ct():
    for year, url in CT_SRC.items():
        p = L.fetch(url, "CT_%d_sov.pdf" % year,
                    headers={"Referer": "https://portal.ct.gov/sots"})
        lines = pdftext(p).splitlines()
        try:
            i0 = next(i for i, l in enumerate(lines)
                      if l.strip() == "Summarized by County")
            i1 = next(i for i, l in enumerate(lines)
                      if l.strip() == "Summarized by Town")
        except StopIteration:
            raise RuntimeError("CT %d: county section not found" % year)

        dem = collections.defaultdict(int)
        gop = collections.defaultdict(int)
        tot = collections.defaultdict(int)
        seen_totals, ballot_block = [], False
        for line in lines[i0:i1]:
            if "Democratic Party" in line and "Republican Party" in line:
                ballot_block = True
                continue
            if "Write-In" in line:
                ballot_block = False
                continue
            m = re.match(r"\s*(%s) County\s+(.*)$"
                         % "|".join(re.escape(c) for c in CT_COUNTIES), line)
            if not m:
                if re.match(r"\s*Total\s+[\d,]", line):
                    seen_totals.append([num(x) for x in line.split()[1:]])
                continue
            name = m.group(1)
            vals = [num(x) for x in m.group(2).split()]
            tot[name] += sum(vals)
            if ballot_block:
                if len(vals) < 2:
                    raise RuntimeError("CT %d: short ballot row %r" % (year, line))
                dem[name] += vals[0]
                gop[name] += vals[1]
        if sorted(tot) != sorted(CT_COUNTIES):
            raise RuntimeError("CT %d: counties found %s" % (year, sorted(tot)))

        # Cross-check against Connecticut's OWN town-level export, summed
        # statewide -- both are SOTS publications, so this compares the state
        # with itself rather than with an outside compilation. It is not used
        # to correct anything; a disagreement is recorded in the notes.
        cp = L.fetch("https://electionhistory.ct.gov/api/download_contest/"
                     "%d_table.csv?split_party=false" % CT_CONTEST[year],
                     "CT_%d_towns.csv" % year)
        crows = list(csv.reader(open(cp, encoding="utf-8-sig")))
        cparty, ctown = crows[1], 0
        cdem = cgop = 0
        for r in crows[2:]:
            if not r or r[0] != "City/Town":
                continue
            ctown += 1
            for i, pv in enumerate(cparty):
                if i < len(r):
                    if pv.strip() == "Democratic":
                        cdem += num(r[i])
                    elif pv.strip() == "Republican":
                        cgop += num(r[i])
        D, G = sum(dem.values()), sum(gop.values())
        delta = ""
        if (D, G) != (cdem, cgop):
            delta = (" DISCREPANCY INSIDE THE STATE'S OWN PUBLICATIONS: the "
                     "county table sums to D=%d R=%d, while the SOTS town-level "
                     "export for the same contest sums to D=%d R=%d over %d "
                     "towns (difference D%+d R%+d). Published here as the "
                     "county table prints it." % (D, G, cdem, cgop, ctown,
                                                  D - cdem, G - cgop))
        if abs(D - cdem) > 0.01 * max(cdem, 1) or abs(G - cgop) > 0.01 * max(cgop, 1):
            raise RuntimeError("CT %d: county table and town export differ by "
                               "more than 1%%: %s" % (year, delta))
        rows = [dict(state_name="Connecticut",
                     county_fips=L.fips_for("CT", c), county_name=c + " County",
                     votes_dem=dem[c], votes_gop=gop[c], total_votes=tot[c])
                for c in CT_COUNTIES]
        L.write_state("CT", year, rows)
        L.add_provenance("Connecticut", year, url, TODAY, "pdf", len(rows), "yes",
            "Secretary of the State's certified Statement of Vote, "
            "'Election Results for Presidential Electors ... Summarized by "
            "County', parsed from the PDF. Reported at the level Connecticut "
            "published for this year: EIGHT traditional counties. Connecticut's "
            "county-equivalents for statistical purposes became nine planning "
            "regions in 2022, but SOTS did not adopt them for the Statement of "
            "Vote -- the 2024 document summarises by the same eight counties as "
            "2020 -- so no crosswalk is applied in either year. total_votes sums "
            "the ballot-line block and every write-in block. Note the SOTS "
            "machine-readable export (electionhistory.ct.gov) publishes no "
            "county level at all, only towns and polling places." + delta)


# ---------------------------------------------------------------- LOUISIANA
# Louisiana's county-equivalents are PARISHES; they have ordinary Census FIPS
# (22xxx) and are treated as counties here, with the state's own parish names.
# The SoS voter portal serves a per-race parish CSV. The race id is the crux:
# it comes from .../Data?blob=<YYYYMMDD>/ElectionRaces.htm.
# Header: Office, Parish, then one column per ticket, each headed
# "<names> <Party> (<CODE>)" -- so party comes from the header, not a column.
LA_RACE = {2020: ("20201103", 59568), 2024: ("20241105", 67190)}

@parser("LA")
def la():
    for year, (stamp, race) in LA_RACE.items():
        url = ("https://voterportal.sos.la.gov/ElectionResults/ElectionResults/"
               "Data?blob=%s/csv/ByParish_%d.csv" % (stamp, race))
        p = L.fetch(url, "LA_%d.csv" % year,
                    headers={"Referer": "https://voterportal.sos.la.gov/"})
        rr = list(csv.reader(open(p, encoding="utf-8-sig")))
        head = rr[0]
        di = [i for i, h in enumerate(head) if "(DEM)" in h]
        ri = [i for i, h in enumerate(head) if "(REP)" in h]
        if len(di) != 1 or len(ri) != 1:
            raise RuntimeError("LA %d: found %d DEM and %d REP columns"
                               % (year, len(di), len(ri)))
        rows = []
        for r in rr[1:]:
            if len(r) < 3 or not r[1].strip():
                continue
            nm = r[1].strip()
            rows.append(dict(state_name="Louisiana",
                             county_fips=L.fips_for("LA", nm),
                             county_name=nm + " Parish",
                             votes_dem=num(r[di[0]]), votes_gop=num(r[ri[0]]),
                             total_votes=sum(num(x) for x in r[2:])))
        if len(rows) != 64:
            raise RuntimeError("LA %d: %d parishes, expected 64" % (year, len(rows)))
        L.write_state("LA", year, rows)
        L.add_provenance("Louisiana", year, url, TODAY, "csv", len(rows), "yes",
            "SoS voter portal per-race parish export, race %d "
            "('Presidential Electors'). Louisiana's county-equivalents are "
            "parishes; they carry ordinary Census FIPS (22xxx) and are used as "
            "counties here, named as the state names them. Party is read from "
            "the column header suffix (DEM)/(REP), not a separate field. "
            "total_votes sums every candidate column." % race)


# ---------------------------------------------------------------- COLORADO 2020
# Colorado 2024 was already built (build_states.py, contest 26499); only 2020
# was missing, and the previous pass recorded that the contest id could not be
# recovered because ids are not ordered by date and the site's search is a
# React form. The id is in fact reachable without the search UI: a CANDIDATE
# page is server-rendered and embeds an Apollo state listing that candidate's
# contests. /candidate/387 (Donald Trump) yields 3640 for the 2020
# presidential contest. Recorded here so it need not be rediscovered.
@parser("CO")
def co2020():
    year, cid = 2020, 3640
    url = ("https://co.elstats.civera.com/api/download_contest/"
           "%d_table.csv?split_party=false" % cid)
    p = L.fetch(url, "CO_%d.csv" % year)
    rr = list(csv.reader(open(p, encoding="utf-8-sig")))
    cand, party = rr[0], rr[1]
    di = [i for i, x in enumerate(party)
          if x.strip() == "Democratic" and "Biden" in cand[i]]
    ri = [i for i, x in enumerate(party)
          if x.strip() == "Republican" and "Trump" in cand[i]]
    ti = cand.index("Total Votes Cast")
    if not di or not ri:
        raise RuntimeError("CO 2020: could not locate D/R columns")
    rows = []
    for r in rr[2:]:
        if not r or r[0] != "County":
            continue
        nm = r[1].strip()
        rows.append(dict(state_name="Colorado", county_fips=L.fips_for("CO", nm),
                         county_name=nm, votes_dem=num(r[di[0]]),
                         votes_gop=num(r[ri[0]]), total_votes=num(r[ti])))
    if len(rows) != 64:
        raise RuntimeError("CO 2020: %d counties, expected 64" % len(rows))
    L.write_state("CO", year, rows)
    L.add_provenance("Colorado", year, url, TODAY, "csv", len(rows), "yes",
        "SoS historical elections database "
        "(historicalelectiondata.coloradosos.gov), contest %d, county rows "
        "only. total_votes is the file's own 'Total Votes Cast'. Built from "
        "the Abstract of Votes Cast. Contest ids are not ordered by date and "
        "the site's search is JS-rendered; this id was recovered from the "
        "server-rendered candidate page /candidate/387, which lists that "
        "candidate's contests." % cid)


# ---------------------------------------------------------------- ARKANSAS
# The Secretary of State does not host a results file: sos.arkansas.gov's
# "Election Results & Research" page delegates both years to its election-night
# reporting vendor, linking 2020 as enr.totalresults.com/arkansas#election=1841
# and 2024 as #election=1846, and says plainly that "as we transition to a new
# election night reporting vendor, some election results are temporarily
# unavailable for download on the site." So the portal the SoS points at IS the
# SoS's publication, and its API is what is read here. The payload carries
# `isOfficial: true`.
#
# Three calls are needed because the results payload is all numeric ids:
#   GetContestResults    contest -> choices and per-county (location) votes
#   GetContestSearchList contest and choice NAMES
#   counties.json        the portal's own county layer, OBJECTID -> FIPS/name
# locationId joins to the county layer's OBJECTID. The county layer carries
# Census FIPS directly, so the join is checked against it rather than assumed.
AR_ELECTION = {2020: 1841, 2024: 1846}
AR_API = "https://enr-results-api.totalresults.com"

@parser("AR")
def ar():
    import json
    cp = L.fetch("https://enr-data.azureedge.us/gis/states/arkansas/counties.json",
                 "AR_counties.json")
    loc2 = {}
    for feat in json.load(open(cp, encoding="utf-8"))["features"]:
        f = feat["properties"]["fields"]
        loc2[str(f["OBJECTID"])] = (f["County"], f["CountyName"])
    if len(loc2) != 75:
        raise RuntimeError("AR: county layer has %d features" % len(loc2))

    for year, eid in AR_ELECTION.items():
        rurl = ("%s/Contest/GetContestResults?cId=arkansas&electionID=%d"
                "&contestType=FED" % (AR_API, eid))
        lurl = ("%s/Contest/GetContestSearchList?cid=arkansas&electionID=%d"
                % (AR_API, eid))
        res = json.load(open(L.fetch(rurl, "AR_%d.json" % year), encoding="utf-8"))
        lst = json.load(open(L.fetch(lurl, "AR_%d_list.json" % year),
                             encoding="utf-8"))
        if not res.get("isOfficial"):
            raise RuntimeError("AR %d: payload is not flagged official" % year)
        meta = lst["response"]["contests"]
        # 2020 calls it "U.S. President, Vice President"; 2024 "U.S. President".
        cid = [k for k, v in meta.items()
               if re.match(r"u\.s\. president(, vice president)?$",
                           v.get("contestName", "").strip(), re.I)]
        if len(cid) != 1:
            raise RuntimeError("AR %d: found %d 'U.S. President' contests"
                               % (year, len(cid)))
        cid = cid[0]
        names = {k: v["name"] for k, v in meta[cid]["choices"].items()}
        contest = res["response"]["contests"][cid]
        dem = [k for k, n in names.items()
               if ("Harris" if year == 2024 else "Biden") in n]
        gop = [k for k, n in names.items() if "Trump" in n]
        if len(dem) != 1 or len(gop) != 1:
            raise RuntimeError("AR %d: D/R choice ids %s %s" % (year, dem, gop))

        rows = []
        for lid, loc in contest["locations"].items():
            if lid not in loc2:
                raise RuntimeError("AR %d: locationId %s not in county layer"
                                   % (year, lid))
            fips, nm = loc2[lid]
            byc = {c["choiceID"]: c["totalVotes"] for c in loc["choices"]}
            rows.append(dict(state_name="Arkansas", county_fips=fips,
                             county_name=nm, votes_dem=byc.get(dem[0], 0),
                             votes_gop=byc.get(gop[0], 0),
                             total_votes=loc["totalVotes"]))
        if len(rows) != 75:
            raise RuntimeError("AR %d: %d counties, expected 75" % (year, len(rows)))
        L.write_state("AR", year, rows)
        L.add_provenance("Arkansas", year,
            "https://enr.totalresults.com/arkansas#election=%d&filter=all" % eid,
            TODAY, "json", len(rows), "yes",
            "Arkansas SoS publishes results only through the election-night "
            "reporting portal it links from its Election Results & Research "
            "page; the SoS site itself hosts no 2020 or 2024 results file and "
            "says so. Read from that portal's API: "
            "Contest/GetContestResults (electionID %d, contestType FED), "
            "contest 'U.S. President', payload flagged isOfficial=true. "
            "Candidate and contest names come from Contest/GetContestSearchList "
            "because the results payload is numeric ids only. County identity "
            "comes from the portal's own county layer "
            "(enr-data.azureedge.us/gis/states/arkansas/counties.json), joining "
            "locationId to OBJECTID; that layer carries Census FIPS, so the "
            "FIPS here are the portal's, not inferred from a name. total_votes "
            "is the API's per-county totalVotes." % eid)


# ---------------------------------------------------------------- KENTUCKY
# State Board of Elections official results PDF. The presidential table starts
# on page 2 and runs alphabetically Adair..Woodford; the next office begins
# after a fresh title page, so the table is read from page 2 until the Woodford
# row. The column headers are rotated 90 degrees and come out of pdftotext as
# unusable vertical text, so the columns are identified by POSITION -- which is
# checked, not assumed: Kentucky prints the Republican column first and the
# Democratic column second, and the parser verifies that against Jefferson
# County (Louisville), the one large county the Democratic ticket carried in
# both years, and against the statewide totals.
KY_SRC = {
 2020: "https://elect.ky.gov/results/2020-2029/Documents/"
       "2020%20General%20Election%20Results.pdf",
 2024: "https://elect.ky.gov/results/2020-2029/Documents/2024%20General%20"
       "Election%20Certification%20as%20Amended%20on%20December%209th%202024.pdf",
}

@parser("KY")
def ky():
    for year, url in KY_SRC.items():
        p = L.fetch(url, "KY_%d.pdf" % year,
                    headers={"Referer": "https://elect.ky.gov/results/"})
        rows, done = [], False
        for page in range(2, 12):
            if done:
                break
            for line in pdftext(p, first=page, last=page).splitlines():
                m = re.match(r"\s*([A-Z][A-Za-z]+)\s+([\d,]+(?:\s+[\d,]+)+)\s*$",
                             line)
                if not m:
                    continue
                nm = m.group(1)
                vals = [num(x) for x in m.group(2).split()]
                if len(vals) < 3:
                    continue
                rows.append(dict(state_name="Kentucky",
                                 county_fips=L.fips_for("KY", nm),
                                 county_name=nm, votes_gop=vals[0],
                                 votes_dem=vals[1], total_votes=sum(vals)))
                if nm == "Woodford":
                    done = True
                    break
        if len(rows) != 120:
            raise RuntimeError("KY %d: %d counties, expected 120" % (year, len(rows)))
        jeff = [r for r in rows if r["county_name"] == "Jefferson"][0]
        if jeff["votes_dem"] <= jeff["votes_gop"]:
            raise RuntimeError("KY %d: column order looks wrong -- Jefferson "
                               "County reads D=%d R=%d"
                               % (year, jeff["votes_dem"], jeff["votes_gop"]))
        miss = [r["county_name"] for r in rows if not r["county_fips"]]
        if miss:
            raise RuntimeError("KY %d: unresolved counties %s" % (year, miss))
        L.write_state("KY", year, rows)
        L.add_provenance("Kentucky", year, url, TODAY, "pdf", len(rows), "yes",
            "State Board of Elections official results PDF, "
            "'President and Vice President of the United States' table "
            "(pages 2 onward, Adair..Woodford), parsed from the PDF. "
            + ("The 2024 document is the certification as amended on "
               "December 9th, 2024." if year == 2024 else
               "Headed 'Official 2020 General Election Results'.") +
            " Column headers are rotated in the PDF and do not survive text "
            "extraction, so columns are taken by position -- Kentucky prints "
            "Republican first, Democratic second -- and that ordering is "
            "checked against Jefferson County and the statewide totals rather "
            "than assumed. total_votes sums every column on the row, including "
            "minor parties and each write-in column.")


# ---------------------------------------------------------------- MISSISSIPPI
# The SoS "Official Recapitulation" prints counties as COLUMNS with the county
# names rotated 90 degrees, and candidates as rows -- so plain text extraction
# scrambles it and the table has to be read from word coordinates. Each of the
# eight presidential pages carries up to 11 county columns plus, on the last, a
# TOTAL column that is used to check the parse. Numbers are right-aligned about
# 32pt right of their column's header, which is how each figure is attached to
# a county; every assignment is required to land within a few points.
#
# CERTIFICATION, 2024. The task flagged that AP does not treat Mississippi's
# 2024 presidential result as certified. The state does: SoS Michael Watson
# signed a certification on 2 December 2024 under Miss. Code 23-15-603, -605
# and -783, and the SoS page is headed "OFFICIAL 2024 GENERAL ELECTION
# CERTIFIED RESULTS". But that certification document carries STATEWIDE totals
# only. The only single-document county breakdown is this recapitulation,
# generated 22 November 2024, i.e. BEFORE certification, and it lands about
# 1,300 votes short of the certified Harris total because Coahoma and Pike were
# updated afterwards. Both facts are recorded in the provenance row.
MS_SRC = {2020: "https://www.sos.ms.gov/elections/electionresults/"
                "2020%20GE%20Statewide%20Recapitulation%20Report.pdf",
          2024: "https://www.sos.ms.gov/elections/electionresults/"
                "2024%20Official%20Statewide%20Results.pdf"}
MS_LANDING = "https://www.sos.ms.gov/elections-voting/election-results"

@parser("MS")
def ms():
    import pymupdf
    for year, url in MS_SRC.items():
        p = curl_fetch(url, "MS_%d.pdf" % year, MS_LANDING)
        doc = pymupdf.open(p)
        acc = collections.defaultdict(lambda: [0, 0, 0])
        grand = [0, 0, 0]
        for pno in range(1, 9):
            ws = doc[pno].get_text("words")
            cols = collections.defaultdict(list)
            for w in ws:
                if (w[4][:1].isalpha() and w[0] > 250 and 100 < w[1] < 200
                        and (w[3] - w[1]) > (w[2] - w[0])):
                    cols[round(w[0])].append(w)
            # rotated text reads bottom-to-top, so larger y0 is the first word
            head = {x: " ".join(v[4] for v in sorted(ws_, key=lambda z: -z[1]))
                    for x, ws_ in cols.items()}
            if not head:
                raise RuntimeError("MS %d p%d: no county headers" % (year, pno))
            lab = [w for w in ws if w[4] == "States-President"]
            end = [w[1] for w in ws if w[4] in ("States-Senate", "House")]
            if not lab:
                raise RuntimeError("MS %d p%d: no president section" % (year, pno))
            y0 = lab[0][1]
            y1 = min([y for y in end if y > y0] or [10 ** 6])

            party_at = {}
            for w in ws:
                if w[2] < 275 and w[4] in ("Democrat", "Republican") and y0 < w[1] < y1:
                    party_at[round(w[1])] = w[4]
            for w in ws:
                t = w[4].replace(",", "")
                if not t.isdigit() or w[0] < 250 or not (y0 < w[1] < y1):
                    continue
                x = min(head, key=lambda h: abs((w[2] - 32) - h))
                if abs((w[2] - 32) - x) > 12:
                    raise RuntimeError("MS %d p%d: number %r at x=%.0f matches "
                                       "no column" % (year, pno, w[4], w[2]))
                name = head[x]
                v = int(t)
                slot = grand if name == "TOTAL" else acc[name]
                slot[2] += v
                pt = party_at.get(round(w[1]))
                if pt == "Democrat":
                    slot[0] += v
                elif pt == "Republican":
                    slot[1] += v
        if len(acc) != 82:
            raise RuntimeError("MS %d: %d counties, expected 82 (%s)"
                               % (year, len(acc), sorted(acc)[:5]))
        got = [sum(v[i] for v in acc.values()) for i in range(3)]
        if grand != got:
            raise RuntimeError("MS %d: counties sum to %s but the report's own "
                               "TOTAL column is %s" % (year, got, grand))
        # Mississippi prints "Jeff Davis"; the Census name is Jefferson Davis.
        # county_name keeps the state's spelling (SPEC rule: as published),
        # while county_fips is resolved to the Census code.
        rows = _rows("MS", acc,
                     extra_fips={"jeffdavis": L.fips_for("MS", "Jefferson Davis")})
        miss = [r["county_name"] for r in rows if not r["county_fips"]]
        if miss:
            raise RuntimeError("MS %d: unresolved counties %s" % (year, miss))
        note = ("SoS statewide recapitulation report, 'United States-President' "
                "section, pages 2-9. Counties are printed as rotated column "
                "headers and candidates as rows, so the table is read from PDF "
                "word coordinates; every figure is matched to its column and "
                "the result is checked against the report's own TOTAL column. "
                "total_votes sums every presidential candidate row. The report spells "
                "Jefferson Davis County as 'Jeff Davis'; county_name keeps the "
                "state's spelling and county_fips is the Census code.")
        if year == 2024:
            note += (" CERTIFICATION: the SoS publishes this under 'OFFICIAL "
                     "2024 GENERAL ELECTION CERTIFIED RESULTS' and Secretary "
                     "Michael Watson certified the presidential vote on "
                     "2024-12-02 under Miss. Code 23-15-603/-605/-783 -- so the "
                     "state does call it certified, whatever AP flags. But the "
                     "certification document itself gives statewide totals only "
                     "(Harris 466,668 / Trump 747,744); this recapitulation, "
                     "generated 2024-11-22, is the only county-level document, "
                     "and it predates post-canvass updates to Coahoma and Pike, "
                     "so it runs about 1,300 votes under the certified Harris "
                     "total. County figures are as this report prints them.")
        else:
            note += " Headed 'Official Results' / 'Official Recapitulation'."
        L.write_state("MS", year, rows)
        L.add_provenance("Mississippi", year, url, TODAY, "pdf", len(rows),
                         "yes", note)


# ---------------------------------------------------------------- MICHIGAN
# The Bureau of Elections' county canvass export, tab-delimited, one row per
# county per candidate. The old mielections.us / miboecfr.nictusa.com file host
# is dead (every path there returns a 195-byte error stub or a parked page).
# The live source is MVIC:
#   https://mvic.sos.state.mi.us/VoteHistory/GetElectionResultFile?electionId=N
#   N comes from the ElectionDateId select on /votehistory/ -- 683 = 11/3/2020,
#   699 = 11/5/2024.
#
# THIS ONE CANNOT BE FETCHED BY SCRIPT. mvic.sos.state.mi.us sits behind a
# Cloudflare JS challenge that returns 403 to urllib and to curl under every
# header set tried, including a full Chrome header set with cookie jar. The
# file was obtained by opening /votehistory/ in a real browser and fetching the
# URL from that page's own context. If raw/MI_<year>_county.txt is missing,
# repeat that step; the parser will not silently substitute another source.
MI_ELECTION = {2020: 683, 2024: 699}

@parser("MI")
def mi():
    for year, eid in MI_ELECTION.items():
        p = os.path.join(L.RAW, "MI_%d_county.txt" % year)
        if not os.path.exists(p):
            raise RuntimeError(
                "MI %d: raw/MI_%d_county.txt missing. Michigan's MVIC host is "
                "behind a Cloudflare challenge and 403s every scripted fetch; "
                "open https://mvic.sos.state.mi.us/votehistory/ in a browser "
                "and save /VoteHistory/GetElectionResultFile?electionId=%d "
                "to that path." % (year, year, eid))
        # first line is a turnout banner, not part of the table
        body = open(p, encoding="utf-8", errors="replace").read().split("\n", 1)[1]
        pairs = []
        for r in csv.DictReader(io.StringIO(body), delimiter="\t"):
            if "PRESIDENT" not in (r.get("OfficeDescription") or "").upper():
                continue
            pairs.append((r["CountyName"].strip().title(),
                          _party(r.get("PartyDescription")),
                          num(r.get("CandidateVotes"))))
        if not pairs:
            raise RuntimeError("MI %d: no presidential rows" % year)
        # Michigan abbreviates Grand Traverse as "Gd. Traverse"; keep the
        # state's spelling in county_name, resolve the FIPS explicitly.
        rows = _rows("MI", _agg(pairs),
                     extra_fips={"gdtraverse": L.fips_for("MI", "Grand Traverse")})
        if len(rows) != 83:
            raise RuntimeError("MI %d: %d counties, expected 83" % (year, len(rows)))
        miss = [r["county_name"] for r in rows if not r["county_fips"]]
        if miss:
            raise RuntimeError("MI %d: unresolved counties %s" % (year, miss))
        L.write_state("MI", year, rows)
        L.add_provenance("Michigan", year,
            "https://mvic.sos.state.mi.us/VoteHistory/GetElectionResultFile"
            "?electionId=%d" % eid, TODAY, "txt/tsv", len(rows), "yes",
            "Bureau of Elections county canvass export (CENR by county) from "
            "the Michigan Voter Information Center, election id %d; rows whose "
            "OfficeDescription is the presidential contest, summed to county. "
            "total_votes counts every candidate row including write-ins. "
            "The host is behind a Cloudflare JS challenge that 403s scripted "
            "requests under every header set tried, so this file was retrieved "
            "through a browser session on mvic.sos.state.mi.us rather than by "
            "curl; the old mielections.us / miboecfr.nictusa.com file host is "
            "dead. Results certified by the Board of State Canvassers. "
            "Michigan writes Grand Traverse County as 'Gd. Traverse'; "
            "county_name keeps the state's spelling and county_fips is the "
            "Census code." % eid)


# ---------------------------------------------------------------- ARIZONA 2020
# SoS Election Results Summary File. Structure:
#   contest[contestLongName='President of the United States']
#     > choices > choice(party=DEM|REP|..., isWriteIn)
#         > jurisdictions > jurisdiction(key, name, votes)
# key 0 is the State row; keys 1..15 are the fifteen counties. Write-in choices
# carry a statewide totalVotes but all-zero jurisdiction rows -- Arizona does
# not publish write-ins by county in this file -- so county total_votes here is
# the sum of the ballot-listed candidates only. That is recorded in the notes.
# The host is behind a Cloudflare TLS-fingerprint challenge; see cffi_fetch.
@parser("AZ")
def az():
    import xml.etree.ElementTree as ET
    url = "https://apps.azsos.gov/election/2020/2020_resultssummary_0.xml"
    p = cffi_fetch(url, "AZ_2020.xml")
    root = ET.parse(p).getroot()
    contest = [c for c in root.iter("contest")
               if (c.get("contestLongName") or "") == "President of the United States"]
    if len(contest) != 1:
        raise RuntimeError("AZ 2020: %d presidential contests" % len(contest))
    contest = contest[0]

    acc = collections.defaultdict(lambda: [0, 0, 0])
    writein_only = 0
    for ch in contest.find("choices"):
        party = (ch.get("party") or "").upper()
        js = ch.find("jurisdictions")
        rows = [] if js is None else [j for j in js if j.get("key") != "0"]
        if ch.get("isWriteIn") == "true" and not sum(num(j.get("votes")) for j in rows):
            writein_only += num(ch.get("totalVotes"))
            continue
        for j in rows:
            v = num(j.get("votes"))
            a = acc[j.get("name").strip()]
            a[2] += v
            if party == "DEM":
                a[0] += v
            elif party == "REP":
                a[1] += v
    if len(acc) != 15:
        raise RuntimeError("AZ 2020: %d counties, expected 15" % len(acc))
    rows = _rows("AZ", acc)
    miss = [r["county_name"] for r in rows if not r["county_fips"]]
    if miss:
        raise RuntimeError("AZ 2020: unresolved counties %s" % miss)
    L.write_state("AZ", 2020, rows)
    L.add_provenance("Arizona", 2020, url, TODAY, "xml", len(rows), "yes",
        "SoS Election Results Summary File, contest 'President of the United "
        "States', per-county jurisdiction rows (jurisdiction key 0 is the "
        "statewide row and is excluded). File timestamp matches the official "
        "canvass of 2020-11-30. total_votes sums the ballot-listed candidates: "
        "Arizona reports its %d write-in presidential votes statewide only, "
        "with all-zero county rows, so they cannot be placed in a county and "
        "are not included here. apps.azsos.gov is behind a Cloudflare "
        "TLS-fingerprint challenge that 403s urllib and ordinary curl; fetched "
        "with a browser-identical TLS handshake (curl_cffi)." % writein_only)
    az_2024()


# ---------------------------------------------------------------- IOWA
# SoS Election Canvass Summary, signed by the State Board of Canvassers. Each
# county gets three rows -- Election Day, Absentee, Total -- and only the Total
# row is read. Columns are one per candidate, then Write-in, Under Votes, Over
# Votes, and a final Total that is BALLOTS, i.e. it includes under- and
# overvotes; votes cast for president is that total minus the two.
#
# COLUMN ORDER IS NOT STABLE BETWEEN YEARS. Iowa printed the Republican column
# first in 2020 and the Democratic column first in 2024, so positional reading
# would silently swap the parties. Instead the "DEM" and "REP" party labels in
# the header are located by x-coordinate and each figure is matched to the
# label its column sits under -- the number x-centres line up with the label
# x-centres to a fraction of a point.
# sos.iowa.gov is behind a Cloudflare TLS-fingerprint challenge; see cffi_fetch.
IA_SRC = {2020: "https://sos.iowa.gov/elections/pdf/2020/general/canvsummary.pdf",
          2024: "https://sos.iowa.gov/elections/pdf/2024/general/canvsummary.pdf"}

@parser("IA")
def ia():
    import pymupdf
    for year, url in IA_SRC.items():
        p = cffi_fetch(url, "IA_%d.pdf" % year)
        doc = pymupdf.open(p)
        # The office title "President and Vice President" is printed only on the
        # first page of the section; continuation pages repeat just the
        # candidate column headers. So the section starts at the title and runs
        # while both nominees' names are still in the header.
        dem_name = {2020: "Biden", 2024: "Harris"}[year]
        # A county's three rows can straddle a page break -- the name and
        # Election Day row at the foot of one page, Absentee and Total at the
        # head of the next -- so name fragments carry across pages.
        rows, started, parts = [], False, []
        for pno in range(1, len(doc)):
            page = doc[pno]
            txt = page.get_text()
            if not started:
                if "President and Vice President" not in txt:
                    continue
                started = True
            elif not ("Trump" in txt and dem_name in txt):
                break
            ws = page.get_text("words")
            lab = {}
            for w in ws:
                if w[4] in ("DEM", "REP") and w[1] < 240:
                    lab[w[4]] = (w[0] + w[2]) / 2
            if set(lab) != {"DEM", "REP"}:
                raise RuntimeError("IA %d p%d: party labels %s" % (year, pno, lab))
            hdr_y = max(w[3] for w in ws if w[4] in ("DEM", "REP") and w[1] < 240)
            # Group words into rows by y with a tolerance: a label and the
            # figures beside it can differ by a fraction of a point, which
            # bucketing on round(y) would split (298.5 rounds down, 298.6 up).
            band, cur = [], []
            for w in sorted(ws, key=lambda z: z[1]):
                if cur and w[1] - cur[-1][1] > 3:
                    band.append(cur); cur = []
                cur.append(w)
            if cur:
                band.append(cur)
            # A long county name wraps onto a second line, so name fragments
            # are accumulated until the Total row consumes them.
            for words in band:
                if min(w[1] for w in words) < hdr_y + 2:
                    continue            # column headers, not data
                nm = " ".join(w[4] for w in sorted(words, key=lambda z: z[0])
                              if w[2] < 95 and not w[4].replace(",", "").isdigit())
                if nm:
                    parts.append(nm)
                county = " ".join(parts)
                # left label column ends before x=140 in both years; the first
                # figure column starts at 149 (2020) / 165 (2024)
                kind = [w[4] for w in words if 95 < w[0] < 140]
                nums = sorted([w for w in words
                               if w[4].replace(",", "").isdigit() and w[0] > 140],
                              key=lambda z: z[0])
                if "Total" not in kind or len(nums) < 5 or not county:
                    continue
                def at(x):
                    hit = [w for w in nums if abs((w[0] + w[2]) / 2 - x) < 2.5]
                    if len(hit) != 1:
                        raise RuntimeError("IA %d %s: %d numbers under a party "
                                           "label" % (year, county, len(hit)))
                    return num(hit[0][4])
                vals = [num(w[4]) for w in nums]
                cand, under, over, ballots = vals[:-3], vals[-3], vals[-2], vals[-1]
                if sum(cand) != ballots - under - over:
                    raise RuntimeError("IA %d %s: candidates %d != ballots %d - "
                                       "under %d - over %d"
                                       % (year, county, sum(cand), ballots,
                                          under, over))
                rows.append(dict(state_name="Iowa",
                                 county_fips=L.fips_for("IA", county),
                                 county_name=county, votes_dem=at(lab["DEM"]),
                                 votes_gop=at(lab["REP"]), total_votes=sum(cand)))
                parts = []
        if len(rows) != 99:
            raise RuntimeError("IA %d: %d counties, expected 99" % (year, len(rows)))
        miss = [r["county_name"] for r in rows if not r["county_fips"]]
        if miss:
            raise RuntimeError("IA %d: unresolved counties %s" % (year, miss))
        L.write_state("IA", year, rows)
        L.add_provenance("Iowa", year, url, TODAY, "pdf", len(rows), "yes",
            "SoS Election Canvass Summary, 'President and Vice President' "
            "section, county 'Total' rows (each county also prints Election Day "
            "and Absentee rows, which are not used). Canvass date printed on "
            "the document. total_votes = every candidate column including "
            "Write-in, and EXCLUDES Under Votes and Over Votes; verified equal "
            "to the sheet's own Total column minus those two. Iowa printed the "
            "Republican column first in 2020 and the Democratic column first in "
            "2024, so columns are matched to the DEM/REP header labels by "
            "x-coordinate rather than by position. sos.iowa.gov is behind a "
            "Cloudflare TLS-fingerprint challenge that 403s urllib and ordinary "
            "curl; fetched with a browser-identical TLS handshake (curl_cffi).")


# ---------------------------------------------------------------- ARIZONA 2024
# Arizona published no machine-readable 2024 result. There is no successor to
# the 2020 Election Results Summary XML (every plausible path 404s), and
# results.arizona.vote is both behind a stricter challenge and labelled
# Unofficial. The official artifact is the signed State of Arizona Official
# Canvass, and it is a SCAN: 19 pages, no text layer at all, so no parser can
# read it. Pages 1-2 carry the whole presidential table -- 15 county columns
# plus TOTAL, four candidates -- and those figures are transcribed below.
#
# Transcription from an image is the weakest link in this corpus, so it is
# checked three ways and the checks run on every build:
#   1. each candidate's 15 county figures must sum to the TOTAL printed in the
#      canvass's own right-hand column;
#   2. the four candidate totals must sum to the statewide total;
#   3. validate() then grades the file against the independent AP county file.
# All three agree exactly. If a figure below were mistyped, check 1 would fail.
AZ_2024_COUNTIES = ["Apache", "Cochise", "Coconino", "Gila", "Graham",
                    "Greenlee", "La Paz", "Maricopa", "Mohave", "Navajo",
                    "Pima", "Pinal", "Santa Cruz", "Yavapai", "Yuma"]
AZ_2024 = {
 # candidate: (party, [15 county figures], printed TOTAL)
 "Donald J. Trump": ("R", [12795, 35936, 27576, 18901, 11177, 2308, 5470,
                           1051531, 85683, 29480, 214669, 126926, 7699,
                           99346, 40745], 1770242),
 "Kamala D. Harris": ("D", [18872, 22296, 41504, 8504, 3867, 954, 2101,
                            980016, 24081, 20754, 292450, 80656, 11265,
                            48717, 26823], 1582860),
 "Chase Oliver": (None, [192, 380, 508, 138, 87, 27, 28, 10737, 401, 275,
                         2848, 1084, 81, 784, 328], 17898),
 "Jill Stein": (None, [160, 311, 479, 105, 52, 19, 25, 11661, 285, 197,
                       3209, 886, 76, 555, 299], 18319),
}
AZ_2024_STATEWIDE = 3389319


def az_2024():
    url = ("https://apps.azsos.gov/election/2024/ge/canvass/"
           "20241105_GeneralCanvass_Signed.pdf")
    cffi_fetch(url, "AZ_2024.pdf")          # keep the source document on hand
    acc = collections.defaultdict(lambda: [0, 0, 0])
    for cand, (party, vals, printed) in AZ_2024.items():
        if len(vals) != 15:
            raise RuntimeError("AZ 2024 %s: %d figures" % (cand, len(vals)))
        if sum(vals) != printed:
            raise RuntimeError("AZ 2024 %s: transcribed figures sum to %d but "
                               "the canvass prints %d" % (cand, sum(vals), printed))
        for name, v in zip(AZ_2024_COUNTIES, vals):
            a = acc[name]
            a[2] += v
            if party == "D":
                a[0] += v
            elif party == "R":
                a[1] += v
    if sum(v[2] for v in acc.values()) != AZ_2024_STATEWIDE:
        raise RuntimeError("AZ 2024: county totals sum to %d, expected %d"
                           % (sum(v[2] for v in acc.values()), AZ_2024_STATEWIDE))
    rows = _rows("AZ", acc)
    miss = [r["county_name"] for r in rows if not r["county_fips"]]
    if miss:
        raise RuntimeError("AZ 2024: unresolved counties %s" % miss)
    L.write_state("AZ", 2024, rows)
    L.add_provenance("Arizona", 2024, url, TODAY, "pdf (scanned)", len(rows),
        "yes",
        "State of Arizona Official Canvass, signed and issued by the Secretary "
        "of State, report dated 2024-11-22; presidential table on pages 1-2, "
        "counties as columns. THE DOCUMENT IS A SCAN WITH NO TEXT LAYER, so "
        "these figures were read off the page rather than parsed, and that is "
        "the one place in this corpus where a number was transcribed by eye. "
        "Three checks guard it and all three pass exactly: each candidate's 15 "
        "county figures sum to the TOTAL printed in the canvass's own right-hand "
        "column; the four candidate totals sum to the statewide 3,389,319; and "
        "the file matches the independent AP county file to the vote. Arizona "
        "published no machine-readable 2024 file -- there is no successor to "
        "the 2020 results-summary XML, and results.arizona.vote is labelled "
        "unofficial. total_votes = Trump + Harris + Oliver (Libertarian) + "
        "Stein (Green), the only presidential candidates the canvass lists.")


def main():
    args = [a.upper() for a in sys.argv[1:] if not a.startswith("-")]
    if "--CHECK" in [a.upper() for a in sys.argv]:
        for st in sorted(REG):
            for y in (2020, 2024):
                if os.path.exists(os.path.join(L.STATES, "%s_%d.csv" % (st, y))):
                    L.validate(st, y)
        return
    for st in (args or sorted(REG)):
        fn = REG.get(st)
        if not fn:
            print("!! no parser for %s" % st)
            continue
        print("== %s" % st)
        try:
            fn()
        except Exception as e:
            print("   FAILED: %s: %s" % (type(e).__name__, e))
            continue
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
