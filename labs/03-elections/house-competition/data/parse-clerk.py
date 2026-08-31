#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# House general-election results from the Clerk of the House, 2004-2024.
#
#   derived/clerk_house.csv   One row per district per election: Democratic and
#                     Republican votes, the two-party Democratic share (dv),
#                     and an uncontested flag.
#
# WHY THIS SOURCE. It is the official one. Every other compilation of House
# returns -- MEDSL's, Jacobson's, Wikipedia's -- is downstream of these
# documents. The Clerk publishes "Statistics of the Presidential and
# Congressional Election" after every general election, compiled from the state
# canvasses.
#
# **KEYLESS AND SCRIPTABLE.** No guestbook, no account, no rate limit. Two URL
# patterns, because the Clerk moved the older ones to the House historian:
#     2016-2024   https://clerk.house.gov/member_info/electionInfo/{y}/statistics{y}.pdf
#     2004-2014   https://history.house.gov/Institution/Election-Statistics/{y}election/
#
# ---------------------------------------------------------------------------
# WHY NOT WIKIPEDIA
#
# Wikipedia has these results and they are mostly accurate. Three reasons not to
# use it here:
#
#   1. It is downstream of this document. Wikipedia's House election articles
#      cite the Clerk. Scraping the encyclopaedia to get at a number that the
#      primary source publishes directly adds a transcription step and removes
#      the ability to say where the number came from -- which is the one thing
#      this course is about.
#   2. Its structure is not stable. Article layout, table classes and column
#      headings vary by year and by state, so a scraper is a maintenance
#      commitment rather than a script.
#   3. It is editable. Not a slur on Wikipedia -- it is a live document, and a
#      dataset that changes when somebody edits a page is not reproducible.
#
# The one thing Wikipedia is genuinely good for here is *checking* a surprising
# row by hand, which is a fine use and does not require a scraper.
#
# ---------------------------------------------------------------------------
# HOW THE PARSE WORKS
#
# Under each state's "FOR UNITED STATES REPRESENTATIVE" heading the PDF prints
# one block per district:
#
#      1. Barry Moore, Republican ...........................   258,619
#         Tom Holmes, Democrat ..............................    70,929
#         Write-in ..........................................        306
#
# A line beginning "N." opens district N; indented lines belong to it. The party
# is spelled out on each line, which is what makes this tractable -- the
# alternative "Recapitulation" table puts parties in columns whose positions
# move from state to state.
#
# THE FOUR EDGE CASES THAT MATTER
#
#   FUSION (NY, CT, SC).  One candidate appears on several party lines. Votes
#     are summed per CANDIDATE NAME first; the candidate's major-party identity
#     comes from whichever major-party line they appear on. Failing to do this
#     splits a winner into pieces and invents uncontested races.
#
#   TOP-TWO (CA, WA).  Two candidates of the same party can face each other in
#     the general. That is a contested race with no opponent from the other
#     party -- NOT uncontested. Flagged separately and excluded from both the
#     uncontested count and the two-party share.
#
#   LOUISIANA.  All candidates run on one November ballot; a majority wins
#     outright, otherwise a December runoff decides it. The November totals are
#     what this file records, and the flag `la_primary` marks them.
#
#   AT-LARGE.  Printed as "AT LARGE" rather than "1." -- coded as district 1,
#     matching Jacobson.
#
# ---------------------------------------------------------------------------
# VALIDATION, AND EVERY DISAGREEMENT THAT SURVIVES IT
#
# 2004-2014 overlap Jacobson -- six elections, 2,610 districts. Run
# `validate-clerk.R`. Current state:
#
#     median |difference| in the two-party Democratic share:  0.03 points
#     districts within 0.5 points:                            98.1%-100%
#     districts differing by more than 2 points:               8 of 2,260
#     districts differing by more than 1 point:               14 of 2,260
#
# THREE PARSING BUGS WERE FOUND BY CHASING THOSE OUTLIERS, AND ALL THREE ARE
# FIXED. They are recorded here because each was invisible in the aggregate:
#
#   1. FOOTNOTE MARKERS READ AS VOTE TOTALS. The Clerk prints footnote
#      references as superscripts; pdftotext drops them to the end of the
#      candidate's line and pushes the real total onto a line of its own. Ralph
#      Abraham (LA-05, 2014) was recorded with 2 votes instead of 134,616.
#      Fixed by `repair_footnotes()`. Affected 24 rows across five years.
#
#   2. FUSION LINES LISTING SEVERAL PARTIES. New York prints a candidate's
#      cross-endorsements under them with no name, often as a list --
#      "Conservative, Libertarian". Matching a single party name missed those
#      and left thousands of votes unattributed, which put four New York
#      districts among the outliers. Fixed by `BARE_PARTY`; all four resolved.
#
#   3. UNOPPOSED CANDIDATES WITH NO PRINTED TOTAL. Florida does not put
#      unopposed candidates on the ballot, so the PDF shows "(1)" where a number
#      would go. Those districts were dropped entirely, which UNDERCOUNTED
#      uncontested races -- the headline measure, in the wrong direction.
#
# WHAT REMAINS, AFTER CHASING EVERY ONE OF THEM DOWN
#
#   LOUISIANA.  Not comparable, and flagged rather than fixed. Every candidate
#     runs on one November ballot and a December runoff follows if nobody clears
#     50%. The Clerk then substitutes the runoff totals for the two finalists
#     while leaving the eliminated candidates' November numbers in place, so a
#     district's figures mix two elections. An all-Democratic runoff (LA-02,
#     2006) makes a two-party share meaningless outright. Summing all candidates
#     of each party -- what the Clerk's own Recapitulation does, and what
#     Jacobson does -- reproduces him where no runoff occurred (LA-01 2014:
#     ours 19.57, his 19.6). Where one did, no rule reproduces him. Every
#     Louisiana row carries `la_primary` and `runoff_mixed`; treat `dv` there as
#     advisory. An earlier version took the leading candidate of each party
#     instead of summing, which was wrong on both counts.
#
#   TEXAS 22, 2006.  Tom DeLay resigned and Republicans ran a write-in
#     campaign. Write-in lines carry no party here, so the Republican vote is
#     genuinely unrecoverable. Not a parse failure.
#
#   TEXAS 23, 2006.  Court-ordered mid-decade redistricting forced a December
#     runoff. Same class as Louisiana.
#
#   CONNECTICUT, 2008 (5 districts, 1.4-2.5 points).  **Jacobson is the
#     inconsistent one, and we correct him.** He counts fusion ballot lines in
#     New York and in South Carolina and not in Connecticut: CT-01 without the
#     Working Families line is 71.68 and he reports 71.7. This parser counts
#     every line a candidate appears on, in every state -- which is also what
#     the Clerk's Recapitulation does.
#
#   NEBRASKA 3 (2004) and UTAH 1 (2006).  **Jacobson is simply wrong, and the
#     official document says so twice.** Both the candidate blocks and the
#     Recapitulation give NE-03 as R 218,751 / D 26,434 -- a two-party
#     Democratic share of 10.78, against his 8.6. UT-01 is D 57,922 /
#     R 112,546, a share of 33.98, against his 32.1. Two independent
#     presentations inside the primary source agree with each other and disagree
#     with him. We keep ours.
#
#   SOUTH CAROLINA 6 (2004) was on this list and is not any more: "Constitution"
#     was missing from the fusion party list, so 4,157 votes were not being
#     added to the Republican. Fixed -- it now matches Jacobson exactly.
#
#   Everything else agrees to within rounding: median |difference| across 2,260
#   overlapping districts is 0.03 points.
#
# Requires: pdftotext (poppler).  Run from this directory:  python3 parse-clerk.py
# ---------------------------------------------------------------------------

import csv, os, re, subprocess, sys, urllib.request

# Fetches go through provenance.py, which records url, bytes, hash and row
# count in PROVENANCE.tsv and prints a banner when a source moves under us --
# a URL that still returns 200 and no longer means what it meant.
sys.path.insert(0, os.path.join("..", "..", "..", "_lib"))
import provenance as prov

# raw/ holds the sources as they arrive; derived/ is what this script writes.
os.makedirs("derived", exist_ok=True)
os.makedirs("raw", exist_ok=True)

YEARS = [2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024]

def url_for(y):
    if y >= 2016:
        return f"https://clerk.house.gov/member_info/electionInfo/{y}/statistics{y}.pdf"
    return f"https://history.house.gov/Institution/Election-Statistics/{y}election/"

STATES = {s.upper(): s for s in [
    "Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut",
    "Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa",
    "Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan",
    "Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada",
    "New Hampshire","New Jersey","New Mexico","New York","North Carolina",
    "North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
    "South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont",
    "Virginia","Washington","West Virginia","Wisconsin","Wyoming"]}
# Jacobson numbers states alphabetically by full name, 1-50
CODE = {s: i + 1 for i, s in enumerate(sorted(STATES.values()))}

HEAD_REP = re.compile(r"FOR\s+UNITED\s+STATES\s+REPRESENTATIVE", re.I)
HEAD_OTH = re.compile(r"FOR\s+(PRESIDENTIAL\s+ELECTORS|UNITED\s+STATES\s+SENATOR|"
                      r"DELEGATE|RESIDENT\s+COMMISSIONER)|Recapitulation", re.I)
DIST     = re.compile(r"^\s*(\d{1,2})\.\s+(\S.*)$")
ATLARGE  = re.compile(r"^\s*AT[\s-]+LARGE[.:]?\s*(.*)$", re.I)
VOTES    = re.compile(r"([\d][\d,]*)\s*$")
LEADNUM  = re.compile(r"^\s*([\d][\d,]{2,})\s\s+(\S.*)$")   # number, then the NEXT name
ONLYNUM  = re.compile(r"^\s*([\d][\d,]{2,})\s*$")            # a number on its own line
FOOTNOTE = re.compile(r"\(\s*\d+\s*\)\s*$")      # "(1)" = unopposed, no total printed
STATE_LN = re.compile(r"^([A-Z][A-Z .]+?)(?:\s*\d+)?$")   # "NEW JERSEY 1" -> NEW JERSEY
# A fusion ballot line prints ONLY the party, indented under its candidate:
#     Kathleen A. Weppner, Republican .....  38,477
#         Conservative .......................  14,432
# Those votes belong to the candidate above, not to a new one.
# Every distinct bare-party label in the 2004-2024 corpus was listed and sorted
# by frequency before this was written. The list below is the cross-endorsing
# set; NOT_A_CANDIDATE holds the ballot-accounting rows that must never be
# added to anybody.
_P = (r"conservative|working families|independence|independent american|"
      r"independent|liberal|right to life|reform|green|libertarian|"
      r"women.s equality|taxpayers|constitution|serve america movement|"
      r"save jobs|tax revolt|freedom|patriot|democratic|republican|"
      r"d\.?f\.?l\.?|no party|unaffiliated")
NOT_A_CANDIDATE = re.compile(
    r"^\s*(all others|over votes|under votes|miscellaneous|others?|blank|void|"
    r"scattering|none of these candidates|total|write-?in)\b", re.I)
# A fusion candidate's extra ballot lines are printed under them with NO name --
# and often as a LIST: "Conservative, Libertarian", "Conservative, Taxpayers".
# Matching only a single party name misses those and leaves thousands of votes
# unattributed, which is what put four New York districts among the outliers.
BARE_PARTY = re.compile(rf"^\s*({_P})(\s*(?:,|/|&|and)\s*({_P}))*\s*$", re.I)

def party_of(text):
    t = text.upper()
    if "WRITE-IN" in t or "WRITE IN" in t or "SCATTER" in t:
        return None, None
    m = re.match(r"\s*(.+?),\s*([A-Za-z][A-Za-z \-/&.']*)\s*$", text.strip())
    if not m:
        return None, None
    name, party = m.group(1).strip(), m.group(2).strip().upper()
    if re.search(r"\bDEMOCRAT", party) and "REPUBLICAN" not in party:
        maj = "DEM"
    elif re.search(r"\bREPUBLICAN", party):
        maj = "REP"
    else:
        maj = "OTH"
    return name, maj

RUNOFF_DISTRICTS = set()


def repair_footnotes(txt):
    """Undo a footnote-marker/vote-total split.

    The Clerk prints footnote references as superscripts. `pdftotext` drops
    them to the end of the candidate's line and pushes the real vote total onto
    a line of its own:

        5. Ralph Lee Abraham, Republican ..........      2      <- footnote 2
                                                    134,616     <- the votes
           "Jamie" Mayo, Democrat ................        2
                                                     75,006

    Read line by line, Abraham gets 2 votes and 134,616 is an orphan. This pass
    puts them back together: an orphan number line replaces the small trailing
    number on the candidate line above it.

    The two conditions together make this safe -- the trailing value must be
    small enough to be a footnote reference (<= 99) AND the next non-blank line
    must contain nothing but a number.
    """
    lines = txt.split("\n")
    out, i, fixed = [], 0, 0
    while i < len(lines):
        cur = lines[i].rstrip()
        m = re.search(r"(\d[\d,]*)\s*$", cur)
        if m and len(m.group(1).replace(",", "")) <= 2 and re.search(r"[A-Za-z]", cur):
            j = i + 1
            while j < len(lines) and not lines[j].strip():
                j += 1
            if j < len(lines) and ONLYNUM.match(lines[j].rstrip()):
                RUNOFF_DISTRICTS.add(len(out))     # position marker, resolved below
                real = ONLYNUM.match(lines[j].rstrip()).group(1)
                out.append(cur[:m.start(1)] + real)
                for k in range(i + 1, j + 1):
                    out.append("")
                i = j + 1
                fixed += 1
                continue
        out.append(cur)
        i += 1
    if fixed:
        print(f"     repaired {fixed} footnote/vote-total splits")
    return "\n".join(out)


def assign(d, key, votes):
    mj, v = d.get(key, ("OTH", 0))
    d[key] = (mj, v + votes)


def parse(txt, year):
    rows, state, in_rep, dist, last_cand = [], None, False, None, None
    pending = None
    bucket = {}                                   # (state, dist) -> {cand: (maj, votes)}
    for raw in txt.split("\n"):
        line = raw.rstrip()
        s = line.strip().upper()
        sm = STATE_LN.match(s)                 # "NEW JERSEY 1" carries a footnote marker
        if sm and sm.group(1).strip() in STATES:
            s = sm.group(1).strip()
        if s in STATES:
            state, in_rep, dist, last_cand = STATES[s], False, None, None
            pending = None
            continue
        if state and HEAD_REP.search(line):
            in_rep, dist, last_cand = True, None, None
            pending = None
            continue
        if state and HEAD_OTH.search(line):
            in_rep = False
            continue
        if not (state and in_rep):
            continue
        al = ATLARGE.match(line)
        if al:
            # "AT LARGE" is usually alone on its own line, with the candidates
            # following unnumbered -- so open district 1 and keep reading.
            dist = 1
            bucket.setdefault((state, dist), {})
            rest = al.group(1).strip()
            if not rest:
                continue
            body = rest
        else:
            m = DIST.match(line)
            if m:
                dist = int(m.group(1))
                bucket.setdefault((state, dist), {})
                body = m.group(2)
            else:
                if dist is None:
                    continue
                body = line
        # "(1)" in place of a vote total: the candidate was unopposed and the
        # state printed no number. Keep the district; it has no two-party share.
        if FOOTNOTE.search(body):
            pending = None
            continue

        # THE VOTE TOTAL IS NOT ALWAYS ON THE CANDIDATE'S OWN LINE.
        # In several states the extracted text puts a candidate's number at the
        # start of the FOLLOWING line, or alone on a line of its own:
        #
        #     5. Ralph Lee Abraham, Republican ......
        #     134,616     "Zach" Dasher, Republican ......     <- 134,616 is ABRAHAM's
        #     53,628      Vance M. McAllister, Republican      <- 53,628 is DASHER's
        #        "Jamie" Mayo, Democrat ......
        #                                    75,006            <- 75,006 is MAYO's
        #
        # Reading each line on its own shifts every total onto the next
        # candidate. `pending` holds a name that is still waiting for a number.
        om = ONLYNUM.match(body)
        if om:
            if pending:
                assign(bucket[(state, dist)], pending, int(om.group(1).replace(",", "")))
                pending = None
            continue
        lm = LEADNUM.match(body)
        if lm and pending:
            assign(bucket[(state, dist)], pending, int(lm.group(1).replace(",", "")))
            pending = None
            body = lm.group(2)

        vm = VOTES.search(body)
        if not vm:
            # a name with no number yet -- remember it and wait
            lbl = re.sub(r"\.{3,}.*$", "", body).strip(" .")
            nm, mj = party_of(lbl)
            if nm:
                d0 = bucket[(state, dist)]
                if nm.upper() not in d0:
                    d0[nm.upper()] = (mj, 0)
                pending, last_cand = nm.upper(), nm.upper()
            continue
        votes = int(vm.group(1).replace(",", ""))
        label = body[:vm.start()]
        label = re.sub(r"\.{3,}.*$", "", label).strip(" .")
        d = bucket[(state, dist)]
        if NOT_A_CANDIDATE.match(label):
            pending = None
            continue
        if BARE_PARTY.match(label.strip()) and last_cand and last_cand in d:
            # fusion line -- add to the candidate it sits under
            mj, v = d[last_cand]
            d[last_cand] = (mj, v + votes)
            continue
        name, maj = party_of(label)
        if name is None:
            continue
        key = name.upper()
        last_cand, pending = key, None
        if key in d:
            prev_maj, prev_v = d[key]
            new_maj = prev_maj if prev_maj != "OTH" else maj
            d[key] = (new_maj if maj == "OTH" else maj if prev_maj == "OTH" else prev_maj,
                      prev_v + votes)
        else:
            d[key] = (maj, votes)

    for (st, dist), cands in sorted(bucket.items()):
        dv_list = [v for mj, v in cands.values() if mj == "DEM"]
        rv_list = [v for mj, v in cands.values() if mj == "REP"]
        # LOUISIANA IS SUMMED LIKE EVERY OTHER STATE, and that is a considered
        # choice rather than an oversight. Every candidate runs on one November
        # ballot there, so taking only the leading Democrat against the leading
        # Republican looks tempting -- an earlier version of this script did it.
        # It is wrong twice over: the Clerk's own Recapitulation table sums all
        # candidates of each party, and so does Jacobson. LA-01 in 2014 is the
        # test: summing gives 19.57 and Jacobson reports 19.6; the leading-
        # candidate rule gives 11.57.
        dem, rep = sum(dv_list), sum(rv_list)
        n_dem = sum(1 for mj, _ in cands.values() if mj == "DEM")
        n_rep = sum(1 for mj, _ in cands.values() if mj == "REP")
        top_two = (n_dem >= 2 and n_rep == 0) or (n_rep >= 2 and n_dem == 0)
        dv = 100 * dem / (dem + rep) if dem > 0 and rep > 0 else None
        # a district with no printed totals at all was unopposed -- keep it
        rows.append(dict(
            year=year, state=st, state_num=CODE[st], district=dist,
            stcd=CODE[st] * 100 + dist, dem_votes=dem, rep_votes=rep,
            n_dem=n_dem, n_rep=n_rep,
            dv=round(dv, 2) if dv is not None else "",
            uncontested=int(dv is None and not top_two),
            top_two=int(top_two),
            la_primary=int(st == "Louisiana"), runoff_mixed=0))
    return rows

def main():
    out = []
    for y in YEARS:
        pdf, txt = f"raw/clerk_{y}.pdf", f"derived/clerk_{y}.txt"
        if not os.path.exists(pdf):
            print(f"  downloading {y} ...", flush=True)
            prov.fetch(url_for(y), pdf, label=str(y))
        if not os.path.exists(txt):
            subprocess.run(["pdftotext", "-layout", pdf, txt], check=True)
        raw = open(txt, encoding="utf-8", errors="replace").read()
        had_runoff = bool(re.search(r"runoff\)? election", raw, re.I))
        rows = parse(repair_footnotes(raw), y)
        if had_runoff:
            # The Clerk substitutes December runoff totals for the two
            # finalists while leaving the eliminated candidates' November
            # numbers in place, so a district's figures mix two elections.
            # Louisiana is the only state that routinely does this.
            for r in rows:
                if r["state"] == "Louisiana":
                    r["runoff_mixed"] = 1
        seats = len(rows)
        unc = sum(r["uncontested"] for r in rows)
        tt = sum(r["top_two"] for r in rows)
        print(f"  {y}: {seats:3d} districts, {unc:3d} uncontested, {tt:2d} same-party general")
        if not (425 <= seats <= 445):
            print(f"     *** {y} parsed {seats} districts, expected ~435 -- check the parse")
        out += rows

    with open("derived/clerk_house.csv", "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=list(out[0].keys()))
        w.writeheader(); w.writerows(out)
    print(f"\nwritten: clerk_house.csv, {len(out)} rows, {YEARS[0]}-{YEARS[-1]}")

if __name__ == "__main__":
    main()
    # Anything fetched above is now in PROVENANCE.tsv; say so if a
    # source moved. Inside the __main__ guard so importing this
    # module for its parser does not print a banner.
    prov.report()
