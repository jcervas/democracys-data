#!/usr/bin/env python3
# raw/ holds the sources as they arrive; derived/ is what this script writes.
os.makedirs("derived", exist_ok=True)

"""
Shared machinery for the county-returns build. See SPEC.md for the rules this
enforces; this file is only the plumbing.

Every state parser ends with the same three calls:

    rows = [...]                       # dicts with the SPEC.md columns
    write_state("NC", 2024, rows)      # writes states/NC_2024.csv
    validate("NC", 2024)               # checks it, loudly
    add_provenance(...)                # one row in provenance.csv

WHY VALIDATION IS NOT OPTIONAL. Fifty jurisdictions publish in fifty formats,
and a parser that silently drops a column, misreads a header row as data, or
picks up the wrong candidate will still produce a tidy file of plausible
numbers. `derived/crosscheck_ap_counties_2024.csv` holds an independent county-level
count for 2024, so every 2024 file gets graded against a source that was
assembled by somebody else from different inputs. Disagreement is not
automatically an error -- AP and a state canvass legitimately differ on
write-ins and late-counted ballots -- but it must be SEEN.
"""

import csv, io, os, re, ssl, sys, zipfile, urllib.request
from collections import defaultdict

# Every state's download in this chapter goes through fetch() below, so that is
# where the drift record belongs. This chapter also keeps provenance.csv, which
# is a different and richer thing: hand-written notes on what each state
# certified and how it was parsed. PROVENANCE.tsv answers only the mechanical
# question that no human can keep up with by hand -- has the file at this URL
# changed since we last looked.
sys.path.insert(0, os.path.join("..", "..", "..", "_lib"))
import provenance as prov

HERE   = os.path.dirname(os.path.abspath(__file__))
RAW    = os.path.join(HERE, "raw")
STATES = os.path.join(HERE, "derived/states")
PROV   = os.path.join(HERE, "provenance.csv")
FIELDS = ["state_name", "county_fips", "county_name",
          "votes_dem", "votes_gop", "total_votes"]
UA = ("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 "
      "(KHTML, like Gecko) Version/17.0 Safari/605.1.15")

os.makedirs(RAW, exist_ok=True)
os.makedirs(STATES, exist_ok=True)

POSTAL2NAME = {
 "AL":"Alabama","AK":"Alaska","AZ":"Arizona","AR":"Arkansas","CA":"California",
 "CO":"Colorado","CT":"Connecticut","DE":"Delaware","DC":"District of Columbia",
 "FL":"Florida","GA":"Georgia","HI":"Hawaii","ID":"Idaho","IL":"Illinois",
 "IN":"Indiana","IA":"Iowa","KS":"Kansas","KY":"Kentucky","LA":"Louisiana",
 "ME":"Maine","MD":"Maryland","MA":"Massachusetts","MI":"Michigan",
 "MN":"Minnesota","MS":"Mississippi","MO":"Missouri","MT":"Montana",
 "NE":"Nebraska","NV":"Nevada","NH":"New Hampshire","NJ":"New Jersey",
 "NM":"New Mexico","NY":"New York","NC":"North Carolina","ND":"North Dakota",
 "OH":"Ohio","OK":"Oklahoma","OR":"Oregon","PA":"Pennsylvania",
 "RI":"Rhode Island","SC":"South Carolina","SD":"South Dakota",
 "TN":"Tennessee","TX":"Texas","UT":"Utah","VT":"Vermont","VA":"Virginia",
 "WA":"Washington","WV":"West Virginia","WI":"Wisconsin","WY":"Wyoming"}


# --- fetching ---------------------------------------------------------------

def fetch(url, name, binary=True, force=False, headers=None):
    """Download to raw/<name>, cached. Returns the local path."""
    dest = os.path.join(RAW, name)
    if os.path.exists(dest) and not force and os.path.getsize(dest) > 0:
        return dest
    h = {"User-Agent": UA, "Accept": "*/*"}
    if headers:
        h.update(headers)
    req = urllib.request.Request(url, headers=h)
    ctx = ssl.create_default_context()
    ctx.check_hostname = False
    ctx.verify_mode = ssl.CERT_NONE
    with urllib.request.urlopen(req, timeout=180, context=ctx) as r:
        data = r.read()
    with open(dest, "wb") as fh:
        fh.write(data)
    # Recorded here rather than by routing through prov.fetch(), which would
    # lose the user agent and the relaxed SSL context above -- several state
    # election sites need both, and one of them serves a certificate that does
    # not validate.
    prov.record(url, dest, label=name)
    print("   fetched %-46s %s bytes" % (name, f"{len(data):,}"))
    return dest


def unzip_one(path, pattern):
    """Return bytes of the first member matching a regex."""
    with zipfile.ZipFile(path) as z:
        for n in z.namelist():
            if re.search(pattern, n, re.I):
                return n, z.read(n)
    raise KeyError("no member matching %r in %s (has: %s)"
                   % (pattern, os.path.basename(path),
                      ", ".join(zipfile.ZipFile(path).namelist()[:8])))


# --- county FIPS ------------------------------------------------------------

_FIPS = None

def _norm(s):
    s = (s or "").lower().strip()
    s = re.sub(r"\b(county|parish|borough|census area|city and borough|"
               r"municipality|city)\b", " ", s)
    s = re.sub(r"[^a-z0-9]+", "", s)
    return s


def fips_table():
    """{postal: {normalised county name: fips}} from the Census national file."""
    global _FIPS
    if _FIPS:
        return _FIPS
    p = fetch("https://www2.census.gov/geo/docs/reference/codes2020/"
              "national_county2020.txt", "national_county2020.txt")
    tbl = defaultdict(dict)
    raw = open(p, encoding="utf-8", errors="replace").read().splitlines()
    for line in raw[1:]:
        parts = line.split("|")
        if len(parts) < 5:
            continue
        st, stfp, cofp, _ns, nm = parts[0], parts[1], parts[2], parts[3], parts[4]
        tbl[st][_norm(nm)] = stfp + cofp
        tbl[st].setdefault(_norm(nm.replace("St.", "Saint")), stfp + cofp)
    _FIPS = tbl
    return tbl


def fips_for(postal, county_name, extra=None):
    """FIPS for a county name, or '' if it cannot be resolved (SPEC rule 5)."""
    t = fips_table().get(postal, {})
    k = _norm(county_name)
    if extra and k in extra:
        return extra[k]
    if k in t:
        return t[k]
    for cand, f in t.items():           # tolerate 'Dekalb' vs 'DeKalb' etc.
        if cand.replace("saint", "st") == k.replace("saint", "st"):
            return f
    return ""


# --- writing ----------------------------------------------------------------

def write_state(postal, year, rows):
    rows = sorted(rows, key=lambda r: (r["county_fips"] or "zzzzz",
                                       r["county_name"]))
    path = os.path.join(STATES, "%s_%d.csv" % (postal, year))
    with open(path, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=FIELDS, extrasaction="ignore")
        w.writeheader()
        for r in rows:
            for k in ("votes_dem", "votes_gop", "total_votes"):
                r[k] = int(round(float(r[k])))
            w.writerow(r)
    return path


def add_provenance(state, year, url, fetched_on, fmt, rows, certified, notes):
    """Upsert one provenance row.

    This is a read-modify-write of the whole file, so it is done under an
    exclusive lock: more than one build process may be working the corpus at
    once, and an unlocked rewrite silently drops the other process's rows.
    """
    import fcntl
    with open(PROV + ".lock", "w") as lk:
        fcntl.flock(lk, fcntl.LOCK_EX)
        try:
            cur = []
            if os.path.exists(PROV):
                cur = list(csv.DictReader(open(PROV, encoding="utf-8")))
            cur = [r for r in cur
                   if not (r.get("state") == state and str(r.get("year")) == str(year))]
            cur.append(dict(state=state, year=year, url=url, fetched_on=fetched_on,
                            format=fmt, rows=rows, certified=certified, notes=notes))
            cur.sort(key=lambda r: (r.get("state", ""), str(r.get("year", ""))))
            tmp = PROV + ".tmp%d" % os.getpid()
            with open(tmp, "w", newline="", encoding="utf-8") as fh:
                w = csv.DictWriter(fh, fieldnames=["state", "year", "url", "fetched_on",
                                                   "format", "rows", "certified", "notes"])
                w.writeheader(); w.writerows(cur)
            os.replace(tmp, PROV)
        finally:
            fcntl.flock(lk, fcntl.LOCK_UN)


# --- validation -------------------------------------------------------------

_CHECK = None

def crosscheck():
    global _CHECK
    if _CHECK is None:
        _CHECK = defaultdict(dict)
        for r in csv.DictReader(open(os.path.join(HERE,
                                "derived/crosscheck_ap_counties_2024.csv"), encoding="utf-8")):
            _CHECK[r["state_postal"]][r["county_fips"]] = r
    return _CHECK


def validate(postal, year, tol=0.005, quiet=False):
    """Check invariants, and for 2024 grade against the AP county file."""
    path = os.path.join(STATES, "%s_%d.csv" % (postal, year))
    rows = list(csv.DictReader(open(path, encoding="utf-8")))
    problems, notes = [], []

    for r in rows:
        f = r["county_fips"]
        if f and (len(f) != 5 or not f.isdigit()):
            problems.append("bad fips %r (%s)" % (f, r["county_name"]))
        d, g, t = int(r["votes_dem"]), int(r["votes_gop"]), int(r["total_votes"])
        if d + g > t:
            problems.append("dem+gop > total in %s (%d+%d>%d)"
                            % (r["county_name"], d, g, t))
        if min(d, g, t) < 0:
            problems.append("negative votes in %s" % r["county_name"])
    if len(set(r["county_fips"] for r in rows if r["county_fips"])) != \
       len([r for r in rows if r["county_fips"]]):
        problems.append("duplicate county_fips")
    blank = [r["county_name"] for r in rows if not r["county_fips"]]
    if blank:
        notes.append("%d rows with no FIPS: %s" % (len(blank), ", ".join(blank[:4])))

    D = sum(int(r["votes_dem"]) for r in rows)
    G = sum(int(r["votes_gop"]) for r in rows)
    T = sum(int(r["total_votes"]) for r in rows)

    grade = ""
    if year == 2024 and postal in crosscheck():
        cc = crosscheck()[postal]
        cD = sum(int(v["votes_dem"]) for v in cc.values())
        cG = sum(int(v["votes_gop"]) for v in cc.values())
        cT = sum(int(v["total_votes"]) for v in cc.values())
        dd = (D - cD) / max(cD, 1); dg = (G - cG) / max(cG, 1); dt = (T - cT) / max(cT, 1)
        grade = "AP D%+.3f%% R%+.3f%% T%+.3f%%" % (100*dd, 100*dg, 100*dt)
        if max(abs(dd), abs(dg)) > tol:
            problems.append("differs from AP beyond %.1f%%: %s" % (100*tol, grade))
        if len(rows) != len(cc):
            notes.append("row count %d vs AP %d" % (len(rows), len(cc)))

    ok = not problems
    if not quiet:
        flag = "OK  " if ok else "FAIL"
        print("   %s %s %d  rows=%-4d D=%-9s R=%-9s T=%-9s %s"
              % (flag, postal, year, len(rows), f"{D:,}", f"{G:,}", f"{T:,}", grade))
        for p in problems:
            print("        ! " + p)
        for n in notes:
            print("        · " + n)
    return ok, dict(rows=len(rows), D=D, G=G, T=T, problems=problems, notes=notes)


def read_csv_bytes(b, **kw):
    txt = b.decode("utf-8-sig", errors="replace")
    return list(csv.DictReader(io.StringIO(txt), **kw))
