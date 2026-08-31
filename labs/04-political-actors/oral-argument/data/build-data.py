#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# Build the oral-argument dataset: what the justices said out loud, and when.
#
# Seven files end up in derived/:
#
#   arguments.csv     One row per argument, 2005-2025 terms: how long it ran, how
#                     many turns and words, how many justices spoke.
#   justice_terms.csv One row per justice per term: turns, words, seconds.
#   measures.csv      The nine current justices ranked six different ways, on the
#                     2022-2025 terms. This is the chapter's central table.
#   silence.csv       Per justice per term, the share of that term's arguments in
#                     which they never asked a question.
#   order.csv         Per term, how often the justices' first questions came in
#                     exact order of seniority.
#   speakers.csv      Every justice in the window, with the date they joined and
#                     where that puts them in seniority.
#   checks.csv        The assertions this script ran, and what they returned.
#
# WHY THIS SOURCE. The sibling chapter (scdb) makes the point that the Supreme
# Court publishes opinions and publishes no data -- every column in the Supreme
# Court Database is a coder's reading of a written opinion. Oral argument is the
# one exception, and it is the reason this chapter exists. The Court records the
# audio itself, and Oyez publishes that audio aligned to a transcript, turn by
# turn, with a start and a stop time on every turn to the hundredth of a second.
# Nobody had to decide what a turn *meant* in order to write down when it began.
#
# That is the sharpest available contrast to SCDB, and it is why the two
# chapters sit beside each other.
#
# WHAT IT STILL COSTS. The contrast is not total, and the chapter says so. A
# transcript is a document a court reporter made. Somebody decided where one
# turn stops and the next starts, whether an interruption is a turn, and which
# of two overlapping voices gets the words. Oyez then attached a name to each
# turn. The timestamps are honest; the turns they are attached to were drawn by
# a person.
#
# THE EPSTEIN-POSNER REPORT. Lee Epstein and Eric Posner published "Two Decades
# of Oral Argument in the Supreme Court, 2005-2025 Terms" on 12 August 2026, for
# the New York Times. Their Roberts Court Oral Argument Corpus is built from
# WESTLAW transcripts and is NOT public -- Westlaw is licensed, so their raw
# file cannot be redistributed and is not obtainable here. This build therefore
# reconstructs the same measures from a public source, and the report's
# published figures are used only as an external check. Where a number here is
# theirs rather than ours, the brief says so in the sentence that uses it.
#
# THE THREE ENDPOINTS, in order:
#   /cases?filter=term:YYYY               the term's case list
#   /cases/YYYY/<docket>                  one case, which names its argument audio
#   /case_media/oral_argument_audio/<id>  the transcript, turn by turn
#
# WHAT GETS EXCLUDED, AND WHY. Two kinds of justice turn are not participation
# and are dropped from every count below:
#
#   ADMINISTRATIVE   "We'll hear argument this morning in case 23-191";
#                    "The case is submitted"; "Justice Sotomayor?"
#   COURTESY         "Thank you, counsel"; "No questions."
#
# The Chief Justice gavels every argument in and out, so counting those turns
# would make the Chief the most talkative justice on the Court by construction.
# Epstein and Posner drop the same two categories and report that it removes 4%
# of their turns. The rule below is this script's own, written from their
# description rather than from their code, and the share it removes is printed
# and written to checks.csv so it can be compared against theirs rather than
# assumed to match.
#
# THE CACHE. The download is about 1,500 cases and 1,400 transcripts, roughly
# half a gigabyte, which has no business in a course repository. It goes to a
# cache directory instead, and re-running skips anything already there:
#
#   DD_OYEZ_CACHE=/some/where python3 build-data.py     (default: $TMPDIR/oyez-cache)
#
# raw/ holds one transcript in full -- the exhibit the brief prints -- because a
# chapter arguing that the record is turn-level and timestamped has to show a
# turn with its timestamps on it.
#
# SOURCE. Oyez, a project of Cornell's Legal Information Institute, Justia and
# Chicago-Kent College of Law. https://api.oyez.org  (no key, no account)
#
# Run from this directory:  python3 build-data.py
# ---------------------------------------------------------------------------

import collections, csv, json, os, re, sys, tempfile, time
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone
from urllib.request import urlopen, Request
from urllib.error import HTTPError, URLError

TERMS = range(2005, 2026)
CACHE = os.environ.get("DD_OYEZ_CACHE",
                       os.path.join(tempfile.gettempdir(), "oyez-cache"))
UA = {"User-Agent": "Mozilla/5.0 (Macintosh) 84-355 course build",
      "Accept": "application/json"}

# The window the chapter ranks justices in. 2022 is the first term with the
# current nine, because Jackson replaced Breyer at the end of the 2021 term.
RECENT_FROM, RECENT_TO = 2022, 2025

os.makedirs("derived", exist_ok=True)
os.makedirs("raw", exist_ok=True)
for sub in ("terms", "cases", "audio"):
    os.makedirs(os.path.join(CACHE, sub), exist_ok=True)

CHECKS = []


def check(name, value, ok):
    """Record an assertion and its answer. Every one of these can fail: each
    compares a computed number against a bound the data could violate."""
    CHECKS.append({"check": name, "value": value, "passed": bool(ok)})
    if not ok:
        sys.stderr.write("CHECK FAILED: %s = %s\n" % (name, value))
    return ok


# ---- fetching -------------------------------------------------------------

def get(url, path, tries=4):
    if os.path.exists(path) and os.path.getsize(path) > 2:
        return open(path, "rb").read()
    for k in range(tries):
        try:
            with urlopen(Request(url, headers=UA), timeout=60) as r:
                b = r.read()
            with open(path, "wb") as f:
                f.write(b)
            return b
        except (HTTPError, URLError, TimeoutError, OSError) as e:
            if isinstance(e, HTTPError) and e.code == 404:
                with open(path, "wb") as f:
                    f.write(b"null")
                return b"null"
            time.sleep(1.5 * (k + 1))
    sys.stderr.write("FAILED %s\n" % url)
    return None


print("cache: %s" % CACHE, flush=True)

cases = []
for t in TERMS:
    b = get("https://api.oyez.org/cases?per_page=0&filter=term:%d" % t,
            os.path.join(CACHE, "terms", "%d.json" % t))
    for c in json.loads(b):
        cases.append((t, c["ID"], c["href"]))
print("cases listed: %d over %d terms" % (len(cases), len(TERMS)), flush=True)

with ThreadPoolExecutor(max_workers=6) as ex:
    list(ex.map(lambda r: get(r[2], os.path.join(CACHE, "cases", "%d.json" % r[1])),
                cases))

audio_jobs, case_of = [], {}
for t, cid, href in cases:
    try:
        d = json.load(open(os.path.join(CACHE, "cases", "%d.json" % cid)))
    except Exception:
        continue
    if not isinstance(d, dict):
        continue
    for a in (d.get("oral_argument_audio") or []):
        audio_jobs.append((a["id"], a["href"]))
        case_of[a["id"]] = d
print("argument audio objects: %d" % len(audio_jobs), flush=True)

with ThreadPoolExecutor(max_workers=6) as ex:
    list(ex.map(lambda r: get(r[1], os.path.join(CACHE, "audio", "%d.json" % r[0])),
                audio_jobs))


# ---- what counts as a word, and what does not -----------------------------

WORD = re.compile(r"[A-Za-z0-9]")


def words(text):
    """Tokens containing at least one letter or digit. This drops the bare '--'
    that transcripts use for an interruption, which would otherwise be counted
    as a word wherever one speaker cut across another."""
    return sum(1 for w in text.split() if WORD.search(w))


# ---- administrative and courtesy turns ------------------------------------
# Written from Epstein and Posner's description of the two categories they
# exclude. Matching is done on a lowercased copy with punctuation stripped, so
# "Thank you, counsel." and "Thank you counsel" are the same string.

# Gavelling in and out. These are dropped whatever their length, because the
# opening runs to a sentence -- "We'll hear argument this morning in case 23-191,
# Williams versus Reed" -- and nothing else in the transcript begins that way.
OPENING = re.compile(r"^we (will|ll) hear (argument|reargument)\b")

# Gavelling out. The closing is always a thank-you plus "the case is submitted",
# and the thank-you names whoever argued last, so the string is never the same
# twice: "Thank you, General. The case is submitted." The phrase is matched
# wherever it falls, under a word cap so a real question mentioning a submitted
# case is not swept up with it.
CLOSING = re.compile(r"\bcases? (is|are) submitted\b")
CLOSING_MAX_WORDS = 15

# Handing the floor to a named person: "Justice Sotomayor?", "Mr. Fletcher?",
# "General Prelogar, anything further?" These need a length guard, because a
# real question can also open with a name -- "Mr. Phillips, does your argument
# depend on that?" -- and six words is comfortably short of one.
HANDOFF = re.compile(r"^(justice \w+|general \w+|m[rs]s? \w+|counsel)\b")
HANDOFF_MAX_WORDS = 6

COURTESY = re.compile(
    r"^(thank you( counsel| mr chief justice| general| your honor)?"
    r"|no( further)? questions( (mr )?chief justice)?"
    r"|nothing further"
    r"|nothing here"
    r"|i have nothing"
    r"|no questions here"
    r"|thanks)\.?$")

PUNCT = re.compile(r"[^a-z0-9 ]+")


def dropped_turn(text, is_chief):
    """True if this justice turn is gavel work or politeness rather than
    participation. Administrative turns are the Chief's alone; courtesy turns
    are anybody's, and are only dropped when they are the WHOLE turn."""
    t = PUNCT.sub(" ", text.lower())
    t = re.sub(r"\s+", " ", t).strip()
    if not t:
        return True
    if COURTESY.match(t):
        return True
    if is_chief and OPENING.match(t):
        return True
    if is_chief and CLOSING.search(t) and len(t.split()) <= CLOSING_MAX_WORDS:
        return True
    if is_chief and HANDOFF.match(t) and len(t.split()) <= HANDOFF_MAX_WORDS:
        return True
    return False


# ---- regimes --------------------------------------------------------------
# Two dates changed the format, and neither was a decision about how much to
# talk. On 4 May 2020 the Court began hearing argument by telephone, and
# imposed a rule it had never had: after the advocate's opening, each justice
# questioned in turn, in order of seniority, one at a time. When the Court
# returned to the courtroom in October 2021 it kept a version of that rule --
# the open free-for-all first, then a seniority round at the end.

TELEPHONE_FROM = datetime(2020, 5, 4, tzinfo=timezone.utc)
HYBRID_FROM = datetime(2021, 10, 1, tzinfo=timezone.utc)


def regime(dt):
    if dt is None:
        return "unknown"
    if dt < TELEPHONE_FROM:
        return "in person, no rounds"
    if dt < HYBRID_FROM:
        return "telephone, seniority rounds"
    return "in person, seniority round added"


DATE_IN_TITLE = re.compile(r"-\s*([A-Z][a-z]+ \d{1,2}, \d{4})\s*$")


def argument_date(audio, case):
    m = DATE_IN_TITLE.search(audio.get("title") or "")
    if m:
        try:
            return datetime.strptime(m.group(1), "%B %d, %Y").replace(
                tzinfo=timezone.utc)
        except ValueError:
            pass
    for ev in (case.get("timeline") or []):
        if ev.get("event") == "Argued" and ev.get("dates"):
            return datetime.fromtimestamp(ev["dates"][0], tz=timezone.utc)
    return None


# ---- read every transcript ------------------------------------------------

arguments = []          # one row per argument
jt = collections.defaultdict(lambda: collections.Counter())   # (justice, term)
seat = {}               # identifier -> {name, joined, chief}
sat = collections.defaultdict(set)    # (justice, term) -> argument ids present
spoke = collections.defaultdict(set)  # (justice, term) -> argument ids spoken in
order_rows = []
n_raw_turns = n_dropped = 0
exhibit = None

for aid, href in audio_jobs:
    try:
        d = json.load(open(os.path.join(CACHE, "audio", "%d.json" % aid)))
    except Exception:
        continue
    if not isinstance(d, dict):
        continue
    tr = d.get("transcript") or {}
    secs = tr.get("sections") or []
    if not secs:
        continue

    case = case_of[aid]
    term = int(case["term"])
    when = argument_date(d, case)

    # every turn in the argument, flattened, in the order it was spoken
    turns = []
    for s in secs:
        for tu in (s.get("turns") or []):
            sp = tu.get("speaker") or {}
            ident = sp.get("identifier")
            if not ident:
                continue
            roles = sp.get("roles") or []
            jrole = next((r for r in roles
                          if r.get("type") == "scotus_justice"), None)
            text = " ".join(b.get("text") or ""
                            for b in (tu.get("text_blocks") or []))
            turns.append({
                "ident": ident, "name": sp.get("name") or ident,
                "justice": jrole is not None,
                "chief": bool(jrole and "Chief" in (jrole.get("role_title") or "")),
                "joined": (jrole or {}).get("date_start"),
                "start": tu.get("start"), "stop": tu.get("stop"),
                "words": words(text), "text": text})

    if not turns:
        continue

    starts = [t["start"] for t in turns if t["start"] is not None]
    stops = [t["stop"] for t in turns if t["stop"] is not None]
    duration = (max(stops) - min(starts)) if starts and stops else None

    n_raw_turns += len(turns)
    kept = []
    for t in turns:
        if t["justice"] and dropped_turn(t["text"], t["chief"]):
            n_dropped += 1
            continue
        kept.append(t)

    for t in kept:
        if not t["justice"]:
            continue
        if t["ident"] not in seat:
            seat[t["ident"]] = {"name": t["name"], "joined": t["joined"],
                                "chief": t["chief"]}
        seat[t["ident"]]["chief"] = seat[t["ident"]]["chief"] or t["chief"]
        k = (t["ident"], term)
        jt[k]["turns"] += 1
        jt[k]["words"] += t["words"]
        if t["start"] is not None and t["stop"] is not None:
            jt[k]["seconds"] += max(0.0, t["stop"] - t["start"])
        spoke[k].add(aid)

    # WHO WAS ON THE BENCH, AND WHY IT TAKES A SECOND SOURCE.
    #
    # A justice who says nothing leaves no row. The transcript records speech,
    # so silence has no timestamp and cannot be counted from the transcript at
    # all -- the first version of this script reported that no justice was ever
    # silent, which is what a file that only records talking will always say.
    #
    # The roster has to come from somewhere else. Oyez's `heard_by` names the
    # "natural court", the stable set of justices sitting in that stretch of the
    # Court's history. It is a roster for a period rather than for a day, so a
    # justice recused from one case reads here as a justice who kept quiet in
    # it. The brief says so where it uses this number.
    roster = set()
    for court in (case.get("heard_by") or []):
        for m in (court.get("members") or []):
            if any(r.get("type") == "scotus_justice"
                   for r in (m.get("roles") or [])):
                roster.add(m["href"].rsplit("/", 1)[-1])
                if m["href"].rsplit("/", 1)[-1] not in seat:
                    jr = next(r for r in m["roles"]
                              if r.get("type") == "scotus_justice")
                    seat[m["href"].rsplit("/", 1)[-1]] = {
                        "name": m.get("name") or "",
                        "joined": jr.get("date_start"),
                        "chief": "Chief" in (jr.get("role_title") or "")}
    present = roster or {t["ident"] for t in turns if t["justice"]}
    for ident in present:
        sat[(ident, term)].add(aid)

    jturns = [t for t in kept if t["justice"]]
    lturns = [t for t in kept if not t["justice"]]

    # first substantive question, per justice, in the order they came
    first = []
    seen = set()
    for t in jturns:
        if t["ident"] not in seen:
            seen.add(t["ident"])
            first.append(t["ident"])
    order_rows.append({"aid": aid, "term": term, "when": when,
                       "regime": regime(when), "first": first})

    arguments.append({
        "term": term,
        "argued": when.date().isoformat() if when else "",
        "regime": regime(when),
        "case": case.get("name") or "",
        "docket": (case.get("docket_number") or "").strip(),
        "minutes": round(duration / 60.0, 2) if duration else "",
        "turns": len(kept),
        "justice_turns": len(jturns),
        "lawyer_turns": len(lturns),
        "justice_words": sum(t["words"] for t in jturns),
        "lawyer_words": sum(t["words"] for t in lturns),
        "justices_present": len(present),
        "justices_speaking": len({t["ident"] for t in jturns}),
    })

    if exhibit is None and (case.get("docket_number") or "").strip() == "23-191":
        exhibit = (aid, d)

print("arguments parsed: %d" % len(arguments), flush=True)

check("arguments parsed is within 10% of the 1,425 Epstein-Posner report",
      len(arguments), 1280 <= len(arguments) <= 1570)
check("no argument falls outside the 2005-2025 terms",
      sum(1 for a in arguments if not (2005 <= a["term"] <= 2025)) == 0,
      all(2005 <= a["term"] <= 2025 for a in arguments))

pct_dropped = round(100.0 * n_dropped / n_raw_turns, 2)
check("share of turns dropped as administrative or courtesy is under 10%",
      pct_dropped, pct_dropped < 10)

with_minutes = [a for a in arguments if a["minutes"] != ""]
check("at least 95% of arguments have a duration",
      round(100.0 * len(with_minutes) / len(arguments), 1),
      len(with_minutes) >= 0.95 * len(arguments))
check("no argument runs longer than 6 hours",
      max(a["minutes"] for a in with_minutes),
      max(a["minutes"] for a in with_minutes) < 360)


# ---- seniority ------------------------------------------------------------
# The Chief is first by office however recently appointed; associates follow in
# order of the date they joined. This is the order the telephone rounds ran in.

def seniority():
    assoc = sorted((v["joined"] or 0, k) for k, v in seat.items()
                   if not v["chief"])
    rank = {}
    for k, v in seat.items():
        if v["chief"]:
            rank[k] = 0
    for i, (_, k) in enumerate(assoc, start=1):
        rank[k] = i
    return rank


SEN = seniority()

with open("derived/speakers.csv", "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["justice", "name", "joined", "chief", "seniority_rank"])
    for k in sorted(seat, key=lambda z: SEN[z]):
        v = seat[k]
        j = (datetime.fromtimestamp(v["joined"], tz=timezone.utc).date().isoformat()
             if v["joined"] else "")
        w.writerow([k, v["name"], j, int(v["chief"]), SEN[k]])

check("every justice who spoke has a seniority rank",
      len(SEN), len(SEN) == len(seat))


# ---- did the first questions come in order of seniority? ------------------
# Under the telephone format the Chief questioned first and the associates
# followed in seniority order, one round each. So the ORDER of first questions
# should snap to the seniority list on 4 May 2020 and stay there. Nothing about
# the justices changed on that date. The rule did.

def in_seniority_order(idents):
    ranks = [SEN[i] for i in idents if i in SEN]
    return len(ranks) >= 4 and ranks == sorted(ranks)


by_term = collections.defaultdict(list)
for r in order_rows:
    by_term[r["term"]].append(r)

with open("derived/order.csv", "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["term", "arguments", "pct_in_seniority_order", "regime"])
    for t in sorted(by_term):
        rows = [r for r in by_term[t] if len(r["first"]) >= 4]
        if not rows:
            continue
        hit = sum(1 for r in rows if in_seniority_order(r["first"]))
        reg = collections.Counter(r["regime"] for r in rows).most_common(1)[0][0]
        w.writerow([t, len(rows), round(100.0 * hit / len(rows), 1), reg])

# the same thing at the finer grain the rule actually changed on: the day
pre = [r for r in order_rows if r["regime"] == "in person, no rounds"
       and len(r["first"]) >= 4]
tel = [r for r in order_rows if r["regime"] == "telephone, seniority rounds"
       and len(r["first"]) >= 4]
hyb = [r for r in order_rows if r["regime"] == "in person, seniority round added"
       and len(r["first"]) >= 4]
PCT = lambda rows: round(100.0 * sum(1 for r in rows
                                     if in_seniority_order(r["first"])) / len(rows), 1)
check("seniority order is rare before the telephone rule", PCT(pre), PCT(pre) < 15)
check("seniority order is common under the telephone rule", PCT(tel), PCT(tel) > 60)


# ---- per justice per term -------------------------------------------------

with open("derived/justice_terms.csv", "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["justice", "name", "term", "arguments_present",
                "arguments_spoke", "turns", "words", "seconds"])
    for (ident, term) in sorted(sat, key=lambda z: (SEN.get(z[0], 99), z[1])):
        if ident not in seat:
            continue
        c = jt[(ident, term)]
        w.writerow([ident, seat[ident]["name"], term,
                    len(sat[(ident, term)]), len(spoke[(ident, term)]),
                    c["turns"], c["words"], round(c["seconds"], 1)])


# ---- silence --------------------------------------------------------------

silent_total = collections.Counter()
with open("derived/silence.csv", "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["justice", "name", "term", "arguments_on_bench", "arguments_silent",
                "pct_silent", "regime"])
    reg_of_term = {t: collections.Counter(r["regime"] for r in by_term[t])
                   .most_common(1)[0][0] for t in by_term}
    for (ident, term) in sorted(sat, key=lambda z: (SEN.get(z[0], 99), z[1])):
        if ident not in seat:
            continue
        n = len(sat[(ident, term)])
        s = n - len(spoke[(ident, term)])
        silent_total[ident] += s
        w.writerow([ident, seat[ident]["name"], term, n, s,
                    round(100.0 * s / n, 1) if n else "",
                    reg_of_term.get(term, "")])

n_silences = sum(silent_total.values())
thomas_share = round(100.0 * silent_total.get("clarence_thomas", 0) /
                     max(n_silences, 1), 1)
check("silences found, against the 1,354 the report counts", n_silences,
      900 <= n_silences <= 1900)
check("Thomas's share of all silences, which the report puts near 80%",
      thomas_share, 60 <= thomas_share <= 95)


# ---- arguments ------------------------------------------------------------

with open("derived/arguments.csv", "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=list(arguments[0].keys()))
    w.writeheader()
    for a in sorted(arguments, key=lambda z: (z["term"], z["argued"], z["case"])):
        w.writerow(a)


# ---- the six measures -----------------------------------------------------
# Six ways to answer "who talks the most", every one of them defensible, all
# six computed from the same turns. The ranks are what the chapter is about.

recent = [a for a in arguments if RECENT_FROM <= a["term"] <= RECENT_TO]
tot_jwords = sum(a["justice_words"] for a in recent)
tot_minutes = sum(a["minutes"] for a in recent if a["minutes"] != "")

cur = collections.Counter()
cur_cases = collections.Counter()
for (ident, term), c in jt.items():
    if RECENT_FROM <= term <= RECENT_TO:
        cur[(ident, "turns")] += c["turns"]
        cur[(ident, "words")] += c["words"]
        cur[(ident, "seconds")] += c["seconds"]
for (ident, term), s in sat.items():
    if RECENT_FROM <= term <= RECENT_TO:
        cur_cases[ident] += len(s)

current = sorted({i for (i, _) in cur if cur_cases[i] >= 100},
                 key=lambda z: SEN.get(z, 99))
check("the recent window holds exactly nine justices", len(current),
      len(current) == 9)

MEASURES = [
    ("turns per argument", lambda i: cur[(i, "turns")] / cur_cases[i]),
    ("words per argument", lambda i: cur[(i, "words")] / cur_cases[i]),
    ("words per turn", lambda i: cur[(i, "words")] / max(cur[(i, "turns")], 1)),
    ("seconds of floor per argument", lambda i: cur[(i, "seconds")] / cur_cases[i]),
    ("share of all justice words", lambda i: 100.0 * cur[(i, "words")] / tot_jwords),
    ("share of the argument clock",
     lambda i: 100.0 * (cur[(i, "seconds")] / 60.0) / tot_minutes),
]

with open("derived/measures.csv", "w", newline="") as f:
    w = csv.writer(f)
    # term_from and term_to travel with the table so the brief can name the
    # window without retyping it, and so changing RECENT_FROM here changes the
    # sentence in the chapter.
    w.writerow(["measure", "justice", "name", "value", "rank",
                "term_from", "term_to"])
    for label, fn in MEASURES:
        vals = sorted(((fn(i), i) for i in current), reverse=True)
        for rank, (v, i) in enumerate(vals, start=1):
            w.writerow([label, i, seat[i]["name"], round(v, 3), rank,
                        RECENT_FROM, RECENT_TO])

# How much does the ranking actually move? The chapter's prediction prompt asks
# the reader to commit to a number before seeing this one.
ranks = collections.defaultdict(dict)
for label, fn in MEASURES:
    vals = sorted(((fn(i), i) for i in current), reverse=True)
    for rank, (v, i) in enumerate(vals, start=1):
        ranks[i][label] = rank
spread = {i: max(r.values()) - min(r.values()) for i, r in ranks.items()}
check("at least one justice moves 3 or more places across the six measures",
      max(spread.values()), max(spread.values()) >= 3)
check("the six measures do not all produce the same ranking",
      sum(spread.values()), sum(spread.values()) > 0)


# ---- the exhibit ----------------------------------------------------------
# One transcript, committed whole, because the brief prints turns out of it.

if exhibit is not None:
    aid, d = exhibit
    with open("raw/oyez-oral-argument-%d.json" % aid, "w") as f:
        json.dump(d, f, indent=1)
    # The transcript names the day but not the case, so the case it belongs to
    # is written down beside it. Otherwise the brief has to retype a case name
    # to caption its own exhibit.
    ec = case_of[aid]
    with open("derived/exhibit.csv", "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["file", "case", "docket", "term", "title"])
        w.writerow(["raw/oyez-oral-argument-%d.json" % aid,
                    ec.get("name") or "", (ec.get("docket_number") or "").strip(),
                    ec.get("term") or "", d.get("title") or ""])
    print("exhibit: raw/oyez-oral-argument-%d.json" % aid, flush=True)
check("the exhibit transcript was captured", exhibit is not None,
      exhibit is not None)


# ---- totals ---------------------------------------------------------------
# The counts the brief compares against the published report. Turns are given
# both before and after the two exclusions, because the report quotes both and
# comparing one against the other would be an error worth avoiding by keeping
# them in separate rows with their own names.

with open("derived/totals.csv", "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["quantity", "value"])
    w.writerow(["arguments", len(arguments)])
    w.writerow(["turns_read", n_raw_turns])
    w.writerow(["turns_dropped", n_dropped])
    w.writerow(["turns_kept", n_raw_turns - n_dropped])
    w.writerow(["justice_turns_kept", sum(a["justice_turns"] for a in arguments)])
    w.writerow(["lawyer_turns_kept", sum(a["lawyer_turns"] for a in arguments)])
    w.writerow(["justice_words", sum(a["justice_words"] for a in arguments)])
    w.writerow(["lawyer_words", sum(a["lawyer_words"] for a in arguments)])
    w.writerow(["silences", n_silences])
    w.writerow(["arguments_with_a_silent_justice",
                sum(1 for a in arguments
                    if a["justices_speaking"] < a["justices_present"])])


# ---- checks ---------------------------------------------------------------

with open("derived/checks.csv", "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=["check", "value", "passed"])
    w.writeheader()
    for c in CHECKS:
        w.writerow(c)

print("\nturns read: %d   dropped as administrative or courtesy: %d (%.2f%%)"
      % (n_raw_turns, n_dropped, pct_dropped))
print("justices in the 2022-2025 window: %d" % len(current))
print("first questions in seniority order: %.1f%% before the telephone rule, "
      "%.1f%% under it, %.1f%% after" % (PCT(pre), PCT(tel), PCT(hyb)))
print("checks: %d run, %d passed"
      % (len(CHECKS), sum(1 for c in CHECKS if c["passed"])))
