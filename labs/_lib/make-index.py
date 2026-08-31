#!/usr/bin/env python3
"""Build the lab index -- INDEX.md and index.html in the labs root.

    python3 _lib/make-index.py

Every field is read from the chapter files themselves, so the index cannot
drift from the corpus: run it again after adding a chapter and it is current.

WHAT IS READ, AND FROM WHERE

  Chapter      the folder name
  Title        the YAML `title:` of <slug>-brief.Rmd
  Topic        the YAML `subtitle:` of the same file
  Data source  the `# SOURCE` block of the folder's build script, if it has
               one; else the first fetch URL in that script; else a note that
               the chapter derives its data from a sibling.

WHY THE SOURCE COLUMN IS BEST-EFFORT. Build scripts are prose at the top and
code below, and the prose was written for a reader rather than a parser. Most
carry a `# SOURCE.` line; some spell it `# SOURCES`, some put the citation two
lines down, and a handful have no build script because the chapter reads
another chapter's output. The extractor handles those cases and marks what it
could not resolve rather than guessing, so an empty cell means "look at the
chapter", not "no source".
"""

import html
import os
import re
import subprocess
from datetime import date

LABS = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SKIP = {"_lib", "_archive", "_syllabus-template"}

# ---------------------------------------------------------------- extraction

def yaml_field(text, field):
    m = re.search(rf'^{field}:\s*(.+)$', text, re.M)
    if not m:
        return ""
    v = m.group(1).strip()
    if v.startswith('"') and v.endswith('"'):
        v = v[1:-1]
    return v.replace('\\"', '"')


def build_scripts(folder):
    d = os.path.join(folder, "data")
    if not os.path.isdir(d):
        return []
    out = []
    for f in sorted(os.listdir(d)):
        if f.endswith((".R", ".py")) and ("build" in f.lower() or f == "assemble.R"):
            out.append(os.path.join(d, f))
    return out


def script_text(folder):
    """Every build script's text, for tagging. The extracted `source` line is
    truncated and often omits the words that identify a dataset, so tags are
    matched against the scripts themselves."""
    out = []
    for path in build_scripts(folder):
        with open(path, encoding="utf-8", errors="replace") as fh:
            out.append(fh.read())
    return "\n".join(out)


def source_of(folder):
    """The chapter's primary source, as its own build script states it."""
    for path in build_scripts(folder):
        with open(path, encoding="utf-8", errors="replace") as fh:
            txt = fh.read()

        # the header comment block only -- code below can contain the word too
        head = txt[:9000]

        # Build scripts announce their provenance under several headings, and
        # several put a rule line (# ------) directly beneath the heading. Take
        # a window after the heading and discard rules and blanks rather than
        # letting the first rule terminate the capture.
        heads = (r'SOURCES?', r'WHERE EVERY INPUT COMES FROM',
                 r'WHERE THE (?:DATA|INPUTS?) COMES? FROM', r'INPUTS?')
        for h in heads:
            m = re.search(rf'^#\s*{h}\b([^\n]*)$', head, re.M)
            if not m:
                continue
            # The heading line usually carries the start of the citation
            # ("# SOURCE. American National Election Studies, Time Series..."),
            # so keep its remainder rather than starting at the line below.
            first = re.sub(r'^[.:]?\s*', '', m.group(1)).strip()
            first = re.sub(r'^\([^)]*\)\s*', '', first)
            tail = head[m.end():].split("\n")[1:]
            keep = [first] if len(first) > 3 else []
            for ln in tail[:18]:
                if not ln.startswith("#"):
                    break
                body = ln[1:].strip()
                if set(body) <= {"-", "="} and body:      # a rule line
                    continue
                if not body:
                    if keep:
                        break                              # blank ends the block
                    continue
                if re.match(r'^[A-Z][A-Z ]{4,}[.:]?$', body):   # next ALL-CAPS heading
                    break
                keep.append(body)
            body = " ".join(" ".join(keep).split())
            body = re.sub(r'\s*Run from this directory.*$', '', body)
            body = re.sub(r'^\(all relative to [^)]*\):?\s*', '', body)
            if len(body) > 12:
                return body[:400], os.path.basename(path)

        u = re.search(r'https?://[^\s"\')]+', head)
        if u:
            return u.group(0), os.path.basename(path)

        # No fetch of its own: name the sibling chapters it reads instead.
        sib = sorted(set(re.findall(r'\.\./\.\./([a-z0-9-]+)/data', txt)
                         + re.findall(r'"\.\.", "([a-z0-9-]+)", "data"', txt)))
        if sib:
            return "Derived from other chapters in this book: " + ", ".join(sib), os.path.basename(path)

    return "", ""


def first_url(text):
    m = re.search(r'https?://[^\s"\')<>]+', text)
    return m.group(0) if m else ""




# ------------------------------------------------------------------- parts
#
# WHICH INSTRUMENT PRODUCED THE DATA. The parts are the book's spine and the
# semester's, and they are the same object: each opens with its `*-source`
# chapter, which asks what that instrument can establish that nothing else can
# and what it can never establish at any level of detail, and every chapter
# after it is a question asked of that same instrument.
#
# The earlier framing here was by SUBJECT -- elections, administration,
# representation, public opinion, demography, money, civic. That taxonomy is
# retired. It cut across the instrument in both directions: the census material
# was split between "representation" (the Bureau's files) and "demography" (the
# people in them), and a voter file and a police stop log sat in different parts
# despite being the same kind of object read the same wrong way.
#
# ORDER IS THE TEACHING ORDER, and it is load-bearing rather than editorial.
# The course rule is that nothing is inferred from a source students have not
# already met, so the parts are sequenced to make every dependency run forward:
#
#   I   Counting people        depends on nothing -- the only part that doesn't
#   II  Record of an outcome   its precinct chapters need census geography (I)
#   III Asking people          grades self-report against counted ballots (II)
#   IV  Administrative records redlining and policing need census geography (I)
#   V   Compelled and commercial  depends on NOTHING in IV -- its only outside
#                              reads are rank-size -> mapping (II), surnames (I).
#                              (This line used to claim finance-source reads the
#                              voter file. It does not, and never did; it reads
#                              campaign-finance and independent-expenditures,
#                              both its own siblings. The false claim is why the
#                              money session looked pinned behind the records.)
#   VI  Scores and constructs  built on I, II and IV, and graded against them
#
# THE ORDER STATES A CLAIM, AND IT IS ABOUT HOW YOU CAME TO BE IN THE DATA.
#
#   I    you were ordered to answer
#   II   the state certified what happened
#   III  somebody asked, and you answered because you felt like it
#   IV   nobody asked you anything and you are in the file anyway
#   V    you disclosed because a statute compelled you to
#   VI   nobody collected it; a researcher built the number
#
# I AND III ARE THE PAIR THIS ORDER EXISTS TO PUT SIDE BY SIDE. Both are data
# made by asking a person a question and writing down the answer, and they
# differ in one thing only: ACS response is mandatory under 13 U.S.C. 221 and
# ANES response is not. That is the distinction the whole book turns on, and
# burying it five weeks apart is what made students ask why a survey was filed
# under "counting people". Teaching them a session apart makes it the lesson.
#
# THEN IV TURNS. Everything up to it was produced by asking somebody something.
# A registration record, a docket, a stop record: nobody asked, and the person
# is a by-product of administering a system. Moon Duchin's phrase for this is
# STATE-MARSHALLED DATA, which the part opener uses and this label does not --
# "state" in an American elections course reads as state-not-federal, and two
# of the part's own chapters (disenfranchisement, cost-of-voting) are not
# produced by any government at all.
#
# THE PREVIOUS ORDER CLAIMED DECLINING COLLECTOR AUTHORITY -- enumerate,
# certify, administer, disclose, answer, build -- and that claim does not
# survive this move, because a voluntary survey answer now precedes a compelled
# administrative record. It was the weaker claim: it described the collector,
# and this book is about the person in the row.
#
# This renumbers the parts against data-redesign-plan.md, which listed them in
# the order they were invented (returns first) rather than the order they are
# taught. Its numerals I-VI no longer match; its argument for each part does.
#
# WITHIN a part, order is also the teaching order, and the source chapter leads.
#
# Unlike KIND and TAGS this cannot be derived from the files: it is a judgment
# about provenance. So it is an explicit map, and a chapter missing from it
# shows as "unassigned" rather than being guessed at. A new chapter appearing
# here unassigned is the signal to decide where it belongs.
PARTS = {
  "front matter": "introduction",

  # THE CENSUS BUREAU. One agency, three instruments that get mistaken for each
  # other, and the geography everything else is published on. This part comes
  # first because every other part borrows a denominator from it.
  "I the census bureau": """part-1-census-bureau
   census-source census-decennial census-acs census-pep
   census-geography census-access census-api
   census-race census-coverage demographics areal-units overplotting zip-codes
   apportionment regional-shift
   migration chord age-structure section-203
   names surnames""",

  # SURVEYS. The ACS previewed this in Part I; here the reader meets the
  # instrument on its own terms and then turns on it. Public opinion first
  # (ANES, GSS, CES), then prediction -- polls, simulation, models and markets.
  # Second because Part I already taught a survey without calling the question.
  "II surveys": """part-2-surveys
   surveys-source survey-access anes party-id gender-gap ideology partisan-economy
   abortion-opinion gss-confidence thermometers
   models-markets
   validated-turnout ces-class ces-states poll-weighting poll-simulation
   perception-gap""",

  # ELECTIONS, in two runs. First the RESULT: what a return is, where it comes
  # from, the rungs it is published at, and the long series. Then the
  # MACHINERY that produced it: who was registered, who was struck off, what
  # the administrators reported. election-night is last because it is the one
  # session the calendar chooses.
  "III elections": """part-3-elections
   returns-source county-returns data-sources
   levels-of-aggregation panhandle-claim
   electoral-map mapping mid-decade mid-decade-florida wind-map sparklines
   historical-campaigns
   clerk-source house-competition distributions crossover nationalization neutral-maps
   rosters-source midterm-loss retirements primary-defeats primary-positions
   nomination-anchors nomination-rules careers
   vote-targeting bellwether whole-foods-cracker-barrel
   ga-precinct-returns precinct-geography cast-vote-records
   voter-files-source voter-file-access voter-files false-matches
   disenfranchisement turnout-denominator
   eavs residual-votes
   seat-forecast senate-2026 election-night""",

  # RECORDS OF POLITICAL ACTORS. What a member of Congress, a committee or a
  # candidate leaves behind: roll calls, filings, disclosures, attention. The
  # row is about somebody who chose to run for office, which is what separates
  # this part from the next one. The weakest part of the book, and the one most
  # worth building out.
  "IV records of political actors": """part-4-political-actors
   rollcalls-source dw-nominate scdb oral-argument officeholder-age
   finance-source campaign-finance pie-radar
   independent-expenditures lobbying
   media-attention media-ideology streamgraph campaign-visits follower-counts
   finance-network rank-size""",

  # RECORDS OF ORDINARY PEOPLE. Same kind of object as Part IV -- a record an
  # institution kept while doing something else -- but the row is about someone
  # who never volunteered for anything. A stop, a jury summons, a grade on a
  # neighbourhood. Thin on purpose for now: commercial mobility data belongs
  # here and has not been built.
  "V records of ordinary people": """part-5-ordinary-people
   admin-records-source policing jury-selection redlining""",

  # PUTTING DATA TOGETHER. Everything above is one source at a time. This part
  # combines them, which is where the interesting questions and nearly all the
  # errors live: imputing race, estimating how a group voted, grading a map.
  "VI putting data together": """part-6-putting-data-together
   uncertainty cost-of-voting bisg-check rpv vote-dilution sweet-spot redistricting""",

}
# `gotv` is deliberately absent: course-map.md retires it, and leaving it
# unassigned keeps that decision visible instead of burying it in a part.
PART_OF, PART_SEQ, SLUG_SEQ = {}, {}, {}
for _i, (_p, _slugs) in enumerate(PARTS.items()):
    PART_SEQ[_p] = _i
    for _j, _s in enumerate(_slugs.split()):
        PART_OF[_s] = _p
        SLUG_SEQ[_s] = _j
PART_SEQ["unassigned"] = len(PARTS)


def toc_key(r):
    """Sort a chapter into the book's reading order: part, then position in it.

    The rows are otherwise built in folder-alphabetical order, which is how the
    index read before the parts became the spine. A book that is also a course
    has to list itself in the order it is taught.
    """
    return (PART_SEQ[r["part"]], SLUG_SEQ.get(r["slug"], 999), r["slug"])

# ------------------------------------------------------------------ tagging
#
# Tags name the DATASET a chapter reads, so a reader can ask "what else uses
# ANES?" and get an answer at a glance. They are matched against the chapter's
# stated source, its title and its topic -- never hand-assigned -- so a chapter
# that changes its source changes its tags when this is re-run.
#
# Order matters only for readability; a chapter may carry several.
TAGS = [
    ("ANES",            r"American National Election Stud|ANES|VCF0"),
    ("CES",             r"Cooperative Election Stud|CCES|CES 20"),
    ("GSS",             r"General Social Survey|gss\.norc|GSS cumulative"),
    ("CPS",             r"Current Population Survey|CPS Voting"),
    ("Census: decennial", r"P\.?L\.? 94-171|decennial|redistricting data summary|apportionment"),
    ("Census: ACS",     r"American Community Survey|\bACS\b"),
    ("Census: geography", r"TIGER|Gazetteer|cartographic boundary|GENZ|Block Assignment|CenPop|ZCTA"),
    ("Census: estimates", r"popest|cc-est|2010surnames|genealogy|population estimates"),
    ("GeoNames",        r"GeoNames|geonames\.org"),
    ("OpenStreetMap",   r"OpenStreetMap|Overpass|brand:wikidata"),
    ("Voteview",        r"[Vv]oteview|HSall|DW-NOMINATE"),
    ("SCDB",            r"Supreme Court Database|scdb\."),
    ("Oyez",            r"Oyez|oyez\.org|api\.oyez"),
    ("FEC",             r"Federal Election Commission|fec\.gov|weball|independent[_ ]expenditure"),
    ("EAC / EAVS",      r"Election Assistance Commission|EAVS"),
    ("Voter file",      r"voter registration|voter file|registration extract|Houston County|vlist_"),
    ("Cast vote records", r"[Cc]ast [Vv]ote [Rr]ecord|\bCVR\b"),
    ("State returns",   r"Secretary of State|Statement of Vote|State Board of Elections|Division of Elections|canvass|precinct returns|county-returns"),
    ("Clerk of the House", r"Clerk of the U\.S\. House|Statistics of the Congressional"),
    ("Lobbying (LDA)",  r"Lobbying Disclosure|lobbying disclosure|senate\.gov/lobby"),
    ("Wikipedia",       r"[Ww]ikimedia|[Ww]ikipedia|pageview"),
    ("Prediction market", r"Polymarket|prediction market|gamma-api"),
    ("HOLC / redlining", r"HOLC|Mapping Inequality|mappinginequality"),
    ("Court records",   r"jury|juror|peremptory|APM-Reports"),
    ("Policing",        r"Stanford Open Policing|traffic stop|police stop"),
    # Must be about the DATA, not about a method. "[Ss]imulat" alone tagged any
    # chapter whose build script mentioned a physics simulation for a graph
    # layout, which told a reader the numbers were invented when they were not.
    ("Simulated",       r"SIMULATION|simulated data|is mostly a simulat"
                        r"|invented outright|generated rather than collected"),
    ("ALARM redistricting simulations",
                        r"ALARM Project|50-State Redistricting Simulations"
                        r"|DVN/SLCD3E"),
    ("Wire service",    r"Associated Press|\bAP\b tracker|interactives\.ap\.org"),
    ("Advocacy compilation", r"Sentencing Project|Brennan Center"),
    # An index built and maintained by academics, distinct from a researcher's
    # data REPO: the numbers here are constructed rather than collected.
    ("Researcher index", r"Cost of Voting|costofvotingindex|\bCOVI\b"),
    # Brookings compiles several of the series this book reads. Matched on the
    # NAMES of those compilations rather than on "Brookings", because Brookings
    # is also a publisher: the gotv chapter's data is Green and Gerber's
    # meta-analysis, which Brookings printed but did not assemble, and tagging
    # it here would tell a reader the numbers came from a think tank's file.
    ("Brookings compilation",
                        r"Vital Statistics on Congress|Primaries Project"),
    # A source that is a printed book rather than a file. Matched on the title
    # so it does not sweep in every chapter that merely cites a monograph.
    ("Book tables",     r"Parties on the Ground"),
    ("Jacobson",        r"Jacobson|house_jacobson"),
    ("House History",   r"history\.house\.gov|Party Divisions"),
    ("Researcher repo", r"jaytimm|PresElectionResults|tonmcg|github\.com/[A-Za-z]"),
    ("Derived in-book", r"Derived from other chapters"),
]

def tags_for(r):
    hay = " ".join([r.get("source",""), r.get("body",""), r.get("title",""),
                    r.get("topic",""), r.get("slug","")])
    out = []
    for name, pat in TAGS:
        if re.search(pat, hay):
            out.append(name)
    # A chapter that reads a sibling is tagged for that alone only if nothing
    # else matched -- otherwise the real dataset tags are more informative.
    if len(out) > 1 and "Derived in-book" in out:
        out.remove("Derived in-book")
    return out

# ------------------------------------------------------------------ assemble

# ---------------------------------------------------------------- traversal
#
# Chapters live one level deeper than they used to: labs/NN-part/<slug>/.
# A chapter with no part (gotv) still sits at the labs root, so both depths
# are walked and the caller gets the same (slug, path) pairs either way.
def chapter_dirs(labs):
    out = []
    for d in sorted(os.listdir(labs)):
        p = os.path.join(labs, d)
        if not os.path.isdir(p) or d.startswith((".", "_")):
            continue
        if re.match(r"^\d\d-", d):                 # a part directory
            for c in sorted(os.listdir(p)):
                q = os.path.join(p, c)
                if os.path.isdir(q) and not c.startswith((".", "_")):
                    out.append((c, q))
        else:
            out.append((d, p))
    return out


rows = []
for name, folder in chapter_dirs(LABS):
    if name in SKIP:
        continue
    briefs = [f for f in os.listdir(folder) if f.endswith("-brief.Rmd")]
    if not briefs:
        continue
    with open(os.path.join(folder, briefs[0]), encoding="utf-8", errors="replace") as fh:
        head = fh.read(4000)

    brief = briefs[0]
    stem  = brief[:-4]                      # <name>-brief
    # Link from the folder's REAL location, not from the slug. Chapters sit at
    # labs/<NN-part>/<slug>/, and a chapter with no part sits at the labs root,
    # so building "<slug>/..." here produced 182 dead links against 92 live
    # chapters -- an index that counted correctly and pointed nowhere.
    rel_dir   = os.path.relpath(folder, LABS).replace(os.sep, "/")
    href_html = f"{rel_dir}/{stem}.html"
    href_pdf  = f"{rel_dir}/{stem}.pdf"
    has_html = os.path.exists(os.path.join(folder, stem + ".html"))
    has_pdf  = os.path.exists(os.path.join(folder, stem + ".pdf"))

    src, script = source_of(folder)
    body = script_text(folder)

    # KIND. Six chapters take an INSTRUMENT as their subject -- what a source
    # records, what it structurally cannot, and the decisions between the
    # record and the number. The rest take a QUESTION as their subject and
    # read an instrument to answer it. The distinction is the book's spine,
    # so it is stated here rather than inferred from the folder name.
    #
    # As of the rename it happens to agree with the suffix: all six part
    # openers end in `-source` and no other chapter does. That was NOT true
    # before -- `surveys` was a source chapter whose folder did not say so,
    # while `surveys-source` was a case chapter whose folder said it was one,
    # and the pair sat adjacent in the index actively misleading the reader.
    # The set stays explicit anyway, so a future `foo-source` that is really a
    # case chapter is a decision someone makes here rather than an accident of
    # naming.
    # Six of these open a part. Two do not, and that is deliberate: a part
    # covering more than one instrument family gets a source chapter for each.
    #
    #   admin-records-source  Part III -- records other institutions kept about
    #                         a person while doing something else
    #   rosters-source        Part II  -- who HELD the seat, which is a
    #                         different question from who won the election
    #   clerk-source          Part II  -- the one federal publication of
    #                         congressional returns, and what it costs to read
    #                         a document that was typeset rather than tabulated
    #
    # The set is explicit precisely so that a `foo-source` which is not a part
    # opener is a decision rather than an accident.
    SOURCE_CHAPTERS = {"surveys-source", "census-source", "voter-files-source",
                       "returns-source", "rollcalls-source", "finance-source",
                       "admin-records-source", "rosters-source", "clerk-source"}
    # A PART OPENER takes the PART as its subject: what its chapters have in
    # common, in what order to read them, and what kind of provenance they
    # share. It is not a source chapter -- it introduces no instrument -- and
    # it is not front matter, which opens the book rather than a part.
    kind = ("part opener" if name.startswith("part-")
            else "source" if name in SOURCE_CHAPTERS
            else "front matter" if name == "introduction"
            else "case")

    rows.append(dict(
        href_html=href_html if has_html else "",
        href_pdf=href_pdf if has_pdf else "",
        part=PART_OF.get(name, "unassigned"),
        kind=kind,
        slug=name,
        title=yaml_field(head, "title"),
        topic=yaml_field(head, "subtitle"),
        source=src,
        body=body,
        url=first_url(src),
        script=script,
        has_data=os.path.isdir(os.path.join(folder, "data")),
        has_raw=os.path.isdir(os.path.join(folder, "data", "raw")),
    ))

for r in rows:
    r['tags'] = tags_for(r)

rows.sort(key=toc_key)

n = len(rows)
n_src = sum(1 for r in rows if r["source"])
n_raw = sum(1 for r in rows if r["has_raw"])
today = date.today().strftime("%-d %B %Y")

# --------------------------------------------------------------- markdown

md = [f"# The book, in reading order\n",
      f"*{n} chapters. Generated from the chapter files on {today} by "
      f"`_lib/make-index.py` — do not edit by hand; re-run it instead.*\n",
      "| Part | Kind | Chapter | Title | Tags | Topic | Data source |",
      "|---|---|---|---|---|---|---|"]
for r in rows:
    src = r["source"] or "—"
    if len(src) > 180:
        src = src[:177] + "…"
    md.append("| {} | {} | `{}` | {} | {} | {} | {} |".format(
        r["part"],
        r["kind"],
        r["slug"],
        ("[{}]({})".format(r["title"].replace("|", "\\|"), r["href_html"])
         if r["href_html"] else r["title"].replace("|", "\\|")),
        ", ".join(r["tags"]) or "—",
        r["topic"].replace("|", "\\|"),
        src.replace("|", "\\|")))
md.append("")
md.append(f"{n_src} of {n} chapters state a source in their build script; "
          f"{n_raw} commit a `data/raw/` capture of the source as it arrives.")
with open(os.path.join(LABS, "INDEX.md"), "w", encoding="utf-8") as fh:
    fh.write("\n".join(md) + "\n")

# ------------------------------------------------------------------- html

def esc(s):
    return html.escape(s or "", quote=True)

trs = []
for r in rows:
    # Source is the longest field by far (median 125 characters, max 178) and
    # it is reference detail, not something anyone scans. It is truncated here
    # and carried in full in the title attribute, so hovering gives the whole
    # citation without the column dictating the table's width.
    full = r["source"] or ""
    short = full if len(full) <= 72 else full[:69].rsplit(" ", 1)[0] + "…"
    srccell = f'<span title="{esc(full)}">{esc(short)}</span>' if full else "&mdash;"
    if r["url"] and r["url"] in short:
        srccell = srccell.replace(esc(r["url"]),
                                  f'<a href="{esc(r["url"])}">{esc(r["url"])}</a>', 1)

    title = (f'<a href="{esc(r["href_html"])}" target="_blank" rel="noopener">{esc(r["title"])}</a>'
             if r["href_html"] else esc(r["title"]))
    pdf = (f'<a class="pdf" href="{esc(r["href_pdf"])}" target="_blank" rel="noopener" '
           f'title="PDF">PDF</a>' if r["href_pdf"] else "")
    tags = "".join(f'<button class="tag" data-tag="{esc(t)}">{esc(t)}</button>'
                   for t in r["tags"]) or ""

    trs.append(
        f'<tr class="k-{esc(r["kind"]).replace(" ", "-")}">'
        f'<td class="part">{esc(r["part"])}</td>'
        f'<td class="ch">'
        f'<div class="ttl">{title}{pdf}</div>'
        f'<div class="slug"><code>{esc(r["slug"])}</code>'
        + ('<span class="raw" title="commits a data/raw capture of the source as it arrives">raw</span>'
           if r["has_raw"] else '') + '</div>'
        f'<div class="topic">{esc(r["topic"])}</div>'
        f'</td>'
        f'<td class="tags">{tags}</td>'
        f'<td class="src">{srccell}</td>'
        f'</tr>')

page = f"""<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Democracy's Data — labs by topic and data source</title>
<style>
:root{{--bg:#EFF1F2;--panel:#fff;--sunk:#E4E8EA;--ink:#12181D;--ink2:#4E5A63;
--ink3:#76838C;--rule:#CBD3D8;--rule2:#DEE4E7;--acc:#1C4C5C}}
@media (prefers-color-scheme:dark){{:root:not([data-theme=light]){{--bg:#101418;
--panel:#171D22;--sunk:#1E262C;--ink:#E3E8EA;--ink2:#9CA9B2;--ink3:#77848D;
--rule:#2B343B;--rule2:#232B31;--acc:#79B6C6}}}}
*{{box-sizing:border-box}}
body{{margin:0;background:var(--bg);color:var(--ink);font:15px/1.5 -apple-system,
BlinkMacSystemFont,"Segoe UI",Roboto,sans-serif;-webkit-font-smoothing:antialiased}}
.wrap{{max-width:1240px;margin:0 auto;padding:0 clamp(14px,3vw,32px) 72px}}
h1{{font-family:"Iowan Old Style",Charter,Palatino,Georgia,serif;font-size:clamp(26px,4vw,40px);
margin:clamp(28px,5vw,56px) 0 6px;font-weight:600;letter-spacing:-.01em}}
p.sub{{color:var(--ink2);margin:0 0 18px;max-width:76ch;font-size:14px}}
.bar{{display:flex;flex-wrap:wrap;gap:10px;align-items:center;margin:0 0 14px}}
input[type=search]{{flex:1 1 260px;padding:9px 12px;font:inherit;border:1px solid var(--rule);
border-radius:5px;background:var(--panel);color:var(--ink)}}
.count{{font-size:12.5px;color:var(--ink3);white-space:nowrap}}
.tablewrap{{overflow-x:auto;border:1px solid var(--rule);background:var(--panel)}}
table{{border-collapse:collapse;width:100%;min-width:720px;font-size:13px}}
th,td{{text-align:left;padding:9px 13px;border-bottom:1px solid var(--rule2);vertical-align:top}}
thead th{{position:sticky;top:0;background:var(--sunk);border-bottom:1px solid var(--rule);
font-size:10.5px;letter-spacing:.1em;text-transform:uppercase;color:var(--ink2);
cursor:pointer;user-select:none;white-space:nowrap}}
thead th:hover{{color:var(--ink)}}
thead th::after{{content:"";opacity:.45;font-size:9px;margin-left:5px}}
thead th.asc::after{{content:"\\25B2"}}
thead th.desc::after{{content:"\\25BC"}}
tbody tr:last-child td{{border-bottom:0}}
tbody tr:hover{{background:var(--sunk)}}
code{{font-family:"SF Mono",ui-monospace,Menlo,monospace;font-size:11.5px;color:var(--acc)}}
td.t a{{color:inherit;text-decoration:none;border-bottom:1px solid var(--rule)}}
td.t a:hover{{color:var(--acc);border-bottom-color:var(--acc)}}
a.pdf{{font-family:"SF Mono",ui-monospace,Menlo,monospace;font-size:9.5px;letter-spacing:.06em;
color:var(--ink3);border:1px solid var(--rule);border-radius:3px;padding:0 4px;margin-left:5px;
text-decoration:none;vertical-align:1px;font-style:normal}}
a.pdf:hover{{color:var(--acc);border-color:var(--acc)}}
td.t{{font-family:"Iowan Old Style",Charter,Palatino,Georgia,serif;font-style:italic;font-size:14px}}
td.src{{color:var(--ink2);font-size:12px;max-width:44ch}}
td.c{{text-align:center;color:var(--ink3);font-size:11.5px}}
/* four columns, and only one of them is prose */
table{{table-layout:fixed}}
th:nth-child(1),td:nth-child(1){{width:9.5rem}}
th:nth-child(3),td:nth-child(3){{width:13rem}}
th:nth-child(4),td:nth-child(4){{width:20rem}}
td.part{{font-size:10.5px;letter-spacing:.07em;text-transform:uppercase;color:var(--ink3);
padding-top:12px}}
tr.k-source td.part{{color:var(--acc);font-weight:600}}
tr.k-source{{background:color-mix(in srgb,var(--acc) 5%,transparent)}}
td.ch{{padding-top:9px;padding-bottom:11px}}
td.ch .ttl{{font-family:"Iowan Old Style",Charter,Palatino,Georgia,serif;font-size:15px;
line-height:1.3;margin-bottom:2px}}
td.ch .ttl a{{color:var(--ink);text-decoration:none;border-bottom:1px solid var(--rule)}}
td.ch .ttl a:hover{{color:var(--acc);border-bottom-color:var(--acc)}}
td.ch .slug{{margin:3px 0 4px}}
td.ch .slug code{{font-size:11px;color:var(--ink3)}}
td.ch .topic{{font-size:12.5px;line-height:1.4;color:var(--ink2)}}
span.raw{{font-family:"SF Mono",ui-monospace,Menlo,monospace;font-size:9px;letter-spacing:.08em;
text-transform:uppercase;color:var(--ink3);border:1px solid var(--rule);border-radius:2px;
padding:0 4px;margin-left:6px;vertical-align:1px}}
a.pdf{{font-family:"SF Mono",ui-monospace,Menlo,monospace;font-size:9px;letter-spacing:.06em;
color:var(--ink3);border:1px solid var(--rule);border-radius:3px;padding:0 4px;margin-left:6px;
text-decoration:none;vertical-align:2px}}
a.pdf:hover{{color:var(--acc);border-color:var(--acc)}}
td.tags{{padding-top:11px}}
td.src{{color:var(--ink2);font-size:11.5px;line-height:1.45;padding-top:12px}}
button.tag{{font:inherit;font-size:10.5px;line-height:1.5;margin:1px 3px 1px 0;padding:1px 7px;
border:1px solid var(--rule);border-radius:10px;background:var(--sunk);color:var(--ink2);
cursor:pointer;white-space:nowrap}}
button.tag:hover{{border-color:var(--acc);color:var(--acc)}}
button.tag.on{{background:var(--acc);border-color:var(--acc);color:var(--panel);font-weight:600}}
#chips{{display:flex;flex-wrap:wrap;gap:4px;margin:0 0 14px}}
#chips button{{font:inherit;font-size:11px;padding:3px 9px;border:1px solid var(--rule);
border-radius:11px;background:var(--panel);color:var(--ink2);cursor:pointer}}
#chips button:hover{{border-color:var(--acc);color:var(--acc)}}
#chips button.on{{background:var(--acc);border-color:var(--acc);color:var(--panel);font-weight:600}}
a{{color:var(--acc);word-break:break-all}}
tr.hide{{display:none}}
footer{{margin-top:26px;font-size:12.5px;color:var(--ink3);max-width:80ch}}
</style></head><body><div class="wrap">
<h1>Labs, by topic and data source</h1>
<p class="sub">{n} chapters. Generated from the chapter files on {today} by
<code>_lib/make-index.py</code> — re-run it after adding a chapter rather than
editing this page. Click a column heading to sort; type to filter.
{n_src} of {n} chapters state a source in their build script, and {n_raw} commit
a <code>data/raw/</code> capture of that source exactly as it arrives.</p>
<div id="chips"></div>
<div class="bar">
  <input type="search" id="q" placeholder="Filter by chapter, title, topic or source…" autocomplete="off">
  <span class="count" id="count"></span>
</div>
<div class="tablewrap"><table id="t">
<thead><tr><th>Part</th><th>Chapter</th><th>Tags</th><th>Data source</th></tr></thead>
<tbody>
{chr(10).join(trs)}
</tbody></table></div>
<footer>Sorting and filtering run in the page; nothing is fetched. An empty
source cell means the chapter&rsquo;s build script does not state one in a form
this generator recognises &mdash; usually because it reads another
chapter&rsquo;s output &mdash; not that the chapter has no source.</footer>
</div>
<script>
(function(){{
  var t=document.getElementById('t'), tb=t.tBodies[0],
      rows=[].slice.call(tb.rows), q=document.getElementById('q'),
      c=document.getElementById('count'), dir={{}};
  function shown(){{ return rows.filter(function(r){{return !r.classList.contains('hide');}}).length; }}
  function tally(){{ c.textContent = shown()+' of '+rows.length+' chapters'; }}
  [].forEach.call(t.tHead.rows[0].cells, function(th,i){{
    th.addEventListener('click', function(){{
      var d = dir[i] = !dir[i];
      [].forEach.call(t.tHead.rows[0].cells,function(o){{o.className='';}});
      th.className = d ? 'asc' : 'desc';
      rows.sort(function(a,b){{
        var x=a.cells[i].textContent.trim().toLowerCase(),
            y=b.cells[i].textContent.trim().toLowerCase();
        if(x===y) return 0;
        if(x==='') return 1;
        if(y==='') return -1;
        return (x<y? -1:1) * (d?1:-1);
      }});
      rows.forEach(function(r){{tb.appendChild(r);}});
    }});
  }});
  // ---- tag filtering -------------------------------------------------
  var active=null;
  function tagsOf(r){{
    return [].slice.call(r.querySelectorAll('button.tag'))
             .map(function(b){{return b.dataset.tag;}});
  }}
  function apply(){{
    var v=q.value.toLowerCase();
    rows.forEach(function(r){{
      var okText = !v || r.textContent.toLowerCase().indexOf(v)>=0;
      var okTag  = !active || tagsOf(r).indexOf(active)>=0;
      r.classList.toggle('hide', !(okText && okTag));
    }});
    [].forEach.call(document.querySelectorAll('button.tag'), function(b){{
      b.classList.toggle('on', active && b.dataset.tag===active);
    }});
    [].forEach.call(document.querySelectorAll('#chips button'), function(b){{
      b.classList.toggle('on', b.dataset.tag===active);
    }});
    tally();
  }}
  function pick(t){{ active = (active===t ? null : t); apply(); }}

  var all={{}};
  rows.forEach(function(r){{ tagsOf(r).forEach(function(t){{ all[t]=(all[t]||0)+1; }}); }});
  var bar=document.getElementById('chips');
  Object.keys(all).sort(function(a,b){{ return all[b]-all[a] || a.localeCompare(b); }})
    .forEach(function(t){{
      var b=document.createElement('button');
      b.textContent=t+' ('+all[t]+')'; b.dataset.tag=t;
      b.addEventListener('click', function(){{ pick(t); }});
      bar.appendChild(b);
    }});
  document.addEventListener('click', function(e){{
    if(e.target.classList && e.target.classList.contains('tag')) pick(e.target.dataset.tag);
  }});
  q.addEventListener('input', apply);
  tally();
}})();
</script></body></html>
"""
with open(os.path.join(LABS, "index.html"), "w", encoding="utf-8") as fh:
    fh.write(page)

print(f"{n} chapters -> INDEX.md and index.html")
print(f"  source stated in build script : {n_src}")
print(f"  raw capture committed         : {n_raw}")
missing = [r["slug"] for r in rows if not r["source"]]
if missing:
    print(f"  no source resolved ({len(missing)}): {' '.join(missing)}")
