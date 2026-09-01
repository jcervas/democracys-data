#!/usr/bin/env python3
"""Build the doc index -- INDEX.md and index.html in the labs root.

    python3 _lib/make-index.py

Every field is read from the doc files themselves, so the index cannot
drift from the corpus: run it again after adding a doc and it is current.

WHAT IS READ, AND FROM WHERE

  Slug         the folder name
  Title        the YAML `title:` of <slug>-brief.Rmd
  Topic        the YAML `subtitle:` of the same file
  Type (YAML)  the YAML `type:` of the same file -- checked against the role
               the SECTIONS structure assigns, and a mismatch is warned about
               (SECTIONS is authoritative for placement)
  Data source  the `# SOURCE` block of the folder's build script, if it has
               one; else the first fetch URL in that script; else a note that
               the doc derives its data from a sibling.

Placement -- which section, which cluster, and whether a doc is an intro, a
data-type chapter or a brief -- is the SECTIONS structure below. The DIRECTORY
a slug lives in is discovered from the filesystem (a slug appears in exactly
one labs/NN-*/ directory), so moving a doc between directories does not touch
this file; only changing what the doc IS does.

WHY THE SOURCE COLUMN IS BEST-EFFORT. Build scripts are prose at the top and
code below, and the prose was written for a reader rather than a parser. Most
carry a `# SOURCE.` line; some spell it `# SOURCES`, some put the citation two
lines down, and a handful have no build script because the doc reads another
doc's output. The extractor handles those cases and marks what it could not
resolve rather than guessing, so an empty cell means "look at the doc", not
"no source".
"""

import html
import os
import re
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
    """The doc's primary source, as its own build script states it."""
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

        # No fetch of its own: name the sibling docs it reads instead.
        sib = sorted(set(re.findall(r'\.\./\.\./([a-z0-9-]+)/data', txt)
                         + re.findall(r'"\.\.", "([a-z0-9-]+)", "data"', txt)))
        if sib:
            return "Derived from other chapters in this book: " + ", ".join(sib), os.path.basename(path)

    return "", ""


def first_url(text):
    m = re.search(r'https?://[^\s"\')<>]+', text)
    return m.group(0) if m else ""


# ---------------------------------------------------------------- sections
#
# THE BOOK'S SPINE IS FOUR SECTIONS, and each section is a kind of provenance:
#
#   I    Data About the Population   you were ordered to answer, and everything
#                                else borrows a denominator from the result
#   II   Survey Data             somebody asked, and you answered voluntarily
#   III  Administrative Data     nobody asked you anything and you are in the
#                                file anyway -- returns, rosters, voter files,
#                                roll calls, money, attention, ordinary people
#   IV   Putting Data Together   nobody collected it; the number is built by
#                                joining the files above
#
# The six labs/NN-*/ DIRECTORIES persist from the previous six-part layout:
# Section III spans 03-elections, 04-political-actors and 05-ordinary-people.
# Nothing here names a directory -- each slug's folder is discovered from the
# filesystem -- so a future `git mv` of one doc edits nothing in this file.
#
# EACH DOC PLAYS ONE OF THREE ROLES, and the role is assigned here:
#
#   intro     opens a section: what its docs have in common, in what order to
#             read them, what kind of provenance they share
#   chapter   a data-type chapter: takes an INSTRUMENT as its subject -- what
#             that source records, what it structurally cannot, and the
#             decisions between the record and the number
#   brief     a short lab: takes a QUESTION as its subject and reads an
#             instrument to answer it
#
# WITHIN A SECTION, CONTENT IS GROUPED INTO CLUSTERS: one (occasionally zero
# or two) data-type chapter(s) plus the briefs that use that data. Order is
# the teaching order and the chapter leads its cluster.
#
# Each Rmd also carries a YAML `type:` (chapter for intros and data-type
# chapters, brief for everything else). SECTIONS is authoritative for
# placement; the generator warns when the YAML disagrees.
#
# Roles and grouping cannot be derived from the files: they are judgments.
# So they are an explicit map, and the generator FAILS if the filesystem and
# this structure disagree in either direction -- a new doc on disk must be
# placed here, and a slug listed here must exist on disk.

SECTIONS = [
  dict(name="Front matter", intros=["introduction"], legacy=[], clusters=[]),

  dict(name="I. Data About the Population", intros=["part-1-census-bureau"], legacy=[],
       clusters=[
    ("The Census and Its Products", ["census-source"],
     ["census-access", "census-api", "census-coverage", "census-race"]),
    ("The Decennial Census", ["census-decennial"],
     ["apportionment", "regional-shift", "demographics", "overplotting"]),
    ("The American Community Survey", ["census-acs"],
     ["migration", "chord", "section-203"]),
    ("Population Estimates", ["census-pep"], []),
    ("Census Geography", ["census-geography"],
     ["areal-units", "zip-codes"]),
  ]),

  dict(name="II. Survey Data", intros=["part-2-surveys"], legacy=[], clusters=[
    ("What a Survey Can Establish", ["surveys-source"],
     ["survey-access", "age-structure"]),
    ("Weighting", ["ces-class"], ["ces-states"]),
    ("Political Polls", [],
     ["poll-weighting", "poll-simulation"]),
    ("Public Opinion", ["anes"],
     ["party-id", "gender-gap", "ideology", "partisan-economy",
      "abortion-opinion", "gss-confidence", "thermometers", "perception-gap"]),
  ]),

  dict(name="III. Administrative Data", intros=["admin-records-source"],
       legacy=[],
       clusters=[
    ("Election Returns", ["returns-source", "levels-of-aggregation"],
     ["county-returns", "panhandle-claim", "electoral-map", "mapping",
      "wind-map", "sparklines", "historical-campaigns", "distributions",
      "crossover", "nationalization", "bellwether", "vote-targeting",
      "ga-precinct-returns", "precinct-geography", "cast-vote-records",
      "residual-votes", "eavs"]),
    ("Rosters of Officeholders", ["rosters-source"],
     ["house-competition", "midterm-loss", "retirements", "primary-defeats",
      "primary-positions", "nomination-anchors", "nomination-rules",
      "careers"]),
    ("Voter Files", ["voter-files-source"],
     ["voter-file-access", "voter-files", "false-matches",
      "disenfranchisement", "gotv"]),
    ("Roll Calls and Courts", ["rollcalls-source"],
     ["dw-nominate", "scdb", "oral-argument", "officeholder-age"]),
    ("Campaign Finance", ["finance-source"],
     ["campaign-finance", "pie-radar", "independent-expenditures", "lobbying",
      "finance-network"]),
    ("Media and Attention", [],
     ["media-attention", "media-ideology", "streamgraph", "campaign-visits",
      "follower-counts"]),
    ("Records of Ordinary People", [],
     ["policing", "jury-selection", "redlining", "names"]),
  ]),

  dict(name="IV. Putting Data Together",
       intros=["part-6-putting-data-together"], legacy=[], clusters=[
    ("Joining Files", ["data-sources"], []),
    ("Survey Meets Record", [], ["validated-turnout"]),
    ("Race Inference", [], ["surnames", "bisg-check", "uncertainty", "rpv"]),
    ("Districts", [],
     ["neutral-maps", "mid-decade", "redistricting",
      "vote-dilution", "sweet-spot"]),
    ("Forecasting", [],
     ["models-markets", "seat-forecast", "senate-2026", "election-night"]),
    ("Proxies and Indices", [],
     ["turnout-denominator", "cost-of-voting", "whole-foods-cracker-barrel",
      "rank-size"]),
  ]),
]

# Derived maps: slug -> section name / cluster label / role / global order.
SECTION_OF, CLUSTER_OF, ROLE_OF, SEQ = {}, {}, {}, {}
LEGACY = set()


def _roman(section_name):
    head = section_name.split(".")[0]
    return head if re.fullmatch(r"[IVX]+", head) else ""


def _place(slug, section, cluster, role, seq):
    if slug in SEQ:
        raise SystemExit(f"make-index: `{slug}` appears twice in SECTIONS")
    SECTION_OF[slug], CLUSTER_OF[slug] = section, cluster
    ROLE_OF[slug], SEQ[slug] = role, seq


_seq = 0
for _sec in SECTIONS:
    for _s in _sec["intros"] + _sec["legacy"]:
        _place(_s, _sec["name"], "", "intro", _seq); _seq += 1
    LEGACY.update(_sec["legacy"])
    for _i, (_cname, _chapters, _briefs) in enumerate(_sec["clusters"], 1):
        _r = _roman(_sec["name"])
        _label = (f"{_r}.{_i} {_cname}" if _r else _cname)
        for _s in _chapters:
            _place(_s, _sec["name"], _label, "chapter", _seq); _seq += 1
        for _s in _briefs:
            _place(_s, _sec["name"], _label, "brief", _seq); _seq += 1


def yaml_type_expected(role):
    """The YAML `type:` each role should carry: intros and data-type chapters
    are `chapter`; briefs are `brief`."""
    return "brief" if role == "brief" else "chapter"


# ------------------------------------------------------------------ tagging
#
# Tags name the DATASET a doc reads, so a reader can ask "what else uses
# ANES?" and get an answer at a glance. They are matched against the doc's
# stated source, its title and its topic -- never hand-assigned -- so a doc
# that changes its source changes its tags when this is re-run.
#
# Order matters only for readability; a doc may carry several.
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
    # doc whose build script mentioned a physics simulation for a graph
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
    # so it does not sweep in every doc that merely cites a monograph.
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
    # A doc that reads a sibling is tagged for that alone only if nothing
    # else matched -- otherwise the real dataset tags are more informative.
    if len(out) > 1 and "Derived in-book" in out:
        out.remove("Derived in-book")
    return out

# ---------------------------------------------------------------- traversal
#
# Docs live at labs/NN-directory/<slug>/. The directory is NOT part of the
# structure above: a slug's folder is discovered here, and a slug must appear
# in exactly one directory. A doc left at the labs root is still picked up.
def chapter_dirs(labs):
    out = []
    for d in sorted(os.listdir(labs)):
        p = os.path.join(labs, d)
        if not os.path.isdir(p) or d.startswith((".", "_")):
            continue
        if re.match(r"^\d\d-", d):                 # a section/part directory
            for c in sorted(os.listdir(p)):
                q = os.path.join(p, c)
                if os.path.isdir(q) and not c.startswith((".", "_")):
                    out.append((c, q))
        else:
            out.append((d, p))
    return out

# ------------------------------------------------------------------ assemble

def build_rows():
    rows, seen, mismatches = [], {}, []
    for name, folder in chapter_dirs(LABS):
        if name in SKIP:
            continue
        briefs = [f for f in os.listdir(folder) if f.endswith("-brief.Rmd")]
        if not briefs:
            continue
        if name in seen:
            raise SystemExit(
                f"make-index: `{name}` exists in two directories:\n"
                f"  {seen[name]}\n  {folder}\n"
                "A slug must live in exactly one labs/NN-*/ directory.")
        seen[name] = folder
        with open(os.path.join(folder, briefs[0]), encoding="utf-8", errors="replace") as fh:
            head = fh.read(4000)

        brief = briefs[0]
        stem  = brief[:-4]                      # <name>-brief
        with open(os.path.join(folder, brief), encoding="utf-8",
                  errors="replace") as fh:
            fulltext = fh.read()
        # Link from the folder's REAL location, not from the slug: docs sit at
        # labs/<NN-dir>/<slug>/, and the index lives at the labs root.
        rel_dir   = os.path.relpath(folder, LABS).replace(os.sep, "/")
        href_html = f"{rel_dir}/{stem}.html"
        href_pdf  = f"{rel_dir}/{stem}.pdf"
        has_html = os.path.exists(os.path.join(folder, stem + ".html"))
        has_pdf  = os.path.exists(os.path.join(folder, stem + ".pdf"))

        src, script = source_of(folder)
        body = script_text(folder)

        # When the doc's own sources last changed — the same convention as the
        # "Last updated" stamp each page computes for itself: the max mtime of
        # the .Rmd/.R files in the doc folder (not data/, not renders).
        stamps = [os.path.getmtime(os.path.join(folder, f))
                  for f in os.listdir(folder) if f.endswith((".Rmd", ".R"))]
        updated = (date.fromtimestamp(max(stamps)).isoformat() if stamps else "")

        # Has this doc been rewritten to the 3rd-edition template? The mtime
        # cannot answer that (mechanical passes touch every file), so the shape
        # of the doc does: a brief ends learned > Extensions > Sources, a
        # chapter carries the learned section and ends on Sources, an intro has
        # shed its "Part N" title. Same tests as check-layout.py --template.
        h2 = re.findall(r"^## (.+?)\s*$", fulltext, re.M)
        role0 = ROLE_OF.get(name, "brief")
        if role0 == "intro":
            e3 = not yaml_field(head, "title").startswith("Part ")
        elif role0 == "chapter":
            e3 = ("What you should have learned" in h2
                  and bool(h2) and h2[-1] == "Sources")
        else:
            e3 = h2[-3:] == ["What you should have learned", "Extensions",
                             "Sources"]

        role = ROLE_OF.get(name)
        ytype = yaml_field(head, "type")
        if role and ytype != yaml_type_expected(role):
            mismatches.append((name, ytype or "(absent)", yaml_type_expected(role)))

        rows.append(dict(
            href_html=href_html if has_html else "",
            href_pdf=href_pdf if has_pdf else "",
            section=SECTION_OF.get(name, ""),
            cluster=CLUSTER_OF.get(name, ""),
            role=role or "",
            legacy=name in LEGACY,
            slug=name,
            title=yaml_field(head, "title"),
            topic=yaml_field(head, "subtitle"),
            source=src,
            body=body,
            url=first_url(src),
            script=script,
            has_data=os.path.isdir(os.path.join(folder, "data")),
            has_raw=os.path.isdir(os.path.join(folder, "data", "raw")),
            updated=updated,
            e3=e3,
        ))

    # The structure and the filesystem must agree exactly, both directions.
    on_disk, in_toc = set(seen), set(SEQ)
    orphans = sorted(on_disk - in_toc)     # on disk, not placed in SECTIONS
    missing = sorted(in_toc - on_disk)     # placed in SECTIONS, not on disk
    if orphans or missing:
        msg = ["make-index: SECTIONS and the filesystem disagree."]
        if orphans:
            msg.append("  on disk but not in SECTIONS (place them): "
                       + " ".join(orphans))
        if missing:
            msg.append("  in SECTIONS but not on disk (typo, or not yet moved?): "
                       + " ".join(missing))
        raise SystemExit("\n".join(msg))

    for r in rows:
        r["tags"] = tags_for(r)
    rows.sort(key=lambda r: SEQ[r["slug"]])
    return rows, mismatches

# --------------------------------------------------------------- markdown

def cluster_cell(r):
    if r["role"] == "intro":
        return r["section"] + (" — legacy part opener" if r["legacy"] else "")
    return r["cluster"]      # cluster labels already carry the section numeral


def write_markdown(rows, n_src, n_raw, today):
    n = len(rows)
    md = [f"# The book, in reading order\n",
          f"*{n} docs — four sections, each a run of clusters: a data-type "
          f"chapter and the briefs that use that data. Generated from the doc "
          f"files on {today} by `_lib/make-index.py` — do not edit by hand; "
          f"re-run it instead.*\n",
          "| Section / Cluster | Type | Doc | Title | Updated | Tags | Topic | Data source |",
          "|---|---|---|---|---|---|---|---|"]
    prev_sec = None
    for r in rows:
        if r["section"] != prev_sec:
            prev_sec = r["section"]
        src = r["source"] or "—"
        if len(src) > 180:
            src = src[:177] + "…"
        md.append("| {} | {} | `{}` | {} | {} | {} | {} | {} |".format(
            cluster_cell(r).replace("|", "\\|"),
            r["role"],
            r["slug"],
            ("[{}]({})".format(r["title"].replace("|", "\\|"), r["href_html"])
             if r["href_html"] else r["title"].replace("|", "\\|")),
            (r["updated"] or "—") + (" ✓" if r["e3"] else ""),
            ", ".join(r["tags"]) or "—",
            r["topic"].replace("|", "\\|"),
            src.replace("|", "\\|")))
    md.append("")
    md.append(f"{n_src} of {n} docs state a source in their build script; "
              f"{n_raw} commit a `data/raw/` capture of the source as it arrives.")
    with open(os.path.join(LABS, "INDEX.md"), "w", encoding="utf-8") as fh:
        fh.write("\n".join(md) + "\n")

# ------------------------------------------------------------------- html

def esc(s):
    return html.escape(s or "", quote=True)


def doc_tr(r):
    # Source is the longest field by far and it is reference detail, not
    # something anyone scans. It is truncated here and carried in full in the
    # title attribute, so hovering gives the whole citation without the column
    # dictating the table's width.
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
    legacy = ('<span class="legacy" title="retired part opener, kept findable '
              'until its material is folded into the section intro">legacy part opener</span>'
              if r["legacy"] else "")

    return (
        f'<tr class="doc r-{esc(r["role"])}" data-type="{esc(r["role"])}" '
        f'data-section="{esc(r["section"])}">'
        f'<td class="role"><span class="badge b-{esc(r["role"])}">{esc(r["role"])}</span></td>'
        f'<td class="ch">'
        f'<div class="ttl">{title}{pdf}</div>'
        f'<div class="slug"><code>{esc(r["slug"])}</code>'
        + ('<span class="raw" title="commits a data/raw capture of the source as it arrives">raw</span>'
           if r["has_raw"] else '') + legacy + '</div>'
        f'<div class="topic">{esc(r["topic"])}</div>'
        f'</td>'
        f'<td class="tags">{tags}</td>'
        f'<td class="upd">{esc(r["updated"]) or "&mdash;"}'
        + ('<span class="e3" title="rewritten to the 3rd-edition template">3e</span>'
           if r["e3"] else '') + '</td>'
        f'<td class="src">{srccell}</td>'
        f'</tr>')


def html_body_rows(rows):
    """Section header rows, cluster subheading rows, and doc rows, in reading
    order. Grouping rows carry class `ghdr` so the page can hide them while a
    column sort or a filter empties them."""
    by_slug = {r["slug"]: r for r in rows}
    out = []
    for sec in SECTIONS:
        out.append(f'<tr class="ghdr sec" data-section="{esc(sec["name"])}">'
                   f'<td colspan="5">{esc(sec["name"])}</td></tr>')
        for s in sec["intros"] + sec["legacy"]:
            out.append(doc_tr(by_slug[s]))
        for i, (cname, chapters, briefs) in enumerate(sec["clusters"], 1):
            rom = _roman(sec["name"])
            label = f"{rom}.{i} {cname}" if rom else cname
            out.append(f'<tr class="ghdr cluster" data-section="{esc(sec["name"])}">'
                       f'<td colspan="5">{esc(label)}</td></tr>')
            for s in chapters + briefs:
                out.append(doc_tr(by_slug[s]))
    return out


def write_html(rows, n_src, n_raw, today):
    n = len(rows)
    trs = html_body_rows(rows)
    page = f"""<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Democracy's Data — chapters and briefs</title>
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
tbody tr.doc:hover{{background:var(--sunk)}}
code{{font-family:"SF Mono",ui-monospace,Menlo,monospace;font-size:11.5px;color:var(--acc)}}
a.pdf{{font-family:"SF Mono",ui-monospace,Menlo,monospace;font-size:9px;letter-spacing:.06em;
color:var(--ink3);border:1px solid var(--rule);border-radius:3px;padding:0 4px;margin-left:6px;
text-decoration:none;vertical-align:2px}}
a.pdf:hover{{color:var(--acc);border-color:var(--acc)}}
td.src{{color:var(--ink2);font-size:11.5px;line-height:1.45;padding-top:12px;max-width:44ch}}
td.upd{{color:var(--ink2);font-size:11.5px;white-space:nowrap;padding-top:12px;font-variant-numeric:tabular-nums}}
.e3{{display:inline-block;margin-left:6px;padding:1px 5px;border-radius:8px;font-size:10px;font-weight:600;background:color-mix(in srgb,var(--accent) 14%,transparent);color:var(--accent)}}
/* four columns, and only one of them is prose */
table{{table-layout:fixed}}
th:nth-child(1),td:nth-child(1){{width:6rem}}
th:nth-child(3),td:nth-child(3){{width:13rem}}
th:nth-child(4),td:nth-child(4){{width:20rem}}
/* section and cluster grouping rows */
tr.sec td{{font-family:"Iowan Old Style",Charter,Palatino,Georgia,serif;font-size:17px;
font-weight:600;letter-spacing:-.005em;background:var(--sunk);color:var(--ink);
border-bottom:1px solid var(--rule);padding-top:14px;padding-bottom:10px}}
tr.cluster td{{font-size:10.5px;letter-spacing:.09em;text-transform:uppercase;
color:var(--ink2);background:color-mix(in srgb,var(--sunk) 45%,transparent);
padding-top:10px;padding-bottom:7px}}
/* role badge, and a tint that lifts the chapters out of the run of briefs */
td.role{{padding-top:12px}}
span.badge{{font-family:"SF Mono",ui-monospace,Menlo,monospace;font-size:9px;
letter-spacing:.08em;text-transform:uppercase;border:1px solid var(--rule);
border-radius:3px;padding:1px 5px;color:var(--ink3);white-space:nowrap}}
span.badge.b-chapter{{color:var(--acc);border-color:var(--acc);font-weight:700}}
span.badge.b-intro{{color:var(--ink2);border-style:dashed}}
tr.r-chapter{{background:color-mix(in srgb,var(--acc) 5%,transparent)}}
tr.r-intro{{background:color-mix(in srgb,var(--ink3) 6%,transparent)}}
td.ch{{padding-top:9px;padding-bottom:11px}}
td.ch .ttl{{font-family:"Iowan Old Style",Charter,Palatino,Georgia,serif;font-size:15px;
line-height:1.3;margin-bottom:2px}}
tr.r-chapter td.ch .ttl,tr.r-intro td.ch .ttl{{font-weight:600}}
td.ch .ttl a{{color:var(--ink);text-decoration:none;border-bottom:1px solid var(--rule)}}
td.ch .ttl a:hover{{color:var(--acc);border-bottom-color:var(--acc)}}
td.ch .slug{{margin:3px 0 4px}}
td.ch .slug code{{font-size:11px;color:var(--ink3)}}
td.ch .topic{{font-size:12.5px;line-height:1.4;color:var(--ink2)}}
span.raw,span.legacy{{font-family:"SF Mono",ui-monospace,Menlo,monospace;font-size:9px;
letter-spacing:.08em;text-transform:uppercase;color:var(--ink3);border:1px solid var(--rule);
border-radius:2px;padding:0 4px;margin-left:6px;vertical-align:1px}}
span.legacy{{border-style:dashed}}
td.tags{{padding-top:11px}}
button.tag{{font:inherit;font-size:10.5px;line-height:1.5;margin:1px 3px 1px 0;padding:1px 7px;
border:1px solid var(--rule);border-radius:10px;background:var(--sunk);color:var(--ink2);
cursor:pointer;white-space:nowrap}}
button.tag:hover{{border-color:var(--acc);color:var(--acc)}}
button.tag.on{{background:var(--acc);border-color:var(--acc);color:var(--panel);font-weight:600}}
.chiprow{{display:flex;flex-wrap:wrap;gap:4px;margin:0 0 8px;align-items:center}}
.chiprow .lbl{{font-size:10.5px;letter-spacing:.09em;text-transform:uppercase;
color:var(--ink3);margin-right:4px}}
.chiprow button{{font:inherit;font-size:11px;padding:3px 9px;border:1px solid var(--rule);
border-radius:11px;background:var(--panel);color:var(--ink2);cursor:pointer}}
.chiprow button:hover{{border-color:var(--acc);color:var(--acc)}}
.chiprow button.on{{background:var(--acc);border-color:var(--acc);color:var(--panel);font-weight:600}}
a{{color:var(--acc);word-break:break-all}}
tr.hide{{display:none}}
table.sorted tr.ghdr{{display:none}}
footer{{margin-top:26px;font-size:12.5px;color:var(--ink3);max-width:80ch}}
</style></head><body><div class="wrap">
<h1>Democracy&rsquo;s Data &mdash; chapters and briefs</h1>
<p class="sub">{n} docs in four sections. Each section opens with an intro and
runs in clusters: a <strong>chapter</strong> about a kind of data, then the
<strong>briefs</strong> that use it. Generated from the doc files on {today} by
<code>_lib/make-index.py</code> — re-run it after adding a doc rather than
editing this page. Click a column heading to sort (click a third time to
restore reading order); type to filter. {n_src} of {n} docs state a source in
their build script, and {n_raw} commit a <code>data/raw/</code> capture of that
source exactly as it arrives.</p>
<div class="chiprow" id="sections"><span class="lbl">Section</span></div>
<div class="chiprow" id="types"><span class="lbl">Type</span></div>
<div class="chiprow" id="chips"><span class="lbl">Data</span></div>
<div class="bar">
  <input type="search" id="q" placeholder="Filter by doc, title, topic or source…" autocomplete="off">
  <span class="count" id="count"></span>
</div>
<div class="tablewrap"><table id="t">
<thead><tr><th>Type</th><th>Doc</th><th>Tags</th><th>Updated</th><th>Data source</th></tr></thead>
<tbody>
{chr(10).join(trs)}
</tbody></table></div>
<footer>Sorting and filtering run in the page; nothing is fetched. An empty
source cell means the doc&rsquo;s build script does not state one in a form
this generator recognises &mdash; usually because it reads another
doc&rsquo;s output &mdash; not that the doc has no source.</footer>
</div>
<script>
(function(){{
  var t=document.getElementById('t'), tb=t.tBodies[0],
      all=[].slice.call(tb.rows),
      docs=all.filter(function(r){{return r.classList.contains('doc');}}),
      q=document.getElementById('q'), c=document.getElementById('count'),
      state={{}}, sorted=false;
  function shown(){{ return docs.filter(function(r){{return !r.classList.contains('hide');}}).length; }}
  function tally(){{ c.textContent = shown()+' of '+docs.length+' docs'; }}

  // ---- column sorting (third click restores reading order) -----------
  function unsort(){{
    sorted=false; t.classList.remove('sorted');
    all.forEach(function(r){{tb.appendChild(r);}});
  }}
  [].forEach.call(t.tHead.rows[0].cells, function(th,i){{
    th.addEventListener('click', function(){{
      var next = state[i]==='asc' ? 'desc' : state[i]==='desc' ? null : 'asc';
      state={{}};
      [].forEach.call(t.tHead.rows[0].cells,function(o){{o.className='';}});
      if(!next){{ unsort(); apply(); return; }}
      state[i]=next; th.className=next;
      sorted=true; t.classList.add('sorted');
      var d = next==='asc';
      docs.slice().sort(function(a,b){{
        var x=a.cells[i].textContent.trim().toLowerCase(),
            y=b.cells[i].textContent.trim().toLowerCase();
        if(x===y) return 0;
        if(x==='') return 1;
        if(y==='') return -1;
        return (x<y? -1:1) * (d?1:-1);
      }}).forEach(function(r){{tb.appendChild(r);}});
      apply();
    }});
  }});

  // ---- filtering: search text, one tag, one type, one section --------
  var activeTag=null, activeType=null, activeSection=null;
  function tagsOf(r){{
    return [].slice.call(r.querySelectorAll('button.tag'))
             .map(function(b){{return b.dataset.tag;}});
  }}
  function groupvis(){{
    // hide a cluster heading whose docs are all hidden, and a section
    // heading whose clusters and intros are all hidden
    if(sorted) return;
    var rows=[].slice.call(tb.rows), lastSec=null, lastClu=null,
        secHas=false, cluHas=false;
    function closeClu(){{ if(lastClu) lastClu.classList.toggle('hide', !cluHas); }}
    function closeSec(){{ if(lastSec) lastSec.classList.toggle('hide', !secHas); }}
    rows.forEach(function(r){{
      if(r.classList.contains('sec')){{
        closeClu(); closeSec(); lastSec=r; lastClu=null; secHas=false; cluHas=false;
      }} else if(r.classList.contains('cluster')){{
        closeClu(); lastClu=r; cluHas=false;
      }} else if(!r.classList.contains('hide')){{
        secHas=true; cluHas=true;
      }}
    }});
    closeClu(); closeSec();
  }}
  function apply(){{
    var v=q.value.toLowerCase();
    docs.forEach(function(r){{
      var okText = !v || r.textContent.toLowerCase().indexOf(v)>=0;
      var okTag  = !activeTag || tagsOf(r).indexOf(activeTag)>=0;
      var okType = !activeType || r.dataset.type===activeType;
      var okSec  = !activeSection || r.dataset.section===activeSection;
      r.classList.toggle('hide', !(okText && okTag && okType && okSec));
    }});
    [].forEach.call(document.querySelectorAll('button.tag'), function(b){{
      b.classList.toggle('on', activeTag && b.dataset.tag===activeTag);
    }});
    [].forEach.call(document.querySelectorAll('#chips button'), function(b){{
      b.classList.toggle('on', b.dataset.tag===activeTag);
    }});
    [].forEach.call(document.querySelectorAll('#types button'), function(b){{
      b.classList.toggle('on', b.dataset.type===activeType);
    }});
    [].forEach.call(document.querySelectorAll('#sections button'), function(b){{
      b.classList.toggle('on', b.dataset.section===activeSection);
    }});
    groupvis(); tally();
  }}
  function pickTag(x){{ activeTag = (activeTag===x ? null : x); apply(); }}
  function pickType(x){{ activeType = (activeType===x ? null : x); apply(); }}
  function pickSection(x){{ activeSection = (activeSection===x ? null : x); apply(); }}

  function chip(bar, text, dataKey, dataVal, fn){{
    var b=document.createElement('button');
    b.textContent=text; b.dataset[dataKey]=dataVal;
    b.addEventListener('click', function(){{ fn(dataVal); }});
    document.getElementById(bar).appendChild(b);
  }}
  // section chips, in reading order
  var secs=[], secN={{}};
  docs.forEach(function(r){{
    var s=r.dataset.section;
    if(!(s in secN)){{ secs.push(s); secN[s]=0; }}
    secN[s]++;
  }});
  secs.forEach(function(s){{ chip('sections', s+' ('+secN[s]+')', 'section', s, pickSection); }});
  // type chips
  var types=['intro','chapter','brief'], typeN={{}};
  docs.forEach(function(r){{ typeN[r.dataset.type]=(typeN[r.dataset.type]||0)+1; }});
  types.forEach(function(x){{
    if(typeN[x]) chip('types', x+' ('+typeN[x]+')', 'type', x, pickType);
  }});
  // dataset tag chips, by frequency
  var tagN={{}};
  docs.forEach(function(r){{ tagsOf(r).forEach(function(x){{ tagN[x]=(tagN[x]||0)+1; }}); }});
  Object.keys(tagN).sort(function(a,b){{ return tagN[b]-tagN[a] || a.localeCompare(b); }})
    .forEach(function(x){{ chip('chips', x+' ('+tagN[x]+')', 'tag', x, pickTag); }});

  document.addEventListener('click', function(e){{
    if(e.target.classList && e.target.classList.contains('tag')) pickTag(e.target.dataset.tag);
  }});
  q.addEventListener('input', apply);
  tally();
}})();
</script></body></html>
"""
    with open(os.path.join(LABS, "index.html"), "w", encoding="utf-8") as fh:
        fh.write(page)

# --------------------------------------------------------------------- main

def main():
    rows, mismatches = build_rows()
    n = len(rows)
    n_src = sum(1 for r in rows if r["source"])
    n_raw = sum(1 for r in rows if r["has_raw"])
    today = date.today().strftime("%-d %B %Y")

    write_markdown(rows, n_src, n_raw, today)
    write_html(rows, n_src, n_raw, today)

    print(f"{n} docs -> INDEX.md and index.html")
    print(f"  source stated in build script : {n_src}")
    print(f"  raw capture committed         : {n_raw}")
    missing = [r["slug"] for r in rows if not r["source"]]
    if missing:
        print(f"  no source resolved ({len(missing)}): {' '.join(missing)}")
    if mismatches:
        print(f"  WARNING: YAML `type:` disagrees with SECTIONS role "
              f"({len(mismatches)}); SECTIONS is authoritative for placement:")
        for slug, got, want in mismatches:
            print(f"    {slug}: YAML has {got}, role says {want}")


if __name__ == "__main__":
    main()
