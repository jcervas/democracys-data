#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# Check every chapter against the layout in labs/DATA-LAYOUT.md.
#
#     python3 _lib/check-layout.py            # from labs/, or from anywhere
#
# Five questions, asked of every chapter:
#
#   0. Does the chapter have a data/ at all, and is anything loose at the
#      chapter root?  See THE BLIND SPOT below — these two are why the other
#      three could pass on a chapter they had never looked inside.
#   1. Is anything still loose in data/?  Every file there is either the build
#      script, a provenance record, or misfiled — raw/ and derived/ are the
#      only two places data lives.
#   2. Does every path resolve?  A brief that reads data/derived/facts.csv,
#      a build script that reads ../01-census-bureau/surnames/data/derived/census_surnames.csv
#      — both must point at a file that exists.
#   3. Does any chapter write into another chapter's folder?
#
# Two further checks are ADVISORY and never touch the exit status (until
# DD_STRICT_TEMPLATE=1): does each document end the way the 3rd-edition
# template says its type ends, and does its title name the data rather than
# withhold a punchline. See "--template" and STYLE.md Part Three.
#
# The build stamps are NOT checked here. check-stamps.py owns all of that --
# whether a BUILD-STAMP.tsv exists, whether it names the right files, and
# whether those files still hash to what it recorded. It asks the same kind of
# question one level down, comparing contents rather than names, and keeping it
# in one place means a single fault is not reported twice by two scripts.
#
# THE BLIND SPOT THIS SCRIPT USED TO HAVE, and it is worth stating because it is
# the failure mode every corpus checker is prone to. Checks 1-3 all iterate over
# chapters(), which SKIPS any chapter with no data/ directory. So a chapter that
# filed nothing correctly was not a chapter with findings — it was a chapter the
# checker never opened, and it reported clean. precinct-geography sat that way
# for months: seven derived CSVs and a build script loose at the chapter root,
# no data/, invisible, while its build had stopped working entirely.
#
# A checker that skips what it cannot parse is worse than one that fails on it.
# So the traversal now starts from what makes a chapter a chapter -- a
# <slug>-brief.Rmd -- and every chapter is accounted for either as checked or as
# a named, deliberate exception.
#
# WHAT THIS DELIBERATELY DOES NOT FLAG. A build script naming a file that is
# not on disk is usually correct: most chapters fetch at run time into a
# temporary directory, and several read a large source the repository does not
# redistribute (the ANES cumulative file). Only paths that name a chapter's
# data/ tree are checked for existence, because only those are promises about
# this repository.
# ---------------------------------------------------------------------------
import os, re, sys

HERE = os.path.dirname(os.path.abspath(__file__))
LABS = os.path.dirname(HERE)

# Two ways to run half of this script, both for check-all.sh's benefit:
#
#   --gate-only   only the gating layout checks. check-all.sh's "layout"
#                 section uses this, so the advisory output below is not
#                 printed twice in one run.
#   --template    only the ADVISORY 3rd-edition checks (template tail and
#                 title policy). check-all.sh runs this next to the other
#                 advisory checks (language, captions).
#
# A bare run prints both, which is what a person at the keyboard wants.
# DD_STRICT_TEMPLATE=1 turns the advisory checks into gates; until the
# rewrite completes they never touch the exit status, because render-brief.R
# runs check-all.sh before every render and a new gate the whole corpus
# fails would stop every render in the book.
GATE_ONLY = "--gate-only" in sys.argv[1:]
TEMPLATE_ONLY = "--template" in sys.argv[1:]
STRICT_TEMPLATE = os.environ.get("DD_STRICT_TEMPLATE") == "1"

# A script may sit at the top of data/ whatever it is written in. .mjs is here
# because follower-counts collects through a browser, which R and Python cannot
# do without a heavier dependency than the chapter is worth.
SCRIPT_EXT = {".R", ".r", ".py", ".mjs", ".js"}
# things that legitimately sit at the top of data/
TOP_OK = {"PROVENANCE.tsv", "provenance.csv", "provenance.csv.lock",
          "BUILD-STAMP.tsv",
          "README.md", "SPEC.md", "ROUTES.md",
          "ai-prompt.txt"}

# --- what may sit at the CHAPTER root, beside data/ -------------------------
#
# A short list on purpose. The chapter root holds the chapter and what the
# renderer wrote; everything else the chapter owns lives under data/. Anything
# not named here is either misfiled data (the case this check exists for) or
# leftovers -- an editor's .bak, a stale render under an old name, the
# Rplots.pdf a base-R device drops when a script plots without a file open.
CHAP_DIRS_OK  = {"data", "img"}          # img/ holds figures the brief includes
CHAP_FILES_OK = {"rerender.log", "README.md"}
# A chapter may keep its chunk bodies beside the brief as <slug>-code.R,
# read back in by knitr::read_chunk(). The brief carries prose, chunk
# labels and options; the .R file carries the code. Checked per chapter
# below, so only the matching name is allowed -- not any loose .R file.

# Chapters that legitimately have no data/ at all. This is an ALLOWLIST, not a
# skip: a chapter absent from both this set and the checked set is a bug in the
# traversal, and the run says so rather than quietly covering fewer chapters
# than it claims.
NO_DATA_OK = {
    "introduction",   # front matter: an argument about the book, computes nothing
}

# any literal naming a chapter's data tree: <chapter>/data/[raw|derived/]<file>
REF = re.compile(r"""([\w-]+)/data/((?:derived|raw)/)?([\w .,%-]+\.[A-Za-z0-9]{2,8})""")
# the same thing assembled from components, which the literal above cannot see:
#     file.path(LAB, "redlining", "data", "tiger", "26", "tl_2020_26_tract.shp")
# This form survived the raw//derived/ migration untouched in three places and
# was only caught by running the build. Checked as a shape, not for existence:
# the next component after "data" has to be raw or derived.
COMPONENTS = re.compile(
    r"""(?:file\.path|os\.path\.join)\([^()]*?["']([\w-]+)["']\s*,\s*["']data["']\s*,\s*["'](\w+)["']""")
# a brief reading its own chapter's data by relative path
OWN = re.compile(r"""["'`]data/((?:derived|raw)/)?([^"'`\n]+?)["'`]""")
# a MARKDOWN LINK in a brief pointing at its own chapter's data:
#     [`flows.csv`](data/derived/flows.csv)
# The "## Sources" convention hands students the files a chapter is built from,
# and a link target is not quoted, so OWN above cannot see it. Without this rule
# a build that renames a derived table leaves 109 briefs offering a download
# that 404s, and every other check still passes.
MDOWN = re.compile(r"""\]\(data/((?:derived|raw)/)?([^)\s]+)\)""")

problems = []
notes = []      # raw files absent on purpose: fetched at run time, or not ours



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


# A chapter is a directory holding <slug>-brief.Rmd. That is the corpus's own
# definition -- it is what the part openers use to build their chapter list --
# and using it here means the two traversals cannot disagree about what exists.
def all_chapters(labs):
    return [(slug, p) for slug, p in chapter_dirs(labs)
            if os.path.isfile(os.path.join(p, slug + "-brief.Rmd"))]


def chapters():
    for lab, labdir in all_chapters(LABS):
        d = os.path.join(labdir, "data")
        if not os.path.isdir(d):
            continue
        yield lab, d


def sources(lab, d):
    """(path, working directory) for every brief and build script.

    `d` is the chapter's data/ dir, so its parent is the chapter dir -- derived
    from the path rather than rebuilt from LABS + slug, which stopped working
    when chapters moved under their part directories."""
    labdir = os.path.dirname(d)
    for f in sorted(os.listdir(labdir)):
        if f.endswith(".Rmd") or os.path.splitext(f)[1] in SCRIPT_EXT:
            yield os.path.join(labdir, f), labdir
    for f in sorted(os.listdir(d)):
        if os.path.splitext(f)[1] in SCRIPT_EXT:
            yield os.path.join(d, f), d


# --- 0. every chapter accounted for, and nothing loose at its root -----------
#
# Run FIRST, because it is the check that decides whether the rest of this
# script is looking at the whole corpus or only at the part of it that already
# files things correctly.
ALL     = all_chapters(LABS)
CHECKED = {lab for lab, _ in chapters()}

for slug, p in ALL:
    if slug not in CHECKED and slug not in NO_DATA_OK:
        # What is it keeping instead? Naming the files makes the finding
        # actionable rather than a bare "no data/".
        stray = sorted(f for f in os.listdir(p)
                       if not f.startswith(".")
                       and not f.startswith(slug + "-brief.")
                       and f not in CHAP_FILES_OK)
        problems.append(("no data/", slug,
                         "chapter has no data/ — every other check skips it"
                         + (f"; holding {', '.join(stray[:4])}"
                            + (" …" if len(stray) > 4 else "") if stray else "")))

for slug in sorted(NO_DATA_OK - {s for s, _ in ALL}):
    problems.append(("stale exception", slug,
                     "listed in NO_DATA_OK but is not a chapter — remove it"))

for slug, p in ALL:
    for f in sorted(os.listdir(p)):
        if f.startswith(".") or f in CHAP_FILES_OK:
            continue
        if os.path.isdir(os.path.join(p, f)):
            if f not in CHAP_DIRS_OK:
                problems.append(("loose at root", f"{slug}/{f}/",
                                 "only data/ and img/ live beside the chapter"))
            continue
        if f.startswith(slug + "-brief.") and os.path.splitext(f)[1] in (
                ".Rmd", ".html", ".pdf"):
            continue
        if f == slug + "-code.R":
            continue
        problems.append(("loose at root", f"{slug}/{f}",
                         "not the brief, not a render — data belongs in data/"))

# --- 1a. stray directories --------------------------------------------------
# A build that unzips into data/<something> puts hundreds of MB in a place the
# convention has no name for. areal-units did exactly that -- data/pl, 342 MB,
# neither raw nor derived -- and it survived every run of this script, because
# the loose-file check below reads files and not directories.
for lab, d in chapters():
    for f in sorted(os.listdir(d)):
        if not os.path.isdir(os.path.join(d, f)) or f.startswith("."):
            continue
        if f not in ("raw", "derived"):
            problems.append(("stray directory", f"{lab}/data/{f}/",
                             "only raw/ and derived/ live under data/"))

# --- 1. loose files ---------------------------------------------------------
for lab, d in chapters():
    for f in sorted(os.listdir(d)):
        if f.startswith(".") or os.path.isdir(os.path.join(d, f)):
            continue
        if f in TOP_OK or os.path.splitext(f)[1] in SCRIPT_EXT:
            continue
        problems.append(("loose file", f"{lab}/data/{f}",
                         "belongs in raw/ or derived/"))

# --- 2. paths that do not resolve -------------------------------------------
for lab, d in chapters():
    for path, wd in sources(lab, d):
        rel = os.path.relpath(path, LABS)
        try:
            text = open(path, encoding="utf-8").read()
        except (UnicodeDecodeError, OSError):
            continue

        for other, sub, name in REF.findall(text):
            if not os.path.isdir(os.path.join(LABS, other)):
                continue
            if os.path.splitext(name)[1] in SCRIPT_EXT or "," in name:
                continue
            if name in TOP_OK:          # provenance records stay at the top
                continue
            if not sub:
                problems.append(("no subfolder", rel,
                                 f"{other}/data/{name} — say raw/ or derived/"))
            elif not os.path.exists(os.path.join(LABS, other, "data", sub, name)):
                if sub == "raw/":       # fetched at run time, or not ours to ship
                    notes.append((rel, f"{other}/data/{sub}{name}"))
                else:
                    problems.append(("missing", rel, f"{other}/data/{sub}{name}"))

        for other, nxt in COMPONENTS.findall(text):
            if not os.path.isdir(os.path.join(LABS, other)):
                continue
            if nxt not in ("raw", "derived"):
                problems.append(("no subfolder", rel,
                                 f'file.path(… "{other}", "data", "{nxt}" …)'
                                 " — say raw or derived"))

        # The brief and its <slug>-code.R are the two files that run with the
        # CHAPTER as their working directory, so "data/derived/x.csv" means the
        # same thing in both. Build scripts under data/ run with data/ as their
        # wd and say "derived/x.csv" instead, which is why they stay out of this
        # branch. When the chunk bodies moved into <slug>-code.R, every one of
        # those paths moved with them -- and this rule, keyed on .Rmd alone,
        # silently stopped reporting a missing input for the whole corpus.
        if path.endswith(".Rmd") or os.path.basename(path) == lab + "-code.R":
            for sub, name in MDOWN.findall(text):
                if not sub:
                    problems.append(("no subfolder", rel,
                                     f"[…](data/{name}) — say raw/ or derived/"))
                elif not os.path.exists(os.path.join(d, sub, name)):
                    problems.append(("dead link", rel, f"[…](data/{sub}{name})"))
            for sub, name in OWN.findall(text):
                if "/" in name or not re.search(r"\.[A-Za-z0-9]{2,8}$", name):
                    continue
                if name in TOP_OK or os.path.splitext(name)[1] in SCRIPT_EXT:
                    continue
                if not sub:
                    problems.append(("no subfolder", rel,
                                     f"data/{name} — say raw/ or derived/"))
                elif not os.path.exists(os.path.join(d, sub, name)):
                    if sub == "raw/":
                        notes.append((rel, f"data/{sub}{name}"))
                    else:
                        problems.append(("missing", rel, f"data/{sub}{name}"))

# --- 3. writing into someone else's chapter ---------------------------------
WRITE = re.compile(r"""(write\.csv|write\.table|writeLines|saveRDS|to_csv|open)\("""
                   r"""[^)\n]*["'](\.\./[^"'\n]+)["']""")
for lab, d in chapters():
    for path, wd in sources(lab, d):
        try:
            text = open(path, encoding="utf-8").read()
        except (UnicodeDecodeError, OSError):
            continue
        for _, target in WRITE.findall(text):
            problems.append(("writes outside", os.path.relpath(path, LABS), target))

# --- 4. the 3rd-edition template — ADVISORY -----------------------------------
#
# STYLE.md Part Three (third edition, 31 Aug 2026): every document is a
# `type: chapter` (a reading about a kind of data) or a `type: brief` (a lab),
# declared in its YAML front matter, absent meaning brief. A brief's H2s end
# "What you should have learned" > "Extensions" > "Sources"; a chapter carries
# "What you should have learned" somewhere and ends on "Sources".
#
# ADVISORY, NOT A GATE, like check-captions.py and for the same reason: the
# corpus is being rewritten toward this template, and gating on it today would
# stop every render. It reports the backlog and a coverage count instead.
# DD_STRICT_TEMPLATE=1 makes both this check and the title check below gate,
# for when the rewrite is done.
#
# Structural chapters — the part openers, the introduction, and
# admin-records-source (becoming Section III's intro) — are exempt, matching
# check-sources.py's STRUCTURAL set.
STRUCTURAL = re.compile(r"^(part-.*|introduction|admin-records-source)$")
BRIEF_TAIL = ("What you should have learned", "Extensions", "Sources")


def front_matter(text):
    """The YAML front matter as a flat dict, scalars only. A real YAML parser
    is not a dependency this script wants for two keys."""
    m = re.match(r"---\s*\n(.*?)\n---\s*(\n|$)", text, re.S)
    out = {}
    if not m:
        return out
    for line in m.group(1).splitlines():
        km = re.match(r"^([A-Za-z_][\w-]*):\s*(.*)$", line)
        if not km:
            continue
        v = km.group(2).strip()
        if len(v) >= 2 and v[0] == v[-1] and v[0] in "\"'":
            v = v[1:-1]
        out[km.group(1)] = v
    return out


def h2_headings(text):
    """Every H2 heading in document order, fenced code blocks skipped —
    a chunk printing '## something' is output, not structure."""
    out, fence = [], False
    for line in text.splitlines():
        if line.lstrip().startswith("```"):
            fence = not fence
            continue
        if fence:
            continue
        m = re.match(r"^##\s+([^#].*?)\s*$", line)
        if m:
            out.append(re.sub(r"\s*\{[^}]*\}\s*$", "", m.group(1)))
    return out


tail_bad = []          # (slug, type, what its H2s actually end with)
tail_ok = tail_total = 0
title_bad = []         # (slug, title)

# Titles already read by a person and judged fine (STYLE.md rule 14). Same
# pattern as check-tables-reviewed.tsv: delete a line to see it again.
REVIEWED_TITLES = os.path.join(HERE, "check-titles-reviewed.tsv")
reviewed_titles = set()
if os.path.exists(REVIEWED_TITLES):
    for line in open(REVIEWED_TITLES, encoding="utf-8"):
        if line.startswith("#") or not line.strip():
            continue
        reviewed_titles.add(line.rstrip("\n").split("\t")[0])

for slug, p in ALL:
    try:
        text = open(os.path.join(p, slug + "-brief.Rmd"), encoding="utf-8").read()
    except OSError:
        continue
    fm = front_matter(text)

    # -- title policy (STYLE.md rule 14). The heuristic is deliberately dumb
    # and low-recall: a title with no digit, colon, comma or em-dash, running
    # five words or more, is the shape a withholding sentence takes
    # ("Everyone Guesses Forty" is spared only by its length; "Everybody
    # Moved, Almost Nobody Left" by its comma — the allowlist is the real
    # instrument, this is just what feeds it).
    title = fm.get("title", "")
    if (title and title not in reviewed_titles
            and not re.search(r"[0-9:,—]", title)
            and len(title.split()) >= 5):
        title_bad.append((slug, title))

    # -- template tail. Structural chapters are shaped like neither type.
    if STRUCTURAL.match(slug):
        continue
    typ = fm.get("type", "brief")
    hs = h2_headings(text)
    tail_total += 1
    if typ == "chapter":
        ok = "What you should have learned" in hs and bool(hs) and hs[-1] == "Sources"
    else:
        ok = tuple(hs[-3:]) == BRIEF_TAIL
    if ok:
        tail_ok += 1
    else:
        got = " > ".join(hs[-3:]) if hs else "(no H2 headings)"
        tail_bad.append((slug, typ, got))


def report_template():
    print("template tail (STYLE.md rule 13; advisory"
          + (", DD_STRICT_TEMPLATE=1 so it gates" if STRICT_TEMPLATE else "")
          + "):")
    for slug, typ, got in tail_bad:
        print("  %-28s (%s) ends: %s" % (slug, typ, got))
    print("%d of %d chapters follow the 3rd-edition template"
          % (tail_ok, tail_total))
    print("  (briefs end learned > Extensions > Sources; chapters carry "
          "learned and end on Sources)")
    print()
    print("title policy (STYLE.md rule 14; advisory):")
    if title_bad:
        for slug, title in title_bad:
            print("  %-28s %s" % (slug, title))
        print("%d title(s) look like withholding sentences — name the data "
              "and the question, or record them in\n"
              "_lib/check-titles-reviewed.tsv once a person has read them"
              % len(title_bad))
    else:
        print("  every title names its data, or has been reviewed "
              "(%d on the reviewed list)" % len(reviewed_titles))


# ------------------------------------------------------------------ reporting
if not TEMPLATE_ONLY:
    if notes:
        print("raw sources not committed (fetched at run time, or not redistributable):")
        for where, what in notes:
            print(f"  {where:52s}  {what}")
        print()

    if problems:
        w = max(len(k) for k, _, _ in problems)
        for kind, where, what in problems:
            print(f"{kind:{w}}  {where:52s}  {what}")
        print(f"\n{len(problems)} problem(s)")
    else:
        # Say what was covered, not just that it passed. "clean" on a number
        # smaller than the corpus is the shape the blind spot took.
        print(f"{len(ALL)} chapters, layout clean "
              f"({len(CHECKED)} with data/, "
              f"{len(ALL) - len(CHECKED)} exempt: {', '.join(sorted(NO_DATA_OK))})")

if not GATE_ONLY:
    if not TEMPLATE_ONLY:
        print()
    report_template()

# The advisory checks touch the exit status only under DD_STRICT_TEMPLATE=1.
advisory_fail = STRICT_TEMPLATE and bool(tail_bad or title_bad)
if TEMPLATE_ONLY:
    sys.exit(1 if advisory_fail else 0)
sys.exit(1 if problems or advisory_fail else 0)
