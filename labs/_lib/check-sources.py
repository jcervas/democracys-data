#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# check-sources.py -- can a reader get the data?
#
# WHY THIS EXISTS
#
# The whole point of these chapters is provenance, and a chapter that names its
# source without giving its address stops one step short of the point. An
# audit on 29 Aug 2026 found 28 chapters that read data and printed no address
# anywhere in "## Sources" -- one of them, levels-of-aggregation, listed its
# inputs as four bare noun phrases ("Alaska's 2024 cast vote record.") with no
# publisher at all, which is what the source-prose rewrite left behind when it
# stripped the script paths out and nothing replaced the subject.
#
# So three things are checked here. The first two are the halves of "where did
# this come from"; the third is what a reader does next:
#
#   1. WHERE THE DATA CAME FROM. Every chapter that reads data carries at
#      least one address in "## Sources" -- the publisher's, not ours.
#
#   2. WHERE THE READER'S COPY IS. Every such chapter closes its data list
#      with "**The data itself**", linking the tables its figures rest on so a
#      student can open one and go further with it. That the links RESOLVE is
#      check-layout.py's job (rule "dead link"); that they EXIST is this one's.
#
#   3. SOMETHING TO DO WITH IT. "**Your turn**" follows, with at least three
#      open questions, and every file a question names is one the chapter
#      actually hands over. That last rule is the one worth having: a question
#      saying "sort `flows.csv`" survives a rename of flows.csv silently,
#      because prose is not a path and no other check reads it. STYLE.md also
#      forbids typing a figure into a question -- "fifteen counties disagree"
#      goes stale -- but that one is left to a person, because a year is a
#      number too and this script cannot tell them apart.
#
# None of this is a style preference. A brief travels alone -- it gets mailed,
# it gets printed -- so "see the policing chapter" is a dead end for whoever is
# holding this one, and the address has to be here.
#
# Exit status is 0 only if every chapter passes, so this can gate a commit.
# ---------------------------------------------------------------------------
import os
import re
import sys

LIB = os.path.dirname(os.path.abspath(__file__))
LABS = os.path.dirname(LIB)

URL = re.compile(r"https?://[^\s>)\]\"'`,]+")
BLOCK = "**The data itself**"
LINK = re.compile(r"\]\((\.\./|data/)[^)\s]+\)")
LINKED = re.compile(r"\[`([^`]+)`\]\((?:\.\./|data/)[^)\s]+\)")
TURN = "**Your turn**"
QFILE = re.compile(r"`([\w./-]+\.(?:csv|tsv|json|geojson))`")
BULLET = re.compile(r"^- ", re.M)
MIN_QUESTIONS = 3

# Chapters that carry no data of their own and so are asked for neither thing.
# The part openers tabulate the book's own structure -- beats and chapter
# counts -- and the introduction computes nothing at all. An allowlist, not a
# skip: a chapter missing from both this set and the checked set is a bug in
# the traversal, and the run says so.
# admin-records-source is here because it is becoming Section III's intro under
# the 3rd-edition rewrite: a reading about administrative records as a kind of
# data, which will stop carrying data of its own.
STRUCTURAL = re.compile(r"^(part-.*|introduction|admin-records-source)$")

# Sources with no address, because there is no file and never was. Keep this
# short and keep the reason in it: "we could not find a URL" is not a reason.
NO_ADDRESS = {
    # Green and Gerber's numbers are printed in a book and published nowhere
    # else. It is cited by ISBN, which is the durable identifier a book has.
    "gotv": "printed book, cited by ISBN",
}


def chapters():
    for part in sorted(os.listdir(LABS)):
        p = os.path.join(LABS, part)
        if not os.path.isdir(p) or part.startswith(("_", ".")):
            continue
        # a chapter is a directory holding <slug>-brief.Rmd, at either depth
        if os.path.isfile(os.path.join(p, part + "-brief.Rmd")):
            yield part, p
        for sub in sorted(os.listdir(p)):
            q = os.path.join(p, sub)
            if os.path.isfile(os.path.join(q, sub + "-brief.Rmd")):
                yield sub, q


problems = []
checked = 0
exempt = []

for slug, d in chapters():
    text = open(os.path.join(d, slug + "-brief.Rmd"), encoding="utf-8").read()
    rel = os.path.relpath(os.path.join(d, slug + "-brief.Rmd"), LABS)

    derived = os.path.join(d, "data", "derived")
    has_data = os.path.isdir(derived) and bool(os.listdir(derived))

    if STRUCTURAL.match(slug) or not has_data:
        exempt.append(slug)
        continue
    checked += 1

    m = re.search(r"^## Sources\s*$", text, re.M)
    if not m:
        problems.append((rel, "no '## Sources' heading"))
        continue
    src = text[m.end():]

    if not URL.search(src) and slug not in NO_ADDRESS:
        problems.append((rel, "reads data, but ## Sources gives no address"))

    if BLOCK not in src:
        problems.append((rel, f"## Sources has no '{BLOCK}' block"))
        continue

    after = src[src.index(BLOCK) + len(BLOCK):]
    # the block runs to the next bold lead or fenced chunk
    end = re.search(r"^(\*\*|```)", after, re.M)
    handed = after[:end.start()] if end else after
    if not LINK.search(handed):
        problems.append((rel, f"'{BLOCK}' block links nothing"))
    offered = set(LINKED.findall(handed))

    if TURN not in src:
        problems.append((rel, f"## Sources has no '{TURN}' block"))
        continue
    rest = src[src.index(TURN) + len(TURN):]
    end = re.search(r"^(\*\*|```)", rest, re.M)
    turn = rest[:end.start()] if end else rest
    n = len(BULLET.findall(turn))
    if n < MIN_QUESTIONS:
        problems.append((rel, f"'{TURN}' has {n} question(s), wanted "
                              f"{MIN_QUESTIONS}"))
    for name in sorted(set(QFILE.findall(turn)) - offered):
        problems.append((rel, f"'{TURN}' names {name}, which the chapter "
                              f"does not hand over"))

if problems:
    w = max(len(r) for r, _ in problems)
    for rel, what in problems:
        print(f"{rel:{w}}  {what}")
    print(f"\n{len(problems)} problem(s)")
    sys.exit(1)

note = ", ".join(f"{k} ({v})" for k, v in sorted(NO_ADDRESS.items()))
print(f"{checked} chapters read data; every one gives an address, hands over "
      f"its tables, and asks at least {MIN_QUESTIONS} questions of them")
print(f"  cited without a URL on purpose: {note}")
print(f"  {len(exempt)} chapters carry no data of their own: "
      f"{', '.join(sorted(exempt))}")
