#!/usr/bin/env python3
"""Two things that go wrong in a rendered brief and nowhere else.

1. A FIGURE EMITTED MORE THAN ONCE.

   Every d3 figure is written by cat()-ing a string built with paste0(). If any
   value interpolated into that string is not length one, paste0() vectorises
   over it and cat() prints the whole figure once per element. The chapter still
   renders, the numbers in it are still right, and the same chart appears two or
   four times in a row with duplicate element ids.

   This was found in oral-argument, where the interpolated value was a
   one-row data frame. A data frame has length equal to its number of COLUMNS,
   so the figure came out four times, and the caption read correctly because
   the first column happened to be the one the caption wanted.

   Nothing else catches it. The build is clean, the language check is clean,
   the tables are clean, and the page is wrong. A repeated element id in the
   render is the signature, and it is exact.

2. A FIGURE WITH NO STATIC FALLBACK.

   Mail and messaging clients strip <script>, so a brief that reaches somebody
   as an attachment shows the prose and the tables and a gap where each figure
   was. syllabus-helpers.R emits every `*-static` chunk into the HTML inside
   .dd-fallback to cover that. A chapter that draws with d3 and has no fallback
   in its render is a chapter that will arrive with holes.

3. THE CHART LIBRARY EMBEDDED AS A data: URL RATHER THAN AS SOURCE.

   dd_libs() writes two <script src> tags, and pandoc replaces each with the
   file's own text -- unless that file contains a literal end-script tag, in
   which case pandoc cannot write it between script tags and falls back to
   src="data:text/javascript,<the whole file, percent-encoded>". The page
   still works in a browser, so nothing about the fallback is visible: what
   changes is that the library near-doubles in size, disappears from every
   grep and DOM-text check (the render then looks as though DD were missing,
   which is how this was first reported), and becomes the first thing any
   Content-Security-Policy blocks. dd-charts.js therefore writes every such
   sequence as <\\/, and this check is what notices if one comes back.

Run from labs/:  python3 _lib/check-figures.py
"""
import collections
import glob
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
LABS = os.path.dirname(HERE)

DIV_ID = re.compile(r'<div id="([A-Za-z][\w-]*)"')
# d3 is inlined by pandoc, so the CDN address survives only in its banner
# comment. Either form means the page draws itself.
DRAWS = re.compile(r"d3js\.org|d3\.select\(")
FALLBACK = 'class="dd-fallback"'
# pandoc's fallback embedding, and the tail of dd-charts.js as pandoc leaves it
# when the inline path is taken. A page that calls DD.fig() wants the second.
DATA_URL_SCRIPT = re.compile(r'<script[^>]*\bsrc="data:text/javascript')
CALLS_DD = re.compile(r'\bDD\.fig\(')
DD_INLINE = "window.DD = DD"


def main():
    briefs = sorted(glob.glob(os.path.join(LABS, "*", "*", "*-brief.html")))
    if not briefs:
        print("no rendered briefs found — nothing to check")
        return 0

    dupes, holes, encoded, checked = [], [], [], 0
    for path in briefs:
        try:
            html = open(path, encoding="utf-8", errors="replace").read()
        except OSError:
            continue
        checked += 1
        rel = os.path.relpath(path, LABS)

        repeated = {k: v for k, v in
                    collections.Counter(DIV_ID.findall(html)).items() if v > 1}
        if repeated:
            dupes.append((rel, repeated))

        if DRAWS.search(html) and FALLBACK not in html:
            holes.append(rel)

        if CALLS_DD.search(html) and DD_INLINE not in html:
            hit = DATA_URL_SCRIPT.search(html)
            encoded.append((rel, "as a data: URL" if hit else "not at all"))

    print("1. the same figure emitted more than once")
    if dupes:
        for rel, rep in dupes:
            worst = ", ".join("%s x%d" % (k, v) for k, v in sorted(rep.items()))
            print("   %-58s %s" % (rel, worst))
        print("   a repeated id means a paste0() was handed something longer "
              "than one value")
    else:
        print("   clean")

    print()
    print("2. a JavaScript figure with no static fallback in the render")
    if holes:
        for rel in holes:
            print("   %s" % rel)
        print("   these arrive with gaps wherever <script> is stripped "
              "(email, messaging)")
    else:
        print("   clean")

    print()
    print("3. the chart library not written into the page as source")
    if encoded:
        for rel, how in encoded:
            print("   %-58s %s" % (rel, how))
        print("   pandoc took the data: URL path. Re-render these; if one "
              "comes back, _lib/dd-charts.js has picked up a literal "
              "end-script tag again -- write the sequence as <\\/")
    else:
        print("   clean")

    print()
    print("%d rendered briefs checked" % checked)
    return 1 if (dupes or holes or encoded) else 0


if __name__ == "__main__":
    sys.exit(main())
