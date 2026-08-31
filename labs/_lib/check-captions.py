#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# STYLE.md rule 4: one figure, one caption.
#
#     python3 _lib/check-captions.py            # report every doubled caption
#     python3 _lib/check-captions.py <slug>     # one chapter
#
# WHY THIS EXISTS. Every interactive figure in this book prints its own caption
# underneath itself, in small grey type, from the string the figure was built
# with. Underneath that sits the chapter's "Figure N." paragraph. Usually the
# two divide the work: the paragraph names the figure and argues from it, the
# caption carries the reading instructions and the numbers. Sometimes they do
# not, and the caption repeats what the paragraph already said. Then the reader
# is told the same thing twice and neither telling is the one to keep.
#
# WHAT COUNTS AS A DUPLICATE, and what does not. The test is containment, not
# similarity: a caption is redundant when nearly every word of it already
# appears in the paragraph below. A caption that shares a subject with the
# paragraph and adds a number, an axis or a "hover a bar" is doing its job, and
# is not reported.
#
# The first version of this check compared the caption against the BOLD TITLE
# alone, which is a summary of the figure by design, and reported 62 pairs on a
# loose word-overlap score. Reading them showed most were complementary: the
# grey half held the hover affordances and the counts. Comparing against the
# whole paragraph, and asking for containment rather than overlap, brings the
# corpus down to the captions that genuinely say nothing new.
#
# ADVISORY, NOT A GATE. It reads the last render rather than the working tree,
# like check-figures.py, and it currently reports a backlog somebody has to
# work through. Making it fail the build today would stop every render in the
# corpus. Move it to a gate once the backlog is clear.
# ---------------------------------------------------------------------------
import glob
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
LABS = os.path.dirname(HERE)

# The small grey caption the figure helpers emit under an interactive figure.
GREY = re.compile(r'<p style="font-size:0\.8\d+em;color:#666[^"]*">(.*?)</p>', re.S)
# The whole "Figure 3. ..." paragraph, title and argument together. Comparing
# against the bolded title alone is what made the first version of this check
# wrong: a title restates the caption's subject because that is what a title is.
PARA = re.compile(r'<p>(<strong>Figure\s*(\d+)\b.*?)</p>', re.S)

# A caption is redundant when this share of its words is already in the
# paragraph. Set from reading the corpus: at 0.9 every reported pair was a
# genuine restatement, and the first caption that earned its place scored 0.87.
CONTAINED = 0.9

WORD = re.compile(r"[a-z]{4,}")


def words(html):
    text = re.sub(r"<[^>]+>", " ", html).lower()
    return set(WORD.findall(text))


def contained(caption, paragraph):
    """Share of the caption's vocabulary the paragraph already carries."""
    wc, wp = words(caption), words(paragraph)
    if not wc:
        return 0.0
    return len(wc & wp) / len(wc)


# An embedded PNG is a hundred thousand characters of base64, and the static
# twin of a figure sits between its grey caption and the bold paragraph below.
# Measuring the gap in raw HTML therefore measures image bytes rather than
# prose, and every doubled caption in a chapter with fallbacks reads as far
# apart. Collapse every data: URI to nothing before looking at distance.
DATA_URI = re.compile(r'src="data:[^"]*"')


def audit(path):
    html = open(path, encoding="utf-8", errors="replace").read()
    html = DATA_URI.sub('src=""', html)
    greys = [(m.start(), m.group(1)) for m in GREY.finditer(html)]
    paras = [(m.start(), m.group(2), m.group(1)) for m in PARA.finditer(html)]
    doubled = []
    for idx, (pos, grey) in enumerate(greys):
        # The paragraph belongs to this figure if it is the next one after the
        # caption and close behind it. 3,000 characters is roughly a screen of
        # prose; further than that and it is a different figure.
        nxt = [p for p in paras if 0 < p[0] - pos < 3000]
        if not nxt:
            continue
        ppos, num, ptext = min(nxt, key=lambda p: p[0])
        share = contained(grey, ptext)
        if share >= CONTAINED:
            doubled.append((num, idx, round(share, 2),
                            re.sub(r"\s+", " ", re.sub(r"<[^>]+>", "", grey)).strip()[:70]))
    return doubled


def main():
    want = sys.argv[1] if len(sys.argv) > 1 else None
    renders = sorted(glob.glob(os.path.join(LABS, "*", "*", "*-brief.html")))
    total = chapters = 0
    for r in renders:
        slug = os.path.basename(os.path.dirname(r))
        if want and slug != want:
            continue
        doubled = audit(r)
        if not doubled:
            continue
        chapters += 1
        total += len(doubled)
        print("  %-28s %d caption(s) saying nothing new" % (slug, len(doubled)))
        if want:
            for num, idx, share, txt in doubled:
                print("      Figure %s  (caption #%d, %.0f%% of it is already in the paragraph)\n        %s"
                      % (num, idx, share * 100, txt))
    if total:
        print("\n%d caption(s) in %d chapter(s) repeat their Figure paragraph "
              "(STYLE.md rule 4)." % (total, chapters))
        print("The paragraph is the one that survives: it is in the PDF too, "
              "and the caption is not.")
    else:
        print("no caption repeats the paragraph below it")
    return 0


if __name__ == "__main__":
    sys.exit(main())
