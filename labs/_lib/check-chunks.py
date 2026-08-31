#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# Every brief's chunk labels must match its <slug>-code.R, one for one.
#
#     python3 _lib/check-chunks.py          # report, exit 1 if any mismatch
#
# WHY THIS EXISTS. A chapter's prose and its code live in two files, paired by
# knitr::read_chunk(): the brief carries `​```{r fig2-d3, ...}` with an empty
# body, the .R file carries `## ---- fig2-d3` with the code. knitr pairs them
# by label and says NOTHING when it cannot. A mistyped or deleted label does
# not error -- the chunk simply produces no output, and the figure vanishes
# from a page that still renders, still passes every other check, and still
# reads as finished. That is the failure this script exists to make loud.
#
# It checks both directions. A label in the brief with no code is a missing
# figure; a label in the .R file with no chunk is dead code that no longer
# runs and that nobody will notice has stopped being true.
# ---------------------------------------------------------------------------
import os, re, sys, glob

HERE = os.path.dirname(os.path.abspath(__file__))
LABS = os.path.dirname(HERE)

CHUNK = re.compile(r'^```\{r([^}]*)\}[ \t]*$', re.M)
MARK  = re.compile(r'^## ---- (\S+)[ \t]*$', re.M)
BOOT  = "knitr-setup"


def briefs():
    out = []
    for p in sorted(glob.glob(os.path.join(LABS, "*", "*", "*-brief.Rmd"))
                    + glob.glob(os.path.join(LABS, "*", "*-brief.Rmd"))):
        chap = os.path.basename(os.path.dirname(p))
        if chap.startswith("_"):
            continue
        part = os.path.basename(os.path.dirname(os.path.dirname(p)))
        if part.startswith("_") and part != "labs":
            continue
        out.append((chap, p))
    if len(out) < 50:
        sys.exit("check-chunks: found only %d briefs -- traversal is broken, "
                 "refusing to report a misleading pass" % len(out))
    return out


problems = []
paired = 0
for slug, rmd in briefs():
    code = os.path.join(os.path.dirname(rmd), slug + "-code.R")
    src = open(rmd, encoding="utf-8").read()
    labels = [h.strip().split(",")[0].strip() for h in CHUNK.findall(src)]
    labels = [l for l in labels if l and l != BOOT]

    if not os.path.exists(code):
        # A brief that never split is fine, so long as its chunks carry bodies.
        if re.search(r'^```\{r[^}]*\}\n```', src, re.M):
            problems.append((slug, "empty chunks but no %s-code.R" % slug))
        continue

    paired += 1
    marks = MARK.findall(open(code, encoding="utf-8").read())

    # The bootstrap names the file in a variable and passes explicit line
    # ranges to read_chunk(), so look for the two parts rather than one literal
    # call: the filename as a quoted string, and a read_chunk() somewhere.
    if '"%s-code.R"' % slug not in src or "read_chunk" not in src:
        problems.append((slug, "has %s-code.R but the brief never read_chunk()s it" % slug))

    for l in labels:
        if l not in marks:
            problems.append((slug, "chunk '%s' has no '## ---- %s' in the code file "
                                   "(renders empty, silently)" % (l, l)))
    for m in marks:
        if m not in labels:
            problems.append((slug, "'## ---- %s' in the code file has no chunk in the "
                                   "brief (dead code)" % m))
    dupes = {l for l in labels if labels.count(l) > 1}
    for d in sorted(dupes):
        problems.append((slug, "chunk label '%s' appears more than once" % d))

for slug, msg in problems:
    print("%-24s %s" % (slug, msg))
print()
print("%d brief(s) paired with a code file, %d problem(s)" % (paired, len(problems)))
sys.exit(1 if problems else 0)
