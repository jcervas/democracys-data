#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# Measure every chapter against labs/STYLE.md.
#
#     python3 _lib/check-language.py             # report, exit 1 if any fail
#     python3 _lib/check-language.py --worst 20  # rank the corpus, exit 0
#     python3 _lib/check-language.py <slug>      # one chapter, with the lines
#
# WHY THIS EXISTS. The author read the book and said much of it was hard to
# follow. That is a judgment, and judgments do not scale to ninety-odd
# chapters -- so the parts of it that can be measured are measured here, and
# the rest is left to a person. What is measured:
#
#   1. sentences over 35 words                 STYLE.md rule 6
#   2. technical words used before glossing    STYLE.md rule 7
#   3. banned jargon                           STYLE.md rule 8
#   4. paragraphs over 6 sentences             STYLE.md rule 9
#   5. em-dashes per paragraph                 STYLE.md rule 10
#   6. a line ending in `r                     not style but rendering: knitr
#      wants a space after the opener, so a span wrapped there prints its
#      code literally on the page
#
# WHAT IT DOES NOT MEASURE. Nothing in Part One of STYLE.md, which is the half
# that matters: whether the brief is about the data or about the machinery that
# fetched it (rule 1), whether measured claims are told apart from borrowed ones
# (rule 2), whether the point is at the front of the paragraph (rule 3), whether
# a figure is captioned once (rule 4), whether the opening is plain (rule 12).
# A chapter can pass this script and still be unreadable. It cannot fail this
# script and be fine.
# ---------------------------------------------------------------------------
import os, re, sys, glob

HERE = os.path.dirname(os.path.abspath(__file__))
LABS = os.path.dirname(HERE)

MAX_SENTENCE = 35
MAX_PARA_SENTENCES = 6
MAX_DASH_PER_PARA = 1

# Rule 7. Word -> a fragment that shows it has been glossed nearby. The gloss
# test is deliberately loose: any of these fragments within 200 characters of
# the first use counts. The point is to catch a chapter that never explains its
# central word at all, not to police phrasing.
GLOSS = {
    "denominator":  ("bottom number", "what you divide", "dividing by", "divided by"),
    "numerator":    ("top number", "the number on top"),
    "margin of error": ("how far off", "could be off", "how wrong"),
    "weighting":    ("counting some", "count some answers", "make the sample look"),
    # "regression" the technique and "regression to the mean" the phenomenon
    # are different ideas, and a gloss for one does not explain the other.
    "regression to the mean": ("drift back", "back toward the middle",
                               "back toward average", "lurching one way"),
    "regression":   ("best line", "line through", "straight line"),
    "impute":       ("fill in", "guess", "filling in"),
    "crosswalk":    ("translates", "matches one set", "converts one set"),
    "residual":     ("left over", "what is left", "subtract"),
}
# Rule 8. Words that should simply not appear in student-facing prose.
#
# Backticked spans are already masked by prose(), so a FILENAME like
# `provenance.csv` is not a finding -- only the English word is.
BANNED = ["estimand", "covariate", "areal", "disaggregate", "provenance",
          "heteroscedastic", "endogeneity", "orthogonal"]
# Named concepts that contain a banned word and are worth teaching by name.
# They are exempt from rule 8 and belong under rule 7 instead: use the name,
# but gloss it. "The modifiable areal unit problem" is the standard term for
# the thing this book demonstrates more often than any other, and inventing a
# private synonym for it would leave a student unable to look it up.
NAMED_OK = ["modifiable areal unit",
            # the chapter is *called* areal-units; naming it is not jargon
            "areal-units"]


def prose(path):
    """The reader-visible text: no YAML, no code, no tables, no headings."""
    s = open(path, encoding="utf-8").read()
    s = re.sub(r"^---\n.*?\n---\n", "", s, flags=re.S)
    s = re.sub(r"```\{r.*?```", "", s, flags=re.S)
    s = re.sub(r"```.*?```", "", s, flags=re.S)
    s = re.sub(r"`r [^`]*`", "NUMBER", s)      # a computed value reads as one word
    s = re.sub(r"`[^`]*`", "NAME", s)
    s = re.sub(r"!\[[^\]]*\]\([^)]*\)", "", s)
    s = re.sub(r"\[([^\]]*)\]\([^)]*\)", r"\1", s)
    s = re.sub(r"<[^>]+>", "", s)
    # Strip EMPHASIS markers before splitting into sentences. "…distribution.**"
    # ends a sentence for the reader, but the splitter looks at the character
    # before the space, sees an asterisk, and glues the caption title to the
    # sentence after it -- reporting one 47-word sentence where the page shows
    # two. Only asterisks touching a non-space character are removed, so a
    # list bullet ("* item", asterisk then space) survives for the filter below.
    s = re.sub(r"\*+(?=\S)", "", s)
    s = re.sub(r"(?<=\S)\*+", "", s)
    out = []
    for para in s.split("\n\n"):
        lines = para.split("\n")
        # A block whose FIRST line is a list item or a table row is a list or a
        # table, and its continuation lines belong to it. Filtering line by line
        # kept those continuations and glued them into a sentence nobody wrote,
        # which reported a bibliography as a 16-sentence paragraph.
        first = next((l for l in lines if l.strip()), "")
        if re.match(r"^\s*([-*+]\s|\d+\.\s|\||>|#)", first):
            continue
        body = "\n".join(l for l in lines
                         if not re.match(r"^\s*(#|\||>|\s*[-*+]\s|\d+\.\s)", l))
        if body.strip():
            out.append(" ".join(body.split()))
    return out


ABBREV = re.compile(
    r"(?:\b(?:U\.S|v|vs|No|Nos|Jr|Sr|St|Dr|Mr|Ms|Mrs|Prof|Inc|Co|Corp|Ct|Cir"
    r"|Ed|Rev|Fig|Rep|Sen|Gov|approx|al|e\.g|i\.e|cf|pp|vol|ch)\.|"
    r"(?:^|\s)[A-Z]\.)$")


def sentences(text):
    # A sentence can end INSIDE a closing quotation mark or bracket: a period
    # followed by a quote and a space is still a full stop to the reader. The
    # naive split looks at the character before the space, sees a quote, and
    # glues three sentences of dialogue into one 45-word "sentence".
    #
    # A period after an ABBREVIATION is not a full stop either. Without the
    # guard below, one legal citation -- "*Shaw v. Reno*, 509 U.S. 630 (1993)"
    # -- counts as three sentences, and a paragraph carrying two citations is
    # reported as over-long when the page shows four lines. Initials
    # ("Michael P. McDonald") are caught by the single-capital rule at the end.
    parts = re.split(r"(?<=[.!?])[\"\')\]]*\s+", text)
    merged = []
    for part in parts:
        if merged and ABBREV.search(merged[-1]):
            merged[-1] = merged[-1] + " " + part
        else:
            merged.append(part)
    return [p for p in (x.strip() for x in merged) if len(p.split()) > 2]


def audit(path):
    paras = prose(path)
    full = " ".join(paras)
    low = full.lower()
    f = {"long": [], "para": 0, "dash": 0, "gloss": [], "banned": [],
         "wrapped": []}
    # Rule 6 works on the raw lines, not the prose: the defect is invisible
    # after masking. A wrap INSIDE the expression is fine; only a line that
    # ends at the opener itself is fatal. The lookbehind spares fence lines.
    for i, raw in enumerate(open(path, encoding="utf-8"), 1):
        if re.search(r"(?<!`)`r$", raw.rstrip("\r\n")):
            f["wrapped"].append(i)
    for p in paras:
        ss = sentences(p)
        for s in ss:
            if len(s.split()) > MAX_SENTENCE:
                f["long"].append((len(s.split()), s))
        if len(ss) > MAX_PARA_SENTENCES:
            f["para"] += 1
        if p.count("—") > MAX_DASH_PER_PARA:
            f["dash"] += 1
    for w, hints in sorted(GLOSS.items(), key=lambda kv: -len(kv[0])):
        # WHOLE WORDS ONLY, with the stem still matching. A plain substring
        # search finds "numerator" inside "enumerators", which is how a
        # chapter came to be flagged for a word it does not use. The trailing
        # \w* keeps "denominators", "imputed" and "weighting" as real uses.
        m = re.search(r"\b" + re.escape(w) + r"\w*", low)
        if not m:
            continue
        i = m.start()
        window = low[max(0, i - 200): i + 200]
        if any(w in longer for longer in f.get("_matched", [])):
            continue
        if not any(h in window for h in hints):
            f["gloss"].append(w)
        f.setdefault("_matched", []).append(w)
    scrubbed = low
    for name in NAMED_OK:
        scrubbed = scrubbed.replace(name, "")
    for w in BANNED:
        if re.search(r"\b" + w, scrubbed):
            f["banned"].append(w)
    f["score"] = (len(f["long"]) * 3 + f["para"] * 2 + f["dash"]
                  + len(f["gloss"]) * 4 + len(f["banned"]) * 4
                  + len(f["wrapped"]) * 4)
    return f


def chapters():
    """Every chapter, found the same way the part openers find them.

    FILTER ON BASENAMES, NEVER ON THE FULL PATH. This corpus lives under
    .../_teaching/_Democracy's Data/, so an absolute path always contains
    "/_" and a `"/_" in p` test silently discards every chapter -- leaving a
    checker that examines nothing and reports clean. That exact bug has now
    been written twice in this repository (check-vacuous.R had it first), so
    the guard below is deliberate and should not be "simplified".
    """
    out = []
    for p in sorted(glob.glob(os.path.join(LABS, "*", "*", "*-brief.Rmd"))):
        chap = os.path.basename(os.path.dirname(p))
        part = os.path.basename(os.path.dirname(os.path.dirname(p)))
        if chap.startswith("_") or part.startswith("_"):
            continue
        out.append((chap, p))
    if len(out) < 50:
        sys.exit(f"check-language: found only {len(out)} chapters — the "
                 "traversal is broken, refusing to report a misleading pass")
    return out


args = sys.argv[1:]
if args and args[0] not in ("--worst",):
    slug = args[0]
    hit = [(s, p) for s, p in chapters() if s == slug]
    if not hit:
        sys.exit(f"no chapter named {slug}")
    s, p = hit[0]
    f = audit(p)
    print(f"{s}  (score {f['score']})\n")
    for n, sent in sorted(f["long"], reverse=True):
        print(f"  {n}w  {sent[:150]}…\n")
    if f["gloss"]:  print("  unglossed:", ", ".join(f["gloss"]))
    if f["banned"]: print("  banned   :", ", ".join(f["banned"]))
    if f["wrapped"]:
        print("  line ends in `r, code will print literally: line",
              ", ".join(str(i) for i in f["wrapped"]))
    print(f"  paragraphs over {MAX_PARA_SENTENCES} sentences: {f['para']}")
    print(f"  paragraphs with >{MAX_DASH_PER_PARA} em-dash:    {f['dash']}")
    sys.exit(0)

rows = [(s, audit(p)) for s, p in chapters()]
rows.sort(key=lambda r: -r[1]["score"])

if args and args[0] == "--worst":
    n = int(args[1]) if len(args) > 1 else 15
    print(f"{'chapter':34s} {'score':>5}  {'>35w':>4} {'para':>4} {'dash':>4}  unglossed / banned")
    for s, f in rows[:n]:
        bad = ", ".join(f["gloss"] + f["banned"])[:38]
        print(f"{s:34s} {f['score']:5d}  {len(f['long']):4d} {f['para']:4d} {f['dash']:4d}  {bad}")
    tot = sum(f["score"] for _, f in rows)
    print(f"\n{len(rows)} chapters, total score {tot}  (0 is clean)")
    sys.exit(0)

fails = [(s, f) for s, f in rows if f["score"] > 0]
if fails:
    print(f"{len(fails)} of {len(rows)} chapters are not yet at the STYLE.md standard.")
    print("Run  python3 _lib/check-language.py --worst 20  to rank them,")
    print("or   python3 _lib/check-language.py <slug>      to see one chapter's lines.")
else:
    print(f"{len(rows)} chapters, language clean")
sys.exit(1 if fails else 0)
