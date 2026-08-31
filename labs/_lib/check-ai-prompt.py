#!/usr/bin/env python3
"""Does every chapter that fetches data carry its AI prompt box, and is
every box telling the truth?

The box is ai_prompt() in syllabus-helpers.R: a verbatim prompt a reader
can paste into an AI assistant to rebuild the chapter's data from its
original source. The prompt text lives in data/ai-prompt.txt; the brief
reads it in a results="asis" chunk at the end of ## Sources. Two tones:
"rebuild" for a source a stranger can fetch today, "frozen" for one that
cannot be fetched again.

Four questions, per chapter:

1. IN SCOPE, NO BOX. A chapter whose build scripts fetch from the network
   (or that is named in FROZEN below) must have data/ai-prompt.txt AND an
   ai_prompt() call in its brief. Half of either is also a finding.

2. A BOX WITHOUT A FETCH. A chapter carrying the box that neither fetches
   nor is named in FROZEN is promising the reader a rebuild of data it
   does not obtain. Chapters that read only a sibling's derived/ or raw
   committed elsewhere are named in NO_PROMPT_OK with the chapter that
   owns the source.

3. THE WRONG TONE. FROZEN chapters must call tone = "frozen"; everyone
   else must not.

4. STALE NUMBERS. Every number quoted in the prompt's verification block
   (after "CHECK YOUR WORK" or "WHAT YOU CAN STILL CHECK") must appear
   somewhere in the chapter's own data — derived tables, build-script
   headers, provenance or stamp records — comma-insensitively. A data
   rebuild that moves a number then surfaces the stale prompt here,
   instead of shipping a box whose checks can no longer be passed.

Fetch detection is a code scan: a non-comment line in a top-of-data/
script that uses a quoted http(s) URL, or a PROVENANCE.tsv. The two
tables below are the hand-curated judgment the scan cannot make.

Run from labs/:  python3 _lib/check-ai-prompt.py
"""
import glob
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
LABS = os.path.dirname(HERE)

# Chapters whose box must be tone = "frozen", with the reason. Membership
# here also puts the chapter in scope even if nothing in data/ fetches.
FROZEN = {
    "surnames":          "2010 surname zip answers 200 with a 247-byte refusal; committed CSV is the archive",
    "anes":              "download page returns 403 to a script, 200 to a browser; 163 MB not redistributable",
    "ces-class":         "Harvard Dataverse sits behind an AWS WAF; 184 MB Common Content is not redistributable",
    "models-markets":    "live market API; the 746-market snapshot cannot be drawn again",
    "survey-access":     "a dated bot-wall scan; re-running measures a different day",
    "bellwether":        "the Dataverse .Rdata needs one browser download past an AWS WAF challenge",
    "ga-precinct-returns": "the 2020 SoS archive is one manual browser download; 2024 refetches",
    "voter-file-access": "a dated 51-state link scan; state websites will not answer the same next month",
    "voter-files":       "county registration extract obtained by records request, not by URL",
    "follower-counts":   "follower counts move by the minute; collected once through a headless browser",
    "house-competition": "the Jacobson 1946-2014 spine circulates privately; the Clerk PDFs refetch",
    "cost-of-voting":    "no stable URL, and the next edition revises every year in the file",
    "gotv":              "hand-keyed from a printed book; there is nothing to refetch",
    "mid-decade":        "Dave's Redistricting exports are manual browser downloads; committed CSVs and plan files are the archive",
    "mid-decade-florida": "Dave's Redistricting exports are manual browser downloads; committed CSVs and plan files are the archive",
    "panhandle-claim":   "the 46 county precinct exports sit behind unlisted per-county election ids and the counties retire the pages",
}

# Fetching chapters that deliberately carry no box, with the chapter that
# introduces their source. Keep this list short; empty is the goal.
NO_PROMPT_OK = {
    "gss-confidence":     "reads the GSS .dta the gender-gap chapter fetches",
    "nomination-rules":   "reads the FEC workbooks the retirements chapter fetches",
    "careers":            "reads the Voteview members file the retirements chapter fetches",
    "voter-files-source": "reads the voter-files chapter's derived counts",
}

SCRIPT_EXT = (".R", ".r", ".py", ".mjs", ".js")
COMMENT = re.compile(r"^\s*(#|//|\*|/\*)")
URL_IN_CODE = re.compile(r"""["']https?://""")
CALL = re.compile(r"ai_prompt\(")
# calls are one-liners by convention, so the tone is on the ai_prompt( line
TONE_FROZEN = re.compile(r"ai_prompt\([^\n]*tone\s*=\s*['\"]frozen['\"]")
VERIFY_HEAD = re.compile(r"CHECK YOUR WORK|WHAT YOU CAN STILL CHECK")
NUM = re.compile(r"\d[\d,]*\d|\d")


def norm(s):
    return s.replace(",", "")


def numbers_in(text):
    """Digit strings worth anchoring: three digits or more once commas are
    dropped, and not a bare year."""
    out = set()
    for m in NUM.finditer(text):
        n = norm(m.group())
        if len(n) < 3:
            continue
        if len(n) == 4 and 1800 <= int(n) <= 2100:
            continue
        out.add(n)
    return out


def chapter_fetches(data_dir):
    if os.path.exists(os.path.join(data_dir, "PROVENANCE.tsv")):
        return True
    for f in sorted(glob.glob(os.path.join(data_dir, "*"))):
        if not f.endswith(SCRIPT_EXT) or not os.path.isfile(f):
            continue
        try:
            lines = open(f, encoding="utf-8", errors="replace").readlines()
        except OSError:
            continue
        for ln in lines:
            if COMMENT.match(ln):
                continue
            if URL_IN_CODE.search(ln):
                return True
    return False


def chapter_corpus(data_dir):
    """Everything a verification number may legitimately anchor to."""
    parts = []
    pats = ["derived/*.csv", "*.R", "*.r", "*.py", "*.mjs",
            "PROVENANCE.tsv", "BUILD-STAMP.tsv", "provenance.csv"]
    for pat in pats:
        for f in glob.glob(os.path.join(data_dir, pat)):
            try:
                parts.append(open(f, encoding="utf-8", errors="replace").read())
            except OSError:
                pass
    return norm("\n".join(parts))


def main():
    briefs = sorted(glob.glob(os.path.join(LABS, "*", "*", "*-brief.Rmd"))
                    + glob.glob(os.path.join(LABS, "*", "*-brief.Rmd")))
    findings = []
    n_boxes = 0

    for rmd in briefs:
        chap = os.path.dirname(rmd)
        slug = os.path.basename(chap)
        rel = os.path.relpath(rmd, LABS)
        data = os.path.join(chap, "data")
        prompt = os.path.join(data, "ai-prompt.txt")

        # The ai_prompt() call lives wherever the chunk bodies live. Since the
        # chapters split their code into <slug>-code.R, searching the brief
        # alone found the call in NO chapter and reported the whole corpus as
        # missing its box -- a check that had quietly stopped checking. Read
        # both halves of the chapter and treat them as one document.
        src = open(rmd, encoding="utf-8", errors="replace").read()
        code = os.path.join(chap, slug + "-code.R")
        if os.path.exists(code):
            src += "\n" + open(code, encoding="utf-8", errors="replace").read()
        has_call = bool(CALL.search(src))
        has_file = os.path.exists(prompt)
        frozen = slug in FROZEN
        in_scope = frozen or (os.path.isdir(data) and chapter_fetches(data))

        if has_call and has_file:
            n_boxes += 1

        # 1. in scope, box missing or half-present
        if in_scope and slug not in NO_PROMPT_OK:
            if not has_file and not has_call:
                findings.append((rel, "fetches but has no AI prompt box"
                                 + (" (frozen: %s)" % FROZEN[slug] if frozen else "")))
            elif has_file and not has_call:
                findings.append((rel, "data/ai-prompt.txt exists but the brief never calls ai_prompt()"))
            elif has_call and not has_file:
                findings.append((rel, "brief calls ai_prompt() but data/ai-prompt.txt does not exist"))

        # 2. a box without a fetch
        if (has_call or has_file) and not in_scope:
            findings.append((rel, "carries an AI prompt box but nothing in data/ fetches"))
        if (has_call or has_file) and slug in NO_PROMPT_OK:
            findings.append((rel, "carries a box but is listed NO_PROMPT_OK: " + NO_PROMPT_OK[slug]))

        # 3. tone
        if has_call:
            calls_frozen = bool(TONE_FROZEN.search(src))
            if frozen and not calls_frozen:
                findings.append((rel, "must use tone = \"frozen\": " + FROZEN[slug]))
            if calls_frozen and not frozen:
                findings.append((rel, "uses tone = \"frozen\" but is not in the FROZEN table"))

        # 4. stale numbers
        if has_file:
            text = open(prompt, encoding="utf-8", errors="replace").read()
            m = VERIFY_HEAD.search(text)
            if not m:
                findings.append((rel, "prompt has no verification block (CHECK YOUR WORK / WHAT YOU CAN STILL CHECK)"))
            else:
                corpus = chapter_corpus(data)
                loose = sorted(n for n in numbers_in(text[m.end():])
                               if n not in corpus)
                if loose:
                    findings.append((rel, "verification numbers not found in the chapter's data: "
                                     + ", ".join(loose)))

    print("%d chapters checked, %d carrying the box" % (len(briefs), n_boxes))
    if findings:
        print()
        w = max(len(r) for r, _ in findings)
        for rel, msg in findings:
            print("  %-*s  %s" % (w, rel, msg))
        print("\n%d finding(s)" % len(findings))
        return 1
    print("every fetching chapter accounted for")
    return 0


if __name__ == "__main__":
    sys.exit(main())
