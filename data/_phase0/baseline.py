#!/usr/bin/env python3
"""
Phase 0, step 3 -- freeze what the book currently SAYS, so a later phase can
prove it did not change what the book says.

THE PROBLEM THIS SOLVES. Phases 1-4 repoint every chapter at master tables.
A repointing that is subtly wrong -- a join that drops Alaska, a FIPS column
read as numeric, a third-party column included where it was not before -- will
still knit, still look right, and still produce a chapter full of confident
numbers. Nothing fails. That is exactly the failure mode the book's own
introduction is about, and there is no reason to think this project is immune
to it.

So before anything moves: extract every number that currently appears in every
rendered chapter, and store it. After a chapter is migrated, extract again and
diff. A migration that changes no number is safe. A migration that changes a
number must explain which one and why -- and sometimes the answer will be "the
old number was wrong", which is a finding rather than a regression.

Numbers are taken from the RENDERED HTML rather than by re-knitting, because
the rendered files are the artifact the students actually read, and because
re-knitting 59 chapters is slow enough that nobody would run it twice.

Outputs:
  baseline.tsv          one row per rendered chapter: hashes, number counts
  numbers/<lab>.txt     every number in that chapter, in document order
"""

import csv, hashlib, html, os, re, sys

HERE = os.path.dirname(os.path.abspath(__file__))
F26  = os.path.abspath(os.path.join(HERE, "..", ".."))
LABS = os.path.join(F26, "labs")
OUT  = os.path.join(HERE, "numbers")

# A number as a reader would see it: 1,234  ·  48.66  ·  -0.31  ·  4.5%
NUM_RX = re.compile(r"-?\d{1,3}(?:,\d{3})+(?:\.\d+)?|-?\d+\.\d+|-?\d+")
SCRIPT_RX = re.compile(r"<(script|style)\b.*?</\1>", re.S | re.I)
TAG_RX    = re.compile(r"<[^>]+>")


def visible_text(path):
    raw = open(path, encoding="utf-8", errors="replace").read()
    raw = SCRIPT_RX.sub(" ", raw)          # drop D3 payloads and CSS
    txt = TAG_RX.sub(" ", raw)
    return html.unescape(txt)


def main():
    os.makedirs(OUT, exist_ok=True)
    rows = []
    for lab in sorted(os.listdir(LABS)):
        d = os.path.join(LABS, lab)
        if not os.path.isdir(d) or lab.startswith("_"):
            continue
        htmls = [f for f in sorted(os.listdir(d)) if f.endswith("-brief.html")]
        if not htmls:
            rows.append(dict(lab=lab, file="", status="NO RENDERED HTML",
                             bytes=0, sha256="", n_numbers=0, numbers_sha=""))
            continue
        for f in htmls:
            p = os.path.join(d, f)
            body = visible_text(p)
            nums = NUM_RX.findall(body)
            with open(os.path.join(OUT, "%s.txt" % lab), "w", encoding="utf-8") as fh:
                fh.write("\n".join(nums))
            rows.append(dict(
                lab=lab, file=f, status="ok",
                bytes=os.path.getsize(p),
                sha256=hashlib.sha256(open(p, "rb").read()).hexdigest()[:16],
                n_numbers=len(nums),
                numbers_sha=hashlib.sha256("\n".join(nums).encode()).hexdigest()[:16]))

            pdf = p.replace(".html", ".pdf")
            rows[-1]["pdf_bytes"] = os.path.getsize(pdf) if os.path.exists(pdf) else 0

    fields = ["lab", "file", "status", "bytes", "pdf_bytes", "sha256",
              "n_numbers", "numbers_sha"]
    with open(os.path.join(HERE, "baseline.tsv"), "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=fields, delimiter="\t", extrasaction="ignore")
        w.writeheader(); w.writerows(rows)

    ok = [r for r in rows if r["status"] == "ok"]
    missing = [r for r in rows if r["status"] != "ok"]
    print("chapters with rendered HTML : %d" % len(ok))
    print("numbers frozen              : %s" % f"{sum(r['n_numbers'] for r in ok):,}")
    print("missing PDF                 : %d" % sum(1 for r in ok if not r.get("pdf_bytes")))
    if missing:
        print("NO RENDERED HTML            : %s" % ", ".join(r["lab"] for r in missing))


if __name__ == "__main__":
    main()
