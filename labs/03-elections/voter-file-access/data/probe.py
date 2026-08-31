#!/usr/bin/env python3
"""The scan. Run deliberately, not as part of a build.

    python3 probe.py 2026-09-01

For each of the 51 jurisdictions, take the "Access/Purchase List" address out of
the NCSL table committed in raw/, request it the way a browser would, and record
what comes back. Redirects ARE followed, because the question is what a person
ends up looking at rather than what the first hop says.

Writes two dated files into raw/, both meant to be committed and then left
alone:

    probe-<date>.psv          state | http | content-type | bytes | final url
    probe-signals-<date>.csv  the same, plus five yes/no readings taken from the
                              body -- is it a PDF, does it ask you to log in,
                              does it present an application, does it mention a
                              fee, and does it link a data file at all

WHY THE RESULTS ARE FROZEN RATHER THAN RE-PROBED ON EVERY BUILD. The chapter
quotes this funnel, and fifty-one state websites will not return the same thing
next month -- 8 of the 51 links were already dead when the first scan ran. A
chapter whose numbers move each time somebody knits it is reporting the weather,
not a measurement. Re-run this when you want a NEW scan, date it, and read what
changed against the old one; that comparison is a finding in its own right.

WHAT THE SIGNALS ARE AND ARE NOT. They are keyword readings of a page's HTML.
"application" means the word appears, not that an application is required.
"data_links" counts hyperlinks ending in a data extension, which is a fact about
markup and nothing more: in the 2026-08-13 scan three states had one, and
opening them showed one voter list, one absentee file and one candidate
directory. Never report a data link as a downloadable voter file without opening
it.
"""

import csv
import os
import re
import sys
import urllib.error
import urllib.request

UA = "Mozilla/5.0 (Macintosh)"
TIMEOUT = 30
NCSL = "raw/ncsl-access-table.html"
PAT = re.compile(r'href="([^"]+)"[^>]*>\s*([A-Za-z .]+?)\s*Access/Purchase List', re.I)
DATA_LINK = re.compile(r'href="[^"]+\.(?:csv|zip|txt|tsv|xlsx)"', re.I)

LOGIN = ("sign in", "log in", "login", "username", "password")
APPLY = ("application", "request form", "order form", "affidavit", "notarized")
FEE = ("fee", "cost", "payment", "purchase")


def addresses():
    """The 51 state -> URL pairs, in the order the survey lists them."""
    import html as _html
    with open(NCSL, encoding="utf-8", errors="replace") as fh:
        page = fh.read()
    seen, out = set(), []
    for m in PAT.finditer(page):
        state = _html.unescape(m.group(2)).strip()
        if state in seen:
            continue
        seen.add(state)
        out.append((state, _html.unescape(m.group(1)).strip()))
    return out


def fetch(url):
    """(status, content_type, body) -- a failure is an outcome, not an error."""
    req = urllib.request.Request(url, headers={"User-Agent": UA})
    try:
        with urllib.request.urlopen(req, timeout=TIMEOUT) as r:
            body = r.read()
            return str(r.status), r.headers.get("Content-Type", ""), body
    except urllib.error.HTTPError as e:
        # 403 and 404 are the interesting answers, so they are recorded rather
        # than raised past.
        try:
            body = e.read()
        except Exception:
            body = b""
        return str(e.code), e.headers.get("Content-Type", "") if e.headers else "", body
    except Exception as e:
        return "000", type(e).__name__, b""


def main():
    if len(sys.argv) != 2:
        sys.exit("usage: python3 probe.py YYYY-MM-DD")
    date = sys.argv[1]
    rows, signals = [], []
    for state, url in addresses():
        status, ctype, body = fetch(url)
        text = body.decode("utf-8", "replace").lower()
        rows.append([state, status, ctype, str(len(body)), url])
        signals.append([
            state, status, ctype.split(";")[0], len(body),
            int("pdf" in ctype.lower()),
            int(any(k in text for k in LOGIN)),
            int(any(k in text for k in APPLY)),
            int(any(k in text for k in FEE)),
            len(DATA_LINK.findall(text)),
        ])
        print(f"  {state:22s} {status:>3s}  {len(body):>9,d} bytes")

    with open(f"raw/probe-{date}.psv", "w", encoding="utf-8") as fh:
        for r in rows:
            fh.write("|".join(r) + "\n")
    with open(f"raw/probe-signals-{date}.csv", "w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh, lineterminator="\n")
        w.writerow(["state", "http", "content_type", "bytes", "is_pdf",
                    "login", "application", "fee", "data_links"])
        w.writerows(signals)

    print(f"\nwrote raw/probe-{date}.psv and raw/probe-signals-{date}.csv")
    print("A data link is not a voter file. Open every one before believing it.")


if __name__ == "__main__":
    main()
