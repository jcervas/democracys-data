#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# The race and ethnicity of members of Congress, from the Office of the
# Historian of the U.S. House.
#
# WHY THIS FILE EXISTS AT ALL, AND WHY IT IS A SCRAPER
#
# There is no government data file that records the race of a member of
# Congress. Not in the Biographical Directory's bulk data, not in Voteview,
# not in the Clerk's election statistics, not in the unitedstates/
# congress-legislators corpus this build also uses -- which carries a member's
# Twitter handle and their Wikipedia page and their term end date, and does not
# carry this. The attribute that the entire minority-representation literature
# uses as its dependent variable is published by the House Historian as a
# *search filter on a photo gallery*, and that is the only machine-reachable
# form of it there is.
#
#   https://history.house.gov/People/Search?filter=1     Black Americans
#   https://history.house.gov/People/Search?filter=11    Hispanic Americans
#
# Those two filters back the Historian's "Black Americans in Congress" and
# "Hispanic Americans in Congress" publications, which are the standard
# institutional reference. They are curated by historians, they are
# retrospective, and they carry no documented inclusion rule.
#
# HOW THE BIOGUIDE ID IS RECOVERED. The gallery does not print an identifier.
# It prints a photograph, and the photograph's filename is the member's
# Bioguide ID:
#
#   .../uploadedImages/People/Listing/A/A000370.jpg  ->  A000370  (Alma Adams)
#
# That is the join key to every other congressional dataset. Members with no
# listing photograph -- 16 of the 371 -- have no such filename, and for those
# the script opens the detail page and reads the ID out of the page body. Both
# routes are exercised on every run and the counts are printed.
#
# PAGINATION. Twelve results a page, and `CurrentPage` alone does nothing: the
# server ignores it unless the request also carries `Command=<n>` and the
# opaque `PreviousSearch` state string from the page you came from. Passing a
# hand-built `PreviousSearch` works for one filter and silently returns page
# one forever for the other, so this follows the pager's own hrefs instead.
# The first version of this script "scraped" 204 rows that were seventeen
# copies of the same twelve people, and the only reason that was caught is the
# reported result count printed at the top of the page. It is checked here.
#
# OUTPUT
#   raw/historian.csv   name, bioguide, detail_id, list ("black" | "hispanic")
# ---------------------------------------------------------------------------

import csv
import html
import os
import re
import sys
import time
import urllib.request

# Fetches go through provenance.py, which records url, bytes, hash and row
# count in PROVENANCE.tsv and prints a banner when a source moves under us --
# a URL that still returns 200 and no longer means what it meant.
sys.path.insert(0, os.path.join("..", "..", "..", "_lib"))
import provenance as prov

UA = {"User-Agent": "Mozilla/5.0 (84-355 Democracy's Data; course dataset build)"}
BASE = "https://history.house.gov"
FILTERS = [(1, "black"), (11, "hispanic")]
PAGE_SIZE = 12


def get(url):
    for attempt in range(4):
        try:
            # UA passed through: this site refuses a request without one.
            return prov.fetch_bytes(url, timeout=60, headers=UA).decode("utf8", "replace")
        except Exception as e:                                   # noqa: BLE001
            if attempt == 3:
                raise
            print("  retry %d after %s" % (attempt + 1, e), file=sys.stderr)
            time.sleep(2 * (attempt + 1))


def parse(page):
    """Every result tile on one page of the gallery."""
    out = []
    for tile in re.split(r'<div class="result', page)[1:]:
        name = re.search(r'<span class="name">([^<]+)</span>', tile)
        if not name:
            continue
        photo = re.search(r"/Listing/[A-Z]/([A-Z]\d{6})\.jpg", tile)
        detail = re.search(r"/People/Detail/(\d+)", tile)
        out.append({
            "name": html.unescape(name.group(1)).strip(),
            "bioguide": photo.group(1) if photo else "",
            "detail_id": detail.group(1) if detail else "",
        })
    return out


def scrape(filter_id):
    page = get("%s/People/Search?filter=%d&CurrentPage=1&SortOrder=LastName"
               "&ResultType=Grid" % (BASE, filter_id))
    m = re.search(r"of ([\d,]+) results", page)
    if not m:
        sys.exit("FATAL: no result count on filter=%d; the page layout moved." % filter_id)
    total = int(m.group(1).replace(",", ""))
    rows = parse(page)
    for p in range(2, -(-total // PAGE_SIZE) + 1):
        # The pager's own href carries the PreviousSearch state the server
        # requires. Building it by hand is what broke the first version.
        link = re.search(r'href="(/People/Search\?filter=%d&amp;[^"]*Command=%d)"'
                         % (filter_id, p), page)
        if not link:
            sys.exit("FATAL: pager link to page %d missing on filter=%d." % (p, filter_id))
        page = get(BASE + html.unescape(link.group(1)))
        rows += parse(page)
        time.sleep(0.35)

    seen, unique = set(), []
    for r in rows:
        key = r["detail_id"] or r["name"]
        if key in seen:
            continue
        seen.add(key)
        unique.append(r)

    # The count printed by the site is the only external check on the scrape.
    if len(unique) != total:
        sys.exit("FATAL: filter=%d reports %d results, scrape produced %d unique."
                 % (filter_id, total, len(unique)))
    return unique, total


def main():
    out = []
    for filter_id, label in FILTERS:
        rows, total = scrape(filter_id)
        missing = [r for r in rows if not r["bioguide"]]
        for r in missing:
            body = get("%s/People/Detail/%s" % (BASE, r["detail_id"]))
            found = re.search(r"\b([A-Z]\d{6})\b", body)
            r["bioguide"] = found.group(1) if found else ""
            time.sleep(0.3)
        still = sum(1 for r in rows if not r["bioguide"])
        if still:
            sys.exit("FATAL: %d %s entries have no Bioguide ID." % (still, label))
        for r in rows:
            r["list"] = label
        out += rows
        print("  %-9s %3d reported, %3d scraped, %2d IDs recovered from detail pages"
              % (label, total, len(rows), len(missing)))

    with open("raw/historian.csv", "w", newline="") as fh:
        w = csv.DictWriter(fh, ["list", "name", "bioguide", "detail_id"])
        w.writeheader()
        w.writerows(out)
    print("  wrote raw/historian.csv, %d rows" % len(out))


if __name__ == "__main__":
    main()
    # Anything fetched above is now in PROVENANCE.tsv; say so if a
    # source moved. Inside the __main__ guard so importing this
    # module for its parser does not print a banner.
    prov.report()
