#!/usr/bin/env python3
"""The scan. Run deliberately, not as part of a build.

    python3 probe.py 2026-08-13

Requests the address at which each major survey archive keeps the thing this
book actually wants, and records what a script gets back -- status, content
type, size, and the two headers that say a bot wall answered instead of a
server: Cloudflare's `cf-mitigated` and AWS WAF's `x-amzn-waf-action`.

WHAT COUNTS AS AN ADDRESS. Only endpoints that are the DATA or its catalogue
entry. Marketing and landing pages are excluded on purpose: Pew's and Roper's
front pages both answer 200, and reporting that beside a 47 MB file download
would say the two archives behave alike when they do not. The previous chapter
learned this the hard way -- a Maine link ending `.txt` turned out to be an
absentee file, not the voter list -- so the rule here is that an address earns
its place by being where the data is.

WHY THE RESULTS ARE FROZEN. The chapter quotes these status codes, and a bot
wall is exactly the kind of infrastructure that changes without notice. Re-run,
date the output, and read what moved.
"""

import csv
import json
import sys
import urllib.error
import urllib.request

UA = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 " \
     "(KHTML, like Gecko) Chrome/120.0 Safari/537.36"
TIMEOUT = 40

# archive, what the address is, url
ENDPOINTS = [
    ("CPS (Census Bureau)", "the voting supplement table itself",
     "https://www2.census.gov/programs-surveys/cps/tables/time-series/"
     "voting-historical-time-series/hst_vote01.xlsx"),
    ("GSS (NORC)", "the cumulative file itself",
     "https://gss.norc.org/content/dam/gss/get-the-data/documents/stata/GSS_stata.zip"),
    ("ANES", "the cumulative file's download page",
     "https://electionstudies.org/data-center/anes-time-series-cumulative-data-file/"),
    ("CES (Harvard Dataverse)", "the dataset's metadata API",
     "https://dataverse.harvard.edu/api/datasets/:persistentId/"
     "?persistentId=doi:10.7910/DVN/X11EP6"),
    ("Harvard Dataverse, another dataset", "a second dataset's metadata API",
     "https://dataverse.harvard.edu/api/datasets/:persistentId/"
     "?persistentId=doi:10.7910/DVN/DGUMFI"),
    ("ICPSR", "a study's catalogue page",
     "https://www.icpsr.umich.edu/web/ICPSR/studies/13"),
]


HEADERS = {
    "User-Agent": UA,
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language": "en-US,en;q=0.9",
}

# TWO CLIENTS, THE SAME HEADERS. This is the whole design of the scan, and it
# is not padding. A bot wall that read the request would answer both the same
# way; these do not. Sending byte-identical headers from curl and from Python's
# urllib gets different answers out of at least one archive, which means what is
# being judged is not the request but the client making it -- how it negotiates
# TLS, which no amount of setting a User-Agent will change.
CLIENTS = ("curl", "python-urllib")


def probe_urllib(url):
    req = urllib.request.Request(url, headers=HEADERS)
    try:
        with urllib.request.urlopen(req, timeout=TIMEOUT) as r:
            return str(r.status), dict(r.headers), len(r.read())
    except urllib.error.HTTPError as e:
        try:
            body = e.read()
        except Exception:
            body = b""
        return str(e.code), dict(e.headers or {}), len(body)
    except Exception:
        return "000", {}, 0


def probe_curl(url):
    import subprocess, tempfile, os
    hdr = tempfile.NamedTemporaryFile(delete=False)
    hdr.close()
    cmd = ["curl", "-s", "-o", os.devnull, "-D", hdr.name, "--max-time", str(TIMEOUT),
           "-w", "%{http_code}|%{size_download}"]
    for k, v in HEADERS.items():
        cmd += ["-H", f"{k}: {v}"]
    cmd.append(url)
    try:
        out = subprocess.run(cmd, capture_output=True, text=True, timeout=TIMEOUT + 10).stdout
        code, size = out.split("|")
        hdrs = {}
        with open(hdr.name, encoding="utf-8", errors="replace") as fh:
            for line in fh:
                if ":" in line:
                    k, _, v = line.partition(":")
                    hdrs[k.strip()] = v.strip()
        return code, hdrs, int(size)
    except Exception:
        return "000", {}, 0
    finally:
        os.unlink(hdr.name)


def main():
    if len(sys.argv) != 2:
        sys.exit("usage: python3 probe.py YYYY-MM-DD")
    date = sys.argv[1]
    rows = []
    for archive, what, url in ENDPOINTS:
        for client in CLIENTS:
            status, hdrs, size = (probe_curl(url) if client == "curl"
                                  else probe_urllib(url))
            low = {k.lower(): v for k, v in (hdrs or {}).items()}
            wall = low.get("cf-mitigated") or low.get("x-amzn-waf-action") or ""
            rows.append([archive, what, url, client, status,
                         low.get("content-type", "").split(";")[0], size,
                         low.get("cf-mitigated", ""), low.get("x-amzn-waf-action", ""),
                         low.get("server", "")])
            print(f"  {archive:36s} {client:14s} {status:>3s}  {size:>10,d} bytes  {wall}")

    with open(f"raw/probe-{date}.csv", "w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh, lineterminator="\n")
        w.writerow(["archive", "what_the_address_is", "url", "client", "http",
                    "content_type", "bytes", "cf_mitigated", "waf_action", "server"])
        w.writerows(rows)
    print(f"\nwrote raw/probe-{date}.csv")


if __name__ == "__main__":
    main()
