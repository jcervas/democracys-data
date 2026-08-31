#!/usr/bin/env python3
"""
Phase 0, step 2 -- who owns every source the corpus downloads from.

WHY THIS EXISTS. The book renders to jcervas.github.io, a public site. Anything
committed is therefore republished to third parties. That is fine for a public
record and fine for an MIT-licensed repository; it is a licence violation for
ICPSR, and it is unsettled for a GitHub repository that declares no licence at
all. Until Aug 2026 no source in this corpus had ever been checked.

The `jaytimm/PresElectionResults` repository -- which supplies pres_state
1864-2024, already built, already committed, already published -- declares no
licence. It was found by accident. This script exists so the rest are found on
purpose.

Outputs `licences.tsv` and `licences.md`. Reads build scripts only; changes
nothing.
"""

import json, os, re, ssl, sys, urllib.request
from collections import defaultdict
from urllib.parse import urlparse

HERE = os.path.dirname(os.path.abspath(__file__))
F26  = os.path.abspath(os.path.join(HERE, "..", ".."))
LABS = os.path.join(F26, "labs")
URL_RX = re.compile(r"https?://[^\s\"'`,;)\]}<>]+")

# Host -> (kind, licence, redistributable, note). Redistributable answers the
# only question that matters here: MAY THIS BE COMMITTED TO A PUBLIC REPO?
KNOWN = {
    "icpsr.umich.edu":   ("consortium", "ICPSR Terms of Use", "NO",
                          "Redistribution prohibited absent written agreement. Membership is a right to receive, not to republish."),
    "uselectionatlas.org": ("commercial", "proprietary", "NO",
                          "Leip: republication requires a contract."),
    "library.cqpress.com": ("commercial", "subscription", "NO",
                          "CMU holds no V&E subscription (3-month trial of other CQ products only)."),
    "dataverse.harvard.edu": ("repository", "per-dataset", "CHECK",
                          "MEDSL county presidential is CC0 1.0; other deposits vary."),
    "doi.org":           ("repository", "per-dataset", "CHECK", "Resolve and check the target."),
    "electionstudies.org": ("academic", "ANES terms", "CHECK",
                          "Free registration; check redistribution clause."),
    "gss.norc.org":      ("academic", "GSS terms", "CHECK", "Generally open; confirm."),
    "voteview.com":      ("academic", "open", "LIKELY", "Long-standing free public release; no explicit licence found."),
    "openpolicing.stanford.edu": ("academic", "ODbL-ish", "CHECK", "Stanford Open Policing; check terms."),
    "stacks.stanford.edu": ("academic", "per-item", "CHECK", ""),
    "dsl.richmond.edu":  ("academic", "CC BY-NC-SA", "CHECK", "Mapping Inequality: non-commercial share-alike."),
    "sentencingproject.org": ("ngo", "report", "CHECK", "Figures from a published report."),
    "brookings.edu":     ("ngo", "report", "CHECK", ""),
    "polymarket.com":    ("commercial", "API terms", "CHECK", ""),
    "gamma-api.polymarket.com": ("commercial", "API terms", "CHECK", ""),
    "clob.polymarket.com": ("commercial", "API terms", "CHECK", ""),
    "wikimedia.org":     ("foundation", "CC BY-SA / open API", "LIKELY", "Pageview API data is factual and openly licensed."),
    "the-downballot.com": ("commercial", "unclear", "CHECK", "Editorial site; presidential-by-CD figures."),
    "docs.google.com":   ("third-party", "unknown", "CHECK", "A sheet someone published; owner unknown."),
    "interactives.ap.org": ("commercial", "AP content", "CHECK", "AP tracker embed."),
}

GOV_SUFFIX = (".gov", ".mil")


def gov_note(host):
    if "census.gov" in host:
        return ("government", "US Gov — public domain", "YES",
                "17 USC 105: works of the US government are not copyrightable.")
    return ("government", "US Gov / state public record", "YES",
            "Federal works are public domain; state election records are public records.")


def github_licence(owner, repo):
    url = "https://api.github.com/repos/%s/%s" % (owner, repo)
    req = urllib.request.Request(url, headers={"User-Agent": "phase0-licence-audit",
                                               "Accept": "application/vnd.github+json"})
    try:
        ctx = ssl.create_default_context()
        with urllib.request.urlopen(req, timeout=25, context=ctx) as r:
            d = json.load(r)
        lic = d.get("license")
        if not lic or not lic.get("spdx_id") or lic.get("spdx_id") == "NOASSERTION":
            return ("NONE DECLARED", "NO",
                    "No licence file. Default in most jurisdictions is all rights reserved.")
        spdx = lic["spdx_id"]
        permissive = spdx in {"MIT", "Apache-2.0", "BSD-2-Clause", "BSD-3-Clause",
                              "CC0-1.0", "Unlicense", "CC-BY-4.0", "ISC"}
        return (spdx, "YES" if permissive else "CHECK", lic.get("name", ""))
    except Exception as e:
        return ("lookup failed", "CHECK", "%s" % type(e).__name__)


def main():
    hosts = defaultdict(lambda: {"n": 0, "example": "", "labs": set(), "repos": set()})
    # Repos are tracked separately from hosts, keyed by (owner, repo), because a
    # repository is the licensable unit -- not the host. Attributing labs by host
    # merges every GitHub repo's users into one list and reports them all against
    # each repo, which is simply false.
    repo_labs = defaultdict(set)
    repo_uses = defaultdict(int)
    for root, dirs, names in os.walk(LABS):
        if "_archive" in root or "_phase0" in root:
            continue
        for nm in names:
            if not nm.endswith((".R", ".r", ".py")):
                continue
            p = os.path.join(root, nm)
            lab = os.path.relpath(p, LABS).split(os.sep)[0]
            try:
                txt = open(p, encoding="utf-8", errors="replace").read()
            except Exception:
                continue
            for u in URL_RX.findall(txt):
                u = u.rstrip(".,);:'\"")
                h = (urlparse(u).netloc or "").lower()
                if not h:
                    continue
                rec = hosts[h]
                rec["n"] += 1
                rec["labs"].add(lab)
                if not rec["example"]:
                    rec["example"] = u
                m = re.match(r"https?://(?:raw\.githubusercontent\.com|github\.com)/([^/]+)/([^/]+)", u)
                if m:
                    key = (m.group(1), m.group(2).replace(".git", ""))
                    rec["repos"].add(key)
                    repo_labs[key].add(lab)
                    repo_uses[key] += 1

    rows = []
    # One row per repository, deduplicated across github.com and
    # raw.githubusercontent.com, with labs and uses counted for that repo alone.
    for (owner, repo) in sorted(repo_labs):
        lic, redis, note = github_licence(owner, repo)
        rows.append(dict(host="github.com", kind="github",
                         source="%s/%s" % (owner, repo), licence=lic,
                         redistributable=redis, note=note,
                         uses=repo_uses[(owner, repo)],
                         labs=",".join(sorted(repo_labs[(owner, repo)]))))

    for host, rec in sorted(hosts.items(), key=lambda kv: -kv[1]["n"]):
        if rec["repos"]:
            continue                      # already emitted, per repository
        base = ".".join(host.split(".")[-2:])
        if host.endswith(GOV_SUFFIX) or base.endswith(GOV_SUFFIX):
            kind, lic, redis, note = gov_note(host)
        elif host in KNOWN:
            kind, lic, redis, note = KNOWN[host]
        elif base in KNOWN:
            kind, lic, redis, note = KNOWN[base]
        else:
            kind, lic, redis, note = ("unclassified", "unknown", "CHECK", "")
        rows.append(dict(host=host, kind=kind, source=host, licence=lic,
                         redistributable=redis, note=note, uses=rec["n"],
                         labs=",".join(sorted(rec["labs"]))))

    import csv
    fields = ["host", "kind", "source", "licence", "redistributable", "uses", "labs", "note"]
    with open(os.path.join(HERE, "licences.tsv"), "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=fields, delimiter="\t", extrasaction="ignore")
        w.writeheader(); w.writerows(rows)

    order = {"NO": 0, "CHECK": 1, "LIKELY": 2, "YES": 3}
    rows.sort(key=lambda r: (order.get(r["redistributable"], 9), -r["uses"]))
    tally = defaultdict(int)
    for r in rows:
        tally[r["redistributable"]] += 1

    md = ["# Phase 0 — licence audit",
          "",
          "Every host any build script downloads from, and whether what it returns",
          "may be **committed to a public repository**. The book renders to",
          "`jcervas.github.io`, so committing is republishing.",
          "",
          "| Verdict | Sources |", "|---|---:|"]
    for k in ("NO", "CHECK", "LIKELY", "YES"):
        md.append("| **%s** | %d |" % (k, tally.get(k, 0)))
    md += ["", "## Sources, worst first", "",
           "| Verdict | Source | Licence | Uses | Labs | Note |",
           "|---|---|---|---:|---|---|"]
    for r in rows:
        labs = r["labs"]
        if len(labs) > 46:
            labs = labs[:44] + "…"
        md.append("| **%s** | `%s` | %s | %d | %s | %s |" %
                  (r["redistributable"], r["source"], r["licence"], r["uses"],
                   labs, r["note"]))
    with open(os.path.join(HERE, "licences.md"), "w", encoding="utf-8") as fh:
        fh.write("\n".join(md) + "\n")

    print("hosts: %d   NO:%d CHECK:%d LIKELY:%d YES:%d"
          % (len(rows), tally.get("NO", 0), tally.get("CHECK", 0),
             tally.get("LIKELY", 0), tally.get("YES", 0)))
    for r in rows:
        if r["redistributable"] in ("NO",):
            print("  NO  ->", r["source"], "|", r["licence"], "|", r["labs"])


if __name__ == "__main__":
    main()
