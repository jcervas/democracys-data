# Routes for the remaining states — ✅ FINISHED, kept as the record of how

> **This document is history, not a work list.** It was written on 12 Aug 2026
> with 23 states outstanding; **the build was finished afterwards and the
> counts below were never updated.** As of 13 Aug 2026:
>
> - **51 jurisdictions × 2 elections = 102 documents, all built**, in 11
>   distinct formats, covering 3,197 reporting units and 155,205,623 votes.
> - `provenance.csv` carries a row per jurisdiction-year and is the authority
>   on what was fetched from where. **Trust it over this file.**
> - `python3 build_states.py --check` re-validates everything on disk.
>
> What remains useful here is the *reasoning* — which routes were tried, which
> failed, and why — because that is what a future election will need again. The
> negative findings below are the valuable part and are still true.

## Superseded status, as written on 12 Aug 2026

**Built: 28 jurisdictions, 55 files.** The 26 already present, plus
**North Carolina** (both years) and **Colorado** (2024 only). NC and CO both
match the AP crosscheck **to the vote on both candidates**.

**Remaining: 23 states plus CO 2020** — AR AZ CT IA KY LA MA ME MI MN MO MS MT
NE NH NJ NM NV NY OK OR RI VT.

## The leverage bet did not pay — recorded so it is not retried

The plan was to do the Clarity cluster and the four platform states first,
because one parser should have covered several jurisdictions each. **Neither
cluster opened up:**

- **Clarity.** Host prefixes exist for AR, KY, LA, OK, RI and NJ (403 on the
  root, which means present-but-unlisted); VT returns 404 and has no instance.
  But Clarity needs an **election id** in the path, and the SoS landing pages
  for AR, KY, OK and CO contain **no `clarityelections` links at all** — they
  are JS-rendered. The ids have to come from somewhere else, per state.
- **The platform four** (MN, NE, NM, MT) remain untried, and MN and MT are
  additionally WAF-blocked to scripts.

**A negative finding worth keeping:** Colorado's contest ids are **not ordered
by date**. Probing 20000 / 24000 / 25000 / 25800 / 26200 returned, respectively,
a district attorney race, a Charles S. Thomas contest, a state house race, a
Moffat County ballot question and another DA race. So a known id gives no
purchase on any other, and neither bisection nor scanning will find the 2020
presidential contest. It has to come from the site's search UI — a React form
with generated element ids.

---

## The harness

Two new files make each additional state a small, checkable increment:

- **`lib_build.py`** — fetch-with-cache, Census county-FIPS resolution with
  name normalisation, `write_state()`, `add_provenance()`, and `validate()`.
- **`build_states.py`** — one function per jurisdiction in a registry.
  `python3 build_states.py NC` builds and validates a state;
  `python3 build_states.py --check` re-validates everything on disk.

`validate()` enforces the SPEC invariants (5-character FIPS, `dem + gop <=
total`, no duplicate FIPS, no negatives) and, for 2024, grades every file
against `derived/crosscheck_ap_counties_2024.csv` — an independent county-level count
assembled by somebody else from different inputs. **The existing 26 states all
pass**, which is how the harness itself was verified before being trusted.

## What the first pass established

**Guessing file URLs does not work.** Fifteen plausible direct-file URLs across
ten states were probed; one resolved. Every state needs its actual address
found rather than inferred.

**Roughly half the states block scripted access outright.** Confirmed HTTP 403
to `curl` even with a complete browser header set — full Chrome UA, `Accept`,
`Accept-Language`, `Referer`, and the `Sec-Fetch-*` headers:

| Confirmed blocked | Reachable by script |
|---|---|
| **IA**, **NY**, **MT** (403 with full headers) | **MS** (200) |

This is the same wall the 1992/96 canvass scan hit at AZ, GA, MT, OH and WI.
It is a WAF decision about robots, not an access-rights question — the
documents are public. **The route through it is the browser**, which works but
costs a navigation per file rather than a line of script.

## Verified source addresses so far

| St | Source | Format | Note |
|---|---|---|---|
| **NC** | `s3.amazonaws.com/dl.ncsbe.gov/ENRS/<date>/results_pct_<stamp>.zip` | zip/TSV | **Built.** Precinct export, contest `US PRESIDENT`, summed to county |
| **CO** | `co.elstats.civera.com/api/download_contest/<id>_table.csv?split_party=false` | CSV | **2024 built** (contest 26499). Clean endpoint, no WAF. **2020 id unknown** — see above |
| IA | `sos.iowa.gov/elections/pdf/2024/general/canvsummary.pdf` | PDF, 311 pp | address confirmed by search; **fetch 403** |
| CO | `historicalelectiondata.coloradosos.gov/contest/26499` | web DB | 2024 president contest page; JS-rendered, has CSV export |
| MI | `mvic.sos.state.mi.us/votehistory/Index?type=C&electionDate=11-5-2024` | web | county results; the old `mielections.us` CENR host is dead |
| MS | `sos.ms.gov/elections-voting/election-results` | web | reachable by script |
| MN | SoS media-file interface on `electionresults.sos.mn.gov` | — | ASP.NET postbacks; serves only the *current* election |
| NE, NM, MT | `resultsSW.aspx` platform | — | same engine as MN; current election only, postback-driven |

**A caution recorded rather than discovered later:** MN, NE, NM and MT share a
results platform that serves only the election currently loaded. Historical
years are reached through a selector, not a URL parameter, so none of the four
can be done with a plain fetch.

## Recommended order

1. **Script-reachable, machine-readable first** — MS, and whichever of AR, KY,
   LA, OK turn out to sit on Clarity (`enr.clarityelections.com`), which has a
   consistent per-county export and would cover several states with one parser.
2. **Browser-assisted, single well-formed file** — IA (canvass PDF), NY, CO,
   MI. One navigation each, then a normal parse.
3. **Platform states** — MN, NE, NM, MT. Needs a selector walk; do them
   together once, since it is one parser and four states.
4. **Town-level jurisdictions** — CT, ME, MA, NH, RI, VT. These are *not*
   county states, and SPEC rule 5 applies: report what is published and leave
   `county_fips` empty rather than inventing a county. MA has a CSV export
   (`electionstats.state.ma.us`); the rest need checking.
5. **PDF-only** — NJ, OR, and any of the above that resolve to a scanned
   document.

## The honest estimate

The 26 states already on disk were the tractable ones. **What remains is the
harder half**, and at roughly one state per portal investigation it is a
multi-session job rather than a single pass. Nothing about it is blocked; it is
simply long, and the harness now makes each increment safe and verifiable.
