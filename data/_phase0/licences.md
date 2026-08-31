# Phase 0 — licence audit

Every host any build script downloads from, and whether what it returns
may be **committed to a public repository**. The book renders to
`jcervas.github.io`, so committing is republishing.

| Verdict | Sources |
|---|---:|
| **NO** | 2 |
| **CHECK** | 16 |
| **LIKELY** | 2 |
| **YES** | 16 |

## Sources, worst first

| Verdict | Source | Licence | Uses | Labs | Note |
|---|---|---|---:|---|---|
| **NO** | `APM-Reports/jury-data` | NONE DECLARED | 4 | jury-selection | No licence file. Default in most jurisdictions is all rights reserved. |
| **NO** | `jaytimm/PresElectionResults` | NONE DECLARED | 4 | electoral-map,historical-campaigns,redistric… | No licence file. Default in most jurisdictions is all rights reserved. |
| **CHECK** | `electionstudies.org` | ANES terms | 10 | abortion-opinion,anes,ideology,party-id,surv… | Free registration; check redistribution clause. |
| **CHECK** | `dsl.richmond.edu` | CC BY-NC-SA | 6 | redlining | Mapping Inequality: non-commercial share-alike. |
| **CHECK** | `docs.google.com` | unknown | 3 | house-competition,retirements | A sheet someone published; owner unknown. |
| **CHECK** | `interactives.ap.org` | AP content | 3 | campaign-visits | AP tracker embed. |
| **CHECK** | `gss.norc.org` | GSS terms | 3 | gss-confidence | Generally open; confirm. |
| **CHECK** | `unitedstates.github.io` | unknown | 2 | election-night |  |
| **CHECK** | `www.sentencingproject.org` | report | 2 | disenfranchisement | Figures from a published report. |
| **CHECK** | `scdb.wustl.edu` | unknown | 2 | scdb |  |
| **CHECK** | `www.brookings.edu` | report | 2 | retirements |  |
| **CHECK** | `stacks.stanford.edu` | per-item | 2 | policing |  |
| **CHECK** | `gamma-api.polymarket.com` | API terms | 2 | models-markets |  |
| **CHECK** | `clob.polymarket.com` | API terms | 2 | models-markets |  |
| **CHECK** | `s3.amazonaws.com` | unknown | 2 | vote-targeting |  |
| **CHECK** | `doi.org` | per-dataset | 2 | house-competition | Resolve and check the target. |
| **CHECK** | `openpolicing.stanford.edu` | ODbL-ish | 1 | policing | Stanford Open Policing; check terms. |
| **CHECK** | `www.the-downballot.com` | unclear | 1 | house-competition | Editorial site; presidential-by-CD figures. |
| **LIKELY** | `voteview.com` | open | 8 | dw-nominate,officeholder-age,retirements,rol… | Long-standing free public release; no explicit licence found. |
| **LIKELY** | `wikimedia.org` | CC BY-SA / open API | 2 | media-attention | Pageview API data is factual and openly licensed. |
| **YES** | `www2.census.gov` | US Gov — public domain | 58 | apportionment,areal-units,census,census-geog… | 17 USC 105: works of the US government are not copyrightable. |
| **YES** | `www.fec.gov` | US Gov / state public record | 8 | campaign-finance,independent-expenditures,re… | Federal works are public domain; state election records are public records. |
| **YES** | `www.eac.gov` | US Gov / state public record | 6 | eavs,residual-votes | Federal works are public domain; state election records are public records. |
| **YES** | `api.census.gov` | US Gov — public domain | 6 | census-api,policing | 17 USC 105: works of the US government are not copyrightable. |
| **YES** | `results.sos.ga.gov` | US Gov / state public record | 6 | ga-precinct-returns,wind-map | Federal works are public domain; state election records are public records. |
| **YES** | `tonmcg/US_County_Level_Election_Results_08-24` | MIT | 4 | data-sources,mapping,wind-map | MIT License |
| **YES** | `history.house.gov` | US Gov / state public record | 4 | house-competition,midterm-loss | Federal works are public domain; state election records are public records. |
| **YES** | `www.elections.alaska.gov` | US Gov / state public record | 3 | cast-vote-records | Federal works are public domain; state election records are public records. |
| **YES** | `lda.gov` | US Gov / state public record | 2 | lobbying | Federal works are public domain; state election records are public records. |
| **YES** | `data.cdc.gov` | US Gov / state public record | 2 | officeholder-age | Federal works are public domain; state election records are public records. |
| **YES** | `www.cdc.gov` | US Gov / state public record | 2 | officeholder-age | Federal works are public domain; state election records are public records. |
| **YES** | `www.census.gov` | US Gov — public domain | 2 | apportionment,section-203 | 17 USC 105: works of the US government are not copyrightable. |
| **YES** | `clerk.house.gov` | US Gov / state public record | 2 | house-competition | Federal works are public domain; state election records are public records. |
| **YES** | `docquery.fec.gov` | US Gov / state public record | 2 | independent-expenditures | Federal works are public domain; state election records are public records. |
| **YES** | `www.ncsbe.gov` | US Gov / state public record | 1 | vote-targeting | Federal works are public domain; state election records are public records. |
| **YES** | `sos.ga.gov` | US Gov / state public record | 1 | ga-precinct-returns | Federal works are public domain; state election records are public records. |
