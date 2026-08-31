# ---------------------------------------------------------------------------
# Build the historical-campaigns datasets.
#
# Two files end up in this folder:
#
#   derived/pres_states_1864_2024.csv   state-level presidential returns, 41 elections
#                               (1,913 rows), Democratic / Republican / other
#                               shares plus the statewide winner
#   derived/pres_national.csv           national popular vote and electoral vote by
#                               candidate, 1824-2024
#
# Run this script from inside the data/ folder. It needs a network connection;
# the whole point of committing the outputs is that the lab does not.
#
# Everything here is downloaded, trimmed, checked, and written. No value is
# edited by hand.
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
# Downloads go through prov_fetch(), which records url, bytes, hash and row
# count in PROVENANCE.tsv and prints a banner when a source moves under us --
# a URL that still returns 200 and no longer means what it meant. See
# ../../../_lib/provenance.R. If the helper is missing the build still runs: the
# fallback is a plain download with the same signature, forwarding every
# argument so a source needing a redirect or a user agent still gets one.
if (file.exists("../../../_lib/provenance.R")) {
  source("../../../_lib/provenance.R")
} else {
  prov_fetch <- function(url, dest, label = NULL, mode = "wb", quiet = TRUE, ...) {
    download.file(url, dest, mode = mode, quiet = quiet, ...)
    invisible(dest)
  }
  prov_report <- function() invisible(FALSE)
}


dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)

# --- Source -----------------------------------------------------------------
#
# Jason Timm, "PresElectionResults: US Presidential Election Results, by county
# (2000-2024), congressional district (2024), and state (1864-2024)"
#   https://github.com/jaytimm/PresElectionResults
#   branch: master
#
# The state series is compiled from Wikipedia's per-election articles, which in
# turn draw on Dave Leip's Atlas and the official state canvasses. Like every
# historical returns compilation, it is somebody's assembly of fifty-one
# separate sources -- exactly the point `data-sources` makes. Treat the deep past with
# more caution than the recent past: nineteenth-century "popular vote" totals
# rest on state records of wildly varying quality, and in 1864 eleven states
# were in armed rebellion and did not vote at all.

base <- "https://raw.githubusercontent.com/jaytimm/PresElectionResults/master/data/"

load_rda <- function(file) {
  tmp <- tempfile(fileext = ".rda")
  prov_fetch(paste0(base, file), tmp, mode = "wb", quiet = TRUE)
  env <- new.env()
  load(tmp, envir = env)
  unlink(tmp)
  get(ls(env)[1], envir = env)
}

# --- 1. State-level returns, 1864-2024 --------------------------------------

st <- as.data.frame(load_rda("pres_by_state.rda"))

stopifnot(nrow(st) == 1913,
          all(c("year", "state_abbrev", "democrat", "republican",
                "other", "winner", "party_win") %in% names(st)))

st <- st[order(st$year, st$state_abbrev),
         c("year", "state_abbrev", "democrat", "republican", "other",
           "winner", "party_win")]

# Sanity checks, reported rather than silently assumed.
cat("state file:", nrow(st), "rows,",
    length(unique(st$year)), "elections,",
    min(st$year), "-", max(st$year), "\n")
cat("  states per election ranges from",
    min(table(st$year)), "to", max(table(st$year)), "\n")
cat("  'other' is NA in", sum(is.na(st$other)), "of", nrow(st), "rows",
    "-- third-party votes are recorded only sporadically,\n",
    "  which is why the lab works in two-party shares throughout\n")

# Nineteen rows have no Democratic or no Republican share. These are NOT gaps
# in the data -- they record elections where a major party had no line on that
# state's ballot at all:
#
#   1892  CO ID KS NE WY   Populist fusion; no separate Democratic ticket
#   1908  OK              no Republican line
#   1912  AZ CA FL NE OK SD WA   Republican line absent (Taft was kept off
#                          California's ballot entirely by the Progressives)
#   1924  CA WA WI        Democratic line absent where La Follette ran
#   1948  AL              no Democratic slate -- the Dixiecrats replaced it,
#                          so Truman could not be voted for in Alabama
#   1964  AL              no Democratic slate -- Johnson was not on the ballot
#
# The lab uses these rows deliberately. Do not fill them in.
na_rows <- st[is.na(st$democrat) | is.na(st$republican), ]
cat("  rows with a major party absent from the ballot:", nrow(na_rows), "\n")
print(table(na_rows$year))

write.csv(st, "derived/pres_states_1864_2024.csv", row.names = FALSE)

# --- 2. National popular and electoral vote ---------------------------------

nat <- as.data.frame(load_rda("pres_results.rda"))

nat <- nat[!is.na(nat$pop_per) & nat$year >= 1864,
           c("year", "candidate", "party", "ec_votes", "ec_total",
             "pop_votes", "pop_per")]
nat <- nat[order(nat$year, -nat$pop_per), ]

cat("national file:", nrow(nat), "candidate-years,",
    min(nat$year), "-", max(nat$year), "\n")

write.csv(nat, "derived/pres_national.csv", row.names = FALSE)

cat("\ndone.\n")

# ---------------------------------------------------------------------------
# Build stamp. Records which script produced what is now in this directory --
# every file under derived/ and raw/ with its size, hash and row count, and the
# date this ran -- into BUILD-STAMP.tsv beside the data. See
# ../../../_lib/provenance.R. Guarded, because a missing helper must not fail a
# build that was otherwise fine.
if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_report()
  prov_stamp()
}
