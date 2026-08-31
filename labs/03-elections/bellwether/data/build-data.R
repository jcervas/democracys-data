# ---------------------------------------------------------------------------
# Build the bellwether dataset: which party carried every county in every
# presidential election from 1960 to 2024.
#
# Three files end up in this folder:
#
#   derived/county_winners.csv   One row per county, one column per election. Values
#                        are "D" or "R" -- who carried the county. Restricted
#                        to counties present, with a winner, in all seventeen
#                        elections.
#   derived/national.csv         One row per election: the popular-vote winner, the
#                        electoral-college winner, and the two nominees.
#   derived/crosscheck.csv       Every county-year where the two independent sources
#                        for 2000-2016 disagree about who won. There are twelve.
#
# ---------------------------------------------------------------------------
# THE ONE MANUAL STEP, AND WHY IT IS HERE
#
# This is the only build script in the corpus that a person has to help. It is
# documented rather than hidden, because the reason is the chapter's subject.
#
# The long series comes from Algara and Amlani's replication archive:
#
#   Algara, Carlos and Sharif Amlani. 2021. Replication Data for:
#   "Partisanship & Nationalization in American Elections ... 1872-2020."
#   Harvard Dataverse, V1. https://doi.org/10.7910/DVN/DGUMFI
#   file: dataverse_shareable_presidential_county_returns_1868_2020.Rdata
#         (1,459,576 bytes; sha256 c3ad5e87a6dfd8aaf257...906c1687)
#
# Harvard Dataverse sits behind an AWS WAF. A scripted request -- curl, wget,
# download.file(), any of them -- comes back HTTP 202 with an empty body and
# the header `x-amzn-waf-action: challenge`. Nothing is refused and nothing
# fails; you receive zero bytes and a success-shaped status code. The challenge
# is JavaScript, so only a real browser can answer it.
#
# TO REBUILD FROM SCRATCH: open the DOI above in a browser, download that one
# file, and put it in raw/ under the name below. Everything else is automatic.
#
# This is worth a sentence in class. The file is free, openly licensed, and
# published for reuse; the obstacle is not permission but plumbing, and it is
# invisible -- a script that did not check would record an empty download as a
# success. Every other route to these numbers is worse: ICPSR 8611 needs an
# institutional login, and CQ Press and the Leip Atlas -- the sources Grofman
# and Chen actually used, and the ones behind the newspaper stories -- are
# commercial products. There is no free official national county file, because
# no federal agency counts votes. See the returns-source chapter.
#
# The other two sources ARE script-fetchable, and they are what check this one.
#
# Run from this directory:  Rscript build-data.R      (needs internet)
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

options(stringsAsFactors = FALSE, scipen = 999, timeout = 900)

RAW <- "raw"
dir.create(RAW, showWarnings = FALSE)
ALGARA <- file.path(RAW, "presidential_county_returns_1868_2020.Rdata")

if (!file.exists(ALGARA))
  stop("\n  Missing: ", ALGARA,
       "\n  This is the one file that must be fetched by hand.",
       "\n  Open https://doi.org/10.7910/DVN/DGUMFI in a browser, download",
       "\n  dataverse_shareable_presidential_county_returns_1868_2020.Rdata,",
       "\n  and save it to raw/ under the name above. See the header of this",
       "\n  script for why a script cannot do it.\n")

fetch <- function(url, dest) {
  if (!file.exists(dest)) {
    cat("downloading", basename(dest), "...\n")
    prov_fetch(url, dest, quiet = TRUE, method = "curl", extra = "-L")
  }
  dest
}

# --- 1. the long series, 1960-2020 ------------------------------------------
# Algara and Amlani built this from CQ Press and the ICPSR historical returns.
# It is a compilation of compilations: nobody at any level of government
# published it, and the two upstream sources are themselves gated. That is the
# honest description and it belongs in the chapter, not in a footnote.

e <- new.env()
load(ALGARA, envir = e)
a <- e$pres_elections_release
a <- a[a$election_year >= 1960 & a$office == "PRES" & a$election_type == "G", ]
a <- a[, c("election_year", "fips", "county_name", "state",
           "democratic_raw_votes", "republican_raw_votes")]
names(a) <- c("year", "fips", "county", "state", "dem", "rep")
cat("Algara-Amlani: ", nrow(a), " county-elections, ",
    min(a$year), "-", max(a$year), "\n", sep = "")

# ALASKA IS NOT IN HERE, and its absence is correct rather than an omission.
# Alaska reports presidential votes by State House district, not by borough or
# census area, so there is no county-level return to carry. The state has no
# counties. Every count below is therefore of the other fifty jurisdictions.
cat("Alaska rows (expected 0 -- Alaska has no counties): ",
    sum(a$state == "AK"), "\n", sep = "")

# --- 2. 2024, which the long series does not reach --------------------------
# The compilation stops at 2020. 2024 comes from the GitHub file three other
# chapters already use, and is checked below against the states' own certified
# publications assembled in ../../county-returns/.
t24 <- read.csv(fetch(paste0("https://raw.githubusercontent.com/tonmcg/",
                             "US_County_Level_Election_Results_08-24/master/",
                             "2024_US_County_Level_Presidential_Results.csv"),
                      file.path(RAW, "tonmcg_2024.csv")),
                colClasses = c(county_fips = "character"))
t24 <- data.frame(year = 2024L,
                  fips = sprintf("%05s", t24$county_fips),
                  county = toupper(t24$county_name),
                  state = t24$state_name,
                  dem = t24$votes_dem, rep = t24$votes_gop)

d <- rbind(a, t24)
d$win <- ifelse(d$dem > d$rep, "D", ifelse(d$rep > d$dem, "R", NA))

# --- 3. the national outcome, hand-typed and disclosed ----------------------
# Neither source carries it. As in the midterm-loss chapter, the column the
# whole analysis turns on is the one nobody downloaded.
#
# TWO COLUMNS, NOT ONE, because "the national winner" is ambiguous and the
# ambiguity does real work here. In 2000 and 2016 the popular vote and the
# electoral college named different men. A county that voted Bush in 2000 is a
# perfect predictor under one definition and a failure under the other.
# Grofman and Chen argue for the popular vote as the sensible domain; the
# newspaper lists are built on the electoral college. Both are carried.
national <- data.frame(
  year = seq(1960, 2024, 4),
  ec = c("D","D","R","R","D","R","R","R","D","D","R","R","D","D","R","D","R"),
  pv = c("D","D","R","R","D","R","R","R","D","D","D","R","D","D","D","D","R"),
  dem_nominee = c("Kennedy","Johnson","Humphrey","McGovern","Carter","Carter",
                  "Mondale","Dukakis","Clinton","Clinton","Gore","Kerry",
                  "Obama","Obama","Clinton","Biden","Harris"),
  rep_nominee = c("Nixon","Goldwater","Nixon","Nixon","Ford","Reagan","Reagan",
                  "Bush","Bush","Dole","Bush","Bush","McCain","Romney",
                  "Trump","Trump","Trump"))
# 1960 is the one row that is genuinely arguable: Alabama's Democratic electors
# were unpledged and split, so the national popular-vote total for Kennedy
# depends on how Alabama's ballots are allocated, and respectable sources reach
# opposite answers. It is recorded as D here, the conventional reading. No
# finding in the chapter turns on it -- 1960 is a D year on both columns either
# way, since Kennedy won the electoral college outright.
stopifnot(nrow(national) == 17)

# --- 4. the panel -----------------------------------------------------------
YRS <- national$year
d <- d[d$year %in% YRS, ]
tab <- table(d$fips)
full <- names(tab)[tab == length(YRS)]

w <- reshape(d[d$fips %in% full, c("fips", "year", "win")],
             idvar = "fips", timevar = "year", direction = "wide")
names(w) <- c("fips", paste0("y", YRS))

# Counties with a tie in some year drop out: a tie has no winner, and a
# bellwether is defined by matching the winner. Six counties, all ties in the
# nineteenth-century-sized electorates of small places.
ties <- w$fips[!complete.cases(w)]
lookup <- d[!duplicated(d$fips), c("fips", "county", "state")]
if (length(ties)) {
  tt <- merge(data.frame(fips = ties), lookup, by = "fips")
  cat("\ndropped for a tie in at least one election (", nrow(tt), "):\n", sep = "")
  for (i in seq_len(nrow(tt)))
    cat("  ", tt$fips[i], " ", tt$county[i], ", ", tt$state[i], "\n", sep = "")
}
w <- w[complete.cases(w), ]
w <- merge(lookup, w, by = "fips")
w <- w[order(w$fips), ]

cat("\ncounties present with a winner in all ", length(YRS),
    " elections: ", nrow(w), "\n", sep = "")
cat("counties appearing at all: ", length(tab),
    " (the rest were created, merged or renamed mid-series)\n", sep = "")

# --- 5. CHECK ONE: the manual download against a script-fetchable source ----
# The file above arrived by hand, which is exactly the circumstance in which a
# silent substitution would never be noticed. MEDSL publishes 2000-2016 county
# returns on GitHub, compiled independently from state sources. Five elections
# overlap. If the hand-carried file were the wrong file, this would say so.
med <- read.csv(fetch(paste0("https://raw.githubusercontent.com/MEDSL/",
                             "county-returns/master/countypres_2000-2016.csv"),
                      file.path(RAW, "medsl.csv")))
med <- med[med$party %in% c("democrat", "republican") &
             !is.na(med$FIPS) & !is.na(med$candidatevotes), ]
med$fips <- sprintf("%05d", as.integer(med$FIPS))
mw <- reshape(aggregate(candidatevotes ~ fips + year + party, med, sum),
              idvar = c("fips", "year"), timevar = "party", direction = "wide")
names(mw) <- c("fips", "year", "dem_medsl", "rep_medsl")

ov <- merge(d[d$year %in% seq(2000, 2016, 4), c("fips", "year", "dem", "rep")],
            mw, by = c("fips", "year"))
ov$win_algara <- ifelse(ov$dem > ov$rep, "D", "R")
ov$win_medsl  <- ifelse(ov$dem_medsl > ov$rep_medsl, "D", "R")
bad <- ov[ov$win_algara != ov$win_medsl, ]
bad <- merge(bad, lookup, by = "fips")
bad <- bad[order(bad$fips, bad$year), ]

cat("\nCHECK 1 -- Algara/Amlani vs MEDSL, 2000-2016\n")
cat("  county-elections compared: ", nrow(ov), "\n", sep = "")
cat("  disagreements about the winner: ", nrow(bad),
    sprintf(" (%.3f%%)\n", 100 * nrow(bad) / nrow(ov)), sep = "")
if (nrow(ov) < 15000) stop("CHECK 1: too few rows matched -- wrong file?")
if (nrow(bad) > 40)   stop("CHECK 1: disagreement rate far above the expected 12")

# Why each disagreement happens, written down so the next reader is not
# starting over. None of them is arithmetic; all are unit or ballot decisions.
bad$reason <- ifelse(
  bad$fips == "29095", "Kansas City reports separately from the rest of Jackson County; MEDSL splits it out, Algara does not",
  ifelse(substr(bad$fips, 1, 2) == "36", "New York fusion voting: a nominee appears on several party lines and the two sources add up different subsets",
  ifelse(bad$fips == "47095", "the Democratic and Republican totals are transposed in one of the two files", "unexplained")))
write.csv(bad[, c("fips", "county", "state", "year", "dem", "rep",
                  "dem_medsl", "rep_medsl", "win_algara", "win_medsl", "reason")],
          "derived/crosscheck.csv", row.names = FALSE)
cat("  wrote crosscheck.csv\n")
if (any(bad$reason == "unexplained"))
  cat("  NOTE: a disagreement appeared that this script has no account of.\n",
      "        Look at crosscheck.csv before trusting the chapter.\n", sep = "")

# Does any of it matter? A disagreement only matters if it changes whether some
# county is a bellwether, and none of these counties is close to being one.
hit_ec <- rowSums(sapply(YRS, function(y) w[[paste0("y", y)]] == national$ec[national$year == y]))
cat("  best record among the disagreeing counties: ",
    max(hit_ec[w$fips %in% bad$fips]), " of ", length(YRS),
    " -- a bellwether needs ", length(YRS), "\n", sep = "")

# --- 6. CHECK TWO: 2024 against the states' own publications ---------------
# ../../county-returns/ is the corpus's standing project to replace the GitHub
# compilation with the fifty-one states' certified returns. It is part-built.
# Every jurisdiction finished there is used here to audit the compilation.
sd <- "../../county-returns/data/derived/states"
cat("\nCHECK 2 -- 2024 compilation vs the states' certified returns\n")
if (!dir.exists(sd)) {
  cat("  skipped: ", sd, " not found\n", sep = "")
} else {
  sf <- list.files(sd, pattern = "_2024[.]csv$", full.names = TRUE)
  # Alaska publishes House districts and DC published wards; neither is a
  # county, and county-returns records them at the level actually published.
  sf <- sf[!grepl("/(AK|DC)_", sf)]
  off <- do.call(rbind, lapply(sf, read.csv, colClasses = c(county_fips = "character")))
  k <- merge(off[, c("county_fips", "votes_dem", "votes_gop")],
             t24[, c("fips", "dem", "rep")], by.x = "county_fips", by.y = "fips")
  k$w_off <- ifelse(k$votes_dem > k$votes_gop, "D", "R")
  k$w_ton <- ifelse(k$dem > k$rep, "D", "R")
  cat("  jurisdictions certified and on hand: ", length(sf), "\n", sep = "")
  cat("  counties compared: ", nrow(k), "\n", sep = "")
  cat("  disagreements about the winner: ", sum(k$w_off != k$w_ton), "\n", sep = "")
  cat("  vote totals identical: dem ",
      sprintf("%.2f%%", 100 * mean(k$votes_dem == k$dem)), ", rep ",
      sprintf("%.2f%%", 100 * mean(k$votes_gop == k$rep)), "\n", sep = "")
  if (sum(k$w_off != k$w_ton) > 0)
    cat("  NOTE: the compilation and a state's own return disagree. Investigate.\n")
}

# --- 7. write --------------------------------------------------------------
write.csv(w, "derived/county_winners.csv", row.names = FALSE)
write.csv(national, "derived/national.csv", row.names = FALSE)
cat("\nwrote county_winners.csv: ", nrow(w), " counties x ", length(YRS),
    " elections\n", sep = "")
cat("wrote national.csv: ", nrow(national), " elections\n", sep = "")

# --- 8. sanity checks against results that are already published -----------
# Grofman and Chen report two numbers for six-election windows that this file
# should reproduce: about 2% of counties perfect in 1960-1980, and 0.3% in
# 2000-2020. If the panel were built wrongly these would not land.
cat("\nChecks against Grofman and Chen (2023), who used CQ Press:\n")
pct6 <- function(yy) {
  h <- rowSums(sapply(yy, function(y) w[[paste0("y", y)]] == national$ec[national$year == y]))
  100 * mean(h == length(yy))
}
cat(sprintf("  perfect over 1960-1980: %.2f%%  (they report about 2%%)\n",
            pct6(seq(1960, 1980, 4))))
cat(sprintf("  perfect over 2000-2020: %.2f%%  (they report 0.3%%)\n",
            pct6(seq(2000, 2020, 4))))

# The Wall Street Journal's list of nineteen bellwethers, published 13 Nov 2020
# and reprinted in the paper's footnote 10, was built on 1980-2016. Searching
# this file for counties perfect over exactly that window should return exactly
# those nineteen -- which tests the file and the newspaper against each other.
W <- seq(1980, 2016, 4)
hw <- rowSums(sapply(W, function(y) w[[paste0("y", y)]] == national$ec[national$year == y]))
cat("  counties perfect over 1980-2016: ", sum(hw == length(W)),
    "  (the WSJ published 19)\n", sep = "")
perfect <- w[hw == length(W), ]
# Not just the same COUNT -- the same COUNTIES. This is the strong form of the
# check, and it tests the newspaper as much as it tests the file.
WSJ <- c("53009","19017","36023","50009","35023","55057","55077","36077","39123",
         "55103","55113","26155","35061","26159","18167","17187","23029","51193","39173")
cat("  and they are the same nineteen as the WSJ's list: ",
    setequal(perfect$fips, WSJ), "\n", sep = "")
if (!setequal(perfect$fips, WSJ))
  cat("    only in ours: ", paste(setdiff(perfect$fips, WSJ), collapse = ", "),
      "\n    only in theirs: ", paste(setdiff(WSJ, perfect$fips), collapse = ", "), "\n", sep = "")
cat("  of those, still perfect through 2020: ",
    sum(perfect$y2020 == "D"), "; through 2024: ",
    sum(perfect$y2020 == "D" & perfect$y2024 == "R"), "\n", sep = "")
cat("\n")

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
