# ---------------------------------------------------------------------------
# Build the turnout-denominator lab dataset.
#
# FETCHED 2026-08-12.  Sources, with what each returned on that date:
#
#   1. McDonald, M.P. and Popkin, S.L. (2001) "The Myth of the Vanishing
#      Voter", APSR 95(4):963-974, TABLE 1, page 966.
#      https://www.jstor.org/stable/3117725
#      Not a download. A table printed in a journal article, captured with
#      `pdftotext -layout` into raw/mp2001-table1.txt. See raw/README.txt.
#      27 election years, 1948-2000, national.
#
#   2. United States Elections Project, "1980-2022 General Election Turnout
#      Rates", version 1.2 (8 October 2024)
#      https://election.lab.ufl.edu/data-downloads/turnoutdata/Turnout_1980_2022_v1.2.csv
#      HTTP 200 - 116,830 bytes - 1,144 rows - 22 elections x 52 rows
#      (United States + 50 states + DC). CC BY 4.0.
#
#   3. United States Elections Project, "2024 General Election Turnout
#      Rates", version 0.4
#      https://election.lab.ufl.edu/data-downloads/turnoutdata/Turnout_2024G_v0.4.csv
#      HTTP 200 - 8,378 bytes - 52 rows. Version 0.x: still preliminary.
#
# WHY BOTH THE ARTICLE AND THE MODERN FILE
#
# Source 2 is the living continuation of source 1 -- same author, same
# construction, thirty years later. But the article is where the argument was
# made, and its Table 1 is the only place the 1948-1978 numbers exist in this
# form. The chapter runs the paper's own regression on the paper's own numbers
# first, then asks whether the correction still matters, so it needs both.
#
# THE NUMERATOR TRAP, AND HOW THIS BUILD HANDLES IT
#
# Source 2 carries TWO numerators. VOTE_FOR_HIGHEST_OFFICE is the vote for the
# top statewide office; TOTAL_BALLOTS_COUNTED is every ballot, including those
# where the voter skipped that office. Total ballots runs about 2-3% higher.
# NEITHER covers the whole period:
#
#   VOTE_FOR_HIGHEST_OFFICE   1980-2018   (absent 2020; a URL in 2022)
#   TOTAL_BALLOTS_COUNTED     1998-2024   (absent before 1998)
#
# The file's own published rate columns switch numerators inside the series --
# they are empty before 1998 and use total ballots after. Splicing the two
# numerators produces a fake jump of two to three points in 1998, which is
# larger than most of the effects this chapter is about. So every trend in the
# chapter is computed on ONE numerator at a time, and both series are written
# out separately rather than merged. The build refuses to produce a spliced
# rate column at all.
#
# REQUIRES `pdftotext` (poppler) only if regenerating raw/mp2001-table1.txt
# from a local copy of the article PDF. Otherwise the committed capture is
# read as-is. On macOS: brew install poppler.
#
# OUTPUTS (all written next to this script)
#
#   derived/mp1948.csv        the article's Table 1, parsed, plus the VEP and the VEP
#                     turnout rate rebuilt from its own component columns
#   derived/national.csv      1980-2024 national, both numerators, both denominators,
#                     both rates, computed here and checked against the
#                     publisher's own rate columns where those exist
#   derived/states.csv        state-year panel, 1980-2024, same columns
#   derived/state_trends.csv  one row per state: the VAP and VEP slopes through the
#                     presidential elections 1980-2016 on one numerator
#   derived/pairs2024.csv     every pair of states that ranks in the opposite order
#                     under the two denominators in 2024
#   derived/overlap.csv       the eleven elections both sources publish, with how far
#                     the numerator and the denominator have each moved since
#   derived/checks.csv        the validation results the chapter prints verbatim
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)

# Run this script from inside this data/ folder. `../../../_lib/provenance.R`
# records url, bytes, hash and row count for every download and prints a loud
# banner when a source moves under us. If it is missing the build still runs.
if (file.exists("../../../_lib/provenance.R")) {
  source("../../../_lib/provenance.R")
} else {
  prov_fetch  <- function(url, dest, ...) { download.file(url, dest, mode = "wb", quiet = TRUE); dest }
  prov_report <- function() invisible(FALSE)
}

FETCH_DATE <- "2026-08-12"
dir.create("raw", showWarnings = FALSE)

# Strip thousands separators, per cent signs and stray spaces. Returns NA for
# anything that is not a number -- which is the point in one specific place:
# in 2022 the VOTE_FOR_HIGHEST_OFFICE column contains a URL, and this is what
# turns that into a countable missing value instead of a silent zero.
num <- function(x) suppressWarnings(as.numeric(gsub("[,%$ ]", "", x)))

# ===========================================================================
# 1. The article's Table 1
# ===========================================================================
#
# Regenerate the capture if a copy of the PDF is on disk; otherwise use the
# committed one. Page 5 of the PDF is page 966 of the journal.

MP_TXT <- "raw/mp2001-table1.txt"
MP_PDF <- Sys.getenv("MP_PDF", unset = file.path(
  "~/Zotero/storage/GYQSTDK8",
  "McDonald and Popkin - 2001 - The Myth of the Vanishing Voter.pdf"))

if (file.exists(path.expand(MP_PDF)) && nchar(Sys.which("pdftotext")) > 0) {
  txt <- system(sprintf("pdftotext -layout -f 5 -l 5 %s -",
                        shQuote(path.expand(MP_PDF))), intern = TRUE)
  a <- grep("TABLE *1\\.", txt)[1]
  b <- grep("^ *Note:", txt)
  b <- b[b > a][1]
  writeLines(txt[a:length(txt)][seq_len(length(a:length(txt)))][
    seq_len(b - a + 1L)], MP_TXT)
  cat("  [mp] capture regenerated from the PDF\n")
} else {
  cat("  [mp] PDF not on disk; using the committed capture\n")
}

RAW <- readLines(MP_TXT, warn = FALSE)

# A data row begins with an even four-digit election year between 1948 and
# 2000 and is followed by a digit. The header lines all begin with words, and
# the Sources block wraps text that never starts with a bare year, so this is
# unambiguous -- but it is asserted below rather than assumed.
DATA_LINES <- grep("^\\s*(19[4-9][0-9]|2000)\\s+[0-9]", RAW, value = TRUE)

parse_row <- function(ln) {
  f <- as.numeric(gsub("[+]", "", strsplit(trimws(ln), "\\s+")[[1]]))
  # Eleven fields to 1970, fifteen from 1972: the 26th-Amendment block (age
  # 18-20 citizens, their votes, the adjustment, and the age-21+ rate) only
  # exists once there were 18-year-old voters to count. Pad the short rows so
  # every row has the same width.
  length(f) <- 15L
  f
}
M <- do.call(rbind, lapply(DATA_LINES, parse_row))
mp <- data.frame(
  year         = as.integer(M[, 1]),
  vote_highest = M[, 2],   # thousands
  vap          = M[, 3],   # thousands
  vap_rate     = M[, 4],
  noncitizens  = M[, 5],   # thousands
  nc_adj       = M[, 6],
  felons       = M[, 7],   # thousands
  fel_adj      = M[, 8],
  overseas     = M[, 9],   # thousands
  ovs_adj      = M[, 10],
  vep_rate     = M[, 11],
  cvap_18_20   = M[, 12],
  voters_18_20 = M[, 13],
  age_adj      = M[, 14],
  vep_rate_21  = M[, 15])

# The table prints its ingredients, so the voting-eligible population it used
# can be rebuilt -- and with it, its own headline rate. This is the check that
# the columns were read in the right order: a parse that is one field off
# produces a rate that is wrong by tens of points, not tenths.
mp$vep            <- mp$vap - mp$noncitizens - mp$felons + mp$overseas
mp$vep_rate_recon <- 100 * mp$vote_highest / mp$vep
mp$recon_err      <- mp$vep_rate_recon - mp$vep_rate

# And a second, independent check on the same rows. The table also prints each
# correction in percentage points. Those should add to the difference between
# the two published rates.
mp$adj_sum     <- mp$vap_rate + mp$nc_adj + mp$fel_adj + mp$ovs_adj
mp$adj_err     <- mp$adj_sum - mp$vep_rate
mp$gap         <- mp$vep_rate - mp$vap_rate
mp$presidential <- mp$year %% 4 == 0

# The share of the voting-age population that is not eligible. This is the
# only quantity in the chapter with no numerator in it at all, which is what
# makes it safe to draw across two sources that count votes differently.
mp$inelig_pct <- 100 * (mp$noncitizens + mp$felons - mp$overseas) / mp$vap

dd_write_csv(mp, "derived/mp1948.csv")

# ===========================================================================
# 2. The Elections Project series, 1980-2022 and 2024
# ===========================================================================

U_TS <- paste0("https://election.lab.ufl.edu/data-downloads/turnoutdata/",
               "Turnout_1980_2022_v1.2.csv")
U_24 <- paste0("https://election.lab.ufl.edu/data-downloads/turnoutdata/",
               "Turnout_2024G_v0.4.csv")
U_TS_DOC <- sub("\\.csv$", "_doc.txt", U_TS)
U_24_DOC <- sub("\\.csv$", "_doc.txt", U_24)

invisible(prov_fetch(U_TS, "raw/Turnout_1980_2022_v1.2.csv", label = "EP 1980-2022"))
invisible(prov_fetch(U_24, "raw/Turnout_2024G_v0.4.csv",     label = "EP 2024"))
invisible(prov_fetch(U_TS_DOC, "raw/Turnout_1980_2022_v1.2_doc.txt", label = "EP 1980-2022 doc"))
invisible(prov_fetch(U_24_DOC, "raw/Turnout_2024G_v0.4_doc.txt",     label = "EP 2024 doc"))

ts <- read.csv("raw/Turnout_1980_2022_v1.2.csv", colClasses = "character")
t4 <- read.csv("raw/Turnout_2024G_v0.4.csv",     colClasses = "character")

stopifnot(all(c("YEAR", "STATE", "TOTAL_BALLOTS_COUNTED", "VOTE_FOR_HIGHEST_OFFICE",
                "VAP", "NONCITIZEN_PCT", "INELIGIBLE_FELONS_TOTAL",
                "ELIGIBLE_OVERSEAS", "VEP") %in% names(ts)))

# How often the highest-office column holds a URL rather than a number. This is
# counted, not repaired: the chapter shows it.
URL_ROWS <- grepl("^http", ts$VOTE_FOR_HIGHEST_OFFICE)
URL_YEARS <- sort(unique(ts$YEAR[URL_ROWS]))

# The 2024 file has no YEAR column and its second column is an unnamed source
# field, which read.csv names X. Give it the shape of the time series.
t4$YEAR <- "2024"
t4$VOTE_FOR_HIGHEST_OFFICE <- NA_character_   # not published in this file
KEEP <- c("YEAR", "STATE", "STATE_ABV", "TOTAL_BALLOTS_COUNTED",
          "VOTE_FOR_HIGHEST_OFFICE", "VAP", "NONCITIZEN_PCT",
          "INELIGIBLE_PRISON", "INELIGIBLE_PROBATION", "INELIGIBLE_PAROLE",
          "INELIGIBLE_FELONS_TOTAL", "ELIGIBLE_OVERSEAS", "VEP",
          "VEP_TURNOUT_RATE", "VAP_TURNOUT_RATE")
ep <- rbind(ts[, KEEP], t4[, KEEP])

for (v in setdiff(KEEP, c("STATE", "STATE_ABV"))) ep[[v]] <- num(ep[[v]])
ep$YEAR <- as.integer(ep$YEAR)

# Rates, computed here, one numerator at a time. NOT spliced.
ep$rate_vap_hi <- 100 * ep$VOTE_FOR_HIGHEST_OFFICE / ep$VAP
ep$rate_vep_hi <- 100 * ep$VOTE_FOR_HIGHEST_OFFICE / ep$VEP
ep$rate_vap_tb <- 100 * ep$TOTAL_BALLOTS_COUNTED   / ep$VAP
ep$rate_vep_tb <- 100 * ep$TOTAL_BALLOTS_COUNTED   / ep$VEP
ep$gap_hi <- ep$rate_vep_hi - ep$rate_vap_hi
ep$gap_tb <- ep$rate_vep_tb - ep$rate_vap_tb
ep$presidential <- ep$YEAR %% 4 == 0

# Is the published VEP the identity the documentation states?
#   state:    VAP*(1 - noncitizen%) - ineligible felons
#   national: the same, plus eligible overseas
# Checked on every row, not asserted.
ep$vep_recon <- ep$VAP * (1 - ep$NONCITIZEN_PCT / 100) -
  ep$INELIGIBLE_FELONS_TOTAL + ifelse(is.na(ep$ELIGIBLE_OVERSEAS), 0,
                                      ep$ELIGIBLE_OVERSEAS)
ep$vep_recon_pct_err <- 100 * (ep$vep_recon - ep$VEP) / ep$VEP

# And does recomputing the rate reproduce the publisher's own rate columns
# where they exist? Those are computed on total ballots, so that is the
# numerator this comparison must use.
ep$pub_vep_err <- ep$rate_vep_tb - ep$VEP_TURNOUT_RATE
ep$pub_vap_err <- ep$rate_vap_tb - ep$VAP_TURNOUT_RATE

ep$inelig_pct <- 100 * (ep$VAP - ep$VEP) / ep$VAP

nat <- ep[ep$STATE == "United States", ]
nat <- nat[order(nat$YEAR), ]
st  <- ep[ep$STATE != "United States", ]
st  <- st[order(st$YEAR, st$STATE), ]

dd_write_csv(nat, "derived/national.csv")
dd_write_csv(st, "derived/states.csv")

# ===========================================================================
# 3a. The same eleven elections, twenty-five years apart
# ===========================================================================
#
# 1980-2000 appears in both sources, and both are the same author measuring
# the same thing. They disagree, and the interesting question is which half of
# the fraction moved. The numerator is a count of votes that were already
# certified in 2001; the denominator is a Census estimate that has been
# revised every year since. Written out so the chapter can show which.

ovl <- merge(mp[, c("year", "vote_highest", "vap", "vap_rate", "vep_rate")],
             nat[, c("YEAR", "VOTE_FOR_HIGHEST_OFFICE", "VAP",
                     "rate_vap_hi", "rate_vep_hi")],
             by.x = "year", by.y = "YEAR")
names(ovl)[names(ovl) == "vote_highest"] <- "num_2001"   # thousands
names(ovl)[names(ovl) == "vap"]          <- "vap_2001"   # thousands
ovl$num_2001 <- ovl$num_2001 * 1000
ovl$vap_2001 <- ovl$vap_2001 * 1000
names(ovl)[names(ovl) == "VOTE_FOR_HIGHEST_OFFICE"] <- "num_now"
names(ovl)[names(ovl) == "VAP"]                     <- "vap_now"
ovl$num_move_pct <- 100 * (ovl$num_now - ovl$num_2001) / ovl$num_2001
ovl$vap_move_pct <- 100 * (ovl$vap_now - ovl$vap_2001) / ovl$vap_2001
ovl$vap_rate_move <- ovl$rate_vap_hi - ovl$vap_rate
ovl$vep_rate_move <- ovl$rate_vep_hi - ovl$vep_rate
dd_write_csv(ovl, "derived/overlap.csv")

# ===========================================================================
# 3. State trends, on one numerator, through one kind of election
# ===========================================================================
#
# Presidential years only: mixing presidential and midterm turnout into one
# regression makes the trend a function of how many of each fell in the window.
# Highest office only, for the reason given at the top. That confines the
# window to 1980-2016 -- ten elections, complete for every state, no splice.

TR_LO <- 1980L
TR_HI <- 2016L
tp <- st[st$presidential & st$YEAR >= TR_LO & st$YEAR <= TR_HI &
           !is.na(st$rate_vap_hi), ]

trend <- do.call(rbind, lapply(split(tp, tp$STATE), function(z) {
  z  <- z[order(z$YEAR), ]
  fv <- summary(lm(rate_vap_hi ~ YEAR, z))$coefficients
  fe <- summary(lm(rate_vep_hi ~ YEAR, z))$coefficients
  data.frame(
    state      = z$STATE[1],
    n          = nrow(z),
    b_vap      = fv[2, 1], se_vap = fv[2, 2], p_vap = fv[2, 4],
    b_vep      = fe[2, 1], se_vep = fe[2, 2], p_vep = fe[2, 4],
    # the fitted change across the window, which is what a reader pictures
    fit_vap    = fv[2, 1] * (TR_HI - TR_LO),
    fit_vep    = fe[2, 1] * (TR_HI - TR_LO),
    nc_first   = z$NONCITIZEN_PCT[1],
    nc_last    = z$NONCITIZEN_PCT[nrow(z)],
    rate_vap_first = z$rate_vap_hi[1], rate_vap_last = z$rate_vap_hi[nrow(z)],
    rate_vep_first = z$rate_vep_hi[1], rate_vep_last = z$rate_vep_hi[nrow(z)])
}))
trend$sign_flip <- sign(trend$b_vap) != sign(trend$b_vep)
trend$divergence <- trend$b_vep - trend$b_vap
trend <- trend[order(-trend$divergence), ]
dd_write_csv(trend, "derived/state_trends.csv")

# ===========================================================================
# 4. Which pairs of states swap places in 2024
# ===========================================================================
#
# Ranks move for two reasons and only one of them is interesting. A state can
# slide down a list because the states around it moved. What matters is a pair
# that reverses: A above B on one denominator, B above A on the other. That is
# a statement about two states and nothing else, so it survives any change to
# the rest of the list.

s24 <- st[st$YEAR == 2024, ]
s24 <- s24[order(-s24$rate_vep_tb), ]
n24 <- nrow(s24)
ij  <- t(combn(n24, 2))
dv  <- s24$rate_vap_tb[ij[, 1]] - s24$rate_vap_tb[ij[, 2]]
de  <- s24$rate_vep_tb[ij[, 1]] - s24$rate_vep_tb[ij[, 2]]
flip <- dv * de < 0
pairs24 <- data.frame(
  state_a  = s24$STATE[ij[flip, 1]], state_b = s24$STATE[ij[flip, 2]],
  vap_a    = s24$rate_vap_tb[ij[flip, 1]], vap_b = s24$rate_vap_tb[ij[flip, 2]],
  vep_a    = s24$rate_vep_tb[ij[flip, 1]], vep_b = s24$rate_vep_tb[ij[flip, 2]])
pairs24$swing <- abs(pairs24$vap_a - pairs24$vap_b) +
  abs(pairs24$vep_a - pairs24$vep_b)
pairs24 <- pairs24[order(-pairs24$swing), ]
dd_write_csv(pairs24, "derived/pairs2024.csv")

N_PAIRS <- nrow(ij)
N_FLIP  <- sum(flip)

# ===========================================================================
# 5. Validation
# ===========================================================================

# The paper's own regression, on the paper's own numbers, over the window the
# paper used. Reported per ELECTION rather than per year, because that is the
# unit the article quotes and the only way the two are comparable.
pres <- mp[mp$presidential, ]
w    <- pres[pres$year >= 1972, ]
fit  <- function(y, x) summary(lm(y ~ x))$coefficients[2, ]
f_vep_72 <- fit(w$vep_rate, w$year) * 4
f_vap_72 <- fit(w$vap_rate, w$year) * 4

ov  <- ep[!is.na(ep$pub_vep_err), ]
ov2 <- ep[!is.na(ep$vep_recon_pct_err), ]

nat24 <- nat[nat$YEAR == 2024, ]
GAP_2024 <- nat24$rate_vep_tb - nat24$rate_vap_tb
# How many additional ballots it would take for the VAP rate to read what the
# VEP rate already reads. The arithmetic that makes the gap concrete.
EXTRA <- nat24$VAP * nat24$rate_vep_tb / 100 - nat24$TOTAL_BALLOTS_COUNTED

f2 <- function(x) formatC(x, format = "f", digits = 2)
f3 <- function(x) formatC(x, format = "f", digits = 3)
cm <- function(x) format(round(x), big.mark = ",")

chk <- data.frame(
  check = c(
    "Table 1 data rows parsed",
    "Table 1 election years",
    "rows where the rebuilt VEP rate matches the printed one within 0.1 pts",
    "largest disagreement between rebuilt and printed VEP rate, pts",
    "rows where the printed adjustments sum to the printed VEP rate within 0.2 pts",
    "paper's VEP trend 1972-2000, pts per election",
    "its standard error",
    "paper's VAP trend over the same years, pts per election",
    "Elections Project rows, 1980-2024",
    "rows where VEP equals VAP*(1-noncitizen%) - felons + overseas within 0.1%",
    "largest disagreement on that identity, %",
    "rows with a published turnout rate to check against",
    "of those, rows reproduced within 0.05 pts",
    "largest disagreement with a published rate, pts",
    "rows where VOTE_FOR_HIGHEST_OFFICE holds a URL",
    "which years those are",
    "election years with no published turnout rate at all",
    "2024 national VAP-to-VEP gap, pts",
    "ballots that gap is worth",
    "state pairs compared in 2024",
    "pairs that reverse under the two denominators",
    "states whose 1980-2016 presidential trend changes sign",
    "elections published in both 1980-2000 sources",
    "mean revision to the numerator since 2001, %",
    "mean revision to the denominator since 2001, %",
    "largest revision to a published VAP turnout rate, pts"),
  value = c(
    length(DATA_LINES),
    paste0(min(mp$year), "-", max(mp$year)),
    sum(abs(mp$recon_err) < 0.1),
    f2(max(abs(mp$recon_err))),
    sum(abs(mp$adj_err) < 0.2),
    sprintf("%+.3f", f_vep_72[1]),
    f2(f_vep_72[2]),
    sprintf("%+.3f", f_vap_72[1]),
    cm(nrow(ep)),
    cm(sum(abs(ov2$vep_recon_pct_err) < 0.1)),
    f3(max(abs(ov2$vep_recon_pct_err))),
    nrow(ov),
    sum(abs(ov$pub_vep_err) < 0.05 & abs(ov$pub_vap_err) < 0.05),
    f2(max(pmax(abs(ov$pub_vep_err), abs(ov$pub_vap_err)))),
    sum(URL_ROWS),
    paste(URL_YEARS, collapse = ", "),
    paste(sort(unique(nat$YEAR[is.na(nat$VEP_TURNOUT_RATE)])), collapse = ", "),
    f2(GAP_2024),
    cm(EXTRA),
    cm(N_PAIRS),
    cm(N_FLIP),
    sum(trend$sign_flip),
    nrow(ovl),
    f3(mean(abs(ovl$num_move_pct))),
    f3(mean(abs(ovl$vap_move_pct))),
    f2(max(abs(ovl$vap_rate_move)))))
dd_write_csv(chk, "derived/checks.csv")

# Hard stops. Each one is a way the build could be quietly wrong.
stopifnot(
  length(DATA_LINES) == 27L,                    # 1948-2000, every even year
  nrow(mp) == 27L,
  max(abs(mp$recon_err)) < 0.1,                 # the parse read the right columns
  sum(abs(mp$adj_err) < 0.2) >= 25L,            # and the second, independent check
  nrow(ep) == 1196L,                            # 22 elections x 52 + 52
  max(abs(ov2$vep_recon_pct_err)) < 0.1,        # VEP is the documented identity
  max(abs(ov$pub_vep_err)) < 0.05,              # our rates are the published ones
  max(abs(ov$pub_vap_err)) < 0.05,
  all(trend$n == 10L),                          # every state, all ten elections
  N_FLIP > 0L,
  nrow(ovl) == 11L)

cat("\nbuilt on", FETCH_DATE, "\n")
print(chk, row.names = FALSE)
cat("\nfiles written:\n")
for (f in c("derived/mp1948.csv", "derived/national.csv", "derived/states.csv", "derived/state_trends.csv",
            "derived/pairs2024.csv", "derived/overlap.csv", "derived/checks.csv"))
  cat(sprintf("  %-18s %6s rows  %6.0f KB\n", f,
              format(nrow(read.csv(f)), big.mark = ","), file.size(f) / 1024))
prov_report()

# ---------------------------------------------------------------------------
# Build stamp. Records which script produced what is now in this directory --
# every file under derived/ and raw/ with its size, hash and row count, and the
# date this ran -- into BUILD-STAMP.tsv beside the data. See
# ../../../_lib/provenance.R. Guarded, because a missing helper must not fail a
# build that was otherwise fine.
if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_stamp()
}
