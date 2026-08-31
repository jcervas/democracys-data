# ---------------------------------------------------------------------------
# Build the age-structure dataset: who is old enough to vote, and who voted.
#
# Four files end up in derived/:
#
#   derived/bands.csv   one row per year per age band: the voting-age
#                       population, the share who said they voted, and the
#                       count that implies
#   derived/shares.csv  each band's share of the adult population beside its
#                       share of the voters, one row per year per band
#   derived/checks.csv  the arithmetic this script verifies before writing
#   derived/facts.csv   single numbers the brief quotes
#
# Run this script from inside the data/ folder. It needs a network connection
# and `readxl`; the committed output means the lab needs neither.
# ---------------------------------------------------------------------------

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
# U.S. Census Bureau, Current Population Survey, Voting and Registration
# Supplement, Table A-1 (historical time series):
#   https://www2.census.gov/programs-surveys/cps/tables/time-series/
#     voting-historical-time-series/hst_vote01.xlsx
#
# The same workbook the validated-turnout chapter reads, opened at a different
# place. That chapter takes the national totals; this one takes the FOUR AGE
# BLOCKS further down the same sheet -- 18 to 24, 25 to 44, 45 to 64, and 65
# and over -- each with a voting-age population and a percentage who said they
# voted, every federal election from 1964 to 2024.
#
# THIS IS A SURVEY, AND IT IS A SURVEY ABOUT VOTING. Roughly 60,000 households
# are asked, after the election, whether they voted, and nobody checks. The
# validated-turnout chapter establishes that the answer is consistently too high.
# Everything below is therefore the age structure of people who SAID they
# voted, which is not quite the age structure of voters, and the brief says so
# rather than quietly calling it turnout.

URL <- paste0("https://www2.census.gov/programs-surveys/cps/tables/",
              "time-series/voting-historical-time-series/hst_vote01.xlsx")
tmp <- tempfile(fileext = ".xlsx")
prov_fetch(URL, tmp, mode = "wb", quiet = TRUE)
stopifnot(requireNamespace("readxl", quietly = TRUE))
suppressMessages(x <- readxl::read_excel(tmp, sheet = 1, col_names = FALSE))
x <- as.data.frame(x, stringsAsFactors = FALSE)

# --- Finding the blocks ------------------------------------------------------
#
# The sheet is not a table; it is ten tables stacked in one column with their
# headings written into column A. Rather than hardcode row numbers that a
# future release would silently move, find the headings and read until the
# years stop.

a <- as.character(x[[1]])
lab <- which(!is.na(a) & grepl(", Percent (Voted|Registered)$", a))
stopifnot(length(lab) == 10)

read_block <- function(start) {
  out <- list(); i <- start + 1L
  while (i <= nrow(x)) {
    y <- a[i]
    if (is.na(y) || !grepl("^[0-9]{4}$", y)) break
    out[[length(out) + 1L]] <- data.frame(
      year = as.integer(y),
      vap  = suppressWarnings(as.numeric(x[i, 2])),   # thousands
      pct_total   = suppressWarnings(as.numeric(x[i, 3])),
      pct_citizen = suppressWarnings(as.numeric(x[i, 4])),
      stringsAsFactors = FALSE)
    i <- i + 1L
  }
  do.call(rbind, out)
}

blocks <- do.call(rbind, lapply(lab, function(i) {
  h <- a[i]
  b <- read_block(i)
  b$band    <- sub(",.*$", "", h)
  b$measure <- ifelse(grepl("Voted$", h), "voted", "registered")
  b
}))

# Percentages are on the TOTAL-population basis, not the citizen basis, because
# the population column beside them is the total voting-age population. Mixing
# the two would divide a citizen percentage into a non-citizen denominator.
vt <- blocks[blocks$measure == "voted", ]
rg <- blocks[blocks$measure == "registered",
             c("year", "band", "pct_total")]
names(rg)[3] <- "pct_registered"
b  <- merge(vt[, c("year", "band", "vap", "pct_total")], rg,
            by = c("year", "band"))
names(b)[names(b) == "pct_total"] <- "pct_voted"
b$voters <- round(b$vap * b$pct_voted / 100, 1)          # thousands

BANDS <- c("18 to 24 Years", "25 to 44 Years", "45 to 64 Years",
           "65 Years and Over")
b$band <- factor(b$band, levels = c("Total", BANDS))
b <- b[order(b$year, b$band), ]

# --- Checks ------------------------------------------------------------------
#
# The four age bands should account for the total, and they are published
# separately, so this is a real check rather than a tautology. The Bureau
# rounds to thousands, so exact equality is not expected; the tolerance is
# stated rather than assumed.

tot <- b[b$band == "Total", ]
par_ <- b[b$band != "Total", ]
agg <- aggregate(cbind(vap, voters) ~ year, par_, sum)
cmp <- merge(agg, tot[, c("year", "vap", "voters")], by = "year",
             suffixes = c("_bands", "_total"))
cmp$vap_err <- 100 * (cmp$vap_bands - cmp$vap_total) / cmp$vap_total
cmp$vot_err <- 100 * (cmp$voters_bands - cmp$voters_total) / cmp$voters_total
stopifnot(max(abs(cmp$vap_err)) < 0.5, max(abs(cmp$vot_err)) < 1.5)

checks <- data.frame(
  check = c("age blocks found", "years covered",
            "largest gap, bands against total (population)",
            "largest gap, bands against total (voters)"),
  value = c(length(lab), length(unique(b$year)),
            paste0(round(max(abs(cmp$vap_err)), 3), "%"),
            paste0(round(max(abs(cmp$vot_err)), 3), "%")),
  stringsAsFactors = FALSE)
write.csv(checks, "derived/checks.csv", row.names = FALSE)

write.csv(b[, c("year", "band", "vap", "pct_voted", "pct_registered",
                "voters")],
          "derived/bands.csv", row.names = FALSE)

# --- Shares ------------------------------------------------------------------
#
# The two pyramids, as numbers: each band's share of the adult population and
# its share of the people who said they voted. The difference between those two
# columns is the whole subject of the chapter.

sh <- do.call(rbind, lapply(split(par_, par_$year), function(z) {
  data.frame(year = z$year[1], band = z$band,
             pop_share  = round(100 * z$vap / sum(z$vap), 3),
             vote_share = round(100 * z$voters / sum(z$voters), 3),
             stringsAsFactors = FALSE)
}))
sh$gap <- round(sh$vote_share - sh$pop_share, 3)
sh <- sh[order(sh$year, sh$band), ]
write.csv(sh, "derived/shares.csv", row.names = FALSE)

# --- Facts -------------------------------------------------------------------

LAST <- max(b$year)
FIRST <- min(b$year)
PRES <- sort(unique(b$year[b$year %% 4 == 0]))
LP <- max(PRES)
now  <- sh[sh$year == LP, ]
old  <- sh[sh$year == min(PRES), ]
bn   <- b[b$year == LP & b$band != "Total", ]

young <- "18 to 24 Years"; older <- "65 Years and Over"
facts <- data.frame(
  key = c("first", "last", "last_pres", "elections",
          "y_pop", "y_vote", "y_gap", "o_pop", "o_vote", "o_gap",
          "y_turn", "o_turn", "turn_ratio",
          "y_pop_first", "y_vote_first", "o_pop_first", "o_vote_first",
          "vap_last", "voters_last", "turn_last",
          "y_reg", "o_reg"),
  value = c(FIRST, LAST, LP, length(unique(b$year)),
            now$pop_share[now$band == young], now$vote_share[now$band == young],
            now$gap[now$band == young],
            now$pop_share[now$band == older], now$vote_share[now$band == older],
            now$gap[now$band == older],
            bn$pct_voted[bn$band == young], bn$pct_voted[bn$band == older],
            round(bn$pct_voted[bn$band == older] /
                  bn$pct_voted[bn$band == young], 2),
            old$pop_share[old$band == young], old$vote_share[old$band == young],
            old$pop_share[old$band == older], old$vote_share[old$band == older],
            round(tot$vap[tot$year == LP]), round(tot$voters[tot$year == LP]),
            tot$pct_voted[tot$year == LP],
            bn$pct_registered[bn$band == young],
            bn$pct_registered[bn$band == older]),
  stringsAsFactors = FALSE)
write.csv(facts, "derived/facts.csv", row.names = FALSE)

cat("bands.csv  ->", nrow(b), "rows,", length(unique(b$year)), "elections\n")
cat("shares.csv ->", nrow(sh), "rows\n")
cat("bands sum to the published total within",
    round(max(abs(cmp$vap_err)), 3), "% (population),",
    round(max(abs(cmp$vot_err)), 3), "% (voters)\n\n")
cat(LP, ": 18-24 are ", now$pop_share[now$band == young], "% of adults and ",
    now$vote_share[now$band == young], "% of voters\n", sep = "")
cat("      65+ are ", now$pop_share[now$band == older], "% of adults and ",
    now$vote_share[now$band == older], "% of voters\n", sep = "")
cat("done.\n")

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
