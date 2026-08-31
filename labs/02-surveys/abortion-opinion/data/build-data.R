# ---------------------------------------------------------------------------
# Build the abortion opinion dataset: the most recent study first, then change.
#
# Six files end up in this folder:
#
#   raw/item.txt   A real capture: the four options as they are read to the
#                  respondent, and five real rows.
#   derived/now.csv        The most recent study, all four categories.
#   derived/by_party.csv   The most recent study, by the seven-point party scale.
#   derived/by_ideology.csv  The most recent study, by ideological self-placement --
#                  INCLUDING the people who declined to place themselves.
#   derived/overtime.csv   Every study year, all four categories.
#   derived/gaps.csv       Every study year, the party gap and the ideology gap.
#
# HOW THIS CHAPTER IS ORDERED, and why. It opens on the latest study and only
# then goes backwards. Most survey chapters run the other way, and the cost of
# that habit is that the reader meets a trend line before they know what a
# single row means. Here the reader sees what Americans said in the most recent
# study, then what it looks like split by the two identities the companion
# chapters built, and only then how any of it moved.
#
# IT DEPENDS ON TWO OTHER CHAPTERS, deliberately. Party identification and
# ideology are not re-derived here; they are used, with the decisions those
# chapters exposed carried through explicitly:
#
#   - PARTY. The seven-point scale is kept at seven points rather than
#     collapsed, because the party-id chapter shows that the collapse is where
#     the leaners disappear. Abortion opinion is reported for leaners
#     separately so a reader can see whether they resemble independents (they
#     do not) or partisans (they do).
#   - IDEOLOGY. The people who answered "haven't thought much about it" are
#     kept as their own row rather than dropped, because the ideology chapter
#     shows the codebook files that response under Valid. They are about a
#     fifth of respondents and they have abortion opinions.
#
# THE FINDING IN THE MOST RECENT STUDY. Between 2020 and 2024 the share saying
# abortion should NEVER be permitted roughly halves, from about 10.5% to about
# 5.3%. The share saying it should ALWAYS be permitted does not rise to meet
# it -- it falls slightly. The movement is into the conditional middle: "only
# after the need has been clearly established" gains about six points. The
# headline number barely moves while the structure underneath it reorganises,
# which is the same shape as the regional-shift chapter's net-versus-gross
# lesson in a different subject.
#
# A DOCUMENTED HOLE. In 2008 this item is about 55% missing, and that is not a
# defect: the codebook records that a random half sample got a branching series
# of questions instead. Read the note and your n is halved for a known reason.
# Miss it and 2008 looks like a catastrophic non-response year. The build keeps
# 2008 and flags it rather than dropping it quietly.
#
# CODING, from the ANES codebook:
#   VCF0004  study year
#   VCF0838  by law, when should abortion be allowed
#            1 never permitted
#            2 only in case of rape, incest, or danger to the woman's life
#            3 for other reasons, but only after the need has been clearly
#              established
#            4 always, as a matter of personal choice
#            9 DK; other        0 NA; no post interview; 2008 split versions
#   VCF0301  party identification, 7 point -- see the party-id chapter
#   VCF0803  liberal-conservative, 7 point, 9 = haven't thought much about it
#            -- see the ideology chapter
#
# SOURCE. American National Election Studies, Time Series Cumulative Data File
# 1948-2024, version of 5 February 2026.
#   https://electionstudies.org/data-center/anes-time-series-cumulative-data-file/
# Requested, not handed over; 403 to a script, 200 to a browser. 163 MB, not
# redistributed here.
#
# Run from this directory:  Rscript build-data.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)

options(stringsAsFactors = FALSE)

CAND <- path.expand(c(
  "raw/anes_timeseries_cdf_csv_20260205.csv",
  "~/Downloads/anes_timeseries_cdf_csv_20260205.csv",
  "~/Downloads/anes_timeseries_cdf_csv_20260205.zip",
  "raw/anes_timeseries_cdf_csv_20260205.zip"))
hit <- CAND[file.exists(CAND)]
if (!length(hit)) stop(
  "ANES cumulative file not found. Download the CSV distribution from\n",
  "  https://electionstudies.org/data-center/anes-time-series-cumulative-data-file/\n",
  "and put it in this folder's raw/ subdirectory.")

src <- hit[1]
if (grepl("\\.zip$", src)) {
  inner <- grep("\\.csv$", unzip(src, list = TRUE)$Name, value = TRUE)[1]
  hdr <- names(read.csv(unz(src, inner), nrows = 1, check.names = FALSE))
  reader <- function(cc) read.csv(unz(src, inner), colClasses = cc,
                                  check.names = FALSE)
} else {
  hdr <- names(read.csv(src, nrows = 1, check.names = FALSE))
  reader <- function(cc) read.csv(src, colClasses = cc, check.names = FALSE)
}
cat("reading:", src, "\n")

KEEP <- c("VCF0004", "VCF0838", "VCF0301", "VCF0803")
stopifnot(all(KEEP %in% hdr))
d <- reader(ifelse(hdr %in% KEEP, NA, "NULL"))
stopifnot(nrow(d) > 70000)

ALAB <- c("Never permitted",
          "Only for rape, incest, or danger to the woman's life",
          "Other reasons, but only after the need is established",
          "Always, as a matter of personal choice")
PLAB <- c("Strong Democrat", "Weak Democrat", "Independent-Democrat",
          "Independent-Independent", "Independent-Republican",
          "Weak Republican", "Strong Republican")
ILAB <- c("Extremely liberal", "Liberal", "Slightly liberal",
          "Moderate, middle of the road", "Slightly conservative",
          "Conservative", "Extremely conservative")

ab <- d$VCF0838; ab[!ab %in% 1:4] <- NA
pid <- d$VCF0301; pid[!pid %in% 1:7] <- NA
ide <- d$VCF0803; ide[!ide %in% c(1:7, 9)] <- NA

LAST <- max(d$VCF0004[!is.na(ab)])
now  <- !is.na(d$VCF0004) & d$VCF0004 == LAST

# --- 1. a real capture ------------------------------------------------------

set.seed(84355)
w <- sort(sample(which(now & !is.na(ab) & !is.na(pid) & !is.na(ide)), 5))
cap <- file("raw/item.txt", "w")
writeLines(c(
"The respondent is handed a booklet and asked which opinion best",
"agrees with their view. The four options, verbatim:",
"",
"  1  By law, abortion should never be permitted.",
"",
"  2  The law should permit abortion only in case of rape, incest,",
"     or when the woman's life is in danger.",
"",
"  3  The law should permit abortion for reasons other than rape,",
"     incest, or danger to the woman's life, but only after the need",
"     for the abortion has been clearly established.",
"",
"  4  By law, a woman should always be able to obtain an abortion as",
"     a matter of personal choice.",
"",
"  9  DK; other",
"",
paste0("Five random rows from the ", LAST, " study, with the two identity"),
"columns this chapter uses alongside:",
""), cap)
utils::write.table(
  data.frame(year = d$VCF0004[w], VCF0838 = ab[w],
             VCF0301 = pid[w], VCF0803 = ide[w],
             abortion_view = substr(ALAB[ab[w]], 1, 34)),
  cap, sep = "  ", quote = FALSE, row.names = FALSE)
writeLines(c("",
"Option 3 is the one to watch. It is not a middle position between",
"1 and 4 -- it is a permission with a gatekeeper, and it is where",
"the movement of the last four years went."), cap)
close(cap)

# --- 2. the most recent study -----------------------------------------------

v <- ab[now & !is.na(ab)]
nw <- data.frame(code = 1:4, view = ALAB,
                 respondents = as.vector(table(factor(v, levels = 1:4))))
nw$pct <- round(100 * nw$respondents / sum(nw$respondents), 1)
nw$year <- LAST
write.csv(nw, "derived/now.csv", row.names = FALSE)

# --- 3. by party, and by ideology, in that study ----------------------------

share4 <- function(g, levels_, labels_) {
  k <- now & !is.na(ab) & !is.na(g)
  tb <- table(factor(g[k], levels = levels_), factor(ab[k], levels = 1:4))
  out <- data.frame(group = labels_, respondents = as.vector(rowSums(tb)))
  for (j in 1:4) out[[paste0("pct_", j)]] <-
    round(100 * as.vector(tb[, j]) / rowSums(tb), 1)
  out
}
bp <- share4(pid, 1:7, PLAB)
write.csv(bp, "derived/by_party.csv", row.names = FALSE)

bi <- share4(ide, c(1:7, 9), c(ILAB, "Haven't thought much about it"))
write.csv(bi, "derived/by_ideology.csv", row.names = FALSE)

# --- 4. every year ----------------------------------------------------------

yrs <- sort(unique(d$VCF0004[!is.na(ab)]))
ot <- do.call(rbind, lapply(yrs, function(y) {
  k <- !is.na(d$VCF0004) & d$VCF0004 == y
  v <- ab[k & !is.na(ab)]
  asked <- sum(k & d$VCF0838 %in% c(1:4, 9))
  data.frame(year = y, respondents = length(v),
             never = round(100 * mean(v == 1), 1),
             rape_incest_life = round(100 * mean(v == 2), 1),
             need_established = round(100 * mean(v == 3), 1),
             always = round(100 * mean(v == 4), 1),
             pct_missing = round(100 * (1 - asked / sum(k)), 1))
}))
write.csv(ot, "derived/overtime.csv", row.names = FALSE)

# --- 5. the two gaps, every year --------------------------------------------
#
# Party gap: share saying "always" among Democrats including leaners, minus the
# same among Republicans including leaners. Leaners are INCLUDED because the
# party-id chapter shows they behave like partisans; the choice is stated here
# rather than buried, and the pure independents are reported separately.
#
# Ideology gap: liberals (1-3) minus conservatives (5-7). Non-placers are
# reported as their own column, not folded into either side.

gp <- do.call(rbind, lapply(yrs, function(y) {
  # VCF0004 carries NAs, so the year test alone yields NA rather than FALSE.
  # Every selector below is forced to a plain logical before it is used.
  k <- !is.na(d$VCF0004) & d$VCF0004 == y & !is.na(ab)
  dm <- k & pid %in% 1:3; rp <- k & pid %in% 5:7; ii <- k & pid %in% 4
  lb <- k & ide %in% 1:3; cs <- k & ide %in% 5:7; dk <- k & ide %in% 9
  f <- function(sel) if (sum(sel, na.rm = TRUE) < 25) NA_real_ else
    round(100 * mean(ab[sel] == 4), 1)
  data.frame(year = y,
             dem_always = f(dm), rep_always = f(rp), ind_always = f(ii),
             lib_always = f(lb), con_always = f(cs), notplaced_always = f(dk))
}))
gp$party_gap    <- round(gp$dem_always - gp$rep_always, 1)
gp$ideology_gap <- round(gp$lib_always - gp$con_always, 1)
write.csv(gp, "derived/gaps.csv", row.names = FALSE)

# --- report -----------------------------------------------------------------

prev <- ot$year[which(ot$year == LAST) - 1]
p <- function(y, col) ot[[col]][ot$year == y]
cat(sprintf("\nnow.csv       : the %d study, %s respondents\n",
            LAST, format(sum(nw$respondents), big.mark = ",")))
print(nw[, c("view", "respondents", "pct")], row.names = FALSE)
cat(sprintf("\n  Change from %d to %d:\n", prev, LAST))
for (cc in c("never", "rape_incest_life", "need_established", "always"))
  cat(sprintf("    %-18s %5.1f%% -> %5.1f%%  (%+.1f)\n", cc,
              p(prev, cc), p(LAST, cc), p(LAST, cc) - p(prev, cc)))
cat("\nby_party.csv  : % saying 'always', by the seven-point scale\n")
print(bp[, c("group", "respondents", "pct_4")], row.names = FALSE)
cat("\nby_ideology.csv : % saying 'always', including the non-placers\n")
print(bi[, c("group", "respondents", "pct_4")], row.names = FALSE)
cat(sprintf("\ngaps.csv      : party gap %+.1f in %d, %+.1f in %d\n",
            gp$party_gap[gp$year == min(yrs)], min(yrs),
            gp$party_gap[gp$year == LAST], LAST))
cat(sprintf("                ideology gap %+.1f -> %+.1f\n",
            gp$ideology_gap[gp$year == min(yrs)],
            gp$ideology_gap[gp$year == LAST]))
cat(sprintf("\n2008 is %.1f%% missing -- the documented split half sample.\n",
            ot$pct_missing[ot$year == 2008]))

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
