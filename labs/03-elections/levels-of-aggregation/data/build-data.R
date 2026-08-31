# build-data.R — levels of aggregation in election data
#
#   Rscript data/build-data.R      (run from the lab folder)
#
# This lab does not download anything. Every input is a CSV already committed to
# another lab in this course, and every number in the brief, the lab and the key
# is computed here from those files. Nothing is asserted.
#
# Inputs (all relative to book/labs/):
#   cast-vote-records/data/derived/sequences.csv     Alaska 2024 US House, one row
#                                                   per distinct ranking sequence
#   ga-precinct-returns/data/derived/ga2020_precincts.csv      Georgia 2020 president
#   ga-precinct-returns/data/derived/ga2020_vote_methods.csv   ... broken out by vote method
#   ga-precinct-returns/data/derived/ga2024_precincts.csv      Georgia 2024 president
#   ga-precinct-returns/data/derived/ga2024_counties.csv
#   data-sources/data/derived/pres2024_counties.csv  every county in the country
#   data-sources/data/derived/pres2024_states.csv
#   redistricting/data/derived/pres_by_cd_2024.csv   every congressional district
#   historical-campaigns/data/derived/pres_national.csv
#   eavs/data/derived/national.csv                   national unit counts
#   rpv/data/derived/houston_primary.csv             the graded ecological case
#
# Outputs: fourteen small CSVs in this folder.

# raw/ holds the sources as they arrive; derived/ is what this script writes.
source("../../../_lib/precision.R")   # dd_signif(): six significant digits
dir.create("derived", showWarnings = FALSE)

options(stringsAsFactors = FALSE, scipen = 999)

HERE <- tryCatch(normalizePath(dirname(sub("^--file=", "", grep("^--file=",
          commandArgs(), value = TRUE)[1]))), error = function(e) getwd())
if (is.na(HERE) || !nzchar(HERE)) HERE <- file.path(getwd(), "data")
L <- normalizePath(file.path(HERE, "..", "..", ".."))   # book/labs
rd <- function(p) read.csv(file.path(L, p), check.names = FALSE)
wr <- function(d, f) { write.csv(dd_signif(d), file.path(HERE, f), row.names = FALSE)
                       cat(sprintf("  %-22s %4d rows\n", f, nrow(d))) }

# weighted moments, used everywhere below
wmean <- function(x, w) sum(x * w) / sum(w)
wvar  <- function(x, w) sum(w * (x - wmean(x, w))^2) / sum(w)
wcor  <- function(x, y, w) {
  mx <- wmean(x, w); my <- wmean(y, w)
  sum(w * (x - mx) * (y - my)) / sum(w) / sqrt(wvar(x, w) * wvar(y, w))
}

cat("\n== 1. the bottom rung: Alaska's cast vote record ==\n")

sq <- rd("03-elections/cast-vote-records/data/derived/sequences.csv")
rk <- as.matrix(sq[, grep("^rank", names(sq))])
rk[is.na(rk)] <- ""
nrank  <- rowSums(rk != "")
BAL    <- sum(sq$ballots)
BLANK  <- sum(sq$ballots[nrank == 0])
ONE    <- sum(sq$ballots[nrank == 1])
VOTED  <- BAL - BLANK
DUP    <- sum(sq$ballots[apply(rk, 1, function(z) { z <- z[z != ""]
                                                   any(duplicated(z)) })])
SECOND <- sum(sq$ballots[rk[, 2] != ""])

# the full first-choice x second-choice table: this is what a ballot file is for
sec <- aggregate(ballots ~ first + second,
                 data.frame(first  = rk[, 1],
                            second = ifelse(rk[, 2] == "", "(no second choice)",
                                            rk[, 2]),
                            ballots = sq$ballots), sum)
sec <- sec[sec$first != "", ]
sec <- sec[order(sec$first, -sec$ballots), ]
wr(sec, "derived/cvr_second.csv")

X <- function(a, b) sec$ballots[sec$first == a & sec$second == b]
cvr <- data.frame(
  quantity = c("ballots in the file",
               "distinct ranking sequences",
               "ballots with no ranking at all",
               "ballots using exactly one ranking",
               "bullet-voting rate among ballots that ranked anyone (%)",
               "ballots that named a second choice",
               "ballots that ranked the same candidate more than once",
               "ranked Begich first and Peltola second",
               "ranked Begich first and Howe second",
               "ranked Peltola first and Begich second",
               "ranked Peltola first and Hafner second"),
  value = c(BAL, nrow(sq), BLANK, ONE, round(100 * ONE / VOTED, 1), SECOND, DUP,
            X("Begich, Nick", "Peltola, Mary S."),
            X("Begich, Nick", "Howe, John Wayne"),
            X("Peltola, Mary S.", "Begich, Nick"),
            X("Peltola, Mary S.", "Hafner, Eric")))
wr(cvr, "derived/cvr_facts.csv")

cat("\n== 2. how many units there are at each rung ==\n")

eav  <- rd("03-elections/eavs/data/derived/national.csv")
ejur <- rd("03-elections/eavs/data/derived/states.csv")
cty  <- rd("03-elections/data-sources/data/derived/pres2024_counties.csv")
cen  <- rd("03-elections/data-sources/data/derived/census_counties.csv")
cd   <- rd("06-putting-data-together/redistricting/data/derived/pres_by_cd_2024.csv")
sta  <- rd("03-elections/data-sources/data/derived/pres2024_states.csv")
nat  <- rd("03-elections/historical-campaigns/data/derived/pres_national.csv")
g24p <- rd("03-elections/ga-precinct-returns/data/derived/ga2024_precincts.csv")
g24c <- rd("03-elections/ga-precinct-returns/data/derived/ga2024_counties.csv")

NVOTERS <- eav$total[eav$item == "Total voters"]
NPREC   <- eav$total[eav$item == "Total precincts"]
NJUR    <- sum(ejur$jurisdictions)

ladder <- data.frame(
  rung = 0:6,
  level = c("ballot", "precinct", "election jurisdiction", "county",
            "congressional district", "state", "nation"),
  units_nationally = c(NVOTERS, NPREC, NJUR, nrow(cen), nrow(cd), nrow(sta), 1),
  units_in_georgia = c(NA, nrow(g24p), NA, nrow(g24c), sum(cd$state == "GA"), 1, NA),
  a_row_is = c("one voter's ballot",
               "one polling place's returns",
               "one office that runs elections",
               "one county's returns",
               "one seat in the U.S. House",
               "one state's returns",
               "the whole electorate"),
  published_for = c("a handful of jurisdictions",
                    "every state, in different formats",
                    "every state, once every two years",
                    "every state except Alaska",
                    "reconstructed from blocks by third parties",
                    "every state", "the country"))
ladder$share_of_voters_pct <- round(100 * ladder$units_nationally / NVOTERS, 4)
wr(ladder, "derived/ladder.csv")

CVR_COVERAGE <- round(100 * BAL / NVOTERS, 3)   # the whole Alaska file, as a
                                                # share of all 2024 voters
cat("  Alaska CVR covers", CVR_COVERAGE, "% of 2024 voters\n")

cat("\n== 3. one election, walked up the ladder (Georgia, 2024 president) ==\n")

two <- function(d, one, other) d[[one]] / (d[[one]] + d[[other]])

pr <- data.frame(level = "precinct",
                 unit  = paste(g24p$county, g24p$precinct, sep = "|"),
                 dem   = g24p$`Kamala D. Harris`,
                 rep   = g24p$`Donald J. Trump`)
co <- data.frame(level = "county", unit = g24c$county,
                 dem = g24c$`Kamala D. Harris`, rep = g24c$`Donald J. Trump`)
gacd <- cd[cd$state == "GA", ]
di <- data.frame(level = "congressional district", unit = gacd$district,
                 dem = NA_real_, rep = NA_real_)
gast <- sta[sta$state == "Georgia", ]
st <- data.frame(level = "state", unit = "Georgia", dem = NA_real_, rep = NA_real_)

lev <- rbind(pr, co, di, st)
lev$two_party_votes <- lev$dem + lev$rep
lev$dem_share <- 100 * lev$dem / lev$two_party_votes
lev$dem_share[lev$level == "congressional district"] <- gacd$dem_share
lev$dem_share[lev$level == "state"] <- 100 * gast$harris / (gast$harris + gast$trump)
lev <- lev[!is.na(lev$dem_share), ]
wr(lev, "derived/ga_levels.csv")

# the state total, from the precincts, is the yardstick every rung is checked against
GA_DEM <- sum(pr$dem); GA_REP <- sum(pr$rep)
GA_SHARE <- 100 * GA_DEM / (GA_DEM + GA_REP)

vp <- wvar(lev$dem_share[lev$level == "precinct"],
           lev$two_party_votes[lev$level == "precinct"])
vc <- wvar(lev$dem_share[lev$level == "county"],
           lev$two_party_votes[lev$level == "county"])

lstat <- do.call(rbind, lapply(unique(lev$level), function(k) {
  z <- lev[lev$level == k, ]
  w <- if (all(is.na(z$two_party_votes))) rep(1, nrow(z)) else z$two_party_votes
  data.frame(level = k, units = nrow(z),
             mean_dem_share = round(wmean(z$dem_share, w), 2),
             sd_dem_share   = round(sqrt(wvar(z$dem_share, w)), 2),
             min_dem_share  = round(min(z$dem_share), 1),
             max_dem_share  = round(max(z$dem_share), 1))
}))
lstat$variance_retained_pct <- round(100 * lstat$sd_dem_share^2 / vp, 1)
wr(lstat, "derived/level_stats.csv")

cat("\n== 4. does the arithmetic survive the climb? ==\n")

gacty <- cty[cty$state_name == "Georgia", ]
US_D <- sum(cty$votes_dem); US_R <- sum(cty$votes_gop)
NAT_D <- nat$pop_votes[nat$year == 2024 & nat$party == "Democratic"]
NAT_R <- nat$pop_votes[nat$year == 2024 & nat$party == "Republican"]

cmp <- function(what, a, b, la, lb)
  data.frame(comparison = what, from = la, value_from = a, against = lb,
             value_against = b, difference = a - b,
             pct_difference = round(100 * (a - b) / b, 4))

con <- rbind(
  cmp("Georgia Democratic votes", sum(pr$dem), sum(co$dem),
      "2,701 precincts summed", "159 counties, same source"),
  cmp("Georgia Democratic votes", sum(co$dem), sum(gacty$votes_dem),
      "159 counties, state source", "159 counties, national compilation"),
  cmp("Georgia Republican votes", sum(co$rep), sum(gacty$votes_gop),
      "159 counties, state source", "159 counties, national compilation"),
  cmp("National Democratic votes", US_D, NAT_D,
      paste(nrow(cty), "counties summed"), "certified national total"),
  cmp("National Republican votes", US_R, NAT_R,
      paste(nrow(cty), "counties summed"), "certified national total"))
con$value_from    <- format(con$value_from, big.mark = ",")
con$value_against <- format(con$value_against, big.mark = ",")
con$difference    <- format(con$difference, big.mark = ",")
wr(con, "derived/consistency.csv")

# a rung is not a rung everywhere. Which rows of the national "county" file are
# not counties? Compare its keys against the Census Bureau's own county list.
odd <- cty[!cty$county_fips %in% cen$fips, ]
notc <- as.data.frame(table(odd$state_name), responseName = "rows_that_are_not_counties")
names(notc)[1] <- "state"
notc$what_the_row_actually_is <- vapply(as.character(notc$state), function(s)
  odd$county_name[odd$state_name == s][1], character(1))
notc <- notc[order(-notc$rows_that_are_not_counties), ]
wr(notc, "derived/not_counties.csv")

# the rung that cannot be reached by summing: districts have no vote counts here,
# and the unweighted mean of district shares is not the state share
CD_MEAN <- mean(gacd$dem_share); CD_MED <- median(gacd$dem_share)
US_CD_MEAN <- mean(cd$dem_share); US_CD_MED <- median(cd$dem_share)
US_SHARE <- 100 * NAT_D / (NAT_D + NAT_R)
gap <- data.frame(
  measure = c("Georgia two-party Democratic share, from the precincts",
              "Georgia: unweighted mean of the 14 district shares",
              "Georgia: median district share",
              "National two-party Democratic share, certified",
              paste("National: unweighted mean of the", nrow(cd), "district shares"),
              "National: median district share",
              "Trump's share of the national popular vote (%)",
              "Trump's share of the electoral college (%)"),
  value = round(c(GA_SHARE, CD_MEAN, CD_MED, US_SHARE, US_CD_MEAN, US_CD_MED,
                  nat$pop_per[nat$year == 2024 & nat$party == "Republican"],
                  100 * nat$ec_votes[nat$year == 2024 & nat$party == "Republican"] /
                        nat$ec_total[nat$year == 2024 & nat$party == "Republican"]), 2))
wr(gap, "derived/seat_vote.csv")

cat("\n== 5. the pivot: a crosstab Georgia publishes, and what the aggregate says ==\n")

vm <- rd("03-elections/ga-precinct-returns/data/derived/ga2020_vote_methods.csv")
MAIL <- "Absentee by Mail Votes"
DEM  <- "Joseph R. Biden"; REP <- "Donald J. Trump"

# ---- the truth, straight off the published joint distribution ---------------
tt <- xtabs(votes ~ candidate + method, vm)
mt <- data.frame(candidate = rownames(tt), by_mail = as.numeric(tt[, MAIL]),
                 not_by_mail = as.numeric(rowSums(tt) - tt[, MAIL]))
mt$total <- mt$by_mail + mt$not_by_mail
mt$pct_of_this_candidates_votes_by_mail <- round(100 * mt$by_mail / mt$total, 1)
wr(mt, "derived/method_truth.csv")

TRUE_MAIL <- 100 * mt$by_mail[mt$candidate == DEM] / sum(mt$by_mail)
TRUE_NOT  <- 100 * mt$not_by_mail[mt$candidate == DEM] / sum(mt$not_by_mail)
MAILSHARE <- 100 * sum(mt$by_mail) / sum(mt$total)
cat(sprintf("  truth: Dem share of mail votes %.2f%%, of everything else %.2f%%\n",
            TRUE_MAIL, TRUE_NOT))

# ---- now throw the crosstab away and keep only the precinct margins ---------
k  <- paste(vm$county, vm$precinct, sep = "|")
tot  <- tapply(vm$votes, k, sum)
demv <- tapply(vm$votes[vm$candidate == DEM], k[vm$candidate == DEM], sum)
mailv<- tapply(vm$votes[vm$method == MAIL],  k[vm$method == MAIL],  sum)
ep <- data.frame(unit = names(tot), votes = as.numeric(tot),
                 dem = as.numeric(demv[names(tot)]),
                 mail = as.numeric(mailv[names(tot)]))
ep$dem[is.na(ep$dem)] <- 0; ep$mail[is.na(ep$mail)] <- 0
ep <- ep[ep$votes > 0, ]
ep$county    <- sub("\\|.*", "", ep$unit)
ep$mail_pct  <- 100 * ep$mail / ep$votes
ep$dem_pct   <- 100 * ep$dem  / ep$votes
wr(ep[, c("unit", "county", "votes", "dem", "mail", "mail_pct", "dem_pct")],
   "derived/eco_precinct.csv")

ec <- aggregate(cbind(votes, dem, mail) ~ county, ep, sum)
ec$mail_pct <- 100 * ec$mail / ec$votes
ec$dem_pct  <- 100 * ec$dem  / ec$votes
wr(ec, "derived/eco_county.csv")

# Goodman's ecological regression, and Duncan and Davis's assumption-free bounds
goodman <- function(d) {
  m <- lm(dem_pct ~ mail_pct, d, weights = d$votes)
  b <- coef(m)
  c(not_by_mail = unname(b[1]), by_mail = unname(b[1] + 100 * b[2]))
}
bounds <- function(d) {                       # per unit, then vote-weighted up
  x <- d$mail / d$votes; y <- d$dem / d$votes
  lo <- pmax(0, (y - (1 - x)) / x); hi <- pmin(1, y / x)
  ok <- x > 0
  c(lower = 100 * sum(d$mail[ok] * lo[ok]) / sum(d$mail[ok]),
    upper = 100 * sum(d$mail[ok] * hi[ok]) / sum(d$mail[ok]))
}
est <- function(d, label) {
  g <- goodman(d); b <- bounds(d)
  data.frame(level = label, units = nrow(d),
             correlation = round(wcor(d$mail_pct, d$dem_pct, d$votes), 3),
             est_by_mail = round(g[["by_mail"]], 1),
             est_not_by_mail = round(g[["not_by_mail"]], 1),
             bound_lower = round(b[["lower"]], 1),
             bound_upper = round(b[["upper"]], 1),
             truth_by_mail = round(TRUE_MAIL, 1),
             truth_not_by_mail = round(TRUE_NOT, 1))
}
eco <- rbind(est(ep, "precinct"), est(ec, "county"))
eco$error_by_mail <- round(eco$est_by_mail - eco$truth_by_mail, 1)
wr(eco, "derived/eco_estimates.csv")
print(eco[, c("level", "units", "correlation", "est_by_mail", "truth_by_mail",
              "bound_lower", "bound_upper")])

# ---- is the county-level correlation a fact about voters, or about lines? ---
# Regroup the same precincts at random into the same number of groups, 500 times.
set.seed(84355)
NG <- nrow(ec)
null_r <- replicate(500, {
  g <- sample(rep(seq_len(NG), length.out = nrow(ep)))
  z <- aggregate(cbind(votes, dem, mail) ~ g, data.frame(ep, g), sum)
  wcor(100 * z$mail / z$votes, 100 * z$dem / z$votes, z$votes)
})
maup <- data.frame(
  grouping = c("2,653 precincts, ungrouped",
               paste(NG, "real counties"),
               paste(NG, "random groups of precincts (mean of 500 draws)")),
  units = c(nrow(ep), NG, NG),
  correlation = round(c(wcor(ep$mail_pct, ep$dem_pct, ep$votes),
                        wcor(ec$mail_pct, ec$dem_pct, ec$votes),
                        mean(null_r)), 3))
R_COUNTY <- wcor(ec$mail_pct, ec$dem_pct, ec$votes)
EXCEED   <- sum(null_r >= R_COUNTY)
maup$note <- c("the finest unit with published returns",
               "the same ballots, grouped by county line",
               sprintf("range %.3f to %.3f; %d of 500 draws reach the county figure",
                       min(null_r), max(null_r), EXCEED))
wr(maup, "derived/maup_null.csv")
wr(data.frame(draw = seq_along(null_r), correlation = round(null_r, 4)),
   "derived/maup_draws.csv")

cat("\n== 6. the graded case, re-derived (not rebuilt) ==\n")

h <- rd("06-putting-data-together/rpv/data/derived/houston_primary.csv")
hm <- lm(I(dem / voters) ~ I(black / voters), h, weights = h$voters)
rpv <- data.frame(
  quantity = c("Black voters in the 17 precincts",
               "Black voters who took a Democratic ballot",
               "the truth: Democratic share of Black voters (%)",
               "Goodman's ecological regression on the same precincts (%)",
               "overshoot, in percentage points"),
  value = c(sum(h$black), sum(h$black_dem),
            round(100 * sum(h$black_dem) / sum(h$black), 1),
            round(100 * sum(coef(hm)), 1),
            round(100 * sum(coef(hm)) - 100 * sum(h$black_dem) / sum(h$black), 1)))
wr(rpv, "derived/rpv_check.csv")

cat("\n== 7. the decision table ==\n")

dec <- data.frame(
  question = c(
    "Did Begich voters who ranked a second choice prefer Peltola or Howe?",
    "What share of a candidate's votes arrived by mail?",
    "Is voting in this county racially polarized?",
    "Which neighbourhoods swung between two elections?",
    "Which doors should a campaign knock on Saturday?",
    "How many mail ballots were rejected, and for what reason?",
    "Did counties with less education move right?",
    "Does this map convert votes into seats fairly?",
    "Who won the presidency?",
    "Is the country's partisan balance shifting across decades?"),
  right_rung = c("ballot", "ballot, or precinct-by-method returns", "precinct",
                 "precinct", "precinct", "election jurisdiction", "county",
                 "congressional district", "state", "nation"),
  why = c(
    "The two rankings must sit on the same ballot. No total can restore them.",
    "It is a joint distribution. Georgia publishes it; most states do not.",
    "Smallest unit with returns; demographics attach through blocks.",
    "County hides the variation; but check the boundary did not move.",
    "Fine enough to route a walk list, coarse enough to be stable.",
    "The jurisdiction is what administers and reports rejection.",
    "'County' is the unit the education measure is published on.",
    "The seat is the district. Precincts must be aggregated into it first.",
    "The electoral college is counted in states, not in voters.",
    "Only the top rung gives one comparable number over a long series."),
  the_trap = c(
    "Precinct returns for an RCV race give round-by-round totals, not pairs.",
    "Regressing precinct Dem share on precinct mail share returns 194%.",
    "The estimate is an inference, not a measurement. Report the bounds.",
    "A precinct that 'swung' may just have gained three hundred houses.",
    "A precinct is not a person; targeting is a bet on an average.",
    "'Jurisdiction' is a township in Wisconsin and a county in Texas.",
    "Robinson (1950): the county pattern need not hold for county residents.",
    "Districts do not nest into counties; someone had to disaggregate blocks.",
    "The popular vote is not the quantity being decided.",
    "Anything below the top rung has changed shape underneath the series."))
wr(dec, "derived/decision.csv")

# the same judgement as a grid, for the matrix figure
qs <- c("second choices on a ballot", "vote method by candidate",
        "racially polarized voting", "neighbourhood swing",
        "ballot rejection rates", "seats from votes",
        "who won the presidency", "national trend since 1864")
lv <- c("ballot", "precinct", "jurisdiction", "county", "district", "state", "nation")
#  2 = the right rung   1 = usable, with care   0 = wrong rung   -1 = impossible
grid <- rbind(
  c( 2, -1, -1, -1, -1, -1, -1),
  c( 2,  1, -1, -1, -1, -1, -1),
  c( 1,  2,  0,  1,  0,  0, -1),
  c( 1,  2,  0,  1,  0,  0, -1),
  c(-1,  0,  2,  1,  0,  1,  1),
  c( 0,  1,  0,  0,  2,  1,  0),
  c( 0,  0,  0,  0,  0,  2,  0),
  c(-1, -1, -1,  0,  0,  1,  2))
dimnames(grid) <- list(qs, lv)
sm <- data.frame(question = rep(qs, times = length(lv)),
                 level = rep(lv, each = length(qs)),
                 fit = as.vector(grid))
sm$label <- c("impossible", "wrong rung", "with care", "the right rung")[sm$fit + 2]
wr(sm, "derived/suitability.csv")

cat("\n== headline numbers ==\n")
cat(sprintf("  ballots in the Alaska file        %s (%.3f%% of 2024 voters)\n",
            format(BAL, big.mark = ","), CVR_COVERAGE))
cat(sprintf("  Begich first, Peltola second      %s\n",
            format(X("Begich, Nick", "Peltola, Mary S."), big.mark = ",")))
cat(sprintf("  GA 2024 two-party Dem share       %.2f%% at every weighted rung\n", GA_SHARE))
cat(sprintf("  variance retained at county level %.1f%%\n",
            lstat$variance_retained_pct[lstat$level == "county"]))
cat(sprintf("  truth / Goodman (precinct)        %.1f%% vs %.1f%%\n",
            TRUE_MAIL, eco$est_by_mail[1]))
cat(sprintf("  truth / Goodman (county)          %.1f%% vs %.1f%%\n",
            TRUE_MAIL, eco$est_by_mail[2]))
cat(sprintf("  Duncan-Davis bounds (precinct)    %.1f%% to %.1f%%\n",
            eco$bound_lower[1], eco$bound_upper[1]))
cat("\ndone.\n")

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
