# ---------------------------------------------------------------------------
# Build the panhandle-claim dataset.
#
#   Rscript build-data.R        (run from this folder, data/)
#
# THE CLAIM UNDER TEST, which circulates in Florida politics and was put to
# this chapter in those words: PANHANDLE VOTERS WOULD NOT VOTE FOR THE BLACK
# CANDIDATE. Byron Donalds won the Republican nomination for governor on
# 18 August 2026 with 47.8% of an eleven-way field, and ran far behind that
# number across north Florida.
#
# SOURCES. Florida Department of State, Division of Elections. Four files, all
# open downloads, no login and no guestbook:
#   2026 primary results, all 67 counties, every office:
#   https://flelectionfiles.floridados.gov/enightfilespublic/20260818_ElecResultsFL.txt
#   2018 and 2014 primary results, from the results archive's extract utility:
#   https://results.elections.myflorida.com/downloadresults.asp?ElectionDate=8/28/2018
#   Book-closing registration for this election, by county, party and race:
#   https://dos.fl.gov/elections/data-statistics/voter-registration-statistics/bookclosing/
# Plus the county outlines and 2024 presidential margin from `mapping`, and
# county population by race from `surnames`.
#
# THE FOUR TESTS, and this is the shape of the chapter:
#
#   1. SAME BALLOT. Four statewide Republican primaries ran that day. If a
#      region is simply hostile to frontrunners, all four winners take the hit.
#      They do not: in the Panhandle Donalds runs -7.5 and Moody runs +0.6.
#
#   2. THE 2018 BENCHMARK. DeSantis, white and Trump-endorsed, ran in the last
#      contested Republican primary for this office. His county-level pattern
#      correlates 0.72 with Donalds' and his SPREAD WAS LARGER. Most of north
#      Florida's 2026 deficit is not new.
#
#   3. WHERE THE VOTES WENT. Half the Big Bend deficit goes to Bobby Williams,
#      who took 4.1% statewide -- and whose best counties include Glades,
#      Hardee and Okeechobee, three hundred miles south. That is an
#      agricultural pattern, not a regional or a racial one.
#
#   4. THE ELECTORATE. Black voters are 1.4% of registered Republicans in the
#      Panhandle. That single number is why no amount of finer data answers
#      the question, and it is the chapter's last section.
#
# WHY THERE IS NO RACIALLY POLARIZED VOTING ESTIMATE HERE. An earlier draft
# gave three reasons and the middle one was wrong, so all three are restated.
#
#   The returns exist. 17 of the 29 north Florida counties published precinct
#   results and they are in raw/enr-precinct/.
#
#   The composition is obtainable. Florida's voter file is public by law, free,
#   and carries RACE, PARTY AND PRECINCT on every record, so precinct-level
#   Republican composition can be counted -- no boundaries, no address
#   matching. It takes a written request rather than a download, which
#   `voter-file-access` measures. The earlier draft claimed this cross "is not
#   a public number anywhere", reasoning from the published summary tables to
#   the underlying record. That is the error this course exists to prevent.
#
#   The method would confirm what bounds already show. Where an electorate is
#   91.3% white the aggregate result nearly IS the white result: Duncan and
#   Davis's bounds pin white support between 34.6% and 44.2% with no
#   assumptions. See PART 5. What no file records is WHY, and that is the
#   actual wall.
#
#   For BLACK Republicans the group share does not vary -- 0.4% to 4.7% across
#   all 67 counties -- so that estimate really would be extrapolation. But they
#   are not the group the claim is about.
#
# Produces, in derived/:
#   county_results.csv    every candidate, every county, the 2026 primary
#   deviations.csv        each statewide winner's county deviation, one row per county
#   benchmark.csv         2026 against 2018, county by county, and the residual
#   regions.csv           the three regions, four winners, two elections
#   williams.csv          the agricultural footprint of a 4% candidate
#   electorate.csv        registered Republicans by race, by county
#   county_map.csv        county outlines, with 2026 deviation attached
#   facts.csv             every number the chapter states
# ---------------------------------------------------------------------------

source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)
options(scipen = 999, stringsAsFactors = FALSE)

norm <- function(x) {
  x <- iconv(x, "UTF-8", "ASCII//TRANSLIT")
  trimws(tolower(ifelse(is.na(x), "", x)))
}
ckey <- function(x) gsub("[^a-z]", "", norm(x))

# ===========================================================================
# PART 1 -- THE 2026 PRIMARY
# ===========================================================================

# Latin-1: the file carries candidate names that are not ASCII, and a UTF-8
# read mangles them without complaining.
off <- read.csv("raw/fldos/20260818_ElecResultsFL.txt", sep = "\t",
                fileEncoding = "latin1", check.names = FALSE)
off$county <- trimws(off$CountyName)
off$last   <- trimws(off$CanNameLast)
stopifnot(length(unique(off$CountyCode)) == 67)

gov <- off[off$RaceCode == "GOV" & off$PartyCode == "REP", ]
res <- aggregate(list(votes = gov$CanVotes),
                 by = list(county = gov$county, candidate = gov$last), FUN = sum)
ctot <- tapply(res$votes, res$county, sum)
res$county_total <- as.integer(ctot[res$county])
res$share <- 100 * res$votes / res$county_total
res <- res[order(res$county, -res$votes), ]
dd_write_csv(res, "derived/county_results.csv")

statewide <- tapply(res$votes, res$candidate, sum)
statewide <- sort(statewide, decreasing = TRUE)
SW_GOV <- 100 * statewide / sum(statewide)
cat("2026 Republican primary for governor, statewide:\n")
print(round(head(SW_GOV, 5), 2))
cat(sprintf("total votes %s across %d counties\n\n",
            format(sum(statewide), big.mark = ","), length(ctot)))

# ---- the four statewide Republican winners, on one ballot -----------------
#
# The comparison the chapter rests on. Field sizes differ wildly -- eleven
# candidates for governor, two for agriculture -- so raw shares are not
# comparable between contests. Each winner's DEVIATION FROM THEIR OWN
# statewide share is, and that is what is built here.
WIN <- c(GOV = "Donalds", USS = "Moody", CFO = "Ingoglia", AGR = "Simpson")
dev <- NULL
for (rc in names(WIN)) {
  d <- off[off$RaceCode == rc & off$PartyCode == "REP", ]
  tt <- tapply(d$CanVotes, d$county, sum)
  ww <- tapply(d$CanVotes[d$last == WIN[[rc]]], d$county[d$last == WIN[[rc]]], sum)
  sh <- 100 * ww[names(tt)] / tt
  sw <- 100 * sum(ww) / sum(tt)
  dev <- rbind(dev, data.frame(office = rc, winner = unname(WIN[[rc]]),
                               county = names(tt), county_votes = as.integer(tt),
                               share = as.numeric(sh), statewide = sw,
                               deviation = as.numeric(sh) - sw))
  cat(sprintf("%-9s %-9s statewide %5.2f%%   counties %5.1f - %5.1f   spread %5.1f\n",
              rc, WIN[[rc]], sw, min(sh), max(sh), max(sh) - min(sh)))
}
dd_write_csv(dev, "derived/deviations.csv")

# The chapter's opening claim is that the governor's spread is the outlier.
# If another winner ever spreads wider, the sentence is wrong and the build
# should say so rather than let the brief keep asserting it.
spread <- tapply(dev$share, dev$office, function(x) max(x) - min(x))
stopifnot(spread[["GOV"]] == max(spread))

# THE REGIONS ARE A JUDGMENT and are written down rather than inferred, because
# the claim under test names one of them. The Panhandle is the usual sixteen
# counties, west of and including the Tallahassee tier. The Big Bend and
# north-central tier is a DIFFERENT place -- no coast cities, no bases -- and
# keeping the two apart is most of what this chapter is for.
PANHANDLE <- c("Escambia","Santa Rosa","Okaloosa","Walton","Holmes","Washington",
               "Bay","Jackson","Calhoun","Gulf","Liberty","Franklin","Gadsden",
               "Leon","Wakulla","Jefferson")
BIGBEND   <- c("Madison","Taylor","Lafayette","Dixie","Hamilton","Suwannee",
               "Columbia","Baker","Union","Bradford","Gilchrist","Levy","Alachua")
stopifnot(all(c(PANHANDLE, BIGBEND) %in% dev$county))
region <- function(x) ifelse(x %in% PANHANDLE, "Panhandle",
                      ifelse(x %in% BIGBEND, "Big Bend / north central",
                             "rest of Florida"))
dev$region <- region(dev$county)
# ===========================================================================
# PART 2 -- THE 2018 BENCHMARK
# ===========================================================================
#
# The last contested Republican primary for this office: DeSantis against
# Putnam, 28 August 2018. DeSantis was white, was endorsed by Trump, and was a
# congressman from the other end of the state from Donalds. If north Florida's
# behaviour in 2026 is new, it should not appear here.

read_past <- function(path, last) {
  d <- read.csv(path, sep = "\t", fileEncoding = "latin1", check.names = FALSE)
  d <- d[d$RaceCode == "GOV" & d$PartyCode == "REP", ]
  d$county <- trimws(d$CountyName)
  tt <- tapply(d$CanVotes, d$county, sum)
  ww <- tapply(d$CanVotes[trimws(d$CanNameLast) == last],
               d$county[trimws(d$CanNameLast) == last], sum)
  list(share = 100 * ww[names(tt)] / tt, sw = 100 * sum(ww) / sum(tt),
       votes = tt)
}
b18 <- read_past("raw/fldos-past/20180828_ElecResultsFL.txt", "DeSantis")
b14 <- read_past("raw/fldos-past/20140826_ElecResultsFL.txt", "Scott")
cat(sprintf("\nDeSantis 2018 statewide %.2f%%   spread %.1f\n",
            b18$sw, max(b18$share) - min(b18$share)))
cat(sprintf("Scott    2014 statewide %.2f%%   spread %.1f\n",
            b14$sw, max(b14$share) - min(b14$share)))

d26 <- dev[dev$office == "GOV", ]
rownames(d26) <- d26$county
cty <- intersect(d26$county, names(b18$share))
stopifnot(length(cty) == 67)

bm <- data.frame(county = cty,
                 county_votes = d26[cty, "county_votes"],
                 donalds_2026 = d26[cty, "share"],
                 donalds_dev  = d26[cty, "deviation"],
                 desantis_2018 = as.numeric(b18$share[cty]),
                 desantis_dev  = as.numeric(b18$share[cty]) - b18$sw,
                 region = region(cty))
# The residual is the part of Donalds' geography that 2018 does not predict.
# Least squares on 67 points, stated as a line rather than a model: the slope
# is the answer to "how much of this map was already there".
fit <- lm(donalds_dev ~ desantis_dev, data = bm)
bm$predicted <- as.numeric(fitted(fit))
bm$residual  <- as.numeric(resid(fit))
bm <- bm[order(bm$residual), ]
dd_write_csv(bm, "derived/benchmark.csv")
R <- cor(bm$donalds_dev, bm$desantis_dev)
cat(sprintf("correlation of the two maps: %.2f   slope %.2f\n",
            R, coef(fit)[["desantis_dev"]]))

# ===========================================================================
# PART 3 -- REGIONS
# ===========================================================================
#
rg <- NULL
for (r in c("Panhandle", "Big Bend / north central", "rest of Florida")) {
  row <- list(region = r, counties = sum(bm$region == r),
              gop_votes = sum(bm$county_votes[bm$region == r]))
  for (rc in names(WIN)) {
    s <- dev[dev$office == rc & dev$region == r, ]
    row[[unname(WIN[[rc]])]] <- sum(s$share * s$county_votes) / sum(s$county_votes) -
                                s$statewide[1]
  }
  w <- bm$county_votes[bm$region == r]
  row[["desantis_2018"]] <- sum(bm$desantis_2018[bm$region == r] * w) / sum(w) - b18$sw
  rg <- rbind(rg, as.data.frame(row, check.names = FALSE))
}
rg$new_since_2018 <- rg$Donalds - rg$desantis_2018
dd_write_csv(rg, "derived/regions.csv")
cat("\nby region, each winner's deviation from their own statewide share:\n")
print(rg, row.names = FALSE, digits = 3)

# ===========================================================================
# PART 4 -- WHERE THE VOTES WENT
# ===========================================================================
#
# Bobby Williams took 4.07% of the state and 13.7% of the Big Bend. His best
# counties are the answer to the chapter's third test, and they are not in
# north Florida at all.
res$region <- region(res$county)
wl <- NULL
for (cand in names(head(SW_GOV, 5))) {
  s <- res[res$candidate == cand, ]
  row <- list(candidate = cand, statewide = unname(SW_GOV[[cand]]))
  for (r in c("Panhandle", "Big Bend / north central", "rest of Florida")) {
    t <- s[s$region == r, ]
    row[[r]] <- 100 * sum(t$votes) / sum(t$county_total)
  }
  wl <- rbind(wl, as.data.frame(row, check.names = FALSE))
}
dd_write_csv(wl, "derived/candidate_by_region.csv")
cat("\nshare by region:\n"); print(wl, row.names = FALSE, digits = 3)

bw <- res[res$candidate == "Williams", c("county", "region", "share", "votes", "county_total")]
bw <- bw[order(-bw$share), ]
dd_write_csv(bw, "derived/williams.csv")
cat(sprintf("\nWilliams: %.2f%% statewide, best county %s at %.1f%%\n",
            SW_GOV[["Williams"]], bw$county[1], bw$share[1]))
# The chapter says his footprint is agricultural rather than northern. That is
# only true if some of his best counties are NOT in north Florida.
stopifnot(any(bw$region[1:10] == "rest of Florida"))

# ===========================================================================
# PART 5 -- THE ELECTORATE, WHICH IS THE LAST WORD
# ===========================================================================
#
# Registered Republicans by race, by county, at the book closing for this
# election. Read from the state's own spreadsheet rather than inferred from
# census population, because the two are different objects and the difference
# is the whole point of this section.
if (!requireNamespace("readxl", quietly = TRUE))
  stop("readxl is needed to read the registration workbook. install.packages('readxl')")
rx <- readxl::read_excel("raw/registration/2026primary_party_by_county_by_race.xlsx",
                         sheet = "RegistrationByPartyRace", skip = 8)
names(rx) <- trimws(names(rx))
rx <- as.data.frame(rx)
rx <- rx[!is.na(rx[["Party Name"]]) & grepl("^Republican", rx[["Party Name"]]), ]
el <- data.frame(county = trimws(rx[["County Name"]]),
                 reg_republican = as.integer(rx[["Total"]]),
                 black  = as.integer(rx[["Black, Not Hispanic"]]),
                 white  = as.integer(rx[["White, Not Hispanic"]]),
                 hispanic = as.integer(rx[["Hispanic"]]))
# The workbook ends with a statewide Total row, and spells one county DeSoto
# where the results file spells it Desoto. Joining on the letters alone fixes
# the second and the assertion below catches anything like it in future -- a
# county silently dropped here would quietly shrink every number in Part 5.
el <- el[ckey(el$county) != "total", ]
el$k <- ckey(el$county)
el$county <- dev$county[match(el$k, ckey(dev$county))]
el <- el[!is.na(el$county), ]
el$k <- NULL
stopifnot(nrow(el) == 67)
el$black_pct <- 100 * el$black / el$reg_republican
el$white_pct <- 100 * el$white / el$reg_republican
el$hispanic_pct <- 100 * el$hispanic / el$reg_republican
el$region <- region(el$county)
el <- el[order(el$black_pct), ]
dd_write_csv(el, "derived/electorate.csv")

pan <- el[el$region == "Panhandle", ]
cat(sprintf("\nregistered Republicans in the Panhandle: %s, of whom %s are Black (%.1f%%)\n",
            format(sum(pan$reg_republican), big.mark = ","),
            format(sum(pan$black), big.mark = ","),
            100 * sum(pan$black) / sum(pan$reg_republican)))
cat(sprintf("Black share of registered Republicans, across all %d counties: %.1f%% to %.1f%%\n",
            nrow(el), min(el$black_pct), max(el$black_pct)))
cat(sprintf("Hispanic share, for contrast: %.1f%% to %.1f%%\n",
            min(el$hispanic_pct), max(el$hispanic_pct)))

# The chapter's closing argument is that the covariate has no range. If that
# ever stops being true the argument has to be rewritten, so it is asserted.
stopifnot(diff(range(el$black_pct)) < 10, diff(range(el$hispanic_pct)) > 50)

# ---- how white Republicans voted, without estimating anything ------------
#
# THE POINT OF THIS BLOCK, and it corrects an earlier version of this chapter.
# The claim is about WHITE voters, and where an electorate is nearly all white
# the aggregate result almost IS the white result. No model is needed: Duncan
# and Davis's method of bounds, which `levels-of-aggregation` teaches, pins the
# white support rate from the composition alone.
#
#   S = w*W + n*(1 - W),  with n the non-white rate, unknown but in [0, 1]
#   so  w lies in [ (S - (1 - W)) / W ,  S / W ]
#
# and the width of that interval is (1 - W) / W. The narrower the white
# majority's dominance, the wider the bound -- which is why the STATE bound is
# useless and the PANHANDLE bound is tight. The same homogeneity that makes an
# estimate for Black Republicans impossible makes the white one nearly exact.
#
# CAVEAT, carried into the brief: W here is the share of REGISTERED
# Republicans, not of those who turned out. Differential turnout by race moves
# the bound, and nothing published says by how much.
bnd <- NULL
for (r in c("Panhandle", "Big Bend / north central", "rest of Florida", "STATEWIDE")) {
  cs <- if (r == "STATEWIDE") el$county else el$county[el$region == r]
  W  <- sum(el$white[el$county %in% cs]) / sum(el$reg_republican[el$county %in% cs])
  d  <- dev[dev$office == "GOV" & dev$county %in% cs, ]
  S  <- sum(d$share * d$county_votes) / sum(d$county_votes) / 100
  bnd <- rbind(bnd, data.frame(
    region = r, donalds_share = 100 * S, white_pct_of_registered = 100 * W,
    white_rate_low = 100 * max(0, (S - (1 - W)) / W),
    white_rate_high = 100 * min(1, S / W)))
}
bnd$width <- bnd$white_rate_high - bnd$white_rate_low
dd_write_csv(bnd, "derived/bounds.csv")
cat("\nwhat white Republicans did, bounded from the composition alone:\n")
print(bnd, row.names = FALSE, digits = 3)
# The chapter says the Panhandle bound is the tight one and the statewide bound
# is not. If that ever reverses the argument is different and must be rewritten.
stopifnot(bnd$width[bnd$region == "Panhandle"] <
          bnd$width[bnd$region == "STATEWIDE"] / 2)

# ---- what the residual tracks, on the ELECTORATE rather than the population -
m <- merge(bm, el[, c("county", "black_pct", "white_pct", "hispanic_pct")], by = "county")
sur <- read.csv(file.path("..", "..", "..", "01-census-bureau", "surnames",
                          "data", "derived", "county_race.csv"),
                fileEncoding = "latin1", colClasses = c(fips = "character"))
sur <- sur[substr(sur$fips, 1, 2) == "12", ]
sur$county <- sub(" County$", "", sur$county)
m <- merge(m, data.frame(county = sur$county,
                         pop_black_pct = 100 * sur$black,
                         pop_white_pct = 100 * sur$white,
                         pop_hispanic_pct = 100 * sur$hispanic), by = "county")
cors <- data.frame(
  covariate = c("Black % of registered Republicans", "Hispanic % of registered Republicans",
                "white % of registered Republicans", "Black % of county population"),
  vs_deviation = c(cor(m$black_pct, m$donalds_dev), cor(m$hispanic_pct, m$donalds_dev),
                   cor(m$white_pct, m$donalds_dev), cor(m$pop_black_pct, m$donalds_dev)),
  vs_residual  = c(cor(m$black_pct, m$residual), cor(m$hispanic_pct, m$residual),
                   cor(m$white_pct, m$residual), cor(m$pop_black_pct, m$residual)),
  range_points = c(diff(range(m$black_pct)), diff(range(m$hispanic_pct)),
                   diff(range(m$white_pct)), diff(range(m$pop_black_pct))))
dd_write_csv(cors, "derived/correlates.csv")
cat("\nwhat the geography tracks:\n"); print(cors, row.names = FALSE, digits = 2)

# ===========================================================================
# PART 6 -- THE MAP, AND THE FACTS
# ===========================================================================
#
# County outlines already exist, projected into one shared frame, in `mapping`.
# Read across by the full path so the dependency is visible here.
RING <- file.path("..", "..", "mapping", "data", "derived", "county_rings.csv")
CTY  <- file.path("..", "..", "mapping", "data", "derived", "counties.csv")
stopifnot(file.exists(RING), file.exists(CTY))
allc <- read.csv(CTY, colClasses = c(fips = "character"))
flc  <- allc[substr(allc$fips, 1, 2) == "12", c("fips", "name", "cx", "cy")]
flc$k <- ckey(flc$name); bm$k <- ckey(bm$county)
# A silent partial join drops counties off a map that still looks like a map.
stopifnot(setequal(flc$k, bm$k), nrow(flc) == 67)
geo <- merge(flc, bm[, c("k", "county", "region", "donalds_2026", "donalds_dev",
                         "desantis_dev", "residual", "county_votes")], by = "k")
rings <- read.csv(RING, colClasses = c(id = "character"))
rings <- merge(rings[rings$id %in% flc$fips, ], geo[, c("fips", "county", "region",
              "donalds_2026", "donalds_dev", "residual")], by.x = "id", by.y = "fips")
rings <- rings[order(rings$id, rings$part), ]
stopifnot(length(unique(rings$id)) == 67)
dd_write_csv(rings, "derived/county_map.csv")
dd_write_csv(geo[order(-geo$county_votes),
                 c("fips", "county", "region", "donalds_2026", "donalds_dev",
                   "desantis_dev", "residual", "county_votes", "cx", "cy")],
             "derived/county_map_labels.csv")

pan_v <- bm$county_votes[bm$region == "Panhandle"]
worst <- bm[order(bm$donalds_2026), ][1:12, ]
facts <- data.frame(
  name = c("donalds_statewide", "donalds_min", "donalds_max", "donalds_spread",
           "donalds_min_county", "donalds_max_county", "gov_total_votes",
           "candidates", "spread_moody", "spread_ingoglia", "spread_simpson",
           "desantis_2018_statewide", "desantis_2018_spread", "benchmark_r",
           "benchmark_slope", "panhandle_counties", "panhandle_votes",
           "panhandle_vote_share", "panhandle_donalds_dev", "panhandle_desantis_dev",
           "panhandle_new", "bigbend_donalds_dev", "bigbend_new",
           "worst12_vote_share", "williams_statewide", "williams_bigbend",
           "williams_best", "reg_republicans_panhandle", "black_reg_panhandle",
           "black_pct_panhandle", "black_pct_min", "black_pct_max",
           "hispanic_pct_min", "hispanic_pct_max",
           "precinct_counties_north", "north_counties",
           "hardee_hispanic_pop", "hardee_donalds",
           "panhandle_white_pct", "panhandle_white_low", "panhandle_white_high",
           "panhandle_bound_width", "statewide_bound_width",
           "holmes_black_reg", "holmes_white_pct"),
  value = c(SW_GOV[["Donalds"]], min(bm$donalds_2026), max(bm$donalds_2026),
            max(bm$donalds_2026) - min(bm$donalds_2026),
            NA, NA, sum(statewide), length(statewide),
            spread[["USS"]], spread[["CFO"]], spread[["AGR"]],
            b18$sw, max(b18$share) - min(b18$share), R, coef(fit)[["desantis_dev"]],
            length(PANHANDLE), sum(pan_v), 100 * sum(pan_v) / sum(bm$county_votes),
            rg$Donalds[rg$region == "Panhandle"],
            rg$desantis_2018[rg$region == "Panhandle"],
            rg$new_since_2018[rg$region == "Panhandle"],
            rg$Donalds[rg$region == "Big Bend / north central"],
            rg$new_since_2018[rg$region == "Big Bend / north central"],
            100 * sum(worst$county_votes) / sum(bm$county_votes),
            SW_GOV[["Williams"]], wl[wl$candidate == "Williams", "Big Bend / north central"],
            bw$share[1], sum(pan$reg_republican), sum(pan$black),
            100 * sum(pan$black) / sum(pan$reg_republican),
            min(el$black_pct), max(el$black_pct),
            min(el$hispanic_pct), max(el$hispanic_pct),
            17, length(PANHANDLE) + length(BIGBEND),
            m$pop_hispanic_pct[m$county == "Hardee"],
            bm$donalds_2026[bm$county == "Hardee"],
            bnd$white_pct_of_registered[bnd$region == "Panhandle"],
            bnd$white_rate_low[bnd$region == "Panhandle"],
            bnd$white_rate_high[bnd$region == "Panhandle"],
            bnd$width[bnd$region == "Panhandle"],
            bnd$width[bnd$region == "STATEWIDE"],
            el$black[el$county == "Holmes"],
            el$white_pct[el$county == "Holmes"]),
  text = "")
facts$text[facts$name == "donalds_min_county"] <- bm$county[which.min(bm$donalds_2026)]
facts$text[facts$name == "donalds_max_county"] <- bm$county[which.max(bm$donalds_2026)]
dd_write_csv(facts, "derived/facts.csv")

# The precinct count in facts.csv is the one number here that is typed rather
# than computed, so it is checked against the manifest that records the harvest.
man <- read.csv("raw/enr_county_manifest.tsv", sep = "\t")
north <- man[man$county_name %in% c(PANHANDLE, BIGBEND), ]
stopifnot(sum(north$status == "precinct_csv") == 17, nrow(north) == 29)
cat(sprintf("\nnorth Florida counties publishing precinct returns: %d of %d\n",
            sum(north$status == "precinct_csv"), nrow(north)))

cat("\ndone.\n")

# ---------------------------------------------------------------------------
# "all" rather than the usual bare call: nothing here fetches, so raw/ is a
# committed archive the build only ever reads, and under the default mtime rule
# it would never be stamped at all.
if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_stamp("all")
}
