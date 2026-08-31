# ---------------------------------------------------------------------------
# Build the sweet-spot lab dataset.
#
# The chapter asks what Lublin, Handley, Brunell and Grofman (2020) asked --
# whether minority candidates now win in districts that are only 40-50%
# minority -- and it needs three things to ask it: how Black or Latino each
# district is, how Republican it is, and whether the person it elected is
# Black or Latino. Two of those three come out of federal files. The third
# does not exist in any federal file at all, and the largest part of this
# script is the business of getting it anyway.
#
# FETCHED 2026-08-12. Sources, with what each returned on that date:
#
#   1. Citizen Voting Age Population Special Tabulation, 2019-2023 ACS
#      https://www2.census.gov/programs-surveys/decennial/rdo/datasets/
#        2023/2023-cvap/CVAP_2019-2023_ACS_csv_files.zip
#      HTTP 200 - 54,962,776 bytes - CD.csv is 5,720 rows
#      Produced by the Census Bureau's REDISTRICTING DATA OFFICE, not by the
#      ACS program, and it exists because the Voting Rights Act needs it.
#      Thirteen race lines x four population bases (total, adult, citizen,
#      citizen voting age), each with a margin of error, for every 118th
#      Congress district. Nothing else publishes citizen voting-age population
#      by race for districts, and Lublin's Table 4B turns on exactly that
#      column.
#
#   2. unitedstates/congress-legislators
#      https://unitedstates.github.io/congress-legislators/legislators-current.json
#      https://unitedstates.github.io/congress-legislators/legislators-historical.json
#      HTTP 200 - 1,466,894 and 13,482,846 bytes
#      Who served, in which district, for which party, between which dates.
#      Carries a member's Twitter handle. Does not carry their race.
#
#   3. Office of the Historian, U.S. House -- see fetch-historian.py, which
#      does the scraping and the explaining. 201 Black and 170 Hispanic
#      members of Congress, all time, keyed by Bioguide ID.
#
#   4. Presidential results by congressional district, 2020, on the 2022
#      lines. NOT fetched here: read from ../../../03-elections/house-competition/data/,
#      which downloads it from The Downballot. See the note on borrowing
#      below.
#
#   5. Frequently Occurring Surnames from the 2010 Census. NOT fetched here:
#      read from ../../../06-putting-data-together/surnames/data/, which downloads it and decides how the
#      Bureau's suppressed cells are handled. Used only by section 3b.
#
#   6. Lublin et al. (2020), Tables 1, 3 and 4, keyed in by hand from the
#      published PDF. A printed table is data, and this chapter's second act
#      is about a row of it that most readers skip.
#
# ON BORROWING SOURCE 4 RATHER THAN FETCHING IT
#
# The house-competition chapter already downloads The Downballot's
# presidential-by-district sheets and does the hard part, which is knowing
# which sheet goes with which set of lines: a presidential result "by
# congressional district" means nothing except relative to a map, and the 118th
# Congress ran on the 2022 maps, so the row needed here is the 2020
# presidential vote recomputed on 2022 lines. Re-fetching it would mean
# re-making that decision, worse, in a second place. The cost is that this
# folder cannot be rebuilt on its own; the compensation is that the decision is
# made once, in a chapter students have already read.
#
# WHAT "BLACK" MEANS IN FILE 1, WHICH IS NOT WHAT IT MEANS IN FILE 3
#
# The CVAP tabulation's race lines 3-12 are all NON-HISPANIC, and it breaks
# multiracial responses into named pairs -- Black and White, American Indian
# and Black -- plus one bucket called "Remainder of Two or More Race
# Responses". Voting Rights Act practice counts ANY-PART BLACK: alone or in
# combination, which is what the vote-dilution chapter uses and what a Section
# 2 expert would compute. That number cannot be recovered from this file,
# because the remainder bucket contains an unknown number of Black people and
# is not broken out. So this build carries BOUNDS rather than a point estimate:
#
#   black_low  = Black alone                                  (line 5)
#   black_high = Black alone + Black&White + AmInd&Black
#                + the entire remainder bucket                (5+10+11+12)
#
# The truth is inside that interval and this file does not know where. The
# federal tabulation built for the Voting Rights Act cannot express the Voting
# Rights Act's own definition of the group it protects.
#
# OUTPUTS (all written next to this script)
#
#   derived/districts.csv   one row per 118th Congress district: composition on three
#                   population bases with margins of error, Republican share,
#                   and who it elected
#   derived/members.csv     one row per person who served in the 118th House
#   derived/bins.csv        the 118th, cut into Lublin's Table 3A bins, with the
#                   denominators printed next to the percentages
#   derived/lublin.csv      Tables 3A/3B/4A/4B as published, percentages and Ns
#   derived/lublin_t1.csv   Table 1 as published
#   derived/simulation.csv  the Figure 1 model, swept over BD, Republican share, and
#                   the standard deviation the authors call arbitrary
#   derived/sweetspot.csv   where the peak sits, and how high, as sigma varies
#   derived/fitted.csv      the logit of Figure 2, refit on the 118th
#   derived/surface.csv     predicted success over (minority share x Republican share)
#   derived/surname_check.csv  one row per seated member: the surname file's racial
#                   distribution for their name, beside the Historian's answer
#   derived/surname_grade.csv  the surname file graded against the Historian, by cutoff
#   derived/surname_hyphen.csv every two-part surname resolved both ways, and whether
#                   the 50% call changes depending on which half is taken
#   derived/bisg_members.csv   one row per member: the Historian's answer beside the
#                   label from geography alone, surname alone, and BISG
#   derived/bisg_calibration.csv BISG's error rate by district Black share -- the table
#                   that decides whether the labels may be used downstream
#   derived/bisg_downstream.csv the chapter's logit run on each set of labels
#   derived/checks.csv      the validation results printed at the end
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)

if (file.exists("../../../_lib/provenance.R")) {
  source("../../../_lib/provenance.R")
} else {
  prov_fetch  <- function(url, dest, ...) {
    if (!file.exists(dest)) download.file(url, dest, mode = "wb", quiet = TRUE)
    dest
  }
  prov_report <- function() invisible(FALSE)
}

FETCH_DATE <- "2026-08-12"
CONGRESS   <- 118L
TERM_START <- as.Date("2023-01-03")
TERM_END   <- as.Date("2025-01-03")
dir.create("raw", showWarnings = FALSE)

CHECKS <- list()
note <- function(what, value, expected = NA, ok = NA) {
  CHECKS[[length(CHECKS) + 1L]] <<-
    data.frame(check = what, value = as.character(value),
               expected = as.character(expected), ok = ok)
  invisible(NULL)
}
must <- function(cond, msg) if (!isTRUE(cond)) stop(msg, call. = FALSE)

# --- 1. CVAP special tabulation --------------------------------------------

U_CVAP <- paste0("https://www2.census.gov/programs-surveys/decennial/rdo/",
                 "datasets/2023/2023-cvap/CVAP_2019-2023_ACS_csv_files.zip")
invisible(prov_fetch(U_CVAP, "raw/cvap.zip", label = "CVAP 2019-2023"))
if (!file.exists("raw/CD.csv")) unzip("raw/cvap.zip", files = "CD.csv", exdir = "raw")

cv <- read.csv("raw/CD.csv", colClasses = "character")
for (v in c("lnnumber", "tot_est", "tot_moe", "adu_est", "adu_moe",
            "cit_est", "cit_moe", "cvap_est", "cvap_moe")) {
  cv[[v]] <- as.numeric(cv[[v]])
}
must(all(range(cv$lnnumber) == c(1, 13)), "CVAP race lines are not 1-13.")

# Districts only: drop delegates, the resident commissioner, and the
# "not defined" residual the Bureau carries for unassigned area. Done before
# the district number is parsed, because those rows do not carry one -- the
# residual is coded ZZ and would become a silent NA.
cv <- cv[!grepl("Delegate|Resident Commissioner|not defined", cv$geoname), ]

# GEOID is "5001800US" + 2-digit state FIPS + 2-digit district; at-large is 00.
cv$stfips <- substr(cv$geoid, 10, 11)
cv$cd     <- as.integer(substr(cv$geoid, 12, 13))
must(!any(is.na(cv$cd)), "A CVAP GEOID did not parse to a district number.")
cv$state  <- trimws(sub("^.*,\\s*", "", cv$geoname))

# One column per race line, per base.
wide <- function(base) {
  m <- tapply(cv[[base]], list(cv$geoid, cv$lnnumber), sum)
  m[is.na(m)] <- 0
  m
}
EST <- lapply(c(pop = "tot_est", vap = "adu_est", cvap = "cvap_est"), wide)
MOE <- lapply(c(pop = "tot_moe", vap = "adu_moe", cvap = "cvap_moe"), wide)

geo <- cv[!duplicated(cv$geoid), c("geoid", "geoname", "stfips", "cd", "state")]
geo <- geo[match(rownames(EST$pop), geo$geoid), ]

d <- data.frame(geoid = geo$geoid, state = geo$state, stfips = geo$stfips,
                cd = geo$cd, geoname = geo$geoname)
for (b in names(EST)) {
  E <- EST[[b]]; M <- MOE[[b]]
  d[[paste0(b, "_total")]]      <- E[, "1"]
  d[[paste0(b, "_black_low")]]  <- E[, "5"]
  d[[paste0(b, "_black_high")]] <- E[, "5"] + E[, "10"] + E[, "11"] + E[, "12"]
  d[[paste0(b, "_hisp")]]       <- E[, "13"]
  # Margins on a sum combine in quadrature, which is the Bureau's own rule.
  d[[paste0(b, "_total_moe")]]  <- M[, "1"]
  d[[paste0(b, "_black_low_moe")]] <- M[, "5"]
  d[[paste0(b, "_hisp_moe")]]   <- M[, "13"]
}

# THE SIX CATEGORIES THE SURNAME FILE USES, BUILT FROM THE LINES RATHER THAN
# BY SUBTRACTION. Section 5b needs a district's population split the same way
# the 2010 surname file splits a name, or the two cannot be multiplied. The
# CVAP lines map onto those six exactly, with nothing left over:
#
#   surname file                            CVAP lines
#   ------------------------------------    -------------------------
#   pctwhite    nH White alone              7
#   pctblack    nH Black alone              5
#   pctapi      nH Asian / NHPI alone       4 + 6
#   pctaian     nH AIAN alone               3
#   pct2prace   nH two or more races        8 + 9 + 10 + 11 + 12
#   pcthispanic Hispanic, any race          13
#
# Computing the last one as "everybody who is not White or Black" instead
# would silently sweep Asian, AIAN and multiracial residents into it, which
# both inflates the Hispanic prior and destroys the categories BISG needs.
# The two identities below are what make this mapping checkable rather than
# plausible: lines 3-12 must reconstruct line 2, and line 2 plus line 13 must
# reconstruct line 1.
E <- EST$pop
d$r_white  <- E[, "7"]
d$r_black  <- E[, "5"]
d$r_api    <- E[, "4"] + E[, "6"]
d$r_aian   <- E[, "3"]
d$r_2prace <- E[, "8"] + E[, "9"] + E[, "10"] + E[, "11"] + E[, "12"]
d$r_hisp   <- E[, "13"]

# THE IDENTITIES HOLD TO ROUNDING, NOT EXACTLY, AND THAT IS THE FILE'S DOING.
# Every estimate in this tabulation is rounded to the nearest 5 -- 99.98% of
# the cells are multiples of 5 -- so a line that is itself rounded cannot equal
# the sum of ten other rounded lines. Ten cells at +/-2.5 each bound the
# discrepancy at 25 people; the observed worst case is smaller. Asserting exact
# equality here failed, and the failure was the file being honest about its own
# precision rather than the mapping being wrong.
ROUND_TOL <- 10 * 2.5
NH   <- rowSums(E[, as.character(3:12), drop = FALSE])
dev1 <- max(abs(NH - E[, "2"]))
dev2 <- max(abs(E[, "2"] + E[, "13"] - E[, "1"]))
note("Lines 3-12 reconstruct line 2 (max deviation, people)", dev1,
     paste("<=", ROUND_TOL), dev1 <= ROUND_TOL)
note("Line 2 + line 13 reconstruct line 1 (max deviation, people)", dev2,
     paste("<=", ROUND_TOL), dev2 <= ROUND_TOL)
note("Estimates that are multiples of 5",
     paste0(round(100 * mean(cv$tot_est %% 5 == 0, na.rm = TRUE), 2), "%"))
must(dev1 <= ROUND_TOL && dev2 <= ROUND_TOL,
     "CVAP race lines miss their own totals by more than rounding explains.")

RCOL <- c("r_white", "r_black", "r_api", "r_aian", "r_2prace", "r_hisp")
dev3 <- max(abs(rowSums(d[, RCOL]) - d$pop_total))
note("The six categories reconstruct the district total (max deviation, people)",
     dev3, paste("<=", ROUND_TOL), dev3 <= ROUND_TOL)
note("Largest such gap as a share of a district",
     paste0(signif(100 * max(abs(rowSums(d[, RCOL]) - d$pop_total) / d$pop_total), 2), "%"))
must(dev3 <= ROUND_TOL, "The six-category split misses the district total.")
# Because the six do not sum exactly to the total, section 5b normalises the
# row rather than trusting pop_total as a denominator.

d <- d[order(d$stfips, d$cd), ]
note("Congressional districts in the CVAP file", nrow(d), 435, nrow(d) == 435L)
must(nrow(d) == 435L, "Did not end up with 435 congressional districts.")

# --- 2. Who served in the 118th House --------------------------------------

if (!requireNamespace("jsonlite", quietly = TRUE))
  stop("jsonlite is required to read the legislator files.", call. = FALSE)

for (f in c("legislators-current.json", "legislators-historical.json")) {
  invisible(prov_fetch(paste0("https://unitedstates.github.io/congress-legislators/", f),
                       file.path("raw", f), label = f))
}
leg <- c(jsonlite::fromJSON("raw/legislators-current.json", simplifyDataFrame = FALSE),
         jsonlite::fromJSON("raw/legislators-historical.json", simplifyDataFrame = FALSE))

rows <- list()
for (p in leg) {
  for (t in p$terms) {
    if (!identical(t$type, "rep")) next
    st <- as.Date(t$start); en <- as.Date(t$end)
    if (!(st < TERM_END && en > TERM_START)) next
    rows[[length(rows) + 1L]] <- data.frame(
      bioguide = p$id$bioguide,
      name  = if (!is.null(p$name$official_full)) p$name$official_full
              else paste(p$name$first, p$name$last),
      last  = p$name$last,
      state = t$state,
      cd    = as.integer(t$district),
      party = if (is.null(t$party)) NA_character_ else t$party,
      start = st, end = en)
  }
}
mem <- do.call(rbind, rows)
# congress-legislators types the six non-voting delegates as "rep" as well;
# they hold no district and are dropped here, which is also what the CVAP
# file's own "Delegate District" rows required above.
DELEGATES <- c("DC", "PR", "VI", "GU", "AS", "MP")
note("Delegates dropped (typed as representatives upstream)",
     length(unique(mem$state[mem$state %in% DELEGATES])), 6,
     length(unique(mem$state[mem$state %in% DELEGATES])) == 6L)
mem <- mem[!mem$state %in% DELEGATES, ]
note("People who served in the 118th House", nrow(mem))

# The district's member is the FIRST person to hold the seat in this Congress
# -- normally the winner of the 2022 general election. Not "whoever was seated
# on 3 January 2023": VA-04 had nobody on that date, because Donald McEachin
# died three weeks after winning the seat, and a rule keyed to opening day
# silently drops the district. Ten seats changed hands during the Congress and
# the chapter reports that rather than averaging it away.
mem <- mem[order(mem$state, mem$cd, mem$start), ]
seated <- mem[!duplicated(paste(mem$state, mem$cd)), ]
note("Seats that changed occupant during the 118th",
     nrow(mem) - nrow(seated))

# --- 3. Race and ethnicity, from the House Historian -----------------------

if (!file.exists("raw/historian.csv")) {
  st <- system2("python3", "fetch-historian.py")
  must(st == 0L, "fetch-historian.py failed.")
}
hist <- read.csv("raw/historian.csv")
must(all(c("black", "hispanic") %in% hist$list), "historian.csv is missing a list.")
BLK <- unique(hist$bioguide[hist$list == "black"])
HSP <- unique(hist$bioguide[hist$list == "hispanic"])
note("Black members of Congress, all time (Historian)", length(BLK), 201, length(BLK) == 201L)
note("Hispanic members of Congress, all time (Historian)", length(HSP), 170, length(HSP) == 170L)

mem$black <- mem$bioguide %in% BLK
mem$hisp  <- mem$bioguide %in% HSP
mem$both  <- mem$black & mem$hisp
seated$black <- seated$bioguide %in% BLK
seated$hisp  <- seated$bioguide %in% HSP
seated$both  <- seated$black & seated$hisp
note("Black members seated in the 118th House", sum(seated$black))
note("Hispanic members seated in the 118th House", sum(seated$hisp))
note("Members on BOTH Historian lists", sum(seated$both))

# --- 3b. The same attribute, asked of a surname file ------------------------
#
# The Historian's list is the only source for this attribute, and a source with
# no inclusion rule should be graded against something. The Census surname file
# publishes a racial distribution for every name held by 100 or more people, so
# it can be asked the same question about the same 435 people. This is one
# unaccountable source checked against another, not a check against truth.
#
# The file is BORROWED from the surnames chapter rather than fetched again, so
# that the decisions about suppression are made once, there.
#
# WHAT IS DELIBERATELY NOT DONE HERE. This is surname-only. BISG -- the method
# graded in bisg-check -- would sharpen it by adding a geographic prior, and in
# this chapter that prior would be the racial composition of the member's
# district. But district composition is this chapter's INDEPENDENT VARIABLE.
# Inferring a member's race partly from their district's race, and then asking
# whether district race predicts member race, is circular: the answer would be
# built into the measurement. The weaker method is the only admissible one.

S_SRC <- "../../../06-putting-data-together/surnames/data/derived/census_surnames.csv"
must(file.exists(S_SRC),
     paste("Missing", S_SRC, "- run the surnames build first."))
sn <- read.csv(S_SRC, stringsAsFactors = FALSE)

nrm  <- function(x) trimws(gsub("[^A-Z -]", "",
          toupper(iconv(x, "UTF-8", "ASCII//TRANSLIT"))))
SIX  <- c("pctwhite", "pctblack", "pctapi", "pctaian", "pct2prace",
          "pcthispanic")
# All six categories are carried through, not just the two this act reads,
# because section 5b multiplies the whole distribution against the district.
grab <- function(k) sn[match(k, sn$name), c("count", SIX)]
part <- function(x, first) vapply(strsplit(x, "[ -]"),
          function(z) if (first) z[1] else z[length(z)], character(1))

sk  <- nrm(seated$last)
hit <- grab(sk)
NW  <- sum(!is.na(hit$count))
# Hyphenated and two-word surnames are absent from the file as written. Fall
# back to one component, then the other. CHAVEZ-DEREMER is CHAVEZ to the file
# and DEREMER is not in it at all, so which half is taken changes the answer --
# a decision the chapter names rather than buries.
for (g in list(part(sk, FALSE), part(sk, TRUE))) {
  i <- is.na(hit$count)
  if (!any(i)) break
  hit[i, ] <- grab(g[i])
}
sc <- cbind(seated[, c("bioguide", "name", "last", "state", "cd",
                       "black", "hisp")], hit)
sc$matched <- !is.na(sc$count)

note("Seated members matched on the whole surname", NW)
note("Matched only after splitting a hyphen or space", sum(sc$matched) - NW)
note("Seated members matched to the surname file", sum(sc$matched))
note("Seated members with no surname-file entry", sum(!sc$matched))
# Suppression is information: the Census withholds the distribution where
# publishing it would identify people, i.e. for the rarest names.
note("Matched members whose Black share is suppressed",
     sum(sc$matched & is.na(sc$pctblack)))
note("Matched members whose Hispanic share is suppressed",
     sum(sc$matched & is.na(sc$pcthispanic)))

# Grade the surname file against the Historian at a range of cutoffs.
grade <- function(p, truth, t) {
  pred <- !is.na(p) & p >= t
  tp <- sum(pred & truth); fp <- sum(pred & !truth); fn <- sum(!pred & truth)
  data.frame(threshold = t, tp = tp, fp = fp, fn = fn,
             precision = if (tp + fp > 0) tp / (tp + fp) else NA_real_,
             recall    = tp / (tp + fn))
}
TH <- c(10, 25, 50, 75, 90)
sg <- rbind(
  cbind(group = "Hispanic",
        do.call(rbind, lapply(TH, function(t) grade(sc$pcthispanic, sc$hisp,  t)))),
  cbind(group = "Black",
        do.call(rbind, lapply(TH, function(t) grade(sc$pctblack,    sc$black, t)))))

h50 <- sg[sg$group == "Hispanic" & sg$threshold == 50, ]
b50 <- sg[sg$group == "Black"    & sg$threshold == 50, ]
note("Hispanic at the 50% cutoff — false positives", h50$fp, 0, h50$fp == 0L)
note("Hispanic at the 50% cutoff — members recovered", h50$tp)
note("Hispanic at the 50% cutoff — members missed", h50$fn)
note("Black at the 50% cutoff — members recovered", b50$tp)
note("Black at the 50% cutoff — members missed", b50$fn)
note("Median Black share of a Black member's surname",
     round(median(sc$pctblack[sc$black & !is.na(sc$pctblack)]), 1))
note("Median Black share of everyone else's surname",
     round(median(sc$pctblack[!sc$black & !is.na(sc$pctblack)]), 1))
note("Median Hispanic share of a Hispanic member's surname",
     round(median(sc$pcthispanic[sc$hisp & !is.na(sc$pcthispanic)]), 1))

dd_write_csv(sc, "derived/surname_check.csv")
dd_write_csv(sg, "derived/surname_grade.csv")

# The hyphen decision, measured rather than asserted. For every surname with a
# space or a hyphen, resolve it BOTH ways and see whether the 50% call changes.
hy <- seated[grepl("[ -]", nrm(seated$last)), ]
hk <- nrm(hy$last)
hf <- grab(part(hk, TRUE)); hl <- grab(part(hk, FALSE))
hyp <- data.frame(
  last = hy$last, black = hy$black, hisp = hy$hisp,
  first_tok = part(hk, TRUE),  first_hisp = hf$pcthispanic,
  last_tok  = part(hk, FALSE), last_hisp  = hl$pcthispanic)
hyp$call_first <- !is.na(hyp$first_hisp) & hyp$first_hisp >= 50
hyp$call_last  <- !is.na(hyp$last_hisp)  & hyp$last_hisp  >= 50
hyp$differs    <- hyp$call_first != hyp$call_last
dd_write_csv(hyp, "derived/surname_hyphen.csv")

note("Surnames containing a space or hyphen", nrow(hyp))
note("Of those, calls that change with the rule", sum(hyp$differs))
note("First-component calls agreeing with the Historian",
     sum(hyp$call_first == hyp$hisp))
note("Last-component calls agreeing with the Historian",
     sum(hyp$call_last == hyp$hisp))

# --- 4. Presidential vote by district, borrowed ----------------------------

P_SRC <- "../../../03-elections/house-competition/data/derived/pres_by_cd.csv"
must(file.exists(P_SRC),
     paste("Missing", P_SRC, "- run the house-competition build first."))
pres <- read.csv(P_SRC)
pres <- pres[pres$lines == "2022" & pres$pres_year == 2020, ]
note("Districts with a 2020 presidential result on 2022 lines", nrow(pres), 435,
     nrow(pres) == 435L)

# THE THIRD SPELLING OF AN AT-LARGE DISTRICT. Alaska's single seat is district
# 00 in the Census GEOID, district 0 in congress-legislators, and district 1 in
# The Downballot's sheet, which writes it "AK-AL". Three files, three answers,
# and a merge on the raw number joins Alaska's presidential result to nothing
# while quietly leaving six districts unmatched. The `district` string is the
# only column that says which of the three convention is in force.
pres$cd[grepl("-AL$", pres$district)] <- 0L
note("At-large seats renumbered to 0 to match the Census",
     sum(grepl("-AL$", pres$district)), 6,
     sum(grepl("-AL$", pres$district)) == 6L)

# --- 5. Join ----------------------------------------------------------------

fips <- read.csv(text = paste(
  "stfips,state_abb",
  "01,AL","02,AK","04,AZ","05,AR","06,CA","08,CO","09,CT","10,DE","12,FL",
  "13,GA","15,HI","16,ID","17,IL","18,IN","19,IA","20,KS","21,KY","22,LA",
  "23,ME","24,MD","25,MA","26,MI","27,MN","28,MS","29,MO","30,MT","31,NE",
  "32,NV","33,NH","34,NJ","35,NM","36,NY","37,NC","38,ND","39,OH","40,OK",
  "41,OR","42,PA","44,RI","45,SC","46,SD","47,TN","48,TX","49,UT","50,VT",
  "51,VA","53,WA","54,WV","55,WI","56,WY", sep = "\n"),
  colClasses = "character")
d <- merge(d, fips, by = "stfips", all.x = TRUE)
must(!any(is.na(d$state_abb)), "A state FIPS code did not map to an abbreviation.")

key <- function(a, b) paste0(a, "-", sprintf("%02d", b))
d$key      <- key(d$state_abb, d$cd)
seated$key <- key(seated$state, seated$cd)
pres$key   <- key(pres$state_abb, pres$cd)

d <- merge(d, seated[, c("key", "bioguide", "name", "party", "black", "hisp", "both")],
           by = "key", all.x = TRUE)
d <- merge(d, pres[, c("key", "dem_pct", "rep_pct", "dpres")], by = "key", all.x = TRUE)
must(!any(is.na(d$bioguide)), "A district has no seated member.")
must(!any(is.na(d$rep_pct)),  "A district has no presidential result.")

for (b in c("pop", "vap", "cvap")) {
  for (g in c("black_low", "black_high", "hisp")) {
    d[[paste0(b, "_", g, "_pct")]] <- 100 * d[[paste0(b, "_", g)]] / d[[paste0(b, "_total")]]
  }
  # Margin of error on a share, by the Bureau's ratio formula, for the two
  # quantities the chapter quotes intervals on.
  for (g in c("black_low", "hisp")) {
    p <- d[[paste0(b, "_", g)]] / d[[paste0(b, "_total")]]
    mn <- d[[paste0(b, "_", g, "_moe")]]; md <- d[[paste0(b, "_total_moe")]]
    inside <- mn^2 - (p^2) * (md^2)
    # The Bureau's fallback for a negative radicand is the sum rather than the
    # difference. pmax() before the sqrt because ifelse() evaluates BOTH arms
    # on the whole vector: without it the discarded arm still takes sqrt() of
    # the negatives and warns "NaNs produced" on a result that is correct,
    # which is a warning that trains you to ignore warnings.
    d[[paste0(b, "_", g, "_pct_moe")]] <-
      100 * ifelse(inside > 0, sqrt(pmax(inside, 0)), sqrt(mn^2 + (p^2) * md^2)) /
      d[[paste0(b, "_total")]]
  }
}
d$minority_pct <- d$vap_black_low_pct + d$vap_hisp_pct
d <- d[order(d$state_abb, d$cd), ]

note("Districts electing a Black member", sum(d$black))
note("Districts electing a Hispanic member", sum(d$hisp))
note("Majority-Black-CVAP districts", sum(d$cvap_black_low_pct > 50))
note("Districts 40-50% Black by voting-age population",
     sum(d$vap_black_low_pct >= 40 & d$vap_black_low_pct < 50))

dd_write_csv(d, "derived/districts.csv")
dd_write_csv(mem[order(mem$state, mem$cd, mem$start), ], "derived/members.csv")

# --- 5b. BISG, and the error structure that decides whether it may be used --
#
# Section 3b asks a surname file alone. This asks the surname file AND the
# district, which is what BISG does: posterior proportional to
# P(race | surname) x P(district | race). The bisg-check chapter builds the
# same arithmetic against a known answer in Georgia.
#
# THE OBJECTION THAT DOES NOT HOLD. It is tempting to call this circular --
# the district's racial composition is the chapter's independent variable, so
# using it to label the member looks like assuming the conclusion. It is not.
# Recovering a member's race is a MEASUREMENT; asking which districts elect
# minority members is an ANALYSIS. A measurement is not disqualified by what
# information it uses, only by being wrong. The test below is whether BISG
# beats a geography-only baseline WITHIN strata of district composition: if it
# were only reading the district's modal race back out, it could not.
#
# THE OBJECTION THAT DOES HOLD. BISG's error is not constant across districts.
# It recovers Black members far more reliably in heavily Black districts than
# in white ones, and that is DIFFERENTIAL MEASUREMENT ERROR correlated with
# the regressor. It biases the downstream coefficient, and it is worst exactly
# where Lublin et al.'s claim lives -- the districts below 50%. So the error
# rates are computed here, by stratum, and reported rather than assumed away.

# SIX CATEGORIES ON BOTH SIDES. The surname file splits a name six ways and
# section 1 now splits a district the same six ways off the CVAP lines, so the
# two multiply directly. The earlier draft of this build collapsed both to
# (Black, Hispanic, other) and derived "other" by subtraction, which forced
# Asian, AIAN and multiracial residents into a residual bucket and then let a
# name that is 40% Asian read as evidence of being Hispanic.
RR <- c("white", "black", "api", "aian", "2prace", "hisp")
SCOL <- c("pctwhite", "pctblack", "pctapi", "pctaian", "pct2prace", "pcthispanic")
DCOL <- c("r_white", "r_black", "r_api", "r_aian", "r_2prace", "r_hisp")

# Suppressed cells become 0 -- the Bureau withholds a cell when the count
# behind it is tiny, so near-zero is the right reading, and unlike the old
# subtraction there is no derived column to be poisoned by the NA.
SM <- as.matrix(sc[, SCOL])
SM[is.na(SM)] <- 0
colnames(SM) <- RR

bi <- merge(sc[, c("bioguide", "black", "hisp", "matched")],
            d[, c("bioguide", "pop_total", "pop_black_low_pct", DCOL)],
            by = "bioguide")
SM <- SM[match(bi$bioguide, sc$bioguide), , drop = FALSE]
keep <- bi$matched & rowSums(SM) > 0
bi <- bi[keep, ]; SM <- SM[keep, , drop = FALSE]
SM <- SM / rowSums(SM)

PP <- as.matrix(bi[, DCOL]); colnames(PP) <- RR
MM <- sweep(PP, 2, colSums(PP), "/")
po <- SM * MM; po <- po / rowSums(po)

# The Historian records two attributes, not a six-way partition: a member is on
# the Black list, the Hispanic list, or neither. Only those two labels can be
# graded, so a prediction of white/api/aian/2prace all count as "neither" --
# scoring the model on the question the truth can actually answer.
lab <- function(mx) {
  z <- RR[max.col(mx, ties.method = "first")]
  ifelse(z %in% c("black", "hisp"), z, "other")
}
bi$truth   <- ifelse(bi$black, "black", ifelse(bi$hisp, "hisp", "other"))
bi$bisg    <- lab(po)
bi$surname <- lab(SM)
bi$geo     <- lab(prop.table(PP, 1))
dd_write_csv(bi, "derived/bisg_members.csv")

note("Members BISG could be run on", nrow(bi))
note("Accuracy — district plurality only", round(mean(bi$geo == bi$truth), 3))
note("Accuracy — surname only", round(mean(bi$surname == bi$truth), 3))
note("Accuracy — BISG", round(mean(bi$bisg == bi$truth), 3))

# The calibration table. Strata are on the SAME axis as the regressor, which
# is the point: if the recovery rate moves down this table, the labels carry a
# bias that moves with the independent variable.
BRK <- c(-1, 10, 20, 30, 50, 101)
BLB <- c("under 10%", "10-20%", "20-30%", "30-50%", "50% and over")
bi$stratum <- cut(bi$pop_black_low_pct, BRK, labels = BLB)
cal <- do.call(rbind, lapply(BLB, function(s) {
  i <- bi$stratum == s; b <- i & bi$truth == "black"
  data.frame(stratum = s, districts = sum(i),
             acc_geo     = mean(bi$geo[i]     == bi$truth[i]),
             acc_surname = mean(bi$surname[i] == bi$truth[i]),
             acc_bisg    = mean(bi$bisg[i]    == bi$truth[i]),
             black_members = sum(b),
             black_found_bisg    = sum(bi$bisg[b]    == "black"),
             black_found_surname = sum(bi$surname[b] == "black"),
             recovery_bisg = if (sum(b)) mean(bi$bisg[b] == "black") else NA_real_)
}))
dd_write_csv(cal, "derived/bisg_calibration.csv")

note("BISG beats geography-only in strata (of 5)",
     sum(cal$acc_bisg > cal$acc_geo))
note("BISG recovery, districts under 10% Black",
     paste0(cal$black_found_bisg[1], " of ", cal$black_members[1]))
note("BISG recovery, districts 30-50% Black",
     paste0(cal$black_found_bisg[4], " of ", cal$black_members[4]))

# What the differential error does downstream: the chapter's own logit, run
# three times on the same districts with three different dependent variables.
co <- function(y) {
  z <- glm(y ~ bi$pop_black_low_pct, family = binomial)
  data.frame(coef = unname(coef(z)[2]), p = summary(z)$coefficients[2, 4])
}
dv <- rbind(cbind(labels = "Historian",   co(bi$truth   == "black")),
            cbind(labels = "Surname only", co(bi$surname == "black")),
            cbind(labels = "BISG",         co(bi$bisg    == "black")))
dd_write_csv(dv, "derived/bisg_downstream.csv")
note("Logit coefficient on the Historian's labels", round(dv$coef[1], 3))
note("Logit coefficient on BISG's labels", round(dv$coef[3], 3))

# --- 6. Lublin's bins, rebuilt ---------------------------------------------

BRK <- c(0, 20, 30, 40, 45, 50, 55, 60, 70, 80, 100)
LAB <- c("0-20%", "20-30%", "30-40%", "40-45%", "45-50%", "50-55%",
         "55-60%", "60-70%", "70-80%", "80-100%")

binup <- function(share, elected, group, base) {
  g <- cut(share, BRK, labels = LAB, right = FALSE, include.lowest = TRUE)
  n <- as.vector(table(g))
  k <- as.vector(tapply(elected, g, sum)); k[is.na(k)] <- 0
  data.frame(group = group, base = base, bin = LAB, n = n, elected = k,
             pct = ifelse(n > 0, 100 * k / n, NA))
}
bins <- rbind(
  binup(d$pop_black_low_pct,  d$black, "Black", "total population"),
  binup(d$vap_black_low_pct,  d$black, "Black", "voting-age population"),
  binup(d$cvap_black_low_pct, d$black, "Black", "citizen voting-age population"),
  binup(d$pop_hisp_pct,       d$hisp,  "Latino", "total population"),
  binup(d$vap_hisp_pct,       d$hisp,  "Latino", "voting-age population"),
  binup(d$cvap_hisp_pct,      d$hisp,  "Latino", "citizen voting-age population"))
bins$bin <- factor(bins$bin, levels = LAB)
dd_write_csv(bins, "derived/bins.csv")

# --- 7. Lublin's published tables, keyed in by hand ------------------------
#
# Table 3 (percent Black elected) and Table 4 (percent Latino elected), 2015,
# with the "Number of cases" rows that sit underneath them. Panels A and B of
# each. Typed from the PDF; the check below is that every panel's case counts
# sum to the N the paper reports for that chamber elsewhere.

lub <- read.csv(text = "
table,panel,measure,chamber,bin,pct,n
3,A,Percent Black in total population,State House,0-20%,0.8,1390
3,A,Percent Black in total population,State House,20-30%,8.6,269
3,A,Percent Black in total population,State House,30-40%,14.6,123
3,A,Percent Black in total population,State House,40-45%,53.1,32
3,A,Percent Black in total population,State House,45-50%,70.6,17
3,A,Percent Black in total population,State House,50-55%,85.7,56
3,A,Percent Black in total population,State House,55-60%,76.1,92
3,A,Percent Black in total population,State House,60-70%,89.8,137
3,A,Percent Black in total population,State House,70-80%,96.6,58
3,A,Percent Black in total population,State House,80-100%,100.0,18
3,A,Percent Black in total population,State Senate,0-20%,0.2,482
3,A,Percent Black in total population,State Senate,20-30%,4.2,120
3,A,Percent Black in total population,State Senate,30-40%,14.6,48
3,A,Percent Black in total population,State Senate,40-45%,45.5,11
3,A,Percent Black in total population,State Senate,45-50%,83.3,6
3,A,Percent Black in total population,State Senate,50-55%,81.3,32
3,A,Percent Black in total population,State Senate,55-60%,77.1,35
3,A,Percent Black in total population,State Senate,60-70%,89.5,38
3,A,Percent Black in total population,State Senate,70-80%,92.9,14
3,A,Percent Black in total population,State Senate,80-100%,100.0,3
3,A,Percent Black in total population,U.S. House,0-20%,1.4,354
3,A,Percent Black in total population,U.S. House,20-30%,13.3,30
3,A,Percent Black in total population,U.S. House,30-40%,21.1,19
3,A,Percent Black in total population,U.S. House,40-45%,100.0,2
3,A,Percent Black in total population,U.S. House,45-50%,100.0,1
3,A,Percent Black in total population,U.S. House,50-55%,100.0,8
3,A,Percent Black in total population,U.S. House,55-60%,100.0,14
3,A,Percent Black in total population,U.S. House,60-70%,85.7,7
3,A,Percent Black in total population,U.S. House,70-80%,NA,0
3,A,Percent Black in total population,U.S. House,80-100%,NA,0
4,A,Percent Hispanic in total population,State House,0-20%,1.9,474
4,A,Percent Hispanic in total population,State House,20-30%,8.2,158
4,A,Percent Hispanic in total population,State House,30-40%,15.1,93
4,A,Percent Hispanic in total population,State House,40-45%,22.6,31
4,A,Percent Hispanic in total population,State House,45-50%,19.1,21
4,A,Percent Hispanic in total population,State House,50-55%,36.4,22
4,A,Percent Hispanic in total population,State House,55-60%,64.5,31
4,A,Percent Hispanic in total population,State House,60-70%,75.9,54
4,A,Percent Hispanic in total population,State House,70-80%,86.2,29
4,A,Percent Hispanic in total population,State House,80-100%,90.9,22
4,A,Percent Hispanic in total population,U.S. House,0-20%,0.6,326
4,A,Percent Hispanic in total population,U.S. House,20-30%,2.4,42
4,A,Percent Hispanic in total population,U.S. House,30-40%,4.6,22
4,A,Percent Hispanic in total population,U.S. House,40-45%,28.6,7
4,A,Percent Hispanic in total population,U.S. House,45-50%,50.0,6
4,A,Percent Hispanic in total population,U.S. House,50-55%,33.3,3
4,A,Percent Hispanic in total population,U.S. House,55-60%,33.3,3
4,A,Percent Hispanic in total population,U.S. House,60-70%,75.0,16
4,A,Percent Hispanic in total population,U.S. House,70-80%,83.3,6
4,A,Percent Hispanic in total population,U.S. House,80-100%,75.0,4
4,B,Percent citizen Hispanic in total population,U.S. House,0-20%,0.8,356
4,B,Percent citizen Hispanic in total population,U.S. House,20-30%,0.0,34
4,B,Percent citizen Hispanic in total population,U.S. House,30-40%,41.7,12
4,B,Percent citizen Hispanic in total population,U.S. House,40-45%,60.0,10
4,B,Percent citizen Hispanic in total population,U.S. House,45-50%,80.0,10
4,B,Percent citizen Hispanic in total population,U.S. House,50-55%,66.7,6
4,B,Percent citizen Hispanic in total population,U.S. House,55-60%,50.0,2
4,B,Percent citizen Hispanic in total population,U.S. House,60-70%,80.0,5
4,B,Percent citizen Hispanic in total population,U.S. House,70-80%,NA,0
4,B,Percent citizen Hispanic in total population,U.S. House,80-100%,NA,0
", strip.white = TRUE)
lub$bin <- factor(lub$bin, levels = LAB)
lub$elected <- round(lub$n * lub$pct / 100)
dd_write_csv(lub, "derived/lublin.csv")

# Every U.S. House panel must account for all 435 districts.
for (p in unique(paste(lub$table, lub$panel))) {
  s <- lub[paste(lub$table, lub$panel) == p & lub$chamber == "U.S. House", ]
  note(paste0("Lublin Table ", sub(" ", "", p), ", U.S. House cases"),
       sum(s$n), 435, sum(s$n) == 435L)
}

t1 <- read.csv(text = "
group,rows,chamber,y1992,y2007,y2015,n
African Americans,Southern states,State House,15.3,18.4,19.9,NA
African Americans,Southern states,State Senate,14.2,16.8,17.8,NA
African Americans,Southern states,U.S. House,13.6,13.7,13.8,NA
African Americans,Non-southern states >10% Black,State House,11.5,13.9,15.1,NA
African Americans,Non-southern states >10% Black,State Senate,10.2,13.8,13.4,NA
African Americans,Non-southern states >10% Black,U.S. House,12.8,13.4,16.3,NA
African Americans,U.S. House,U.S. House,8.7,9.4,10.1,435
Latinos,States >10% Latino,State House,10.3,15.2,17.2,NA
Latinos,States >10% Latino,State Senate,9.8,13.0,14.2,NA
Latinos,States >10% Latino,U.S. House,9.1,13.0,14.8,NA
Latinos,U.S. House,U.S. House,3.9,5.7,7.1,435
", strip.white = TRUE)
dd_write_csv(t1, "derived/lublin_t1.csv")

# --- 8. The Figure 1 simulation, with sigma turned into a dial -------------
#
# The model, exactly as described at pp. 281-282. The electorate is BD Black
# Democrats, R Republicans (all White), and WD = 1 - BD - R White Democrats.
# A Black Democrat must win the Democratic primary, where voting is polarized
# by race, and then the general, where the Democratic coalition is BD + WD:
#
#   P(primary) = Phi( (BD/(BD+WD) - 0.5) / sigma )
#   P(general) = Phi( ((BD+WD)     - 0.5) / sigma )
#   P(success) = P(primary) * P(general)
#
# Lublin et al. set sigma = 0.03 and say so: "a fixed standard deviation set
# arbitrarily at .03". Everything the paper is named after lives in that
# number, which is why it is swept here.

simulate <- function(BD, R, sigma) {
  WD <- 1 - BD - R
  ok <- WD >= 0
  p_pri <- pnorm((BD / (BD + WD) - 0.5) / sigma)
  p_gen <- pnorm(((BD + WD) - 0.5) / sigma)
  ifelse(ok, p_pri * p_gen, NA_real_)
}

BDS   <- c(0.30, 0.35, 0.40, 0.45)
RS    <- seq(0, 0.60, by = 0.005)
SIGMA <- c(0.005, 0.01, 0.02, 0.03, 0.05, 0.08, 0.12)

sim <- expand.grid(BD = BDS, R = RS, sigma = SIGMA)
sim$p <- simulate(sim$BD, sim$R, sim$sigma)
sim <- sim[!is.na(sim$p), ]
dd_write_csv(sim, "derived/simulation.csv")

# Where the peak is, and how wide the near-flat top is, for each (BD, sigma).
peak <- do.call(rbind, lapply(split(sim, list(sim$BD, sim$sigma), drop = TRUE),
  function(z) {
    z <- z[order(z$R), ]
    i <- which.max(z$p)
    hi <- z$R[z$p >= 0.99 * max(z$p)]
    data.frame(BD = z$BD[1], sigma = z$sigma[1], peak_R = z$R[i], peak_p = z$p[i],
               plateau_lo = min(hi), plateau_hi = max(hi),
               plateau_width = max(hi) - min(hi))
  }))
peak <- peak[order(peak$BD, peak$sigma), ]
dd_write_csv(peak, "derived/sweetspot.csv")

# The limit the paper never draws: as sigma goes to zero the probability
# becomes an indicator on the window 1-2BD < R < 0.5, which has no interior
# peak at all.
note("Figure 1(a) peak at sigma = .03 (BD = .30)",
     sprintf("R = %.3f, p = %.3f",
             peak$peak_R[peak$BD == .30 & peak$sigma == .03],
             peak$peak_p[peak$BD == .30 & peak$sigma == .03]))
note("Same curve at sigma = .005, width of the 99%-of-max plateau",
     sprintf("%.3f", peak$plateau_width[peak$BD == .30 & peak$sigma == .005]))

# --- 9. The logit of Figure 2, refit on the 118th --------------------------

fit_one <- function(y, x, label) {
  m <- glm(y ~ x, family = binomial)
  g <- seq(0, 1, by = 0.005)
  data.frame(model = label, x = g,
             p = as.vector(predict(m, data.frame(x = g), type = "response")),
             coef = coef(m)[2], intercept = coef(m)[1],
             half = (-coef(m)[1]) / coef(m)[2])
}
fitted <- rbind(
  fit_one(d$black, d$pop_black_low_pct  / 100, "Black, total population"),
  fit_one(d$black, d$cvap_black_low_pct / 100, "Black, citizen voting-age"),
  fit_one(d$hisp,  d$pop_hisp_pct       / 100, "Latino, total population"),
  fit_one(d$hisp,  d$cvap_hisp_pct      / 100, "Latino, citizen voting-age"))
dd_write_csv(fitted, "derived/fitted.csv")

for (m in unique(fitted$model))
  note(paste0("Share at which ", m, " reaches even odds"),
       sprintf("%.1f%%", 100 * fitted$half[fitted$model == m][1]))

# The paper's own claim, as a surface: does success peak at an intermediate
# Republican share, holding minority share fixed?
sm <- glm(black ~ cvap_black_low_pct + rep_pct + I(rep_pct^2),
          family = binomial, data = d)
grid <- expand.grid(cvap_black_low_pct = seq(0, 70, by = 1),
                    rep_pct = seq(20, 80, by = 1))
grid$p <- as.vector(predict(sm, grid, type = "response"))
dd_write_csv(grid, "derived/surface.csv")
note("Republican share maximizing predicted Black success (fitted)",
     sprintf("%.1f%%", -coef(sm)["rep_pct"] / (2 * coef(sm)["I(rep_pct^2)"])))

# --- 10. Checks -------------------------------------------------------------

ck <- do.call(rbind, CHECKS)
dd_write_csv(ck, "derived/checks.csv")
print(ck, row.names = FALSE)
invisible(prov_report())
cat("\nDone.\n")

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
