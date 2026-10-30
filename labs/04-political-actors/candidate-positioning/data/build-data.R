# ---------------------------------------------------------------------------
# Build the candidate-positioning dataset.
#
# SOURCES: Bonica, DIME v4.0, Stanford University Libraries (1979-2024); Voteview; MEDSL U.S. House 1976-2024; presidential vote by district.
#
# The chapter replicates and extends Ansolabehere, Snyder and Stewart,
# "Candidate Positioning in U.S. House Elections," American Journal of
# Political Science 45:1 (January 2001), 136-159. The paper asks two things
# of every House race: do the two candidates in a district converge on that
# district's voters, and do they move with the district? It answers the
# first with a 1996 candidate survey (Project Vote Smart's NPAT) and the
# second with roll-call scores going back to 1874. Neither of its sources can
# be extended as it was built, so this script substitutes one source for each:
#
#   the 1996 survey     ->  DIME campaign-finance scores, every cycle 1980-2024
#   Heckman-Snyder      ->  DW-NOMINATE, every Congress from the 44th
#
# FETCHED 2026-10-20. Sources:
#
#   1. Bonica, Adam. 2024. Database on Ideology, Money in Politics, and
#      Elections: Public version 4.0. Stanford University Libraries.
#      https://data.stanford.edu/dime
#      The candidate file, dime_recipients_1979_2024.csv.gz (30,488,833
#      bytes), is a Dropbox link published on that page. It is kept whole in
#      raw/, with the v4.0 codebook beside it.
#
#      A CFscore places a candidate by WHO GAVE THEM MONEY. Donors who give to
#      several candidates tie those candidates together, and a scaling of the
#      whole web of giving puts every donor and every candidate on one line.
#      It needs no roll calls, so it scores challengers who never served,
#      which is the one thing the 1996 survey did and roll calls cannot.
#
#   2. Voteview, HSall_members.csv. NOT fetched here: read from
#      ../../officeholder-age/data/raw/, the newest of the three committed
#      copies in this corpus (through the 119th Congress).
#        https://voteview.com/static/data/out/members/HSall_members.csv
#
#   3. MIT Election Data and Science Lab, "U.S. House 1976-2024", Harvard
#      Dataverse, https://doi.org/10.7910/DVN/IG0UN2. NOT fetched here: read
#      from ../../../03-elections/incumbency-advantage/data/raw/. It supplies
#      the list of general-election nominees and their votes. DIME has the
#      scores but records no 2024 winners, and records earlier ones unevenly.
#
#   4. Presidential vote by congressional district, 1952-2024. NOT fetched
#      here: read from ../../../03-elections/house-competition/data/derived/.
#      That chapter joins Gary Jacobson's district file (through 2014) to The
#      Downballot's calculations (2016-2024) and decides which presidential
#      year belongs to which map. Re-making that decision here would make it
#      twice.
#        https://www.the-downballot.com/p/the-downballots-calculations-of-presidential
#
#   5. The paper's own published numbers for 1996, keyed in by hand from the
#      AJPS article (pp. 141-144 and footnote 10), so the replication can be
#      read beside the original.
#
# TWO SCORES IN DIME, AND WHY THIS SCRIPT USES THE PLAINER ONE
#
# DIME carries recipient.cfscore (the donor scaling above) and dwdime (a model
# trained to predict a member's DW-NOMINATE score from their donors). Among
# incumbents, dwdime tracks DW-NOMINATE almost perfectly within each party and
# the CFscore only loosely -- see the checks. But dwdime is fitted to the very
# roll calls it is then compared with, it is missing for a third of
# challengers, and the comparison the chapter cares about is challengers
# against incumbents. So the CFscore is the measure; dwdime is carried in
# nominees.csv for anyone who wants to try the other answer; and the long
# responsiveness series is taken from roll calls directly.
#
# OUTPUTS
#
#   derived/nominees.csv        every major-party House nominee 1980-2024, with
#                               a CFscore where DIME has a usable one
#   derived/races.csv           one row per race with both nominees scored
#   derived/by_cycle.csv        one row per cycle: spread, crossings, response
#   derived/by_status.csv       incumbents, open-seat candidates, challengers,
#                               in districts within five points of even
#   derived/gap_by_district.csv the gap between the two candidates, by how
#                               Republican the district is
#   derived/replacements.csv    every seat that changed party, 1874-2024, with
#                               the outgoing and incoming member's position
#   derived/replacement_by_decade.csv   the paper's Figure 5, extended
#   derived/responsiveness.csv  the paper's Figure 6, from 1952 on, two sources
#   derived/paper_1996.csv      the paper's 1996 numbers beside DIME's
#   derived/facts.csv           single numbers the brief quotes
#   derived/checks.csv          the validation results
# ---------------------------------------------------------------------------

source("../../../_lib/precision.R")
dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)
options(scipen = 999, stringsAsFactors = FALSE)
suppressPackageStartupMessages(library(data.table))

if (file.exists("../../../_lib/provenance.R")) {
  source("../../../_lib/provenance.R")
} else {
  prov_fetch  <- function(url, dest, ...) {
    if (!file.exists(dest)) download.file(url, dest, mode = "wb", quiet = TRUE)
    dest
  }
  prov_report <- function() invisible(FALSE)
}

CHECKS <- list()
check <- function(what, value, expected, passed) {
  CHECKS[[length(CHECKS) + 1L]] <<-
    data.frame(check = what, value = as.character(value),
               expected = as.character(expected), passed = isTRUE(passed))
  invisible(NULL)
}
FACTS <- list()
fact <- function(key, value) {
  FACTS[[length(FACTS) + 1L]] <<- data.frame(key = key, value = as.character(dd_num(value)))
  invisible(value)
}
must <- function(cond, msg) if (!isTRUE(cond)) stop(msg, call. = FALSE)

SOUTH      <- c("AL", "AR", "FL", "GA", "LA", "MS", "NC", "SC", "TN", "TX", "VA")
MIN_DONORS <- 10    # distinct donors over a career, below which a CFscore is not used

# --- 1. DIME ----------------------------------------------------------------

U_DIME <- paste0("https://www.dropbox.com/scl/fi/bcpmxvxwc7iwmqvchm2sp/",
                 "dime_recipients_1979_2024.csv.gz?rlkey=4ii8765xw8y2svlzcbxzfvnbh&dl=1")
U_BOOK <- "https://bit.ly/dimeV4codebook"
invisible(prov_fetch(U_DIME, "raw/dime_recipients_1979_2024.csv.gz", label = "DIME v4.0 recipients"))
invisible(prov_fetch(U_BOOK, "raw/dime_codebook_v4.pdf", label = "DIME v4.0 codebook"))

dime <- fread("raw/dime_recipients_1979_2024.csv.gz", showProgress = FALSE,
              colClasses = list(character = c("district", "party", "seat",
                                              "ico.status", "gwinner")))
must(all(c("cycle", "seat", "party", "district", "lname", "fname", "dwdime",
           "recipient.cfscore", "ico.status", "num.givers.total", "dwnom1") %in% names(dime)),
     "DIME columns have changed.")
fact("dime_rows", nrow(dime))
dh <- dime[seat == "federal:house" & recipient.type == "cand" &
           party %in% c("100", "200") & cycle >= 1980 &
           grepl("^[A-Z]{2}[0-9]{2}$", district) & !is.na(recipient.cfscore)]
dh[, `:=`(pty = ifelse(party == "100", "D", "R"),
          cd  = as.integer(substr(district, 3, 4)))]
dh[cd == 0L, cd := 1L]                      # DIME codes a few at-large seats 00
fact("dime_house_records", nrow(dh))

# How well each DIME score tracks roll calls WITHIN a party, among members who
# have both. This is the reason the long responsiveness series uses roll calls.
inc <- dh[!is.na(dwnom1) & !is.na(dwdime)]
r_cf <- inc[, .(r = cor(recipient.cfscore, dwnom1)), by = pty]
r_dw <- inc[, .(r = cor(dwdime, dwnom1)), by = pty]
fact("r_cf_dem", round(r_cf[pty == "D", r], 2)); fact("r_cf_rep", round(r_cf[pty == "R", r], 2))
fact("r_dwdime_dem", round(r_dw[pty == "D", r], 2)); fact("r_dwdime_rep", round(r_dw[pty == "R", r], 2))
fact("r_cf_all", round(inc[, cor(recipient.cfscore, dwnom1)], 2))
check("CFscore tracks DW-NOMINATE across both parties (r)",
      sprintf("%.2f", inc[, cor(recipient.cfscore, dwnom1)]), "> 0.85",
      inc[, cor(recipient.cfscore, dwnom1)] > 0.85)

# --- 2. The nominees, from MEDSL --------------------------------------------
#
# MEDSL lists every line a candidate appeared on. A New York candidate on the
# Democratic and Working Families lines appears twice; the votes are summed
# per candidate and the candidate keeps the major-party label. Minnesota's
# Democrats run as DFL and North Dakota's as Democratic-NPL; they are
# Democrats here. Special elections and runoffs are dropped -- the paper is
# about regular general elections.

medsl <- fread("../../../03-elections/incumbency-advantage/data/raw/1976-2024-house.tab",
               showProgress = FALSE)
medsl <- medsl[stage == "GEN" & special == FALSE &
               (is.na(runoff) | runoff == FALSE) & year >= 1980]
DP <- c("DEMOCRAT", "DEMOCRATIC", "DEMOCRATIC-FARMER-LABOR", "DEMOCRATIC-FARM-LABOR",
        "DEMOCRATIC-NONPARTISAN LEAGUE", "DEMOCRATIC-NPL")
RP <- c("REPUBLICAN", "INDEPENDENT-REPUBLICAN")
medsl[, pty := fifelse(party %in% DP, "D", fifelse(party %in% RP, "R", NA_character_))]
medsl[district == 0L, district := 1L]

# Some names in the file were UTF-8 encoded twice, so RAUL GRIJALVA's accented
# U arrives as two garbage characters and every curly quote as three. Undo the
# second encoding, then normalise quotes and drop stray escape backslashes.
fix_name <- function(s) {
  bad <- grepl("Ã|â\u0080", s)
  z <- iconv(s[bad], "UTF-8", "latin1"); Encoding(z) <- "UTF-8"
  s[bad] <- ifelse(is.na(z), s[bad], z)
  s <- gsub("[\\]", "", s)
  s <- gsub("[“”]", "\"", s)
  s <- gsub("’", "'", s)
  s <- gsub("\"+", "\"", s)
  trimws(gsub("\\s+", " ", s))
}
medsl[, candidate := fix_name(candidate)]
check("Candidate names left with double-encoded characters",
      sum(grepl("Ã|â\u0080", medsl$candidate)), 0,
      !any(grepl("Ã|â\u0080", medsl$candidate)))

cv  <- medsl[, .(votes = sum(candidatevotes), pty = pty[!is.na(pty)][1]),
             by = .(year, state_po, district, candidate)]
nom <- cv[!is.na(pty)][order(-votes)][, .SD[1], by = .(year, state_po, district, pty)]
setnames(nom, c("year", "state_po", "district"), c("cycle", "state", "cd"))
npc <- nom[, .N, by = cycle]
check("Major-party nominees per cycle (two per seat, less uncontested seats)",
      sprintf("%d to %d", min(npc$N), max(npc$N)), "between 700 and 870",
      all(npc$N >= 700 & npc$N <= 870))

# Match each nominee to a DIME record: same cycle, state, party, and a last
# name contained in the MEDSL name, letters only (so ROYBAL-ALLARD meets
# "roybal allard" and O'CONNOR meets "o connor"). Same district first. DIME
# sometimes files a candidate under the district they ran in two years before,
# or under a new map's number before the map changed; those are taken only
# when the first name agrees too. Where several DIME records qualify, the one
# with the most donors wins -- that is the general-election campaign, not a
# stray committee.
lett <- function(s) gsub("[^A-Z]", "", toupper(iconv(s, "UTF-8", "ASCII//TRANSLIT")))
nom[, NC := lett(candidate)]
dh[, `:=`(NL = lett(lname), NF = lett(fname))]

j <- merge(nom, dh[, .(cycle, state, pty, dcd = cd, NL, NF, name, ico.status,
                       cf = recipient.cfscore, dwdime, donors = num.givers.total,
                       num.givers)],
           by = c("cycle", "state", "pty"), allow.cartesian = TRUE)
j <- j[nchar(NL) >= 2 & mapply(grepl, NL, NC, fixed = TRUE)]
j[, tier := fifelse(dcd == cd, 1L,
            fifelse(nchar(NF) >= 2 & mapply(grepl, NF, NC, fixed = TRUE), 2L, NA_integer_))]
j <- j[!is.na(tier)][order(tier, -num.givers)][, .SD[1], by = .(cycle, state, cd, pty)]
j <- j[!duplicated(paste(cycle, name, pty))]     # one DIME record, one nominee

nom <- merge(nom, j[, .(cycle, state, cd, pty, status = ico.status,
                        cfscore = cf, dwdime, donors, match_tier = tier)],
             by = c("cycle", "state", "cd", "pty"), all.x = TRUE)

# A CFscore resting on a handful of donors is mostly noise: several of the
# "Republican to the left of the Democrat" races in a first pass were
# nominees with two or three donors in a hopeless seat. Below MIN_DONORS
# distinct donors over a career, the score is kept in the file but not used.
nom[, usable := !is.na(cfscore) & donors >= MIN_DONORS]
fact("min_donors", MIN_DONORS)
fact("n_nominees", nrow(nom))
fact("n_matched", sum(!is.na(nom$cfscore)))
fact("n_thin", sum(!is.na(nom$cfscore) & !nom$usable))
fact("n_usable", sum(nom$usable))
fact("pct_usable", round(100 * mean(nom$usable), 1))
mr <- nom[, .(rate = mean(usable)), by = cycle]
fact("worst_cycle", mr$cycle[which.min(mr$rate)])
fact("worst_cycle_pct", round(100 * min(mr$rate), 1))
check("Nominees with a usable DIME score, all cycles",
      sprintf("%.1f%%", 100 * mean(nom$usable)), ">= 80%", mean(nom$usable) >= 0.80)
check("Lowest share in any one cycle",
      sprintf("%.1f%% (%d)", 100 * min(mr$rate), mr$cycle[which.min(mr$rate)]),
      ">= 70%", min(mr$rate) >= 0.70)
check("Nominees matched through the first-name fallback, not the district",
      sum(nom$match_tier == 2, na.rm = TRUE), "under 2% of nominees",
      sum(nom$match_tier == 2, na.rm = TRUE) < 0.02 * nrow(nom))

# Who goes unscored matters more than how many: they are the nominees who
# raised too little to leave a trace in the FEC's records.
v_un <- median(nom[usable == FALSE, votes]); v_m <- median(nom[usable == TRUE, votes])
fact("votes_unscored", v_un); fact("votes_scored", v_m)
fact("unscored_challenger_pct",
     round(100 * nom[usable == FALSE, mean(is.na(status) | status == "C")], 1))
check("Unscored nominees won fewer votes than scored ones (medians)",
      sprintf("%s vs %s", format(v_un, big.mark = ","), format(v_m, big.mark = ",")),
      "unscored lower", v_un < v_m)

# --- 3. District presidential vote ------------------------------------------

pres <- fread("../../../03-elections/house-competition/data/derived/races.csv")
key  <- unique(fread("../../../03-elections/house-competition/data/derived/pres_by_cd.csv")[
                 , .(snum = stcd %/% 100, state = state_abb)])
must(nrow(key) == 50 && !anyDuplicated(key$snum), "State code key is not 50 unique states.")
pres[, `:=`(snum = stcd %/% 100, cd = stcd %% 100, state = NULL)]
pres <- merge(pres, key, by = "snum")
pres[cd == 0L, cd := 1L]
pres <- pres[!is.na(dpres), .(year, state, cd, rep_pres = 100 - dpres)]
check("District-years with a presidential vote, 1952-2024",
      nrow(pres), "over 15,000", nrow(pres) > 15000)

# --- 4. The common scale ----------------------------------------------------
#
# A CFscore of 1.0 does not mean the same thing in 1980 and 2024, because the
# donor pool moved. The paper meets the same problem with roll calls and
# solves it the same way: in each cycle, set the average Democratic nominee
# to 0 and the average Republican nominee to 1. A distance of 1.0 then means
# "as far apart as the two parties are nationally".

nom <- merge(nom, pres, by.x = c("cycle", "state", "cd"),
             by.y = c("year", "state", "cd"), all.x = TRUE)
nom[, `:=`(mD = mean(cfscore[pty == "D" & usable]), mR = mean(cfscore[pty == "R" & usable])),
    by = cycle]
nom[, position := fifelse(usable, (cfscore - mD) / (mR - mD), NA_real_)]
nom[, c("mD", "mR", "NC") := NULL]
nom[, status := fcase(status == "I", "incumbent", status == "C", "challenger",
                      status == "O", "open seat", default = NA_character_)]
nom[, south := state %in% SOUTH]

out_nom <- nom[, .(cycle, state, district = cd, south, party = ifelse(pty == "D", "Democrat", "Republican"),
                   candidate, votes, status, donors, cfscore, dwdime, position, rep_pres)][
               order(cycle, state, district, party)]
dd_write_csv(out_nom, "derived/nominees.csv")

# --- 5. Races: both nominees scored -----------------------------------------

w <- dcast(nom[usable == TRUE], cycle + state + cd + south + rep_pres ~ pty,
           value.var = c("position", "votes", "status", "candidate"))
w <- w[!is.na(position_D) & !is.na(position_R)]
w[, `:=`(gap = position_R - position_D,
         midpoint = (position_R + position_D) / 2,
         dem_share = 100 * votes_D / (votes_D + votes_R))]
w[, seat := fcase(status_D == "incumbent", "Democratic incumbent",
                  status_R == "incumbent", "Republican incumbent",
                  default = "open seat")]
races <- w[, .(cycle, state, district = cd, south, rep_pres,
               dem_candidate = candidate_D, rep_candidate = candidate_R,
               dem_position = position_D, rep_position = position_R,
               gap, midpoint, dem_share, seat)][order(cycle, state, district)]
dd_write_csv(races, "derived/races.csv")
fact("n_races", nrow(races))
fact("n_crossed", sum(races$gap <= 0))
fact("n_close", sum(races$gap < 0.25))
fact("pct_close", round(100 * mean(races$gap < 0.25), 1))
check("Races with both nominees scored, 1980-2024", nrow(races), "over 6,000", nrow(races) > 6000)

cr <- races[gap <= 0][order(gap)]
cat("\nRaces where the Republican is not to the right:\n"); print(cr[, .(cycle, state, district, dem_candidate, rep_candidate, gap = round(gap, 2))])

# --- 6. Responsiveness ------------------------------------------------------
#
# The paper's Figure 6: within one party, are nominees from more Republican
# districts more conservative? Fit a line through each party's nominees,
# position against the Republican share of the presidential vote, and report
# what the line says about two districts twenty points apart -- one where the
# Republican won 40% of the presidential vote, one where he won 60%. In units
# of the national party gap: 0.25 means the second district's nominee sits a
# quarter of the distance between the parties further right.

slope20 <- function(y, x) {
  ok <- !is.na(y) & !is.na(x)
  if (sum(ok) < 20) return(NA_real_)
  unname(coef(lm(y[ok] ~ x[ok]))[2]) * 20
}

resp_dime <- nom[usable == TRUE & !is.na(rep_pres),
                 .(n = .N, shift_40_60 = slope20(position, rep_pres)),
                 by = .(year = cycle, party = pty)]
resp_dime[, source := "Campaign donors (DIME), all nominees"]

vv <- fread("../../officeholder-age/data/raw/HSall_members.csv", showProgress = FALSE)
vv <- vv[chamber == "House" & party_code %in% c(100, 200) & !is.na(nominate_dim1) &
         district_code >= 1 & district_code < 98]
vv[, `:=`(pty = ifelse(party_code == 100, "D", "R"),
          year = 1786 + 2 * congress,               # the election that seated it
          occ = fifelse(is.na(occupancy), 0L, as.integer(occupancy)))]
vv[, `:=`(mD = mean(nominate_dim1[pty == "D"]), mR = mean(nominate_dim1[pty == "R"])),
   by = congress]
vv[, position := (nominate_dim1 - mD) / (mR - mD)]
check("Latest Congress in the Voteview file", max(vv$congress), ">= 119", max(vv$congress) >= 119)

# The member seated by the election is the seat's first occupant that Congress.
first_occ <- vv[order(occ, -nominate_number_of_votes)][, .SD[1], by = .(congress, state_abbrev, district_code)]
mv <- merge(first_occ[, .(year, state = state_abbrev, cd = district_code, pty, position)],
            pres, by = c("year", "state", "cd"))
resp_vv <- mv[, .(n = .N, shift_40_60 = slope20(position, rep_pres)), by = .(year, party = pty)]
resp_vv[, source := "Roll calls (Voteview), winners only"]

resp <- rbind(resp_vv, resp_dime)[order(source, party, year)]
resp[, party := ifelse(party == "D", "Democrat", "Republican")]
dd_write_csv(resp[, .(year, party, source, n, shift_40_60)], "derived/responsiveness.csv")

ov <- merge(resp_vv[, .(year, party, a = shift_40_60)],
            resp_dime[, .(year, party, b = shift_40_60)], by = c("year", "party"))
fact("resp_vv_above_dime", sum(ov$a > ov$b)); fact("resp_overlap", nrow(ov))
check("Roll calls show more responsiveness than donors in most overlapping years",
      sprintf("%d of %d", sum(ov$a > ov$b), nrow(ov)), "most", sum(ov$a > ov$b) > nrow(ov) / 2)

pk <- function(p) { r <- resp_vv[party == p][which.max(shift_40_60)]; r }
fact("vv_dem_peak_year", pk("D")$year); fact("vv_dem_peak", round(pk("D")$shift_40_60, 2))
fact("vv_rep_peak_year", pk("R")$year); fact("vv_rep_peak", round(pk("R")$shift_40_60, 2))
fact("vv_dem_2024", round(resp_vv[party == "D" & year == 2024, shift_40_60], 2))
fact("vv_rep_2024", round(resp_vv[party == "R" & year == 2024, shift_40_60], 2))
fact("vv_dem_1996", round(resp_vv[party == "D" & year == 1996, shift_40_60], 2))
fact("vv_rep_1996", round(resp_vv[party == "R" & year == 1996, shift_40_60], 2))

# --- 7. Incumbents, open seats, challengers ---------------------------------
#
# The paper's Table 1: holding the district fixed, incumbents sit closest to
# the middle, open-seat candidates next, challengers furthest out. Holding
# the district fixed is done here the plain way -- only districts where the
# Republican presidential share was within five points of 50 -- so the
# comparison is a column of averages rather than a regression.

era <- function(y) fcase(y < 2000, "1980-1998", y < 2012, "2000-2010", default = "2012-2024")
bs <- nom[usable == TRUE & !is.na(status) & !is.na(rep_pres) & abs(rep_pres - 50) <= 5,
          .(n = .N, mean_position = mean(position)),
          by = .(era = era(cycle), party = ifelse(pty == "D", "Democrat", "Republican"), status)]
# distance from the party's own centre toward the other party
bs[, toward_other_party := ifelse(party == "Democrat", mean_position, 1 - mean_position)]
bs <- bs[order(era, party, -toward_other_party)]
dd_write_csv(bs, "derived/by_status.csv")
ord_ok <- bs[, all(status[order(-toward_other_party)] == c("incumbent", "open seat", "challenger")),
             by = .(era, party)]
fact("status_order_holds", sum(ord_ok$V1)); fact("status_order_cells", nrow(ord_ok))
check("Incumbent > open seat > challenger in moderation, era-party cells",
      sprintf("%d of %d", sum(ord_ok$V1), nrow(ord_ok)), "most", sum(ord_ok$V1) >= nrow(ord_ok) / 2)

# --- 8. The gap, by district ------------------------------------------------
#
# The paper's Figure 3: is the gap between the two candidates smallest in the
# evenly divided districts? Five-point bins of the Republican presidential
# share, three eras, bins with fewer than 15 races dropped.

races[, bin := 5 * floor(rep_pres / 5) + 2.5]
gd <- races[!is.na(bin), .(n = .N, mean_gap = mean(gap)), by = .(era = era(cycle), bin)][n >= 15]
dd_write_csv(gd[order(era, bin)], "derived/gap_by_district.csv")
races[, bin := NULL]
mid <- gd[abs(bin - 50) < 5, .(m = mean(mean_gap)), by = era]
edge <- gd[abs(bin - 50) >= 15, .(e = mean(mean_gap)), by = era]
me <- merge(mid, edge, by = "era")
fact("gap_mid_8098", round(me[era == "1980-1998", m], 2)); fact("gap_edge_8098", round(me[era == "1980-1998", e], 2))
fact("gap_mid_1224", round(me[era == "2012-2024", m], 2)); fact("gap_edge_1224", round(me[era == "2012-2024", e], 2))

# --- 9. By cycle ------------------------------------------------------------

by_cycle <- races[, .(races = .N, median_gap = median(gap),
                      crossed = sum(gap <= 0), close_pairs = sum(gap < 0.25)), by = cycle]
sdp <- nom[usable == TRUE, .(sd = sd(position)), by = .(cycle, pty)]
by_cycle <- merge(by_cycle, dcast(sdp, cycle ~ pty, value.var = "sd"), by = "cycle")
setnames(by_cycle, c("D", "R"), c("dem_spread", "rep_spread"))
mt <- nom[, .(nominees = .N, scored = sum(usable)), by = cycle]
by_cycle <- merge(mt, by_cycle, by = "cycle")
rr <- dcast(resp_dime, year ~ party, value.var = "shift_40_60")
setnames(rr, c("year", "D", "R"), c("cycle", "dem_shift_40_60", "rep_shift_40_60"))
by_cycle <- merge(by_cycle, rr, by = "cycle")
dd_write_csv(by_cycle, "derived/by_cycle.csv")
check("Cycles 1980-2024", nrow(by_cycle), 23, nrow(by_cycle) == 23)
fact("spread_dem_1980", round(by_cycle[cycle == 1980, dem_spread], 2))
fact("spread_dem_2024", round(by_cycle[cycle == 2024, dem_spread], 2))
fact("spread_rep_1980", round(by_cycle[cycle == 1980, rep_spread], 2))
fact("spread_rep_2024", round(by_cycle[cycle == 2024, rep_spread], 2))
fact("close_8090", round(100 * by_cycle[cycle <= 1990, sum(close_pairs) / sum(races)], 1))
fact("close_1424", round(100 * by_cycle[cycle >= 2014, sum(close_pairs) / sum(races)], 1))
fact("races_2024", by_cycle[cycle == 2024, races]); fact("crossed_2024", by_cycle[cycle == 2024, crossed])
fact("races_1996", by_cycle[cycle == 1996, races]); fact("crossed_1996", by_cycle[cycle == 1996, crossed])

# --- 10. Partisan replacement, 1874-2024 ------------------------------------
#
# The paper's Figure 5. When a seat changes party, how far does the district's
# representative move? If candidates fit their districts, less than the
# distance between the parties (below 1.0). If each party just sends one of
# its own, about 1.0.
#
# Seats are compared across consecutive Congresses by state and district
# number. After the maps are redrawn for a new census the same number names different ground, so
# every election in a year ending in 2 is dropped, as the paper drops its
# redistricting cases. Mid-decade redraws are not caught by that rule.

last_occ <- vv[order(-occ, -nominate_number_of_votes)][, .SD[1], by = .(congress, state_abbrev, district_code)]
a <- last_occ[, .(congress, state = state_abbrev, cd = district_code,
                  old_party = pty, old_member = bioname, old_icpsr = icpsr, old_position = position)]
b <- first_occ[, .(congress = congress - 1L, state = state_abbrev, cd = district_code,
                   new_party = pty, new_member = bioname, new_icpsr = icpsr, new_position = position)]
rp <- merge(a, b, by = c("congress", "state", "cd"))
rp[, year := 1786 + 2 * (congress + 1L)]
rp <- rp[year >= 1874 & year %% 10 != 2 & old_party != new_party & old_icpsr != new_icpsr]
rp[, `:=`(shift = abs(new_position - old_position),
          south = state %in% SOUTH,
          decade = 10L * (year %/% 10L))]
repl <- rp[, .(year, state, district = cd, south,
               old_member, old_party = ifelse(old_party == "D", "Democrat", "Republican"), old_position,
               new_member, new_party = ifelse(new_party == "D", "Democrat", "Republican"), new_position,
               shift)][order(year, state, district)]
dd_write_csv(repl, "derived/replacements.csv")
fact("n_replacements", nrow(repl))
check("Seats that changed party, 1874-2024, years ending in 2 excluded",
      nrow(repl), "over 2,000", nrow(repl) > 2000)

dec <- rbind(rp[, .(region = "All districts", n = .N, mean_shift = mean(shift)), by = decade],
             rp[south == TRUE,  .(region = "South", n = .N, mean_shift = mean(shift)), by = decade],
             rp[south == FALSE, .(region = "Outside the South", n = .N, mean_shift = mean(shift)), by = decade])
# A decade-region with fewer than 10 seats changing hands is too thin to plot:
# the South had single digits in every decade from the 1900s to the 1950s,
# when almost no Southern seat ever changed party.
fact("repl_south_thin", sum(dec$region == "South" & dec$n < 10))
dec <- dec[n >= 10][order(region, decade)]
dd_write_csv(dec, "derived/replacement_by_decade.csv")
ns <- dec[region == "Outside the South"]
fact("repl_out_peak", round(max(ns$mean_shift), 2)); fact("repl_out_peak_decade", ns$decade[which.max(ns$mean_shift)])
fact("repl_south_7080_max", round(dec[region == "South" & decade %in% c(1970, 1980), max(mean_shift)], 2))
al <- dec[region == "All districts"]
fact("repl_peak_decade", al$decade[which.max(al$mean_shift)]); fact("repl_peak", round(max(al$mean_shift), 2))
fact("repl_low_decade", al$decade[which.min(al$mean_shift)]); fact("repl_low", round(min(al$mean_shift), 2))
fact("repl_1870s", round(al[decade == 1870, mean_shift], 2))
fact("repl_2020s", round(al[decade == 2020, mean_shift], 2)); fact("repl_2020s_n", al[decade == 2020, n])
fact("repl_since2000", round(rp[year >= 2000, mean(shift)], 2))
fact("repl_crossed", sum(rp$new_party == "R" & rp$new_position < rp$old_position |
                         rp$new_party == "D" & rp$new_position > rp$old_position))

# --- 11. The paper's 1996 numbers beside DIME's -----------------------------
#
# The paper's scale runs 0 (most liberal candidate) to 1 (most conservative);
# ours sets the party means at 0 and 1. Ratios survive the change of scale,
# so each published figure is divided by the published party gap (.47).
# Footnote 10 gives the slopes on the presidential vote (.50 Democrats, .28
# Republicans); twenty points of presidential vote is .2 of that.

P_GAP <- 0.47
r96 <- races[cycle == 1996]; n96 <- nom[cycle == 1996 & usable == TRUE]
cmp <- data.table(
  quantity = c("Races with both candidates placed",
               "Races where the Republican is not to the right of the Democrat",
               "Spread of Democratic candidates, in party gaps",
               "Spread of Republican candidates, in party gaps",
               "Democrats, 40% to 60% Republican district, in party gaps",
               "Republicans, 40% to 60% Republican district, in party gaps"),
  paper = c(301, 1, 0.15 / P_GAP, 0.14 / P_GAP, 0.50 * 0.2 / P_GAP, 0.28 * 0.2 / P_GAP),
  dime  = c(nrow(r96), sum(r96$gap <= 0),
            sd(n96[pty == "D", position]), sd(n96[pty == "R", position]),
            resp_dime[year == 1996 & party == "D", shift_40_60],
            resp_dime[year == 1996 & party == "R", shift_40_60]))
cmp[, paper := round(paper, 2)]; cmp[, dime := round(dime, 2)]
dd_write_csv(cmp, "derived/paper_1996.csv")

# --- 12. Report -------------------------------------------------------------

cat("\nBy cycle:\n"); print(by_cycle[, .(cycle, races, crossed, close_pairs,
                                         D = round(dem_shift_40_60, 2), R = round(rep_shift_40_60, 2),
                                         sdD = round(dem_spread, 2), sdR = round(rep_spread, 2))])
cat("\nBy status:\n"); print(bs)
cat("\nGap by district:\n"); print(dcast(gd, bin ~ era, value.var = "mean_gap"))
cat("\nReplacement by decade (all):\n"); print(al)
cat("\nPaper vs DIME, 1996:\n"); print(cmp)

fa <- do.call(rbind, FACTS)
must(!anyDuplicated(fa$key), "Duplicate fact key.")
write.csv(fa, "derived/facts.csv", row.names = FALSE)
ck <- do.call(rbind, CHECKS)
write.csv(ck, "derived/checks.csv", row.names = FALSE)
print(ck, row.names = FALSE)
must(all(ck$passed), "A check failed.")
invisible(prov_report())

if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_stamp()
}
