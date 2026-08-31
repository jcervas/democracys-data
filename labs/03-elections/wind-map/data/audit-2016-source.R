# ---------------------------------------------------------------------------
# WHY THE 2016 FILE WAS REPLACED -- the audit, run rather than asserted.
#
# build-1620.R used to take 2016 from the same repository as 2020 and 2024. It
# now takes it from the MIT Election Data and Science Lab. This script is the
# evidence for that swap, and it exists so the chapter can quote measured
# numbers instead of a claim about numbers.
#
# THE METHOD.  Three files, two of which are being judged:
#
#   A  tonmcg 2016   the election-night scrape that used to be in use
#   B  MEDSL  2016   official state records; what build-1620.R now fetches
#   R  jaytimm       state-level D% and R%, an INDEPENDENT referee -- it is the
#                    source behind ../historical-campaigns/, it is not derived
#                    from either A or B, and it is already in this repository
#
# Both A and B are aggregated to states and scored against R on the TWO-PARTY
# margin, which is the quantity the arrows actually draw. A referee that shares
# a source with a contestant proves nothing, which is the whole reason for
# reaching outside this lab folder for it.
#
# One caution on reading the output. Where A and B miss the referee by the SAME
# amount in the same direction, the referee is the odd one out, not the file --
# South Dakota does this in 2016 and in 2020 alike, so it is a quirk of the
# state file rather than a fault in anyone's county returns.
#
# Run from inside data/:  Rscript audit-2016-source.R
# Writes: source_audit_2016.csv (per state, both files),
#         derived/correction_2016.csv  (per county, old swing vs new swing),
#         and facts.csv keys.
#
# RUN ORDER: after build-1620.R -- the county correction reads its output.
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE, timeout = 900)
say <- function(...) cat(..., "\n", sep = "")

# A is FROZEN, not fetched. This script is the evidence for a decision the
# chapter states as fact, so the file it judges has to be the file it judged.
# The compilation is maintained by one person under no obligation to anybody,
# and an upstream correction would silently rewrite the audit's verdict --
# possibly into agreement, which would read as though the swap had been
# unnecessary. The capture in raw/ was taken on 2026-08-13.
#
# To re-audit against a fresh copy, replace raw/ from the address below and
# re-run; the numbers the chapter quotes should then be re-read, not assumed.
A_URL <- paste0("https://raw.githubusercontent.com/tonmcg/",
                "US_County_Level_Election_Results_08-24/master/",
                "2016_US_County_Level_Presidential_Results.csv")
A_RAW <- "raw/2016_US_County_Level_Presidential_Results.csv"
if (!file.exists(A_RAW))
  stop(sprintf("%s is missing. It is a committed specimen, not a download. Upstream: %s",
               A_RAW, A_URL))
B_URL <- paste0("https://raw.githubusercontent.com/MEDSL/county-returns/",
                "master/countypres_2000-2016.csv")
REF   <- file.path("..", "..", "historical-campaigns", "data", "derived",
                   "pres_states_1864_2024.csv")

FACTS <- list()
fact  <- function(k, v) { FACTS[[k]] <<- v; invisible(v) }

# ---- A: the scrape ---------------------------------------------------------
say("[1] tonmcg 2016  (frozen specimen, raw/)")
a <- read.csv(A_RAW, colClasses = "character")
A <- data.frame(fips = sprintf("%05d", as.integer(a$combined_fips)),
                d = as.numeric(a$votes_dem), r = as.numeric(a$votes_gop),
                t = as.numeric(a$total_votes))

# ---- B: the official compilation. Same three repairs build-1620.R makes -----
say("[2] MEDSL 2016")
b <- read.csv(B_URL, colClasses = "character")
b <- b[b$year == "2016", ]
b$v <- as.numeric(b$candidatevotes)
b <- b[!(is.na(b$FIPS) | b$FIPS %in% c("", "NA")), ]
b$fips <- sprintf("%05d", as.integer(b$FIPS))
b$fips[b$fips == "36000"] <- "29095"                    # Kansas City
gv <- function(sel, f) as.numeric(tapply(b$v[sel], b$fips[sel], sum,
                                         na.rm = TRUE)[f])
f  <- sort(unique(b$fips))
B  <- data.frame(fips = f,
                 d = gv(b$party == "democrat"   & !is.na(b$party), f),
                 r = gv(b$party == "republican" & !is.na(b$party), f),
                 t = gv(rep(TRUE, nrow(b)), f))
B <- B[B$t > 0, ]                                        # Bedford City, VA

# ---- R: the referee --------------------------------------------------------
H  <- read.csv(REF); H <- H[H$year == 2016, ]
po <- read.csv(text = paste(
  "po,fp", "AL,01","AZ,04","AR,05","CA,06","CO,08","CT,09","DE,10","DC,11",
  "FL,12","GA,13","HI,15","ID,16","IL,17","IN,18","IA,19","KS,20","KY,21",
  "LA,22","ME,23","MD,24","MA,25","MI,26","MN,27","MS,28","MO,29","MT,30",
  "NE,31","NV,32","NH,33","NJ,34","NM,35","NY,36","NC,37","ND,38","OH,39",
  "OK,40","OR,41","PA,42","RI,44","SC,45","SD,46","TN,47","TX,48","UT,49",
  "VT,50","VA,51","WA,53","WV,54","WI,55","WY,56", sep = "\n"),
  colClasses = "character")

tpm <- function(d, r) 100 * (r - d) / (r + d)
say("[3] score")
rows <- do.call(rbind, lapply(seq_len(nrow(po)), function(i) {
  sa <- A[substr(A$fips, 1, 2) == po$fp[i], ]
  sb <- B[substr(B$fips, 1, 2) == po$fp[i], ]
  hh <- H[H$state_abbrev == po$po[i], ]
  if (!nrow(sa) || !nrow(sb) || !nrow(hh)) return(NULL)
  ref <- tpm(hh$democrat, hh$republican)
  data.frame(state = po$po[i],
             err_scrape   = tpm(sum(sa$d), sum(sa$r)) - ref,
             err_official = tpm(sum(sb$d), sum(sb$r)) - ref,
             # how much of the canvass-final total the scrape was holding
             share_of_final = 100 * sum(sa$t) / sum(sb$t))
}))
rows <- rows[order(rows$share_of_final), ]
dd_write_csv(rows, "derived/source_audit_2016.csv")

fact("src16_err_scrape",   round(median(abs(rows$err_scrape)),   3))
fact("src16_err_official", round(median(abs(rows$err_official)), 3))
w <- rows[which.max(abs(rows$err_scrape)), ]
fact("src16_worst_state",  w$state)
fact("src16_worst_err",    round(w$err_scrape, 2))
for (s in c("AZ", "CA", "UT", "WA", "NY", "TX", "FL")) {
  fact(paste0("src16_share_", tolower(s)),
       round(rows$share_of_final[rows$state == s]))
}
fact("src16_states_under95", sum(rows$share_of_final < 95))
fact("src16_votes_recovered",
     round(sum(B$t) - sum(A$t[A$fips %in% B$fips])))

print(head(rows, 8), row.names = FALSE)
say("  scrape   median |err| ", FACTS$src16_err_scrape, " pts")
say("  official median |err| ", FACTS$src16_err_official, " pts")

# ---- the correction, county by county, for the figure ----------------------
# RUN ORDER: this needs wind_us_1620.csv, so build-1620.R has to have run first.
# The 2020 end is identical under both 2016 sources -- only the 2016 margin
# moves -- so the old swing can be rebuilt from the new file's margin_20 plus
# the scrape's own 2016 votes, and no second join to the 2020 returns is needed.
say("[4] county-level correction")
if (!file.exists("derived/wind_us_1620.csv"))
  stop("run build-1620.R before this script: correction_2016.csv needs it")
W <- read.csv("derived/wind_us_1620.csv", colClasses = c(county_fips = "character"))
cm <- merge(W[, c("county_fips", "county_name", "x", "y", "in_frame",
                  "margin_20", "swing")],
            A, by.x = "county_fips", by.y = "fips")
cm <- cm[(cm$d + cm$r) > 0, ]
cm$swing_old <- cm$margin_20 - 100 * (cm$r - cm$d) / (cm$r + cm$d)
cm$delta     <- cm$swing - cm$swing_old          # + => the scrape understated R
out <- data.frame(county_fips = cm$county_fips, county_name = cm$county_name,
                  x = round(cm$x, 1), y = round(cm$y, 1), in_frame = cm$in_frame,
                  swing_old = round(cm$swing_old, 4),
                  swing_new = round(cm$swing, 4),
                  delta     = round(cm$delta, 4))
out <- out[order(-abs(out$delta)), ]
dd_write_csv(out, "derived/correction_2016.csv")

k <- abs(out$delta) > 0.25
fact("corr_n_moved",  sum(k))
fact("corr_n_1pt",    sum(abs(out$delta) > 1))
fact("corr_n_5pt",    sum(abs(out$delta) > 5))
fact("corr_understated", sum(out$delta >  0.25))   # scrape too Democratic
fact("corr_overstated",  sum(out$delta < -0.25))   # scrape too Republican
fact("corr_flips",    sum(sign(out$swing_old) != sign(out$swing_new)))
fact("corr_max_county", sub(" County$", "", out$county_name[1]))
fact("corr_max_delta",  round(abs(out$delta[1]), 1))
fact("corr_max_old",    round(out$swing_old[1], 1))
fact("corr_max_new",    round(out$swing_new[1], 1))
say("  ", sum(k), " counties moved >0.25 pt (", sum(abs(out$delta) > 1),
    " >1 pt); ", FACTS$corr_flips, " change direction")

# ---- merge into facts.csv, disturbing nothing else -------------------------
new <- data.frame(key = names(FACTS),
                  value = vapply(FACTS, function(z) as.character(dd_num(z))[1],
                                 character(1)))
rownames(new) <- NULL
old <- read.csv("derived/facts.csv", colClasses = "character")
# facts.csv was written with row names at some point, so it carries a stray
# unnamed first column that read.csv surfaces as `X`. `new` has no such column,
# and rbind() on frames of different widths is an error -- which is why this
# merge failed and the script could not refresh its own facts. The values it
# writes were already correct, having been written before the stray column
# appeared, so the failure was silent and durable rather than visible.
#
# Keep only the two columns that mean anything, from both sides.
KEEP <- c("key", "value")
stopifnot(all(KEEP %in% names(old)))
old <- old[!old$key %in% new$key, KEEP, drop = FALSE]
merged <- rbind(old, new[, KEEP, drop = FALSE])
stopifnot(!anyDuplicated(merged$key), nrow(merged) >= nrow(new))
dd_write_csv(merged, "derived/facts.csv")
say("[4] facts.csv now carries ", nrow(old) + nrow(new), " keys (",
    nrow(new), " from this script)")
