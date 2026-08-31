# ---------------------------------------------------------------------------
# Extend the House competition dataset from 1946-2014 to 1946-2024.
#
# WHY THIS IS A SEPARATE SCRIPT. Jacobson's file ends in 2014. Everything after
# it comes from a different source with a different schema, and splicing two
# sources is the kind of thing that should be visible rather than buried inside
# build-data.R. This script does the splice, validates it, and says what it
# could not carry across.
#
# ---------------------------------------------------------------------------
# THE ONE MANUAL STEP
#
# MEDSL's "U.S. House 1976-2024" is behind a Dataverse **guestbook** -- a form a
# human has to submit. The API returns:
#
#     {"status":"ERROR","message":"You may not download this file without the
#      required Guestbook response for guestbookID 458."}
#
# So: open
#     https://doi.org/10.7910/DVN/IG0UN2
# download **1976-2024-house.tab** into this directory, and run this script.
# Nothing else is needed -- no key, no account.
#
# (This is the third time in this course a public dataset has turned out to be
# human-downloadable and not scriptable. It is worth saying out loud to the
# class: public is not the same as usable.)
#
# ---------------------------------------------------------------------------
# WHAT THE SPLICE CAN AND CANNOT CARRY
#
# CARRIES CLEANLY, because both sources are just votes:
#   dv           Democratic share of the two-party vote
#   uncontested  no major-party opponent
#   margin, competitive, landslide
#   south, midterm
#
# DERIVED, AND WEAKER THAN JACOBSON'S:
#   incwin       Jacobson coded incumbency by hand. MEDSL does not carry it, so
#                for 2016+ it is inferred by matching the winner's name to the
#                previous election's winner in the same district. Redistricting
#                breaks that in 2022; the script flags affected years rather
#                than pretending otherwise.
#
# NOW SOLVED ON THE PRESIDENTIAL SIDE:
#   dpres        The presidential vote INSIDE each congressional district.
#                Neither MEDSL nor the Clerk of the House reports it. It comes
#                from The Downballot (formerly Daily Kos Elections), fetched by
#                `fetch-pres-by-cd.R` in this directory -- keyless, scriptable,
#                already run, output in `derived/pres_by_cd.csv`.
#
#                This script joins it on below. The pairing has to respect
#                which district lines each House election used:
#                    2016, 2018 House -> 2016 presidential, 2012-2021 lines
#                    2020       House -> 2020 presidential, 2012-2021 lines
#                    2022       House -> 2020 presidential, 2022 lines
#                    2024       House -> 2024 presidential, 2024 lines
#
#                So split districts DO extend to 2024 -- but only once the
#                House winner's party is known, which is what the MEDSL file
#                supplies. Presidential side: done. House side: still one
#                manual download away.
#
# ---------------------------------------------------------------------------
# THE VALIDATION, WHICH IS THE POINT
#
# The two sources OVERLAP for 1976-2014 -- 20 elections. That is not a nuisance,
# it is a free test. This script recomputes uncontested / competitive /
# landslide rates from MEDSL for those years and compares them to Jacobson's.
# If they agree, the parse is right and the 2016-2024 extension can be trusted.
# If they do not, the extension is NOT written.
#
# Run from this directory:  Rscript extend-to-2024.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

MEDSL <- "1976-2024-house.tab"
TOL   <- 2.5   # max acceptable disagreement, percentage points, on overlap years

if (!file.exists(MEDSL)) {
  cat("\n", strrep("-", 70), "\n", sep = "")
  cat("MEDSL file not found:  ", MEDSL, "\n\n", sep = "")
  cat("Download 1976-2024-house.tab from\n")
  cat("    https://doi.org/10.7910/DVN/IG0UN2\n")
  cat("into this directory, then re-run. It is behind a Dataverse guestbook,\n")
  cat("so it cannot be fetched by script.\n")
  cat(strrep("-", 70), "\n\n", sep = "")
  quit(status = 0)
}

SOUTH <- c("ALABAMA","ARKANSAS","FLORIDA","GEORGIA","LOUISIANA","MISSISSIPPI",
           "NORTH CAROLINA","SOUTH CAROLINA","TENNESSEE","TEXAS","VIRGINIA")

m <- read.delim(MEDSL, stringsAsFactors = FALSE, quote = "")
cat(sprintf("MEDSL rows: %s\n", format(nrow(m), big.mark = ",")))
need <- c("year","state","district","stage","special","party","candidate",
          "candidatevotes","totalvotes")
miss <- setdiff(need, names(m))
if (length(miss)) stop("MEDSL schema changed; missing: ", paste(miss, collapse = ", "))

# ---- general elections only, regular seats only ---------------------------
m$stage <- toupper(as.character(m$stage))
m <- m[m$stage %in% c("GEN", "GENERAL"), ]
sp <- tolower(as.character(m$special))
m  <- m[sp %in% c("false", "f", "no", ""), ]
m  <- m[!is.na(m$candidatevotes), ]
cat(sprintf("after general/non-special filter: %s\n", format(nrow(m), big.mark = ",")))

m$state <- toupper(trimws(m$state))
m <- m[!m$state %in% c("DISTRICT OF COLUMBIA","PUERTO RICO","GUAM",
                       "VIRGIN ISLANDS","AMERICAN SAMOA",
                       "NORTHERN MARIANA ISLANDS"), ]

# at-large districts appear as 0 or "AL" depending on release
m$district <- toupper(trimws(as.character(m$district)))
m$district[m$district %in% c("AL","AT-LARGE","")] <- "0"
m$stcd <- paste(m$state, sprintf("%02d", suppressWarnings(as.integer(m$district))))

# ---- fusion: one candidate, several ballot lines --------------------------
# Sum a candidate's votes across every line they appear on before deciding
# anything. NY and CT would otherwise split a winner into pieces.
m$party <- toupper(trimws(as.character(m$party)))
m$major <- ifelse(grepl("^DEMOCRAT",  m$party), "DEM",
           ifelse(grepl("^REPUBLICAN", m$party), "REP", "OTH"))
# a fusion candidate's major-party identity comes from their major-party line
key <- paste(m$year, m$stcd, toupper(trimws(m$candidate)))
maj <- tapply(m$major, key, function(v) {
  u <- unique(v[v != "OTH"]); if (length(u) == 1) u else "OTH"
})
m$cand_major <- as.vector(maj[key])

agg <- aggregate(candidatevotes ~ year + state + stcd + cand_major,
                 data = m, FUN = sum)

wide <- reshape(agg, idvar = c("year","state","stcd"), timevar = "cand_major",
                direction = "wide")
for (k in c("candidatevotes.DEM","candidatevotes.REP","candidatevotes.OTH"))
  if (!k %in% names(wide)) wide[[k]] <- NA
names(wide)[match(c("candidatevotes.DEM","candidatevotes.REP","candidatevotes.OTH"),
                  names(wide))] <- c("dem","rep","oth")
wide$dem[is.na(wide$dem)] <- 0
wide$rep[is.na(wide$rep)] <- 0
wide$oth[is.na(wide$oth)] <- 0

# ---- the definitions, matched to Jacobson's ------------------------------
# dv is the Democratic share of the TWO-PARTY vote, and is NA when one major
# party did not run -- which is exactly how Jacobson encodes uncontested.
two <- wide$dem + wide$rep
wide$dv <- ifelse(wide$dem > 0 & wide$rep > 0, 100 * wide$dem / two, NA)

# TOP-TWO STATES ARE NOT UNCONTESTED. California and Washington send the two
# highest primary finishers to the general, so a district can offer two
# Democrats. That is a contested race with no Republican, and coding it as
# uncontested would be wrong. Flag it separately and exclude from both counts.
nd <- tapply(m$cand_major == "DEM", key, any)
nr <- tapply(m$cand_major == "REP", key, any)
cands <- aggregate(cbind(n_dem = m$cand_major == "DEM",
                         n_rep = m$cand_major == "REP") ~ year + stcd,
                   data = m, FUN = function(x) length(unique(x[x])))
wide$top_two <- FALSE
tt <- wide$state %in% c("CALIFORNIA","WASHINGTON") & is.na(wide$dv) &
      (wide$dem > 0) & (wide$rep == 0)
tt2 <- wide$state %in% c("CALIFORNIA","WASHINGTON") & is.na(wide$dv) &
       (wide$rep > 0) & (wide$dem == 0)
wide$top_two <- tt | tt2
cat(sprintf("top-two same-party generals flagged (CA/WA): %d\n", sum(wide$top_two)))

wide$uncontested <- is.na(wide$dv) & !wide$top_two
wide$margin      <- abs(wide$dv - 50)
wide$competitive <- !is.na(wide$margin) & wide$margin <= 5
wide$landslide   <- !is.na(wide$margin) & wide$margin >= 20
wide$south       <- as.integer(wide$state %in% SOUTH)
wide$midterm     <- as.integer(wide$year %% 4 == 2)
wide$split_district <- NA        # dvp does not exist in this source -- see header

# ---- THE VALIDATION ------------------------------------------------------
jac <- read.csv("derived/races.csv", stringsAsFactors = FALSE)
ov  <- intersect(sort(unique(jac$year)), sort(unique(wide$year)))
ov  <- ov[ov <= 2014]
cat(sprintf("\noverlap years for validation: %d  (%d-%d)\n",
            length(ov), min(ov), max(ov)))

cmp <- do.call(rbind, lapply(ov, function(y) {
  a <- jac [jac$year  == y, ]
  b <- wide[wide$year == y, ]
  data.frame(year = y,
             n_jac = nrow(a), n_medsl = nrow(b),
             unc_jac   = round(100 * mean(a$uncontested), 1),
             unc_medsl = round(100 * mean(b$uncontested), 1),
             comp_jac   = round(100 * mean(a$competitive), 1),
             comp_medsl = round(100 * mean(b$competitive), 1))
}))
cmp$unc_diff  <- round(cmp$unc_medsl  - cmp$unc_jac, 1)
cmp$comp_diff <- round(cmp$comp_medsl - cmp$comp_jac, 1)
print(cmp, row.names = FALSE)

worst <- max(abs(c(cmp$unc_diff, cmp$comp_diff)))
cat(sprintf("\nlargest disagreement on overlap: %.1f points (tolerance %.1f)\n",
            worst, TOL))

if (worst > TOL) {
  cat("\n*** VALIDATION FAILED. The two sources do not agree on years they\n")
  cat("*** both cover, so the parse above is wrong somewhere. Nothing has\n")
  cat("*** been written. Fix the parse before trusting 2016-2024.\n\n")
  quit(status = 1)
}
cat("validation passed -- the parse reproduces Jacobson on shared years.\n")

# ---- splice: Jacobson through 2014, MEDSL after --------------------------
new <- wide[wide$year > 2014, ]
cat(sprintf("\nappending %d races for %s\n", nrow(new),
            paste(sort(unique(new$year)), collapse = ", ")))

keep <- c("year","state","stcd","south","midterm","dv","dvp","dpres","incwin",
          "uncontested","margin","competitive","landslide","split_district")
new$dvp <- NA
new$incwin <- NA          # see header: not carried; derive separately if wanted

# ---- join the presidential-by-district figures ---------------------------
PBCD <- "derived/pres_by_cd.csv"
if (file.exists(PBCD)) {
  pb <- read.csv(PBCD, stringsAsFactors = FALSE)
  # House election year -> (presidential year, district lines) -- see header
  pair <- data.frame(
    year      = c(2016, 2018, 2020, 2022, 2024),
    pres_year = c(2016, 2016, 2020, 2020, 2024),
    lines     = c("2012-2021","2012-2021","2012-2021","2022","2024"),
    stringsAsFactors = FALSE)
  new <- merge(new, pair, by = "year", all.x = TRUE)
  new <- merge(new, pb[, c("lines","pres_year","stcd","dpres")],
               by = c("lines","pres_year","stcd"), all.x = TRUE)
  new$split_district <- ifelse(!is.na(new$dv) & !is.na(new$dpres),
                               (new$dv > 50) != (new$dpres > 50), NA)
  cat(sprintf("presidential-by-district matched for %d of %d new races\n",
              sum(!is.na(new$dpres)), nrow(new)))
} else {
  cat("pres_by_cd.csv not found -- run fetch-pres-by-cd.R; split stays NA\n")
  new$dpres <- NA
}
new$source <- "MEDSL"
jac$source <- "Jacobson"
new$state <- as.character(new$state)

all <- rbind(jac[, c(keep, "source")], new[, c(keep, "source")])
write.csv(all, "derived/races.csv", row.names = FALSE)

pc <- function(x, y) round(100 * as.vector(tapply(x, y, mean, na.rm = TRUE)), 1)
yrs <- sort(unique(all$year))
by <- data.frame(
  year  = yrs,
  races = as.vector(table(all$year)),
  pct_uncontested = pc(all$uncontested, all$year),
  pct_competitive = pc(all$competitive, all$year),
  pct_landslide   = pc(all$landslide,   all$year),
  stringsAsFactors = FALSE)
s <- all[all$south == 1, ]; n <- all[all$south == 0, ]
by$pct_uncontested_south     <- pc(s$uncontested, s$year)
by$pct_uncontested_non_south <- pc(n$uncontested, n$year)
sp <- tapply(all$split_district, all$year, function(x) mean(x, na.rm = TRUE))
by$pct_split <- round(100 * as.vector(sp), 1)
by$split_coverage <- round(100 * as.vector(tapply(!is.na(all$dpres), all$year, mean)), 1)
by$pct_split[by$split_coverage < 90] <- NA
write.csv(by, "derived/by_year.csv", row.names = FALSE)

cat(sprintf("\nwritten: %d elections, %d-%d\n", nrow(by), min(yrs), max(yrs)))
cat("split districts extend through 2024 where pres_by_cd.csv supplied dpres.\n")
