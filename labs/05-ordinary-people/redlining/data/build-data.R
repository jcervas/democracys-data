# ---------------------------------------------------------------------------
# build-data.R -- the `redlining` lab
#
# Builds `derived/tracts.csv` (2020 census tracts carrying a 1930s HOLC grade)
# and `derived/cities.csv` (the A-versus-D summary the brief's headline uses).
#
# WHY THIS FILE WAS WRITTEN LATE. It did not exist. `tracts.csv` and
# `cities.csv` were dated 9 August 2026; every other file in derived/ was
# written on the 10th by build-brief-figures.R, which READS those two. The
# script that made them was never committed, so the chapter's central table had
# no way to be re-derived: the brief cited three sources and the folder held
# only two of them. This reconstructs the missing step.
#
# ---------------------------------------------------------------------------
# SOURCES
#
#   [1] HOLC areas -- COMMITTED at raw/holc/mappinginequality.json (10.5 MB).
#       Nelson, Winling, Marciano, Connolly et al., "Mapping Inequality:
#       Redlining in New Deal America", Digital Scholarship Lab, University of
#       Richmond, CC BY-NC-SA. <https://dsl.richmond.edu/panorama/redlining/>
#       10,154 areas, of which 9,324 carry a grade A-D.
#
#       `grade` ARRIVES PADDED. The file contains "A", "A  " and "   A" for the
#       same grade. Filtering on grade %in% c("A","B","C","D") without trimws()
#       silently drops areas -- it looks like a clean subset and is not one.
#
#   [2] 2020 Census tract boundaries -- COMMITTED at raw/tiger/<FIPS>/, one
#       shapefile per state, for exactly the six states this chapter covers.
#       U.S. Census Bureau, TIGER/Line 2020.
#
#   [3] 2020 Census race counts -- FETCHED at run time, P.L. 94-171
#       Redistricting Data, table P1. Not committed: the files are ~40 MB per
#       state and the Bureau still serves them (verified 15 Aug 2026).
#
# ---------------------------------------------------------------------------
# THE RULE, and it is a choice rather than a fact
#
# A tract carries a grade if the tract's INTERNAL POINT -- TIGER's own
# INTPTLON/INTPTLAT, a point the Bureau guarantees to lie inside the tract --
# falls within a graded HOLC polygon. First matching polygon wins.
#
# This is the rule build-brief-figures.R documents and measures against, and it
# is worth being clear about what it costs. A 1937 appraisal area and a 2020
# tract are different shapes drawn eighty-three years apart for unrelated
# purposes; no assignment between them is correct, only defensible. Testing one
# interior point per tract is severe -- a tract 45% covered by a D area whose
# centre sits in an ungraded strip carries no grade at all. Assigning by any
# overlap instead would put 930 Michigan tracts in the table rather than 580.
# The brief's figures report how much of each tract was actually graded, which
# is the honest way to show what this rule threw away.
#
# ---------------------------------------------------------------------------
# WHAT THIS SCRIPT DOES NOT YET DO, stated here rather than discovered later
#
# Run against the committed table it reproduces 4,464 of 4,489 rows exactly --
# every city correct, and no row invented. It does not account for 25 rows in
# the committed file, and disagrees about the grade on 4 more:
#
#     CA 13 missing, 2 grade   IL  4 missing   MI 2 missing
#     OH  3 missing, 1 grade   PA  3 missing, 1 grade   MO exact
#
# Those 25 tracts DO touch a graded area -- they are not inventions -- but
# their internal point lies outside every graded polygon in the national file,
# so no version of this rule reaches them. Neither a centroid, a
# point-on-surface, nor a graded-share threshold separates them: the closest
# non-member sits at 85.5% graded and one of the missing at 77.2%, so any
# threshold that admits them admits others too.
#
# THE FOUR GRADE DISAGREEMENTS RULE OUT A TIE-BREAK. All four run the same way
# -- this build says D, the committed table says C -- which looks like two
# polygons overlapping and the two builds picking different ones. They are not:
# each of the four internal points falls inside EXACTLY ONE graded polygon in
# this file, and that polygon is D. No rule for choosing among candidates can
# return C when C is not a candidate.
#
# So the difference is in the INPUT, not the rule -- and the obvious candidate
# for a different input has been TESTED AND RULED OUT. The per-city geojson
# files (`static/citiesData/<STATE><City><Year>/geojson.json`) are not
# different data: probing resolved MIFlint1937 and MISaginaw1937, and each
# carries exactly the same areas as the national file for that city (Flint
# 55/55, Saginaw 32/32, identical graded counts). The two tracts those cities
# are missing lie outside every graded polygon in BOTH sources.
#
# WHICH LEAVES A LEAD WORTH FOLLOWING, in the brief's own words. Its "How the
# figures were made" note says of build-brief-figures.R that "it is the only
# place here that touches a shapefile". If that was true when it was written,
# then whatever produced tracts.csv never opened a shapefile at all -- and no
# point-in-polygon rule of any kind, this one included, is the method being
# reproduced here. A published HOLC-to-tract crosswalk would fit: it would
# agree with a spatial join almost everywhere and differ exactly like this, in
# a couple of dozen boundary cases out of four and a half thousand.
#
# Until someone identifies it, THIS SCRIPT IS A RECONSTRUCTION AND NOT A
# RECOVERY. It is defensible, documented and reproducible, and it agrees with
# the committed table on 99.4% of rows -- but agreeing is not the same as being
# the thing, and the file says so rather than letting the next reader assume.
#
# WHAT THE DIFFERENCE COSTS. The chapter argues from cities.csv, and there it
# is nearly invisible: same 33 cities, the widest gap unchanged (Cleveland,
# 46.9 points), 7 of 33 city gaps moving and the largest move 2.7 points. So
# this is a provenance problem rather than a findings problem -- which is
# exactly the kind this book says to fix while it is still small.
#
# UNTIL THAT IS RESOLVED THIS SCRIPT WILL NOT OVERWRITE derived/. It builds its
# tables, compares them to the committed ones, prints the difference and stops.
# Set DD_ADOPT=1 to accept the new output. A build script that quietly replaces
# a published table with one 29 rows different, in a book whose whole argument
# is that such differences are invisible, would be the joke writing itself.
# ---------------------------------------------------------------------------

suppressMessages(library(sf))
options(scipen = 999, stringsAsFactors = FALSE)
sf_use_s2(FALSE)          # planar point-in-polygon; see THE RULE above

# Run this script from inside this data/ folder. `../../../_lib/provenance.R`
# records url, bytes, hash and row count for every download and prints a loud
# banner when a source moves under us. If it is missing the build still runs.
if (file.exists("../../../_lib/provenance.R")) {
  source("../../../_lib/provenance.R")
} else {
  prov_fetch  <- function(url, dest, ...) { download.file(url, dest, mode = "wb", quiet = TRUE); dest }
  prov_report <- function() invisible(FALSE)
  prov_stamp  <- function(...) invisible(NULL)
}

dir.create("derived", showWarnings = FALSE)

# The six states, and nothing else. This is not a national build: the chapter
# covers the cities whose TIGER files are committed beside it, and adding a
# seventh state means committing its boundaries too.
STATES <- c("06" = "CA", "17" = "IL", "26" = "MI",
            "29" = "MO", "39" = "OH", "42" = "PA")
PLNAME <- c("06" = "California", "17" = "Illinois", "26" = "Michigan",
            "29" = "Missouri",   "39" = "Ohio",     "42" = "Pennsylvania")
PLABB  <- c("06" = "ca", "17" = "il", "26" = "mi",
            "29" = "mo", "39" = "oh", "42" = "pa")
PLBASE <- paste0("https://www2.census.gov/programs-surveys/decennial/2020/",
                 "data/01-Redistricting_File--PL_94-171/")


# ---- 1. the graded areas ----------------------------------------------------
H <- st_read("raw/holc/mappinginequality.json", quiet = TRUE)
H$grade <- trimws(H$grade)                       # see SOURCES [1]
HG <- st_make_valid(H[H$grade %in% c("A", "B", "C", "D"), ])
cat(sprintf("HOLC areas: %s, of which graded A-D: %s\n",
            format(nrow(H), big.mark = ","), format(nrow(HG), big.mark = ",")))


# ---- 2. race counts, per state ----------------------------------------------
#
# P.L. 94-171 ships as pipe-delimited segments that join on LOGRECNO:
#   <ss>geo2020.pl     geographic header -- field 3 SUMLEV, 8 LOGRECNO, 10 GEOCODE
#   <ss>000012020.pl   segment 1 -- field 5 LOGRECNO, then table P1 from field 6
# SUMLEV 140 is the tract, and its GEOCODE is the 11-digit tract GEOID.
# P1 cell 1 is the total population, cell 3 White alone, cell 4 Black alone.
#
# LOGRECNO MUST BE READ AS TEXT ON BOTH SIDES. It is zero-padded ("0000001"),
# and a numeric read turns it into 1, so the join silently matches nothing and
# every count comes back NA. That failure produced a table of 0 rows here, and
# the checks over it all passed, because every one of them was `all(...)` over
# an empty vector. It is the chapter's own lesson arriving uninvited.
#
# The six zips come to 329 MB and are re-fetched on every run, because they are
# too big to commit and the Bureau still serves them. Point DD_CACHE at a
# directory to keep them between runs -- R deletes its own tempdir() on exit,
# so the default really does download them again each time.
CACHE <- Sys.getenv("DD_CACHE", unset = tempdir())
dir.create(CACHE, showWarnings = FALSE, recursive = TRUE)

pl_tracts <- function(fips) {
  ab <- PLABB[[fips]]
  zip <- file.path(CACHE, paste0(ab, "2020.pl.zip"))
  if (!file.exists(zip))
    prov_fetch(paste0(PLBASE, PLNAME[[fips]], "/", ab, "2020.pl.zip"), zip)
  ex <- file.path(CACHE, paste0("pl_", ab))
  dir.create(ex, showWarnings = FALSE)
  unzip(zip, exdir = ex)

  geo <- read.delim(file.path(ex, paste0(ab, "geo2020.pl")), sep = "|",
                    header = FALSE, quote = "", colClasses = "character")
  names(geo)[c(3, 8, 10)] <- c("SUMLEV", "LOGRECNO", "GEOCODE")
  tg <- geo[geo$SUMLEV == "140", c("LOGRECNO", "GEOCODE")]

  seg <- file.path(ex, paste0(ab, "000012020.pl"))
  w   <- ncol(read.delim(seg, sep = "|", header = FALSE, quote = "", nrows = 1))
  cls <- rep("integer", w); cls[1:5] <- "character"     # 5 is LOGRECNO
  s1  <- read.delim(seg, sep = "|", header = FALSE, quote = "", colClasses = cls)
  p1  <- data.frame(LOGRECNO = s1[[5]], total = s1[[6]],
                    white = s1[[8]], black = s1[[9]])

  m <- merge(tg, p1, by = "LOGRECNO")
  stopifnot(nrow(m) == nrow(tg))         # never let an empty join pass as clean
  data.frame(GEOID = m$GEOCODE, total = m$total, white = m$white, black = m$black)
}


# ---- 3. assign a grade to every tract ---------------------------------------
one_state <- function(fips) {
  hc <- HG[HG$state == STATES[[fips]], ]
  shp <- file.path("raw/tiger", fips, sprintf("tl_2020_%s_tract.shp", fips))
  tr  <- st_transform(st_make_valid(st_read(shp, quiet = TRUE)), st_crs(hc))

  pt <- st_as_sf(data.frame(GEOID = tr$GEOID,
                            lon = as.numeric(tr$INTPTLON),
                            lat = as.numeric(tr$INTPTLAT)),
                 coords = c("lon", "lat"), crs = 4326)
  pt <- st_transform(pt, st_crs(hc))
  i1 <- vapply(st_within(pt, hc),
               function(i) if (length(i)) i[1] else NA_integer_, integer(1))

  got <- data.frame(GEOID = pt$GEOID, state = STATES[[fips]],
                    city = hc$city[i1], grade = hc$grade[i1])
  got <- got[!is.na(got$grade), ]
  out <- merge(got, pl_tracts(fips), by = "GEOID")
  cat(sprintf("  %s %s: %4d graded tracts, %4d with census counts\n",
              fips, STATES[[fips]], nrow(got), nrow(out)))
  out
}

cat("\nstates:\n")
TR <- do.call(rbind, lapply(names(STATES), one_state))
TR <- TR[order(TR$GEOID), c("GEOID", "state", "city", "grade",
                            "total", "white", "black")]

# Rounded to two decimals, and NaN where a tract holds nobody. R writes NaN as
# "NaN" whatever `na=` says, so the not-a-number is turned into a real NA and
# the file gets an empty field -- which is what the committed table has.
TR$pct_black <- round(100 * TR$black / TR$total, 2)
TR$pct_black[!is.finite(TR$pct_black)] <- NA_real_
row.names(TR) <- NULL
cat(sprintf("\ntracts carrying a grade: %s\n", format(nrow(TR), big.mark = ",")))


# ---- 4. the city summary ----------------------------------------------------
# One row per city that has BOTH an A tract and a D tract -- the comparison the
# brief makes is between the two ends, so a city with only one end cannot make
# it. Percentages are of pooled population, not a mean of tract percentages,
# and `gap` is the difference before rounding rather than between the two
# rounded numbers, which is why it can end in a digit neither of them shows.
side <- function(g) {
  s <- TR[TR$grade == g, ]
  a <- aggregate(cbind(n = rep(1, nrow(s)), total = s$total, black = s$black),
                 by = list(city = s$city), sum)
  a
}
A <- side("A"); D <- side("D")
m <- merge(A, D, by = "city", suffixes = c(".a", ".d"))
m$a_pct <- 100 * m$black.a / m$total.a
m$d_pct <- 100 * m$black.d / m$total.d
CI <- data.frame(city = m$city, a_tracts = m$n.a, d_tracts = m$n.d,
                 a_pop = m$total.a, d_pop = m$total.d,
                 a_pct_black = round(m$a_pct, 1),
                 d_pct_black = round(m$d_pct, 1),
                 gap = round(m$d_pct - m$a_pct, 1))
CI <- CI[order(-CI$gap), ]
row.names(CI) <- NULL
cat(sprintf("cities with both an A and a D tract: %d\n", nrow(CI)))


# ---- 5. compare with what is committed, and only then write -----------------
# See "WHAT THIS SCRIPT DOES NOT YET DO". The comparison is the deliverable
# until the 29-row difference is explained.
adopt <- nzchar(Sys.getenv("DD_ADOPT"))
tmp_t <- file.path(tempdir(), "tracts.csv")
tmp_c <- file.path(tempdir(), "cities.csv")
write.csv(TR, tmp_t, row.names = FALSE, na = "")
write.csv(CI, tmp_c, row.names = FALSE, na = "")

same <- FALSE
if (file.exists("derived/tracts.csv")) {
  old <- read.csv("derived/tracts.csv", colClasses = c(GEOID = "character"))
  same <- identical(readLines(tmp_t), readLines("derived/tracts.csv"))
  ms <- setdiff(old$GEOID, TR$GEOID)
  cm <- merge(TR[c("GEOID", "grade")], old[c("GEOID", "grade")],
              by = "GEOID", suffixes = c(".new", ".old"))
  gd <- cm[cm$grade.new != cm$grade.old, ]
  cat(sprintf("\nagainst the committed table: %s rows there, %s here\n",
              format(nrow(old), big.mark = ","), format(nrow(TR), big.mark = ",")))
  cat(sprintf("  rows only in the new build : %d\n", length(setdiff(TR$GEOID, old$GEOID))))
  cat(sprintf("  rows only in the committed : %d\n", length(ms)))
  cat(sprintf("  grade disagreements        : %d\n", nrow(gd)))
  cat(sprintf("  byte-identical             : %s\n", same))

  # Print the differing rows rather than a path to them. An earlier version
  # wrote the candidate table to tempdir() and told the reader to go and look
  # at it -- but R deletes tempdir() when the session ends, so the path was
  # always dead by the time anyone read the message.
  if (length(ms)) {
    cat("\n  in the committed table, not reached by this rule:\n")
    show <- old[old$GEOID %in% ms, c("GEOID", "state", "city", "grade", "total")]
    print(utils::head(show[order(show$GEOID), ], 30), row.names = FALSE)
  }
  if (nrow(gd)) {
    cat("\n  same tract, different grade (new vs committed):\n")
    print(gd[order(gd$GEOID), ], row.names = FALSE)
  }

  # What the difference actually costs the chapter's headline. The brief argues
  # from cities.csv, so a 29-row disagreement in a 4,489-row table matters only
  # insofar as it moves these.
  if (file.exists("derived/cities.csv")) {
    oc <- read.csv("derived/cities.csv")
    mc <- merge(CI, oc, by = "city", suffixes = c(".new", ".old"))
    cat(sprintf("\n  cities: %d then, %d now, same set: %s\n",
                nrow(oc), nrow(CI), identical(sort(oc$city), sort(CI$city))))
    cat(sprintf("  gap differs in %d of %d cities; largest change %.1f points\n",
                sum(mc$gap.new != mc$gap.old), nrow(mc),
                max(abs(mc$gap.new - mc$gap.old))))
    cat(sprintf("  widest gap: %s %.1f then, %s %.1f now\n",
                oc$city[which.max(oc$gap)], max(oc$gap),
                CI$city[which.max(CI$gap)], max(CI$gap)))
  }
}

if (same || adopt || !file.exists("derived/tracts.csv")) {
  file.copy(tmp_t, "derived/tracts.csv", overwrite = TRUE)
  file.copy(tmp_c, "derived/cities.csv", overwrite = TRUE)
  cat("\nwrote derived/tracts.csv and derived/cities.csv\n")
} else {
  cat("\n  NOT written. The build differs from the committed table and the\n")
  cat("  difference is not yet explained, so the published numbers stand.\n")
  cat("  Accept with: DD_ADOPT=1 Rscript build-data.R\n")
}

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
