# ---------------------------------------------------------------------------
# Build the datasets for the "false matches" chapter.
#
# The chapter is about record linkage: what happens when an election office
# decides that two registration records carrying the same name and the same
# date of birth must belong to the same person. The birthday problem is the
# way in; a purge rule is the object.
#
# ---------------------------------------------------------------------------
# SOURCES (consulted 2026-08-11; neither file is redistributed)
# ---------------------------------------------------------------------------
#
# 1. NEW JERSEY -- the primary source, and the one that carries a FULL DATE OF
#    BIRTH, which is the field the Interstate Crosscheck program actually
#    matched on.
#
#      ~/My Drive/Redistricting/2026/New Jersey/State Voter File/
#         vlist_<County>.csv.zip     21 files, one per county
#
#    Statewide: 6,657,135 registration records across all 21 counties.
#    Schema (28 columns, verified identical across all 21 files):
#      displayId, leg_id, party, status, reg_date, last, first, middle,
#      suffix, dob, street_num, street_pre, street_post, street_base,
#      street_suff, street_name, apt_unit, city, zip, county, municipality,
#      ward, district, congressional, legislative, freeholder, school, fire
#    `dob` is a complete ISO date (YYYY-MM-DD). Six columns are read; the
#    other 22 are discarded at parse time via colClasses = "NULL", which is
#    what keeps 1.2 GB of CSV inside a few hundred MB of memory.
#
#    Because all 21 counties are in one state, a match key shared by two
#    records in DIFFERENT counties is exactly the cross-jurisdiction match
#    Crosscheck was built to find. That comparison is computed below.
#
# 2. GEORGIA -- Houston County only, and used for one specific contrast.
#
#      ~/My Drive/Redistricting/2026/Houston County/Superseding Report/
#         data/voter-reg/71754 - Houston County.csv
#      46,092,604 bytes, 127,560 registration records.
#      Read already by bisg-check/data/build-data.R, which is where the
#      colClasses = "character" convention comes from.
#
#    Georgia's public extract publishes BIRTH YEAR, not full date of birth.
#    That is not a limitation of this script -- it is the finding. A state
#    that publishes only a birth year forces any matcher working from public
#    data onto a key roughly two orders of magnitude coarser than Crosscheck's.
#    New Jersey lets us measure exactly how much coarser, because there the
#    same 6.66 million records can be keyed both ways.
#
# ---------------------------------------------------------------------------
# PRIVACY
# ---------------------------------------------------------------------------
# These are public records, but this script writes COUNTS AND DISTRIBUTIONS
# ONLY. No name, no date of birth, no displayId, no registration number, no
# address, and no row that could identify a person is written to this folder
# or reaches the chapter. The largest group of registrants sharing a first
# name, last name and full date of birth in New Jersey has four members;
# publishing the number four is safe, publishing the key would not be. Raw
# files are never copied here.
#
# ---------------------------------------------------------------------------
# WHAT IS COUNTED
# ---------------------------------------------------------------------------
# All registration records in each file, regardless of status. New Jersey's
# `status` field distinguishes Active from Inactive and several conditional
# states; both are counted, because list-maintenance programs act on the
# registration list as a whole and inactive registrations are the ones most
# often targeted for removal. The active-only count is reported in facts.csv
# so the chapter can state the universe precisely.
#
# Run this script from inside the data/ folder. It reads about 1.2 GB from
# local disk, downloads nothing, and needs no API key and no network.
# It takes a few minutes, almost all of it in the subsample scaling loop.
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)
t_start <- Sys.time()

DRIVE <- file.path(path.expand("~"),
  "Library/CloudStorage/GoogleDrive-jcervas@andrew.cmu.edu/My Drive")
NJDIR <- file.path(DRIVE, "Redistricting/2026/New Jersey/State Voter File")
GAFILE <- file.path(DRIVE, "Redistricting/2026/Houston County/Superseding Report",
                    "data/voter-reg", "71754 - Houston County.csv")
stopifnot(dir.exists(NJDIR), file.exists(GAFILE))

up <- function(x) toupper(trimws(x))

# Collision counting is done by sorting, not by table(). table() on 6.7 million
# character strings is quadratic enough in practice to dominate the runtime;
# order(method = "radix") followed by rle() gives the same group sizes in a
# few seconds. Everything downstream is derived from those group sizes.
grpsizes <- function(key) rle(sort(key, method = "radix"))$lengths
npairs   <- function(key) sum(choose(grpsizes(key), 2))

# --- 1. Read New Jersey -----------------------------------------------------

KEEP <- c("last", "first", "middle", "dob", "county", "status")
zs <- list.files(NJDIR, pattern = "^vlist_.*\\.csv\\.zip$", full.names = TRUE)
stopifnot(length(zs) == 21)

readco <- function(z) {
  nm <- sub("\\.zip$", "", basename(z))
  h  <- names(read.csv(unz(z, nm), nrows = 1, colClasses = "character"))
  stopifnot(all(KEEP %in% h))              # schema check, every county
  read.csv(unz(z, nm), colClasses = ifelse(h %in% KEEP, "character", "NULL"))
}
cat("reading", length(zs), "New Jersey counties")
nj <- do.call(rbind, lapply(zs, function(z) { cat("."); readco(z) }))
N  <- nrow(nj)
cat("\nNew Jersey:", format(N, big.mark = ","), "records,",
    length(unique(nj$county)), "counties\n")
stopifnot(N == 6657135, length(unique(nj$county)) == 21)

LAST  <- up(nj$last)
FIRST <- up(nj$first)
MIDI  <- substr(up(nj$middle), 1, 1)
DOB   <- trimws(nj$dob)
BYEAR <- substr(DOB, 1, 4)
COUNTY <- nj$county
n_active <- sum(grepl("^Active", nj$status))
rm(nj); invisible(gc())

# Four records statewide have an empty dob. They are kept -- dropping them
# would quietly change the denominator -- but an empty key cannot collide
# with a populated one, so they contribute nothing to any count below.
cat("records with an empty date of birth:", sum(!nzchar(DOB)), "\n")

# --- 2. The match-key ladder ------------------------------------------------
#
# Six rules applied to the SAME 6.66 million records, from the crudest thing
# an office might match on to something tighter than Crosscheck recommended.
# The pair count is the quantity that matters: a purge rule acts on pairs, and
# pairs are what grow with the square of the file.

keyrow <- function(label, note, key) {
  s <- grpsizes(key)
  data.frame(label = label, note = note,
             keys = length(s), dup_keys = sum(s > 1),
             flagged = sum(s[s > 1]), pct = 100 * sum(s[s > 1]) / N,
             pairs = sum(choose(s, 2)), max_group = max(s))
}
cat("counting collisions at six key precisions")
keys <- rbind(
  keyrow("first + last name", "no date of any kind",
         paste(FIRST, LAST, sep = "|")),
  keyrow("last name + full date of birth", "first name ignored",
         paste(LAST, DOB, sep = "|")),
  keyrow("first + last name + birth year",
         "the coarsest key a Georgia-style public file allows",
         paste(FIRST, LAST, BYEAR, sep = "|")),
  keyrow("first initial + last name + full date of birth",
         "a fuzzy key: first name abbreviated to its initial",
         paste(substr(FIRST, 1, 1), LAST, DOB, sep = "|")),
  keyrow("first + last name + full date of birth",
         "the Crosscheck key",
         paste(FIRST, LAST, DOB, sep = "|")),
  keyrow("first + last name + full date of birth + middle initial",
         "Crosscheck's own tightening",
         paste(FIRST, LAST, DOB, MIDI, sep = "|")))
cat("\n")
keys <- keys[order(-keys$pairs), ]
dd_write_csv(keys, "derived/keys.csv")
print(keys[, c("label", "dup_keys", "flagged", "pairs", "max_group")], row.names = FALSE)

kp <- function(lab) keys$pairs[keys$label == lab]
CCKEY <- "first + last name + full date of birth"

# --- 3. Cross-county collisions --------------------------------------------
#
# The Crosscheck failure mode itself. A key held by registrants in two
# different counties is a match that a cross-jurisdiction program would flag
# and that a single-jurisdiction check would never see.
#
# Sorting by (key, county) makes this an O(n) scan: a group spans more than
# one county exactly when some adjacent pair inside it differs in county.

K  <- paste(FIRST, LAST, DOB, sep = "|")
o  <- order(K, COUNTY, method = "radix")
ks <- K[o]; cs <- COUNTY[o]
s  <- rle(ks)$lengths
gid <- rep(seq_along(s), s)
same_grp <- gid[-1] == gid[-length(gid)]
multi <- as.logical(tapply(c(FALSE, same_grp & (cs[-1] != cs[-length(cs)])), gid, any))
dup <- s > 1
# cross-county pairs within a group: total pairs minus same-county pairs
xpairs <- sum(as.numeric(tapply(cs, gid, function(z) {
  t <- table(z); (sum(t)^2 - sum(t^2)) / 2 })))

cc <- data.frame(
  quantity = c("colliding keys", "keys spanning more than one county",
               "keys confined to a single county",
               "registrants in a cross-county key",
               "colliding pairs", "pairs that are cross-county"),
  value = c(sum(dup), sum(multi & dup), sum(!multi & dup),
            sum(s[multi & dup]), sum(choose(s, 2)), xpairs))
dd_write_csv(cc, "derived/crosscounty.csv")
cat("\ncross-county:\n"); print(cc, row.names = FALSE)

gs <- as.data.frame(table(s[dup]), stringsAsFactors = FALSE)
names(gs) <- c("group_size", "n_keys")
gs$group_size <- as.integer(gs$group_size)
gs$registrants <- gs$group_size * gs$n_keys
dd_write_csv(gs, "derived/groupsize.csv")
rm(o, ks, cs, gid, same_grp, multi); invisible(gc())

# --- 4. How collisions grow with the size of the pool -----------------------
#
# The central mechanism, measured rather than modeled. Crosscheck did not run
# on a county or a state; in 2012 it handled more than 45 million registration
# records. Rather than extrapolate -- which would require assuming names and
# birth dates are distributed identically everywhere, and they are not -- this
# draws random subsamples of the real file at increasing sizes and counts the
# collisions that actually occur in each.
#
# The per-100,000 column is the one to read. If false matches were a fixed
# property of a rule, it would be flat. It is not: it rises roughly in
# proportion to the size of the pool being searched.

set.seed(84355)
fr <- c(.005, .01, .02, .05, .1, .2, .35, .5, .7, .85, 1)
cat("\nsubsampling for the scaling curve")
scaling <- do.call(rbind, lapply(fr, function(f) {
  n <- round(N * f); cat(".")
  p <- mean(replicate(if (f < 1) 3 else 1, npairs(sample(K, n))))
  data.frame(n = n, pairs = p, per100k = 1e5 * p / n)
}))
cat("\n")
sc2 <- scaling[scaling$pairs > 0, ]
slope <- unname(coef(lm(log(pairs) ~ log(n), sc2))[2])
dd_write_csv(scaling, "derived/scaling.csv")
print(scaling, row.names = FALSE)
cat(sprintf("log-log slope %.3f (2.0 would be exactly quadratic)\n", slope))

# --- 5. What is actually in the date-of-birth field -------------------------
#
# A match key is only as good as the field underneath it. Two things are true
# of this one at once, and the chapter needs both:
#
#   * January 1 is wildly over-represented -- an administrative default, the
#     same artifact that leads Goel et al. to restrict part of their analysis
#     to states where fewer than 10% of vote records carry a birthday on the
#     first of a month. A single sentinel date, 1800-01-01, is carried by
#     thousands of records.
#
#   * And yet, across all 366 month-days, the aggregate departure from
#     uniformity is tiny. Clustering can only ever raise the collision
#     probability, never lower it, but here it raises it by a fraction of a
#     percent. The coarseness of the key, not the shape of the birthday
#     distribution, is what governs the false-positive rate. Saying so
#     honestly is more useful than implying the spike does more work than it
#     does.

# MDAY stays parallel to DOB and YR so the two can be crossed further down;
# `md` is the same thing with the four empty dates dropped, for the aggregate
# clustering statistic. Keeping both is what stops a length-mismatch quietly
# recycling one vector against the other.
MDAY <- substr(DOB, 6, 10)
md <- MDAY[nzchar(MDAY)]
tmd <- table(md)
pmd <- as.numeric(tmd) / length(md)
mdd <- data.frame(monthday = names(tmd), n = as.numeric(tmd))
mdd <- mdd[order(mdd$monthday), ]
dd_write_csv(mdd, "derived/monthday.csv")

exp_uniform <- length(md) / length(tmd)
jan1 <- as.numeric(tmd[["01-01"]])
clust <- sum(pmd^2) * length(tmd)
sentinel_n <- sum(DOB == "1800-01-01")
pairs_nosent <- npairs(K[DOB != "1800-01-01"])

cat(sprintf("\nJan 1: %s records, %.2fx the uniform expectation of %s\n",
            format(jan1, big.mark = ","), jan1 / exp_uniform,
            format(round(exp_uniform), big.mark = ",")))
cat(sprintf("aggregate month-day clustering multiplier: %.4f\n", clust))
cat(sprintf("sentinel date 1800-01-01: %s records; removing them takes pairs %s -> %s\n",
            format(sentinel_n, big.mark = ","),
            format(kp(CCKEY), big.mark = ","), format(pairs_nosent, big.mark = ",")))

# --- 5b. The birth YEAR half of the same field ------------------------------
#
# The day-of-year artifact has a counterpart in the year, and the two turn out
# to be the same artifact rather than two. What the year field contains:
#
#   * ONE dominant placeholder. Not 1900, which is the usual suspect and is
#     nearly absent here, but 1800 -- and almost every record carrying it
#     carries 1800-01-01 exactly. This is the single most damaging value a
#     name-plus-DOB matcher can meet, because inside that group the date
#     contributes nothing and the key silently collapses to name alone.
#   * A thin tail of plainly corrupt years (three-digit years, 1652, 1776).
#   * An otherwise smooth age structure. The decade-boundary check below is
#     the test for the usual "round year" data-entry pattern, and it finds
#     nothing: the null here is a plausible age curve, not a flat line, so the
#     comparison is each decade year against the mean of its two neighbours.
#
# Ages are computed against 2026, the year this file was consulted.

YR <- as.integer(BYEAR)
NOW <- 2026
SENT_Y <- as.integer(names(sort(table(YR[YR < 1900]), decreasing = TRUE))[1])
SENT_D <- names(sort(table(DOB[YR == SENT_Y]), decreasing = TRUE))[1]
sent <- DOB == SENT_D
n_sent <- sum(sent)

# The strongest single comparison in the chapter: how often does the Crosscheck
# key collide INSIDE the placeholder group, against a same-sized group of
# records carrying a real date of birth? Twenty draws, so the comparison is not
# one lucky sample.
pairs_in_sent <- npairs(K[sent])
real_idx <- which(!sent & YR >= 1910 & YR <= 2008)
set.seed(84355)
pairs_in_real <- replicate(20, npairs(K[sample(real_idx, n_sent)]))

cat(sprintf("\nplaceholder date %s: %s records (%.2f%% of the file)\n",
            SENT_D, format(n_sent, big.mark = ","), 100 * n_sent / N))
cat(sprintf("  colliding pairs inside it: %d (%.1f%% of the statewide %s)\n",
            pairs_in_sent, 100 * pairs_in_sent / kp(CCKEY),
            format(kp(CCKEY), big.mark = ",")))
cat(sprintf("  same-sized draws of real dates: mean %.1f, max %d\n",
            mean(pairs_in_real), max(pairs_in_real)))

# Year counts over the plotted range, plus the junk summarised separately.
YLO <- 1900
yt <- table(YR[YR >= YLO & YR <= 2009])
yrs <- data.frame(year = as.integer(names(yt)), n = as.numeric(yt))
yrs <- yrs[order(yrs$year), ]
dd_write_csv(yrs, "derived/birthyears.csv")

impossible <- sum(YR < 1900 & YR != SENT_Y, na.rm = TRUE)
n_over110  <- sum(YR >= 1900 & (NOW - YR) > 110, na.rm = TRUE)
n_over100  <- sum(YR >= 1900 & (NOW - YR) > 100, na.rm = TRUE)
n_under18  <- sum((NOW - YR) < 18, na.rm = TRUE)

# Decade-boundary test against the mean of the two neighbouring years.
dec <- seq(1940, 2000, 10)
dr <- vapply(dec, function(y) {
  a <- yrs$n[yrs$year == y]
  b <- mean(c(yrs$n[yrs$year == y - 1], yrs$n[yrs$year == y + 1]))
  a / b }, numeric(1))
cat(sprintf("decade-boundary ratios %.2f to %.2f -- no rounding artifact\n",
            min(dr), max(dr)))

# How much of the 1 January spike is really the placeholder?
j_all <- sum(md == "01-01")
j_mod <- sum(MDAY == "01-01" & YR >= 1930, na.rm = TRUE)
n_mod <- sum(YR >= 1930, na.rm = TRUE)
stopifnot(length(MDAY) == length(YR), n_mod > 0)

# FIRST-OF-THE-MONTH, the quantity Goel et al. report at national scale.
# They found 14% of 2012 vote records in a commercially assembled national
# file carried a first-of-the-month birthday and deleted them all before
# estimating anything. Measuring the same quantity here is what lets the
# chapter compare a single state's own extract against that national file
# instead of implying the two are alike. They are not: New Jersey is far
# cleaner, which is itself the point, because a pooled program inherits the
# worst file in the pool rather than the best.
fom   <- substr(DOB, 9, 10) == "01" & nzchar(DOB)
n_fom <- sum(fom)
fom_expected <- 12 / 366
cat(sprintf("first-of-the-month: %s records, %.2f%% of the file (%.2fx the %.2f%% an even spread implies)\n",
            format(n_fom, big.mark = ","), 100 * n_fom / N,
            (n_fom / N) / fom_expected, 100 * fom_expected))
cat(sprintf("1 Jan: %s records overall (%.2fx uniform); among 1930+ births %.2fx\n",
            format(j_all, big.mark = ","), j_all / exp_uniform,
            (j_mod / n_mod) / (1 / 366)))

# --- 6. Georgia: the same rule where the public file is coarser -------------

ga <- read.csv(GAFILE, colClasses = "character")
NG <- nrow(ga)
stopifnot(NG == 127560, !any(duplicated(ga$Voter.Registration.Number)))
gL <- up(ga$Last.Name); gF <- up(ga$First.Name); gY <- trimws(ga$Birth.Year)
gMI <- substr(up(ga$Middle.Name), 1, 1)
stopifnot(all(nzchar(gL)), all(nzchar(gF)), all(nzchar(gY)))

garow <- function(label, key) {
  s <- grpsizes(key)
  data.frame(label = label, keys = length(s), dup_keys = sum(s > 1),
             flagged = sum(s[s > 1]), pct = 100 * sum(s[s > 1]) / NG,
             pairs = sum(choose(s, 2)), max_group = max(s))
}
gak <- rbind(
  garow("first + last name", paste(gF, gL, sep = "|")),
  garow("first + last name + birth year", paste(gF, gL, gY, sep = "|")),
  garow("first + last name + birth year + middle initial",
        paste(gF, gL, gY, gMI, sep = "|")))
dd_write_csv(gak, "derived/ga_keys.csv")
cat("\nGeorgia, Houston County (", format(NG, big.mark = ","), " records):\n", sep = "")
print(gak[, c("label", "dup_keys", "flagged", "pct", "pairs", "max_group")], row.names = FALSE)

# Georgia's birth-year distribution is genuinely non-uniform in a way the
# month-day distribution is not: it is an age structure, not a calendar.
tgy <- table(gY); pgy <- as.numeric(tgy) / NG
ga_clust <- sum(pgy^2) * length(tgy)

# --- 7. The exact birthday problem -----------------------------------------
#
# No data required. P(at least two of n people share a birth date), computed
# exactly rather than by the usual exponential approximation. The class this
# chapter was written for has 20 people in it.
#
# The range runs to 366 rather than to the plotted maximum for two reasons: the
# HTML figure lets a reader set the room size, so the curve has to be defined
# wherever the control can go, and 366 is the one point where the probability
# is genuinely 1. Everywhere below it the value is strictly less than 1, which
# is why the chapter's formatter prints ">99.9%" rather than "100%" there.
#
# `pmine` is the other question, the one people mistake this for: the chance
# that somebody in the room shares ONE NAMED person's birthday.

pshare <- function(n, d = 365) if (n > d) 1 else 1 - prod((d - seq_len(n) + 1) / d)
pmine  <- function(n, d = 365) 1 - (1 - 1 / d)^(max(n - 1, 0))
bd <- data.frame(n = 1:366)
bd$p     <- vapply(bd$n, pshare, numeric(1))
bd$pmine <- vapply(bd$n, pmine,  numeric(1))
bd$pairs <- choose(bd$n, 2)

# CERTAINTY IS A FACT ABOUT n, NOT ABOUT THE STORED DOUBLE. Mathematically the
# probability is below 1 for every n up to 365 and exactly 1 at 366, by the
# pigeonhole principle. A double cannot hold that: from n = 153 the complement
# underflows and `p` stores as exactly 1.0, some 213 people early. So the
# chapter tests this column, not the value, when deciding whether it is allowed
# to print "100%". Reading certainty off a rounded number is the mistake the
# whole chapter is about, and it would be a poor look to make it here.
bd$certain <- bd$n >= 366
stopifnot(bd$p[bd$n == 366] == 1,
          all(bd$p[bd$n <= 152] < 1),        # the range doubles still resolve
          all(diff(bd$p) >= 0))
cat("note: p saturates to 1.0 in double precision at n =",
    which(bd$p >= 1)[1], "although it is mathematically below 1 until 366\n")
dd_write_csv(bd, "derived/birthday.csv")
cat(sprintf("\nbirthday problem: n=20 -> %.4f, n=23 -> %.4f, even odds at n=%d\n",
            pshare(20), pshare(23), which(bd$p > .5)[1]))

# --- 8. Scalar facts, so the chapter types no numbers ----------------------
#
# The Crosscheck and double-voting figures were verified on 2026-08-11 against
# the sources named in the `source` column: the published APSR article for the
# citation and abstract, and the authors' full working paper for the section
# numbers, Table 1 and the sentences quoted. They are carried here as data so
# the prose cannot drift from them.

f <- function(k, v, s) data.frame(key = k, value = as.character(v), source = s)
facts <- rbind(
  f("nj_n", N, "NJ state voter file, 21 counties, consulted 2026-08-11"),
  f("nj_active", n_active, "same, status field begins 'Active'"),
  f("nj_counties", 21, "same"),
  f("nj_empty_dob", sum(!nzchar(DOB)), "same"),
  f("ga_n", NG, "GA Houston County extract, consulted 2026-08-11"),
  f("year_vs_dob", sprintf("%.0f", kp("first + last name + birth year") / kp(CCKEY)),
    "computed: birth-year pairs / full-DOB pairs, same records"),
  f("initial_vs_first",
    sprintf("%.0f", kp("first initial + last name + full date of birth") / kp(CCKEY)),
    "computed"),
  f("middle_cut", sprintf("%.0f",
    100 * (1 - kp("first + last name + full date of birth + middle initial") / kp(CCKEY))),
    "computed: percent of colliding pairs removed by adding a middle initial"),
  f("scaling_slope", sprintf("%.2f", slope), "computed, lm(log pairs ~ log n)"),
  # A like-for-like rate comparison needs a subsample big enough to have found
  # any collisions at all; the 5% draw is the smallest that has.
  f("rate_small_n", scaling$n[scaling$n == round(N * .05)], "computed, scaling.csv"),
  f("rate_small", sprintf("%.2f", scaling$per100k[scaling$n == round(N * .05)]),
    "computed, scaling.csv"),
  f("rate_full", sprintf("%.1f", scaling$per100k[scaling$n == N]), "computed, scaling.csv"),
  f("rate_growth", sprintf("%.0f", scaling$per100k[scaling$n == N] /
                                   scaling$per100k[scaling$n == round(N * .05)]),
    "computed: how many times higher the per-voter false-match rate is in the full state than in a 5% draw"),
  f("jan1_n", jan1, "computed"),
  f("jan1_mult", sprintf("%.2f", jan1 / exp_uniform), "computed"),
  f("md_clustering", sprintf("%.3f", clust), "computed"),
  f("ga_year_clustering", sprintf("%.2f", ga_clust), "computed"),
  f("sentinel_n", sentinel_n, "computed"),
  f("pairs_nosentinel", pairs_nosent, "computed"),
  # --- the birth-year half of the field ------------------------------------
  f("sent_date", SENT_D, "computed: most common date among pre-1900 years"),
  f("sent_year", SENT_Y, "computed"),
  # 1900 is the placeholder people expect; recording how rare it actually is
  # here is what lets the chapter say so rather than imply it.
  f("n_year_1900", sum(YR == 1900, na.rm = TRUE), "computed"),
  f("sent_n", n_sent, "computed"),
  f("sent_pct_records", sprintf("%.2f", 100 * n_sent / N), "computed"),
  f("sent_pairs", pairs_in_sent, "computed: colliding pairs inside the placeholder group"),
  f("sent_pct_pairs", sprintf("%.1f", 100 * pairs_in_sent / kp(CCKEY)), "computed"),
  f("sent_ratio", sprintf("%.0f", (100 * pairs_in_sent / kp(CCKEY)) / (100 * n_sent / N)),
    "computed: share of collisions divided by share of records"),
  f("real_draw_mean", sprintf("%.1f", mean(pairs_in_real)),
    "computed: mean colliding pairs in 20 same-sized draws of records with real dates"),
  f("real_draw_max", max(pairs_in_real), "computed: worst of those 20 draws"),
  f("impossible_years", impossible,
    "computed: records with a birth year before 1900 other than the placeholder"),
  f("n_over110", n_over110, "computed: implied age over 110, placeholder excluded"),
  f("n_over100", n_over100, "computed: implied age over 100, placeholder excluded"),
  f("n_under18", n_under18, "computed: implied age under 18 (NJ pre-registers 17-year-olds)"),
  f("dec_lo", sprintf("%.2f", min(dr)), "computed: decade-boundary ratio, lowest"),
  f("dec_hi", sprintf("%.2f", max(dr)), "computed: decade-boundary ratio, highest"),
  f("jan1_mult_modern", sprintf("%.2f", (j_mod / n_mod) / (1 / 366)),
    "computed: 1 January over-representation among births from 1930 on"),
  # --- first-of-the-month, measured here for comparison with the paper -------
  f("fom_n", n_fom, "computed"),
  f("fom_pct", sprintf("%.2f", 100 * n_fom / N), "computed"),
  f("fom_mult", sprintf("%.2f", (n_fom / N) / fom_expected), "computed"),
  f("fom_expected_pct", sprintf("%.2f", 100 * fom_expected), "computed: 12 of 366 dates"),

  # --- VERIFIED EXTERNAL FIGURES --------------------------------------------
  #
  # All page numbers below refer to the PUBLISHED article, checked against the
  # final PDF on 2026-08-11:
  #
  #   Goel, S., Meredith, M., Morse, M., Rothschild, D. and Shirani-Mehr, H.
  #   (2020). "One Person, One Vote: Estimating the Prevalence of Double Voting
  #   in U.S. Presidential Elections." American Political Science Review
  #   114(2): 456-469. doi:10.1017/S000305541900087X
  #
  # An earlier draft of this lab cited the authors' working paper for the
  # section numbers and Table 1. That was replaced: every figure below now
  # comes from the published article, and the Iowa attribution in particular
  # was re-checked there. It survives -- the published article states it
  # directly on p. 460 -- so the chapter keeps it and cites the page.
  f("cc_records_2012", "45 million",
    "Goel et al. 2020, p. 460: Crosscheck 'handled more than 45 million voter registration records and flagged more than a million'"),
  f("cc_flagged_2013", "1,395,074",
    "Goel et al. 2020, p. 468: in 2013 Crosscheck reported identifying 1,395,074 'potential duplicate voters' among the 15 states then participating"),
  f("cc_states_2013", "15", "same sentence, p. 468"),
  f("cc_iowa_2012", "100,140",
    "Goel et al. 2020, p. 460: 'We obtained the list of 100,140 and 139,333 pairings that Crosscheck provided to the Iowa Secretary of State before the 2012 and 2014 elections, respectively.'"),
  f("cc_dup_of_known", "25,987",
    "Goel et al. 2020, p. 468: of 34,900 pairings with SSN4 known for both records, 25,987 had the same SSN4"),
  f("cc_known_total", "34,900", "same sentence, p. 468"),
  f("cc_dup_voted_twice", "fewer than 10",
    "Goel et al. 2020, p. 457: 'Fewer than 10 of the roughly 26,000 known duplicate registrations we identified in the consortium data were used to cast two votes in 2012.'"),
  f("t1_dup_regs", "roughly 26,000", "same sentence, p. 457"),
  f("t1_earlier_only", "more than 2,500",
    "Goel et al. 2020, p. 457: 'we identified more than 2,500 cases in which only the registration record with an earlier registration date was used to vote in 2012'"),

  # THE DENOMINATOR. The abstract says VOTERS; p. 457 says "votes cast". The
  # paper uses both framings, and the derivation on p. 464 is explicitly per
  # voter: 32,890 double voters in a population of 129 million voters. The
  # chapter quotes the abstract's wording so the ambiguity cannot propagate.
  f("dbl_rate", "one in 4,000",
    "Goel et al. 2020, abstract: 'We estimate that about one in 4,000 voters cast two ballots'"),
  f("dbl_voters_n", "32,890",
    "Goel et al. 2020, p. 464: estimated double voters nationally in 2012 (s.e. 2,649)"),
  f("dbl_base", "129 million",
    "Goel et al. 2020, p. 464: votes cast in the 2012 presidential election, per FEC (2013)"),
  f("dbl_ratio", "approximately 300",
    "Goel et al. 2020, abstract: 'could impede approximately 300 legitimate votes for each double vote prevented'"),

  # The three distinctness measurements, whose spread is itself informative.
  f("distinct_national", "97",
    "Goel et al. 2020, p. 457: 'In the national voter file, we estimate that 97% of the votes cast with the same first name, last name, and DOB were cast by two distinct individuals.'"),
  f("distinct_crosscheck", "99.4",
    "Goel et al. 2020, p. 457: 'If we limit our focus to Crosscheck states, we estimate that fully 99.4% of votes cast with the same name and DOB were cast by distinct individuals.'"),
  f("distinct_consortium", "99.5",
    "Goel et al. 2020, p. 457: 'In the consortium data, where we can measure this statistic more directly, we estimate this quantity to be 99.5%.'"),

  f("t1_both_same_ssn", "7",
    "Goel et al. 2020, Table 1 (p. 466), 2012: pairings with MATCHING SSN4 where both registrations were used to vote"),
  f("t1_both_diff_ssn", "1,476",
    "Goel et al. 2020, Table 1 (p. 466), 2012: pairings with DIFFERENT SSN4 where both were used to vote"),
  f("cc_true_dup_share", "70-75",
    "Goel et al. 2020, p. 466: among pairings WITH KNOWN SSN4 FOR BOTH RECORDS, the share that are in fact double registrations"),

  # --- the placeholder finding, at national scale ----------------------------
  f("natl_fom_pct", "14",
    "Goel et al. 2020, p. 459: 'Across all years, we found an improbable 14% of 2012 vote records that were associated with a first-of-the-month birthday.'"),
  f("mcdonald_year", "2007",
    "Goel et al. 2020, p. 459, citing McDonald (2007) on placeholder dates of birth"),
  f("clerical_rate", "1.3%",
    "Goel et al. 2020, p. 457: 'a 1.3% clerical error rate would be sufficient to explain all of these apparent double votes'"),

  # --- the New Jersey precedent ---------------------------------------------
  f("ml_pairs", "884",
    "Goel et al. 2020, p. 457: 'McDonald and Levitt identify 884 pairs of vote records that have the same first name, last name, and DOB' (New Jersey, 2004)"),
  f("ml_ci", "300 to 500",
    "Goel et al. 2020, p. 457: McDonald and Levitt's 95% confidence interval for people voting twice in New Jersey in 2004"),
  f("ml_year", "2004", "same passage, p. 457"),
  f("ml_overestimate", "fewer than one-tenth",
    "Goel et al. 2020, p. 457: 'we estimate that the actual number of double votes is fewer than one-tenth of what their approach suggests'"),
  f("ml_uniform", "uniform",
    "Goel et al. 2020, p. 457: McDonald and Levitt drew birth years from the empirical age distribution while 'assuming that birthdays within years follow a uniform distribution'"),
  f("indiana_code", "Ind. Code Ann. 3-7-38.2-5(d)(2)",
    "Goel et al. 2020, pp. 457 and 466, citing also Com. Cause Indiana v. Lawson, 937 F.3d 944 (7th Cir. 2019)"))
dd_write_csv(facts, "derived/facts.csv")
cat("\nwrote facts.csv --", nrow(facts), "rows\n")

cat("\nNo name, date of birth, registration number or address is written into\n")
cat("this folder. Counts and distributions only.\n")
cat(sprintf("\ndone in %.1f minutes.\n",
            as.numeric(difftime(Sys.time(), t_start, units = "mins"))))

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
