# ---------------------------------------------------------------------------
# Build the careers dataset: how long a congressional career lasts, with the
# unfinished ones handled rather than dropped.
#
# Six files end up in derived/:
#
#   derived/careers.csv   one row per member per chamber: how many Congresses
#                         they served and whether the career has ended
#   derived/cohorts.csv   entry cohort against how much of it is unfinished
#   derived/km.csv        Kaplan-Meier survival, overall and by stratum
#   derived/naive.csv     the three wrong answers, computed so they can be shown
#   derived/timeline.csv  one entering class, career by career, for the figure
#   derived/facts.csv     single numbers the brief quotes
#
# Run this script from inside the data/ folder.
# ---------------------------------------------------------------------------

dir.create("derived", showWarnings = FALSE)
options(scipen = 999, stringsAsFactors = FALSE)

# --- Source -----------------------------------------------------------------
#
# Voteview's congressional membership file, one row per member per Congress,
# 1st through 119th:
#   https://voteview.com/  (HSall_members.csv)
#
# The copy read here is the one the retirements chapter already downloaded and
# committed, at ../../retirements/data/raw/HSall_members.csv. The same file is
# described column by column in the dw-nominate and officeholder-age chapters.

SRC <- "../../retirements/data/raw/HSall_members.csv"
stopifnot(file.exists(SRC))
m <- read.csv(SRC, stringsAsFactors = FALSE)
m <- m[m$chamber %in% c("House", "Senate"), ]
LASTC <- max(m$congress)

# --- What counts as one career ----------------------------------------------
#
# TWO DECISIONS, BOTH ARGUABLE, BOTH STATED.
#
# 1. A career is a member IN ONE CHAMBER. Somebody who serves in the House and
#    then the Senate produces two careers here, not one. The alternative -- one
#    career per person, spanning chambers -- answers a different question
#    ("how long do people stay in Congress") and would make the House and
#    Senate curves impossible to compare, because every senator who came up
#    from the House would carry their House years into the Senate figure.
#
# 2. Tenure is the NUMBER OF CONGRESSES SERVED, not the span from first to
#    last. Some members leave and come back; counting the span would credit
#    them with the years they were out of office. The number of careers this
#    affects is written to facts.csv rather than assumed to be small.

m$key <- paste(m$icpsr, m$chamber)
sp <- split(m$congress, m$key)

careers <- data.frame(
  key      = names(sp),
  icpsr    = as.integer(sub(" .*$", "", names(sp))),
  chamber  = sub("^.* ", "", names(sp)),
  first    = vapply(sp, min, numeric(1)),
  last     = vapply(sp, max, numeric(1)),
  tenure   = lengths(sp),
  stringsAsFactors = FALSE)
careers$span    <- careers$last - careers$first + 1
careers$gap     <- careers$span != careers$tenure
# A career is UNFINISHED if the member was still there in the last Congress the
# file covers. That is the whole of the censoring: not missing data, but a
# career the record has not seen the end of.
careers$ongoing <- careers$last == LASTC
careers$ended   <- !careers$ongoing

nm <- m[!duplicated(m$key), c("key", "bioname", "party_code", "state_abbrev")]
careers <- merge(careers, nm, by = "key")
careers$party <- ifelse(careers$party_code == 100, "Democrat",
                 ifelse(careers$party_code == 200, "Republican", "Other"))
careers <- careers[order(careers$first, careers$icpsr), ]
write.csv(careers[, c("icpsr", "bioname", "chamber", "party", "state_abbrev",
                      "first", "last", "tenure", "span", "gap", "ongoing")],
          "derived/careers.csv", row.names = FALSE)
cat("careers.csv ->", nrow(careers), "careers,",
    sum(careers$ongoing), "still running\n")

# --- Censoring is not evenly spread -----------------------------------------
#
# Over two centuries only a few per cent of careers are unfinished. Inside a
# recent entering class, half of them are. The table is the argument.

brk <- c(seq(1, 113, 8), 120)
careers$cohort <- cut(careers$first, breaks = brk, right = FALSE, dig.lab = 4)
co <- do.call(rbind, lapply(split(careers, careers$cohort), function(z) {
  if (!nrow(z)) return(NULL)
  data.frame(
    cohort_lo = min(z$first), cohort_hi = max(z$first),
    n = nrow(z), ongoing = sum(z$ongoing),
    pct_ongoing = round(100 * mean(z$ongoing), 1),
    mean_ended = round(mean(z$tenure[z$ended]), 2),
    stringsAsFactors = FALSE)
}))
co <- co[!is.na(co$cohort_lo), ]
write.csv(co, "derived/cohorts.csv", row.names = FALSE)

# --- Kaplan-Meier, written out ----------------------------------------------
#
# The estimator is four lines of arithmetic and it is worth seeing rather than
# importing. At each length t where at least one career ENDED:
#
#   n(t) = careers still running at t   (tenure >= t)
#   d(t) = careers that ended exactly at t
#   S(t) = S(t-1) * (1 - d(t)/n(t))
#
# An unfinished career contributes to n(t) for every t up to where the record
# stops, and never to d(t). That is the whole trick: it is counted as evidence
# for as long as it is evidence, and not counted as an ending it never had.

km <- function(time, event) {
  ts <- sort(unique(time[event]))
  n  <- vapply(ts, function(t) sum(time >= t), numeric(1))
  d  <- vapply(ts, function(t) sum(time == t & event), numeric(1))
  data.frame(t = ts, at_risk = n, ended = d,
             surv = cumprod(1 - d / n), stringsAsFactors = FALSE)
}

# CHECKED AGAINST THE REFERENCE IMPLEMENTATION. If the hand-rolled estimator
# above ever stops agreeing with survival::survfit, this build fails rather
# than publishing a curve nobody verified.
if (requireNamespace("survival", quietly = TRUE)) {
  mine <- km(careers$tenure, careers$ended)
  ref  <- survival::survfit(
            survival::Surv(careers$tenure, careers$ended) ~ 1)
  rs <- ref$surv[match(mine$t, ref$time)]
  stopifnot(max(abs(mine$surv - rs)) < 1e-12)
  cat("Kaplan-Meier agrees with survival::survfit to 1e-12\n")
} else {
  cat("NOTE: survival not installed; hand-rolled estimator not cross-checked\n")
}

strata <- list()
strata[["All"]] <- careers
for (cc in c("House", "Senate")) strata[[cc]] <- careers[careers$chamber == cc, ]
eras <- list("1st-49th" = c(1, 49), "50th-79th" = c(50, 79),
             "80th-103rd" = c(80, 103), "104th-119th" = c(104, 119))
for (e in names(eras)) {
  r <- eras[[e]]
  strata[[e]] <- careers[careers$first >= r[1] & careers$first <= r[2], ]
}
kmall <- do.call(rbind, lapply(names(strata), function(s) {
  z <- strata[[s]]
  k <- km(z$tenure, z$ended)
  k$stratum <- s
  k$n_total <- nrow(z)
  k$n_ongoing <- sum(z$ongoing)
  k
}))
kmall$surv <- round(kmall$surv, 6)
write.csv(kmall[, c("stratum", "t", "at_risk", "ended", "surv",
                    "n_total", "n_ongoing")],
          "derived/km.csv", row.names = FALSE)

# median survival: the smallest t where S(t) <= 0.5
km_median <- function(k) {
  i <- which(k$surv <= 0.5)
  if (!length(i)) return(NA_real_)
  k$t[min(i)]
}

# --- The three answers you get without it -----------------------------------
#
# All three are computed on the modern window, where the censoring actually
# bites, so the brief can put them beside the Kaplan-Meier figure.

MOD <- careers[careers$first >= 104, ]
kmod <- km(MOD$tenure, MOD$ended)
naive <- data.frame(
  method = c("Count every career, finished or not",
             "Drop the unfinished careers",
             "Kaplan-Meier"),
  what_it_assumes = c(
    "that a career still running has already ended",
    "that the careers still running are like the ones that ended",
    "that a career still running is evidence up to today and no further"),
  median = c(median(MOD$tenure), median(MOD$tenure[MOD$ended]),
             km_median(kmod)),
  mean_or_na = c(round(mean(MOD$tenure), 2),
                 round(mean(MOD$tenure[MOD$ended]), 2), NA),
  stringsAsFactors = FALSE)
write.csv(naive, "derived/naive.csv", row.names = FALSE)

# --- One entering class, career by career -----------------------------------
#
# The figure that makes censoring visible. Everyone who first entered the House
# in the 111th Congress, one bar each, ordered by length.

ENTRY <- 111
tl <- careers[careers$first == ENTRY & careers$chamber == "House", ]
tl <- tl[order(tl$tenure, tl$bioname), ]
tl$row <- seq_len(nrow(tl))
write.csv(tl[, c("row", "bioname", "party", "state_abbrev", "first", "last",
                 "tenure", "ongoing")],
          "derived/timeline.csv", row.names = FALSE)

# --- Facts ------------------------------------------------------------------

kall <- km(careers$tenure, careers$ended)
khou <- km(strata$House$tenure, strata$House$ended)
ksen <- km(strata$Senate$tenure, strata$Senate$ended)
kmodern <- km(strata[["104th-119th"]]$tenure, strata[["104th-119th"]]$ended)
kold    <- km(strata[["50th-79th"]]$tenure,   strata[["50th-79th"]]$ended)

facts <- data.frame(
  key = c("last_congress", "careers", "ongoing", "pct_ongoing", "members",
          "gaps", "median_km", "median_house", "median_senate",
          "median_modern_km", "median_old_km",
          "mod_n", "mod_ongoing", "mod_pct", "mod_median_all",
          "mod_median_ended", "entry_congress", "entry_n", "entry_ongoing",
          "recent_cohort_lo", "recent_cohort_pct", "recent_cohort_mean_ended",
          "max_tenure", "max_name"),
  value = c(LASTC, nrow(careers), sum(careers$ongoing),
            round(100 * mean(careers$ongoing), 1),
            length(unique(careers$icpsr)), sum(careers$gap),
            km_median(kall), km_median(khou), km_median(ksen),
            km_median(kmodern), km_median(kold),
            nrow(MOD), sum(MOD$ongoing), round(100 * mean(MOD$ongoing), 1),
            median(MOD$tenure), median(MOD$tenure[MOD$ended]),
            ENTRY, nrow(tl), sum(tl$ongoing),
            co$cohort_lo[nrow(co) - 1], co$pct_ongoing[nrow(co) - 1],
            co$mean_ended[nrow(co) - 1],
            max(careers$tenure),
            careers$bioname[which.max(careers$tenure)]),
  stringsAsFactors = FALSE)
write.csv(facts, "derived/facts.csv", row.names = FALSE)

cat("\nmedian career, Kaplan-Meier:", km_median(kall), "Congresses\n")
cat("  House", km_median(khou), " Senate", km_median(ksen), "\n")
cat("modern window (104th+):", nrow(MOD), "careers,", sum(MOD$ongoing),
    "unfinished\n")
cat("  median if you count them as ended :", median(MOD$tenure), "\n")
cat("  median if you drop them           :", median(MOD$tenure[MOD$ended]), "\n")
cat("  median, Kaplan-Meier              :", km_median(kmod), "\n")
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
