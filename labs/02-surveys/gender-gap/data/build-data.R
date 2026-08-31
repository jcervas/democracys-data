# ---------------------------------------------------------------------------
# Build the gender-gap dataset: two series, and the one number people quote.
#
# THE OBJECT. "The gender gap" is among the most-cited numbers in American
# politics, and it is a SUBTRACTION. Somebody computes how Democratic women
# are, computes the same for men, and reports the difference. The difference
# is what gets printed. The two numbers behind it usually do not.
#
# This script writes both, every survey year from 1972 to 2024, so the chapter
# can ask which side actually moved.
#
# SOURCE. General Social Survey cumulative file, NORC at the University of
# Chicago, release 3a (2024).
#   https://gss.norc.org/us/en/gss/get-the-data.html
#   https://gss.norc.org/content/dam/gss/get-the-data/documents/stata/GSS_stata.zip
# A 47 MB zip holding a 570 MB Stata file, `gss7224_r3a.dta`, 75,699
# respondents and 6,943 variables. Keyless: no account, no form, no guestbook.
# The `survey-access` chapter measured that and found the GSS the most open
# archive it probed.
#
# WHY THE GSS RATHER THAN ANES OR THE CES. Both would answer this question and
# neither can be fetched by a script. ANES sits behind a registration page; the
# CES sits on Harvard Dataverse, which answers an automated request with a
# success code and an empty body. The GSS hands the file to anybody. That is
# the whole reason this chapter is built on it, and it is a fact about access
# rather than about quality.
#
# THE WEIGHT. `wtssps`, the same one the `gss-confidence` chapter uses and for
# the same reason: it is the only weight spanning every year in the file, and
# splicing two weights inside a fifty-year series would put a seam exactly
# where the argument is.
#
# WHAT IS AND IS NOT MEASURED. `sex` in the GSS is an interviewer-coded or
# self-reported two-box item. It is not gender identity. The file gained
# `sexnow1` in 2021, which asks a different question of a fraction of the
# sample; it is counted here and deliberately not used to build the series,
# because a variable that exists for three years cannot carry a fifty-year
# comparison. The chapter says so rather than letting the reader assume.
#
# PARTY IDENTIFICATION, NOT VOTE. `partyid` runs 0 (strong Democrat) to 6
# (strong Republican), with 7 for "other party". Leaners are counted with the
# party they lean toward, which is the standard treatment and is a decision:
# see this book's `party-id` chapter, where the same choice moves the most
# repeated number in American politics by a factor of five.
# ---------------------------------------------------------------------------

source("../../../_lib/precision.R")
suppressWarnings(try(source("../../../_lib/provenance.R"), silent = TRUE))
if (!exists("prov_fetch"))
  prov_fetch <- function(url, dest, ...) { download.file(url, dest, ...); invisible(dest) }

dir.create("raw",     showWarnings = FALSE)
dir.create("derived", showWarnings = FALSE)
say <- function(...) cat(sprintf(...), "\n")

FACTS <- list(); CHECKS <- list()
fact <- function(key, value, note) {
  FACTS[[key]] <<- list(value = value, note = note); invisible(value) }
check <- function(what, ok) {
  CHECKS[[length(CHECKS) + 1]] <<- list(check = what, passed = isTRUE(ok))
  if (!isTRUE(ok)) stop("FAILED: ", what)
  invisible(TRUE) }

if (!requireNamespace("haven", quietly = TRUE))
  stop("needs 'haven' to read a Stata 13+ file. install.packages('haven')")

# --- acquire ---------------------------------------------------------------
#
# The 570 MB source is not committed. What is committed is the handful of
# small tables below, so nobody has to download it to read the chapter.

URL <- paste0("https://gss.norc.org/content/dam/gss/get-the-data/",
              "documents/stata/GSS_stata.zip")
# Own copy first, then the sibling's. `gss-confidence` reads the same 570 MB
# file and looks here in turn, so the pair is symmetric: whichever chapter is
# built first pays for the download and the other borrows it. Neither owns it,
# and deleting either folder costs one re-download rather than breaking a build.
DTA <- c(Sys.glob("raw/*.dta"), Sys.glob("../../gss-confidence/data/raw/*.dta"))
if (!length(DTA)) {
  say("downloading the GSS cumulative file (47 MB zip, 570 MB unpacked)")
  zf <- tempfile(fileext = ".zip")
  prov_fetch(URL, zf, mode = "wb", quiet = TRUE)
  fs  <- unzip(zf, exdir = "raw", junkpaths = TRUE)
  DTA <- grep("\\.dta$", fs, value = TRUE)
}
check("the Stata file is where this script expects it", length(DTA) == 1)

KEEP <- c("year", "sex", "partyid", "polviews", "marital", "degree",
          "age", "wtssps", "sexnow1")
d <- haven::read_dta(DTA[1], col_select = KEEP)
d <- as.data.frame(lapply(d, as.numeric))

fact("src_rows", nrow(d), "respondents in the GSS cumulative file")
fact("first_year", min(d$year), "first survey year in it")
fact("last_year",  max(d$year), "and the most recent")
check("the whole cumulative file arrived, not one wave", nrow(d) > 70000)
check("every row carries the weight this chapter uses", !any(is.na(d$wtssps)))

# `sexnow1` is counted and then set aside. Counting it is the honest way to
# say how much of the file it covers.
fact("sexnow_answers", sum(!is.na(d$sexnow1)), "respondents ever asked the newer sex/gender item")
fact("sexnow_years", paste(range(d$year[!is.na(d$sexnow1)]), collapse = "-"),
     "the years in which it was asked at all")
check("the newer item covers a small minority of the file",
      sum(!is.na(d$sexnow1)) < 0.2 * nrow(d))

# --- the two series ---------------------------------------------------------

DEMS <- c(0, 1, 2)     # strong Democrat, not very strong, leans Democratic
REPS <- c(4, 5, 6)     # leans Republican, not very strong, strong Republican

u <- d[!is.na(d$sex) & !is.na(d$partyid) & d$partyid <= 6, ]
fact("n_used", nrow(u), "respondents with both a sex and a party identification")
fact("n_dropped_other", sum(!is.na(d$partyid) & d$partyid == 7),
     "who named another party and are not in either series")

net <- function(z) {
  w <- z$wtssps
  100 * (sum(w * (z$partyid %in% DEMS)) - sum(w * (z$partyid %in% REPS))) / sum(w)
}
yr <- sort(unique(u$year))
S <- do.call(rbind, lapply(yr, function(y) {
  s <- u[u$year == y, ]; m <- s[s$sex == 1, ]; f <- s[s$sex == 2, ]
  if (nrow(m) < 100 || nrow(f) < 100) return(NULL)
  data.frame(year = y,
             men   = round(net(m), 2), women = round(net(f), 2),
             n_men = nrow(m), n_women = nrow(f), stringsAsFactors = FALSE)
}))
S$gap <- round(S$women - S$men, 2)
dd_write_csv(S, "derived/partyid_by_sex.csv")

fact("n_years", nrow(S), "survey years with enough of both to report")
check("the series is continuous enough to draw", nrow(S) > 25)
check("no year rests on fewer than a hundred of either",
      min(c(S$n_men, S$n_women)) >= 100)

FY <- S[which.min(S$year), ]; LY <- S[which.max(S$year), ]
fact("men_first",  FY$men,   "men's net Democratic identification in the first year")
fact("women_first", FY$women, "women's, in the same year")
fact("gap_first",  FY$gap,   "the gap between them then")
fact("men_last",   LY$men,   "men's net Democratic identification in the most recent year")
fact("women_last", LY$women, "women's")
fact("gap_last",   LY$gap,   "and the gap now")
fact("men_move",   round(LY$men - FY$men, 2),   "how far men moved across the series")
fact("women_move", round(LY$women - FY$women, 2), "how far women moved")

# THE FINDING. Both series fall. The gap widens because one falls faster, not
# because the two moved in opposite directions -- which is what "the gender
# gap widened" is nearly always taken to mean.
check("both series move in the same direction across the series",
      sign(LY$men - FY$men) == sign(LY$women - FY$women))
check("the gap is wider at the end than at the start", LY$gap > FY$gap)
fact("faster_side", ifelse(abs(LY$men - FY$men) > abs(LY$women - FY$women), "men", "women"),
     "which side moved further")
fact("move_ratio", round(abs(LY$men - FY$men) / abs(LY$women - FY$women), 2),
     "how many times as far that side moved")

# --- by decade --------------------------------------------------------------

S$decade <- 10 * floor(S$year / 10)
D <- do.call(rbind, lapply(sort(unique(S$decade)), function(dd) {
  s <- S[S$decade == dd, ]
  data.frame(decade = paste0(dd, "s"), years = nrow(s),
             men = round(mean(s$men), 1), women = round(mean(s$women), 1),
             gap = round(mean(s$gap), 1), stringsAsFactors = FALSE)
}))
dd_write_csv(D, "derived/by_decade.csv")
fact("gap_1970s", D$gap[D$decade == "1970s"], "the average gap in the 1970s")
fact("gap_2020s", D$gap[D$decade == "2020s"], "and in the 2020s")
check("the 1970s gap is small and the 2020s gap is not",
      D$gap[D$decade == "1970s"] < 5 && D$gap[D$decade == "2020s"] > 10)

MAXY <- S$year[which.max(S$gap)]
fact("gap_max", max(S$gap), "the widest gap in any single year")
fact("gap_max_year", MAXY, "the year it happened")

# --- the marriage cut -------------------------------------------------------
#
# The second-most-quoted version of this number splits women by marital
# status. It is included because it is quoted, and because it shows the same
# structural point one level down.

m <- u[!is.na(u$marital), ]
m$married <- m$marital == 1
M <- do.call(rbind, lapply(sort(unique(m$year)), function(y) {
  s <- m[m$year == y, ]
  cell <- function(sx, mar) { z <- s[s$sex == sx & s$married == mar, ]
    if (nrow(z) < 75) return(NA_real_); round(net(z), 2) }
  data.frame(year = y,
             married_women   = cell(2, TRUE),  unmarried_women = cell(2, FALSE),
             married_men     = cell(1, TRUE),  unmarried_men   = cell(1, FALSE),
             stringsAsFactors = FALSE)
}))
M <- M[complete.cases(M), ]
dd_write_csv(M, "derived/partyid_by_sex_marital.csv")
fact("marital_years", nrow(M), "years where all four marital cells clear the size floor")

LM <- M[which.max(M$year), ]
fact("unmarried_women_last", LM$unmarried_women, "unmarried women's net Democratic identification, most recent year")
fact("married_women_last",   LM$married_women,   "married women's")
fact("marriage_gap_last", round(LM$unmarried_women - LM$married_women, 2),
     "the gap between them")
fact("sex_gap_same_year", round(S$gap[S$year == LM$year], 2),
     "the gap between women and men in that same year")
check("the marriage split among women is not smaller than the sex gap that year",
      (LM$unmarried_women - LM$married_women) > S$gap[S$year == LM$year])

# ---------------------------------------------------------------------------

facts <- data.frame(key = names(FACTS),
                    value = vapply(FACTS, function(x) as.character(x$value), ""),
                    note  = vapply(FACTS, function(x) x$note, ""))
dd_write_csv(facts, "derived/facts.csv")
dd_write_csv(do.call(rbind, lapply(CHECKS, as.data.frame)), "derived/checks.csv")
say("done: %d facts, %d checks, %d years", nrow(facts), length(CHECKS), nrow(S))

try(prov_stamp(), silent = TRUE)
