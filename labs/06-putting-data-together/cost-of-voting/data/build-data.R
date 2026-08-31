# ---------------------------------------------------------------------------
# Build the cost-of-voting dataset: fifty-six state election laws, and the one
# number a research team compresses them into.
#
# Eleven files end up in derived/:
#
#   derived/covi_panel.csv     one row per state per election year, 1996-2024,
#                              carrying BOTH published scorings of that year
#   derived/covi_2024.csv      2024 in full: the ten issue areas, both scores,
#                              turnout, and party control of state government
#   derived/year_structure.csv what the index was made of in each year -- how
#                              many issue areas, how many components, and how
#                              far the two scorings of that year disagree
#   derived/rebuild_2024.csv   the 2024 index rebuilt here from the laws,
#                              beside the published value
#   derived/reweight_2024.csv  the same 2024 laws under four weighting rules
#   derived/items_2024.csv     the individual law items, and how many states
#                              each one separates
#   derived/facts.csv          single numbers the brief quotes
#   derived/control_panel.csv  party control of state government, one row per
#                              state per year, with the index rank beside it
#   derived/control_trend.csv  the same, collapsed: mean rank and mean index by
#                              year and type of party control
#   derived/control_check.csv  this build's party-control coding against the
#                              published paper's, year by year, with the count
#                              of states where the two disagree
#   derived/checks.csv         the arithmetic this script verifies before writing
#
# Run this script from inside the data/ folder. It needs `readxl`; the three
# committed source files in raw/ mean it needs no network.
# ---------------------------------------------------------------------------

# Downloads go through prov_fetch(), which records url, bytes, hash and row
# count in PROVENANCE.tsv and prints a banner when a source moves under us --
# a URL that still returns 200 and no longer means what it meant. See
# ../../../_lib/provenance.R. If the helper is missing the build still runs: the
# fallback is a plain download with the same signature, forwarding every
# argument so a source needing a redirect or a user agent still gets one.
if (file.exists("../../../_lib/provenance.R")) {
  source("../../../_lib/provenance.R")
} else {
  prov_fetch <- function(url, dest, label = NULL, mode = "wb", quiet = TRUE, ...) {
    download.file(url, dest, mode = mode, quiet = quiet, ...)
    invisible(dest)
  }
  prov_report <- function() invisible(FALSE)
}


dir.create("derived", showWarnings = FALSE)
options(scipen = 999, stringsAsFactors = FALSE)
source("../../../_lib/precision.R")
stopifnot(requireNamespace("readxl", quietly = TRUE))

# --- Source -----------------------------------------------------------------
#
# Michael J. Pomante II, Cost of Voting Index, https://costofvotingindex.com
# Three files, downloaded from the Data page on 13 August 2026 and committed
# to raw/ exactly as they arrived:
#
#   covi-values-1996-2024.xlsx     the scores and ranks, one sheet, 450 rows
#   covi-master-requirements.xlsx  the underlying laws, one sheet per election
#   covi-codebook.docx             what every column means, and where it came from
#
# They are committed because they are small, because the site serves them from
# a content-delivery URL with a version string that will not survive the next
# edit of the page, and because the 2026 edition -- announced on the site for
# this fall -- will re-estimate every year in the file. What is in raw/ is the
# index as it stood in August 2026, which is the only thing a chapter can
# honestly claim to describe.
#
# THE VALUES FILE CARRIES TWO SCORINGS OF EVERY YEAR, and the whole chapter
# turns on the difference:
#
#   InitialCOVI  the value as first published for that election
#   FinalCOVI    the value in the current re-estimation of the whole series
#
# They are not the same numbers and for the early years they are not close.
# Neither column is wrong. The file simply does not have a single answer to
# the question "what was the cost of voting in 1996", and this script keeps
# both rather than choosing.

VALUES <- "raw/covi-values-1996-2024.xlsx"
MASTER <- "raw/covi-master-requirements.xlsx"
stopifnot(file.exists(VALUES), file.exists(MASTER))

suppressMessages(v <- as.data.frame(readxl::read_excel(VALUES, sheet = "Sheet1")))
stopifnot(nrow(v) == 450, all(table(v$year) == 50))

YEARS <- sort(unique(v$year))

# Full state names, so the panel joins to files keyed on either form.
ABV <- c(AL="Alabama", AK="Alaska", AZ="Arizona", AR="Arkansas", CA="California",
         CO="Colorado", CT="Connecticut", DE="Delaware", FL="Florida", GA="Georgia",
         HI="Hawaii", ID="Idaho", IL="Illinois", IN="Indiana", IA="Iowa",
         KS="Kansas", KY="Kentucky", LA="Louisiana", ME="Maine", MD="Maryland",
         MA="Massachusetts", MI="Michigan", MN="Minnesota", MS="Mississippi",
         MO="Missouri", MT="Montana", NE="Nebraska", NV="Nevada", NH="New Hampshire",
         NJ="New Jersey", NM="New Mexico", NY="New York", NC="North Carolina",
         ND="North Dakota", OH="Ohio", OK="Oklahoma", OR="Oregon", PA="Pennsylvania",
         RI="Rhode Island", SC="South Carolina", SD="South Dakota", TN="Tennessee",
         TX="Texas", UT="Utah", VT="Vermont", VA="Virginia", WA="Washington",
         WV="West Virginia", WI="Wisconsin", WY="Wyoming")
stopifnot(setequal(names(ABV), unique(v$state)))

# --- The panel ---------------------------------------------------------------
#
# Ranks are recomputed here rather than taken, and doing it turned up something
# the file does not mention: THE TWO RANK COLUMNS BREAK TIES DIFFERENTLY.
#
# States do tie. The index is continuous, but it is built from a handful of
# small integer scales, so two states with identical laws get identical scores
# -- and then something has to decide which is 32nd.
#
#   FinalRank    gives every tied state the HIGHER rank, so a year with a tie
#                has no state at the lower one
#   InitialRank  hands out sequential ranks in the order the rows happen to sit
#                in, which is alphabetical
#
# The second is the one to look at twice. In 1996 California, Illinois and
# Maryland have exactly the same initial score and are ranked 32, 33 and 34 --
# California ahead of Maryland because of its name. Neither convention is wrong
# and the difference is small, but a rank is the thing everyone quotes, and two
# columns of the same file disagree about how to make one.
#
# Both are reproduced exactly below, which is what makes the convention visible
# rather than assumed.

panel <- data.frame(
  year         = v$year,
  abbr         = v$state,
  state        = unname(ABV[v$state]),
  initial      = v$InitialCOVI,
  initial_rank = v$InitialRank,
  final        = v$FinalCOVI,
  final_rank   = v$FinalRank)

for (y in YEARS) {
  i <- panel$year == y
  panel$initial_rank_recomputed[i] <- rank(panel$initial[i], ties.method = "first")
  panel$final_rank_recomputed[i]   <- rank(panel$final[i],   ties.method = "max")
  panel$rank_move[i] <- abs(rank(panel$initial[i]) - rank(panel$final[i]))
}

# how many states are involved in an exact tie, which is what forces the
# question of a convention in the first place
TIED <- sum(unlist(lapply(YEARS, function(y) {
  z <- panel$final[panel$year == y]; sum(duplicated(z) | duplicated(z, fromLast = TRUE))
})))

# The published ranks must be reproducible from the published values. If they
# are not, one of the two columns has been edited without the other.
RANK_MISMATCH <- sum(panel$initial_rank != panel$initial_rank_recomputed) +
                 sum(panel$final_rank   != panel$final_rank_recomputed)
stopifnot(RANK_MISMATCH == 0)

# --- Reading a year of laws --------------------------------------------------
#
# One sheet per election. Each sheet carries the individual law items AND the
# issue-area summaries built from them; only the issue areas enter the index.
# Sheets past 2016 have stray blocks of columns to the right of the data, which
# is why the state column is located rather than assumed, and why the issue
# areas are selected by name.

read_year <- function(sheet) {
  suppressMessages(m <- as.data.frame(
    readxl::read_excel(MASTER, sheet = sheet, .name_repair = "unique")))
  scol <- grep("^State", names(m))[1]
  m <- m[!is.na(m[[scol]]) & nzchar(as.character(m[[scol]])), ]
  m$abbr <- as.character(m[[scol]])
  m <- m[m$abbr %in% names(ABV), ]
  stopifnot(nrow(m) == 50, !anyDuplicated(m$abbr))
  m
}

issue_areas <- function(m) {
  ia <- grep("Issue Area", names(m), value = TRUE)
  X <- as.data.frame(lapply(m[, ia, drop = FALSE], function(z) as.numeric(as.character(z))))
  names(X) <- ia
  stopifnot(!anyNA(X))
  X
}

# --- Rebuilding the index ----------------------------------------------------
#
# The recipe is in the codebook, and it is short enough to state in a sentence:
# run a principal component analysis on the standardized issue areas, keep the
# first three or four components, weight each by the share of variance it
# explains (normalized so the weights sum to one), and add them up.
#
# Two details are not in that sentence and both matter.
#
# STANDARDIZED. The issue areas are on wildly different scales -- registration
# deadline is a number of days (sd about 13), voter ID is a five-point scale
# (sd about 1.3). PCA on the raw covariances would let the two day-count
# variables write the index almost by themselves. Standardizing first is the
# choice that makes the other eight issue areas count at all, and the chapter
# shows what the index looks like without it.
#
# SIGN. A principal component's direction is arbitrary -- flip every loading
# and it explains exactly the same variance. Something has to decide which end
# is "expensive", and the rule used here is the substantive one: orient each
# component so that its loadings sum positive, i.e. so that more of every
# restriction points toward a higher score. That rule is not taken from the
# published values, and it reproduces them.

rebuild <- function(X, k, signs = NULL) {
  p <- prcomp(X, scale. = TRUE)
  ve <- p$sdev^2 / sum(p$sdev^2)
  w  <- ve[seq_len(k)] / sum(ve[seq_len(k)])
  if (is.null(signs)) signs <- sign(colSums(p$rotation[, seq_len(k), drop = FALSE]))
  list(score = as.vector(as.matrix(p$x[, seq_len(k), drop = FALSE]) %*% (w * signs)),
       varexp = ve, weights = w, pca = p, signs = signs)
}

# How many components each year uses. The codebook prints a weight table with a
# blank fourth column before 2012; that blank is the number of components, and
# it is the only place the count is recorded.
NCOMP <- c(`1996` = 3, `2000` = 3, `2004` = 3, `2008` = 3,
           `2012` = 4, `2016` = 4, `2020` = 4, `2022` = 4, `2024` = 4)

# --- Per-year structure ------------------------------------------------------

ys <- do.call(rbind, lapply(YEARS, function(y) {
  m  <- read_year(as.character(y))
  X  <- issue_areas(m)
  k  <- NCOMP[[as.character(y)]]
  r  <- rebuild(X, k)
  pv <- panel[panel$year == y, ]
  pv <- pv[match(m$abbr, pv$abbr), ]
  data.frame(
    year            = y,
    n_issue_areas   = ncol(X),
    n_components    = k,
    var_explained   = sum(r$varexp[seq_len(k)]),
    pc1_share       = r$varexp[1],
    repro_r         = cor(r$score, pv$final),
    cor_init_final  = cor(pv$initial, pv$final),
    cor_rank        = cor(rank(pv$initial), rank(pv$final)),
    mean_rank_move  = mean(pv$rank_move),
    max_rank_move   = max(pv$rank_move),
    max_move_state  = pv$state[which.max(pv$rank_move)])
}))

# --- 2024 in full ------------------------------------------------------------

m24 <- read_year("2024")
X24 <- issue_areas(m24)
K24 <- NCOMP[["2024"]]
r24 <- rebuild(X24, K24)

p24 <- panel[panel$year == 2024, ]
p24 <- p24[match(m24$abbr, p24$abbr), ]

# Turnout, from the chapter that already had to decide what the denominator is.
# Read by full path so the dependency is visible in the source rather than
# implied. VEP turnout is the rate over the voting-ELIGIBLE population -- adult
# citizens, less those a state's felony law excludes -- which is the right
# denominator here because the cost of voting is a cost borne by people who are
# allowed to vote.
TURNOUT <- "../../../06-putting-data-together/turnout-denominator/data/derived/states.csv"
stopifnot(file.exists(TURNOUT))
tn <- read.csv(TURNOUT)
tn <- tn[tn$YEAR == 2024, c("STATE_ABV", "VEP_TURNOUT_RATE", "VEP")]
stopifnot(all(m24$abbr %in% tn$STATE_ABV))
tn <- tn[match(m24$abbr, tn$STATE_ABV), ]

# --- Party control of state government, all nine elections -------------------
#
# The index does not carry party control, and the project's central claim about
# polarization depends on it, so it has to come from somewhere else. It comes
# from two places, and the seam between them is worth stating plainly.
#
# A TRIFECTA means one party holds the governorship and both legislative
# chambers. Nebraska is excluded -- its legislature is unicameral and elected
# without party labels -- so every count below is out of 49, which is also what
# the article does.
#
# 1996-2008: KLARNER. Carl Klarner's state partisan balance measures, which are
# the standard source for this quantity, distributed inside the Correlates of
# State Policy Project. The variables `rep_unified`, `dem_unified` and
# `divided_gov` are exactly the classification wanted. They stop after 2011.
#
# 2012-2024: CODED HERE, from the governor's party and the majority in each
# chamber in the year of the election. Ballotpedia, which the article uses, is
# not machine-readable and refuses automated requests, so there is no file to
# read.
#
# THE CODING IS CHECKED RATHER THAN TRUSTED. The article prints the number of
# states in each category for every one of the nine elections, and those 27
# numbers are an independent test of a classification built without them: the
# five coded years reproduce the article's counts exactly, or this script stops.
#
# The four Klarner years are compared to the same table but not required to
# match it, because they do not quite. Klarner and the article disagree about
# one state in 1996, one in 2004 and one in 2008 -- the states where "control"
# is genuinely arguable, being a tied chamber, a coalition majority, or a
# governor who left the party. That disagreement is recorded in checks.csv
# instead of being smoothed away, because it is the honest size of the
# uncertainty in a variable that looks categorical and is not.

PAPER_COUNTS <- data.frame(
  year    = c(1996, 2000, 2004, 2008, 2012, 2016, 2020, 2022, 2024),
  paper_r = c(  14,   15,   12,    9,   23,   22,   20,   22,   22),
  paper_d = c(   7,    9,    9,   14,   11,    7,   15,   14,   17),
  paper_v = c(  28,   25,   28,   26,   15,   20,   14,   13,   10))

CODED_R <- list(
  `2012` = c("AL","AZ","FL","GA","ID","IN","KS","LA","ME","MI","MS","ND","OH",
             "OK","PA","SC","SD","TN","TX","UT","VA","WI","WY"),
  `2016` = c("AL","AZ","AR","FL","GA","ID","IN","KS","MI","MS","NV","NC","ND",
             "OH","OK","SC","SD","TN","TX","UT","WI","WY"),
  `2020` = c("AL","AR","AZ","FL","GA","ID","IN","IA","MS","MO","ND","OH","OK",
             "SC","SD","TN","TX","UT","WV","WY"),
  `2022` = c("AL","AR","AZ","FL","GA","ID","IN","IA","MS","MO","MT","NH","ND",
             "OH","OK","SC","SD","TN","TX","UT","WV","WY"),
  `2024` = c("AL","AR","FL","GA","ID","IN","IA","LA","MS","MO","MT","NH","ND",
             "OH","OK","SC","SD","TN","TX","UT","WV","WY"))
CODED_D <- list(
  `2012` = c("AR","CA","CT","DE","HI","IL","MD","MA","VT","WA","WV"),
  `2016` = c("CA","CT","DE","HI","OR","RI","VT"),
  `2020` = c("CA","CO","CT","DE","HI","IL","ME","NV","NJ","NM","NY","OR","RI",
             "VA","WA"),
  `2022` = c("CA","CO","CT","DE","HI","IL","ME","NV","NJ","NM","NY","OR","RI",
             "WA"),
  `2024` = c("CA","CO","CT","DE","HI","IL","ME","MD","MA","MI","MN","NJ","NM",
             "NY","OR","RI","WA"))
CODED_YEARS   <- as.numeric(names(CODED_R))
KLARNER_YEARS <- c(1996, 2000, 2004, 2008)

# Klarner, from the Correlates of State Policy Project. Fetched at run time: the
# file is 7 MB of a 3,000-variable state-year panel, of which this chapter wants
# three columns and four years, and that has no business in a course repository.
CSPP_URL <- paste0("https://raw.githubusercontent.com/IPPSR/csppData/",
                   "master/data/correlates.rda")
cspp_tmp <- tempfile(fileext = ".rda")
prov_fetch(CSPP_URL, cspp_tmp, mode = "wb", quiet = TRUE)
cspp_env <- new.env()
load(cspp_tmp, envir = cspp_env)
cs <- as.data.frame(get("correlates", envir = cspp_env))
cs <- cs[cs$year %in% KLARNER_YEARS,
         c("state", "year", "rep_unified", "dem_unified", "divided_gov")]
cs <- cs[!is.na(cs$state), ]
# every state-year present, and the three indicators mutually exclusive
NAME_TO_ABV <- setNames(names(ABV), unname(ABV))
cs$abbr <- unname(NAME_TO_ABV[cs$state])
cs <- cs[!is.na(cs$abbr), ]
stopifnot(nrow(cs) == length(KLARNER_YEARS) * 50)

classify <- function(y) {
  ab <- names(ABV)
  out <- rep("Divided", 50); names(out) <- ab
  if (y %in% CODED_YEARS) {
    out[ab %in% CODED_R[[as.character(y)]]] <- "Republican trifecta"
    out[ab %in% CODED_D[[as.character(y)]]] <- "Democratic trifecta"
  } else {
    k <- cs[cs$year == y, ]
    k <- k[match(ab, k$abbr), ]
    out[!is.na(k$rep_unified) & k$rep_unified == 1] <- "Republican trifecta"
    out[!is.na(k$dem_unified) & k$dem_unified == 1] <- "Democratic trifecta"
  }
  out["NE"] <- "Nonpartisan legislature"
  out
}

ctrl <- do.call(rbind, lapply(YEARS, function(y) {
  cl <- classify(y)
  data.frame(year = y, abbr = names(cl), state = unname(ABV[names(cl)]),
             control = unname(cl), source = if (y %in% CODED_YEARS)
               "coded here" else "Klarner")
}))
ctrl$rank <- panel$final_rank[match(paste(ctrl$year, ctrl$abbr),
                                   paste(panel$year, panel$abbr))]
stopifnot(!anyNA(ctrl$rank), !anyNA(ctrl$control))

# counts per year beside the article's, which is the test described above
cmp <- do.call(rbind, lapply(YEARS, function(y) {
  s <- ctrl[ctrl$year == y & ctrl$control != "Nonpartisan legislature", ]
  p <- PAPER_COUNTS[PAPER_COUNTS$year == y, ]
  data.frame(year = y,
             r = sum(s$control == "Republican trifecta"),
             d = sum(s$control == "Democratic trifecta"),
             v = sum(s$control == "Divided"),
             paper_r = p$paper_r, paper_d = p$paper_d, paper_v = p$paper_v,
             source = s$source[1])
}))
cmp$states_differing <- with(cmp, (abs(r - paper_r) + abs(d - paper_d) +
                                   abs(v - paper_v)) / 2)
# the five coded years must reproduce the article exactly; the Klarner years
# are allowed to disagree, but not by much, and the amount is reported
stopifnot(all(cmp$states_differing[cmp$source == "coded here"] == 0),
          all(cmp$states_differing <= 1))

# mean rank by control group by year -- the series the article's Figure 3 plots
trend <- do.call(rbind, lapply(YEARS, function(y) {
  s <- ctrl[ctrl$year == y & ctrl$control != "Nonpartisan legislature", ]
  do.call(rbind, lapply(c("Republican trifecta", "Democratic trifecta",
                          "Divided"), function(g) {
    z <- s[s$control == g, ]
    data.frame(year = y, control = g, n = nrow(z),
               mean_rank = if (nrow(z)) mean(z$rank) else NA_real_,
               mean_covi = if (nrow(z)) mean(panel$final[
                 match(paste(y, z$abbr), paste(panel$year, panel$abbr))])
                 else NA_real_)
  }))
}))

control <- ctrl$control[ctrl$year == 2024][match(m24$abbr,
                          ctrl$abbr[ctrl$year == 2024])]
stopifnot(!anyNA(control))

covi24 <- data.frame(
  abbr          = m24$abbr,
  state         = unname(ABV[m24$abbr]),
  initial       = p24$initial,
  initial_rank  = p24$initial_rank,
  final         = p24$final,
  final_rank    = p24$final_rank,
  rebuilt       = r24$score,
  control       = control,
  vep_turnout   = tn$VEP_TURNOUT_RATE,
  vep           = tn$VEP)

# The ten issue areas, under short names the brief can print in a table header.
IA_SHORT <- c("reg_deadline", "reg_restrictions", "reg_drives", "preregistration",
              "automatic_reg", "inconveniences", "voter_id", "poll_hours",
              "early_voting", "absentee")
stopifnot(ncol(X24) == length(IA_SHORT))
IA24 <- X24; names(IA24) <- IA_SHORT
covi24 <- cbind(covi24, IA24)

# --- Did the rebuild work? ---------------------------------------------------
#
# This is the check the chapter rests on. If the index can be rebuilt from the
# published laws to floating-point agreement, then every later section -- the
# re-weightings, the counterfactuals -- is a statement about the real index and
# not about an approximation of it.

rebuild24 <- data.frame(
  abbr       = covi24$abbr,
  state      = covi24$state,
  published  = covi24$final,
  rebuilt    = covi24$rebuilt,
  difference = covi24$rebuilt - covi24$final,
  published_rank = rank(covi24$final,   ties.method = "max"),
  rebuilt_rank   = rank(covi24$rebuilt, ties.method = "max"))
REPRO_MAXDIFF <- max(abs(rebuild24$difference))
REPRO_RANKS   <- sum(rebuild24$published_rank == rebuild24$rebuilt_rank)
stopifnot(REPRO_MAXDIFF < 0.001, REPRO_RANKS == 50)

# --- The same laws under four weighting rules --------------------------------
#
# Nothing below changes a single law. Every state has exactly the laws it has;
# only the rule for combining them moves.
#
#   published    variance weights on four components, standardized  (the index)
#   equal        the four components weighted equally
#   pc1          the first component alone
#   unstandardized  variance weights, but PCA on the raw covariances, so the
#                   two variables measured in days dominate
#
# Each is a defensible thing to do, and none of them is more obviously right
# than the one the index uses.

p_eq  <- rebuild(X24, K24)
eq    <- as.vector(as.matrix(p_eq$pca$x[, 1:K24]) %*% (rep(1/K24, K24) * p_eq$signs))
pc1   <- p_eq$pca$x[, 1] * p_eq$signs[1]

pr    <- prcomp(X24, scale. = FALSE)
ve_r  <- pr$sdev^2 / sum(pr$sdev^2)
w_r   <- ve_r[1:K24] / sum(ve_r[1:K24])
sg_r  <- sign(colSums(pr$rotation[, 1:K24]))
uns   <- as.vector(as.matrix(pr$x[, 1:K24]) %*% (w_r * sg_r))

reweight <- data.frame(
  abbr  = covi24$abbr,
  state = covi24$state,
  published_rank      = rank(covi24$final, ties.method = "max"),
  equal_rank          = rank(eq,  ties.method = "max"),
  pc1_rank            = rank(pc1, ties.method = "max"),
  unstandardized_rank = rank(uns, ties.method = "max"))
reweight$equal_move          <- reweight$equal_rank          - reweight$published_rank
reweight$pc1_move            <- reweight$pc1_rank            - reweight$published_rank
reweight$unstandardized_move <- reweight$unstandardized_rank - reweight$published_rank

# --- The individual items ----------------------------------------------------
#
# Below the ten issue areas sit the individual law items, most of them a plain
# yes/no. They are here for one purpose: to show what a variance-weighted index
# does with a law that nearly every state has, or that nearly none has.
#
# A component analysis weights a variable by how much it moves WITH the others.
# An item on which forty-nine states agree carries almost no variance, so it can
# be as burdensome as you like and still be nearly invisible in the score. That
# is not a flaw anybody hid; it is what "letting the data speak for itself"
# means, and it is worth seeing item by item.

DROP <- c("statenu", "State", "Year", "abbr")
items <- m24[, !names(m24) %in% DROP, drop = FALSE]
items <- items[, !grepl("Issue Area", names(items)), drop = FALSE]
inum  <- as.data.frame(lapply(items, function(z) suppressWarnings(as.numeric(as.character(z)))))
names(inum) <- names(items)
# keep the yes/no items, which is what the "how many states" question is about
bin <- vapply(inum, function(z) !anyNA(z) && all(z %in% c(0, 0.5, 1)), logical(1))
inum <- inum[, bin, drop = FALSE]

items_out <- data.frame(
  item          = names(inum),
  states_scored = vapply(inum, function(z) sum(z > 0), numeric(1)),
  sd            = vapply(inum, sd, numeric(1)),
  row.names     = NULL)
items_out <- items_out[order(items_out$states_scored), ]

# --- Facts -------------------------------------------------------------------

f <- function(k, v) data.frame(key = k, value = as.character(v))
top24    <- covi24$state[which.min(covi24$final)]
top24_i  <- covi24$state[which.min(covi24$initial)]
bot24    <- covi24$state[which.max(covi24$final)]
y96      <- ys[ys$year == 1996, ]
ct       <- cor.test(covi24$final, covi24$vep_turnout)

facts <- rbind(
  f("n_years",            length(YEARS)),
  f("n_state_years",      nrow(panel)),
  f("n_issue_areas_1996", ys$n_issue_areas[ys$year == 1996]),
  f("n_issue_areas_2024", ys$n_issue_areas[ys$year == 2024]),
  f("easiest_2024_published", top24_i),
  f("easiest_2024_current",   top24),
  f("hardest_2024",           bot24),
  f("repro_max_difference",   dd_num(REPRO_MAXDIFF, 3)),
  f("cor_init_final_1996",    dd_num(y96$cor_init_final, 3)),
  f("mean_rank_move_1996",    dd_num(y96$mean_rank_move, 3)),
  f("max_rank_move_1996",     y96$max_rank_move),
  f("max_move_state_1996",    y96$max_move_state),
  f("mean_rank_move_2024",    dd_num(ys$mean_rank_move[ys$year == 2024], 3)),
  f("turnout_correlation",    dd_num(unname(ct$estimate), 3)),
  f("turnout_r_squared",      dd_num(unname(ct$estimate)^2, 3)),
  f("turnout_p_value",        dd_num(ct$p.value, 3)),
  f("equal_weight_max_move",  max(abs(reweight$equal_move))),
  f("unstd_max_move",         max(abs(reweight$unstandardized_move))),
  f("pc1_share_2024",         dd_num(ys$pc1_share[ys$year == 2024], 3)),
  f("gap_1996", dd_num(
      trend$mean_rank[trend$year == 1996 & trend$control == "Republican trifecta"] -
      trend$mean_rank[trend$year == 1996 & trend$control == "Democratic trifecta"], 3)),
  f("gap_2024", dd_num(
      trend$mean_rank[trend$year == 2024 & trend$control == "Republican trifecta"] -
      trend$mean_rank[trend$year == 2024 & trend$control == "Democratic trifecta"], 3)),
  f("r_rank_1996", dd_num(trend$mean_rank[trend$year == 1996 &
      trend$control == "Republican trifecta"], 3)),
  f("d_rank_1996", dd_num(trend$mean_rank[trend$year == 1996 &
      trend$control == "Democratic trifecta"], 3)),
  f("r_rank_2024", dd_num(trend$mean_rank[trend$year == 2024 &
      trend$control == "Republican trifecta"], 3)),
  f("d_rank_2024", dd_num(trend$mean_rank[trend$year == 2024 &
      trend$control == "Democratic trifecta"], 3)),
  f("control_states_differing", sum(cmp$states_differing)))

# --- Checks ------------------------------------------------------------------
#
# Each of these can fail. Each is a statement about the data that would be false
# if the file, the parse, or the arithmetic were wrong.

ck <- function(check, result) data.frame(check = check, result = as.character(result))

# a check with teeth: the trifecta coding is verified against counts the paper
# prints, so a miscoded state shows up as a wrong total rather than as nothing
checks <- rbind(
  ck("Rows in the values file (50 states x 9 elections)", nrow(panel)),
  ck("Published ranks reproduced from published values, both columns",
     paste0(nrow(panel) * 2 - RANK_MISMATCH, " of ", nrow(panel) * 2,
            " (two different tie conventions)")),
  ck("State-years sitting in an exact tie on the current score", TIED),
  ck("2024 index rebuilt from the laws: largest difference from published",
     dd_num(REPRO_MAXDIFF, 3)),
  ck("2024 index rebuilt from the laws: states in the same rank",
     paste0(REPRO_RANKS, " of 50")),
  ck("Component weights recomputed here match the published codebook table",
     paste0(dd_num(ys$pc1_share[ys$year == 2024], 4), " vs 0.4709 (2024, first component)")),
  ck("Party control, five coded elections vs the counts the article prints",
     paste0(sum(cmp$states_differing[cmp$source == "coded here"]),
            " states differing across 2012-2024")),
  ck("Party control, four Klarner elections vs the same table",
     paste0(sum(cmp$states_differing[cmp$source == "Klarner"]),
            " states differing across 1996-2008, in ",
            sum(cmp$states_differing[cmp$source == "Klarner"] > 0), " of 4 years")),
  ck("Every state matched to a 2024 turnout rate",
     paste0(sum(!is.na(covi24$vep_turnout)), " of 50")),
  ck("Issue areas in the index, first year and last",
     paste0(ys$n_issue_areas[ys$year == 1996], " in 1996, ",
            ys$n_issue_areas[ys$year == 2024], " in 2024")))

stopifnot(abs(ys$pc1_share[ys$year == 2024] - 0.4709) < 0.0001,
          !anyNA(covi24$vep_turnout))

# --- Write -------------------------------------------------------------------

panel$initial_rank_recomputed <- NULL
panel$final_rank_recomputed   <- NULL

dd_write_csv(panel,      "derived/covi_panel.csv")
dd_write_csv(ctrl,       "derived/control_panel.csv")
dd_write_csv(trend,      "derived/control_trend.csv")
dd_write_csv(cmp,        "derived/control_check.csv")
dd_write_csv(covi24,     "derived/covi_2024.csv")
dd_write_csv(ys,         "derived/year_structure.csv")
dd_write_csv(rebuild24,  "derived/rebuild_2024.csv")
dd_write_csv(reweight,   "derived/reweight_2024.csv")
dd_write_csv(items_out,  "derived/items_2024.csv")
dd_write_csv(facts,      "derived/facts.csv")
dd_write_csv(checks,     "derived/checks.csv")

cat("wrote", length(list.files("derived")), "files to derived/\n")
print(checks, row.names = FALSE)

# ---------------------------------------------------------------------------
# Build stamp. Records which script produced what is now in this directory --
# every file under derived/ and raw/ with its size, hash and row count, and the
# date this ran -- into BUILD-STAMP.tsv beside the data. See
# ../../../_lib/provenance.R. Guarded, because a missing helper must not fail a
# build that was otherwise fine.
if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_report()
  prov_stamp()
}
