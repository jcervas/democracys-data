# ---------------------------------------------------------------------------
# Build the media-ideology dataset: news outlets placed by who links to them.
#
# THE OBJECT. 641 members of the 116th Congress and other political actors
# tweeted 375,979 links to 184 news domains between 2016 and 2021. Each row is
# a politician, each of the last 184 columns is a domain, and each cell is a
# count of times that person shared that outlet. Seven columns in front carry
# who the person is, including their DW-NOMINATE score from roll-call votes.
#
# That shape is unusual and it is the whole reason this chapter exists. Part IV
# already has `media-attention`, which counts Wikipedia pageviews -- an
# audience measure. Nothing in this book has had a NEWS OUTLET as the unit of
# analysis. This file does, and it arrives with an independent measure of the
# people doing the sharing already attached, so the estimate can be graded
# rather than admired.
#
# SOURCE. `PolShares`, the demonstration dataset shipped with the R package
# `mediascores`, MIT licensed.
#   https://github.com/SMAPPNYU/mediascores  (file data/PolShares.RData)
# The package implements the model in Gregory Eady, Richard Bonneau, Joshua A.
# Tucker and Jonathan Nagler, "News Sharing on Social Media: Mapping the
# Ideology of News Media, Politicians, and the Mass Public," Political Analysis
# (2024), doi:10.1017/pan.2024.19.
#
# WHY THIS FILE AND NOT THE PAPER'S REPLICATION ARCHIVE. The archive is on
# Harvard Dataverse at doi:10.7910/DVN/1QMLOV, and Dataverse answers an
# automated request with HTTP 202 and an empty body -- a success-shaped
# refusal, the same wall this book's `survey-access` chapter measures. Every
# endpoint was tried, including with a browser user-agent. A person with a
# browser gets the file; a script never does. The package's demonstration data
# is on GitHub, keyless, and is the politician half of the same object.
#
# WHAT IS THEREFORE MISSING. The paper's title names three things: news media,
# politicians, and the mass public. This file has the first two. The ordinary
# citizens are in the walled archive. Every claim below is about what
# POLITICIANS share, and the chapter says so rather than letting "media
# ideology" stand unqualified.
#
# THE ESTIMATOR IS DELIBERATELY THE SIMPLE ONE. Eady et al. fit a Bayesian
# scaling model in Stan that estimates outlet positions and politician
# positions jointly, with a term for how much each person shares at all. This
# script does not run that model and does not pretend to. It computes a
# share-weighted average of the sharers' NOMINATE scores, which is transparent,
# runs in base R, and is a SIMPLIFICATION -- it takes NOMINATE as given rather
# than estimating position from the sharing itself. The chapter states the
# difference and the checks below bound it.
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

# --- acquire ----------------------------------------------------------------
# The full source is preserved in raw/ and subset into derived/. It is 70 KB,
# which makes that easy here; the rule is the same when it is 570 MB.

URL <- "https://raw.githubusercontent.com/SMAPPNYU/mediascores/master/data/PolShares.RData"
RAW <- "raw/PolShares.RData"
if (!file.exists(RAW)) { say("downloading %s", URL); prov_fetch(URL, RAW, mode = "wb", quiet = TRUE) }

e <- new.env(); load(RAW, envir = e)
P <- as.data.frame(get("PolShares", envir = e), stringsAsFactors = FALSE)

META <- c("name", "role", "nominate_name", "party", "affiliation", "group", "nominate")
DOM  <- setdiff(names(P), META)

fact("n_actors",  nrow(P),     "politicians and political actors in the file")
fact("n_domains", length(DOM), "news domains they shared links to")
check("the metadata columns are the seven documented ones",
      all(META %in% names(P)) && length(DOM) == ncol(P) - length(META))

M <- as.matrix(P[, DOM]); M[is.na(M)] <- 0
fact("n_shares", sum(M), "link shares recorded in total")
check("the counts are non-negative whole numbers",
      all(M >= 0) && all(M == floor(M)))

# --- who has a NOMINATE score, and who does not -----------------------------
#
# Governors and non-legislators never cast a roll-call vote, so they have no
# NOMINATE. They are dropped from the ESTIMATOR and counted here, because a
# dropped row is a decision.

has_n <- !is.na(P$nominate)
fact("n_with_nominate", sum(has_n),  "of them with a roll-call ideology score")
fact("n_without",       sum(!has_n), "without one, who cannot enter the estimate")
fact("dropped_roles", paste(sort(unique(P$role[!has_n])), collapse = ", "),
     "the roles that lack one")
fact("shares_lost", sum(M[!has_n, ]),
     "link shares belonging to those people, and therefore unused")
fact("shares_lost_pct", round(100 * sum(M[!has_n, ]) / sum(M), 1),
     "percent of all shares that removes")
check("the people without a score are a minority of the shares",
      sum(M[!has_n, ]) < 0.35 * sum(M))

Mn <- M[has_n, , drop = FALSE]
nom <- P$nominate[has_n]

# --- the outlet scale --------------------------------------------------------
#
# An outlet's score is the average NOMINATE of the people who shared it,
# weighted by how often each shared it. A domain shared only by conservatives
# scores where conservatives sit.

MIN_SHARES <- 100      # declared cut, reported everywhere it matters
tot <- colSums(Mn)
sc  <- as.numeric((nom %*% Mn) / tot)

O <- data.frame(domain = DOM, shares = as.numeric(tot), score = round(sc, 4),
                stringsAsFactors = FALSE)
O$sharers <- as.numeric(colSums(Mn > 0))
# How lopsided the sharing is: the share of an outlet's links contributed by
# the single politician who shared it most. A score resting on one person is
# not a measurement of anything.
O$top_sharer_pct <- round(100 * apply(Mn, 2, max) / pmax(tot, 1), 1)
O <- O[order(O$score), ]
O$kept <- O$shares >= MIN_SHARES
dd_write_csv(O, "derived/outlets.csv")

fact("min_shares", MIN_SHARES, "the share count an outlet needs to be scored here")
fact("n_kept", sum(O$kept), "outlets that clear it")
fact("n_cut",  sum(!O$kept), "that do not and are drawn in gray rather than dropped")
fact("kept_share_pct", round(100 * sum(O$shares[O$kept]) / sum(O$shares), 1),
     "percent of all link shares the kept outlets account for")
check("the cut keeps most of the sharing", sum(O$shares[O$kept]) > 0.9 * sum(O$shares))

K <- O[O$kept, ]
fact("left_most",  K$domain[1],        "the outlet the most liberal politicians share")
fact("left_score", K$score[1],         "its score")
fact("right_most", K$domain[nrow(K)],  "the outlet the most conservative politicians share")
fact("right_score", K$score[nrow(K)],  "its score")
fact("scale_span", round(K$score[nrow(K)] - K$score[1], 3),
     "the distance between the two ends")
check("the two ends are on opposite sides of zero",
      K$score[1] < 0 && K$score[nrow(K)] > 0)

for (d in c("foxnews.com", "nytimes.com")) {
  if (d %in% K$domain) fact(paste0("score_", sub("\\..*", "", d)),
                            K$score[K$domain == d], paste("the score for", d))
}
check("Fox News scores to the right of the New York Times",
      K$score[K$domain == "foxnews.com"] > K$score[K$domain == "nytimes.com"])

# --- the check that matters --------------------------------------------------
#
# Turn the estimator around. Score each POLITICIAN by the average score of the
# outlets they share, then compare that to their NOMINATE. NOMINATE comes from
# roll-call votes and the sharing score comes from links; the two are different
# behaviours by the same people. They are not independent -- the outlet scores
# were built from NOMINATE in the first place -- and the chapter says so. What
# the comparison bounds is how much of one behaviour the other reproduces.

domsc <- setNames(O$score, O$domain)[DOM]
keep_d <- O$kept[match(DOM, O$domain)]
Mk <- Mn[, keep_d, drop = FALSE]; dk <- domsc[keep_d]
pt  <- rowSums(Mk)
pol <- data.frame(
  name = P$name[has_n], role = P$role[has_n], party = P$party[has_n],
  nominate = nom, shares = pt,
  media_score = round(as.numeric(Mk %*% dk) / pmax(pt, 1), 4),
  stringsAsFactors = FALSE)
pol <- pol[pol$shares >= 20, ]
dd_write_csv(pol, "derived/politicians.csv")

fact("n_pol_scored", nrow(pol), "politicians with at least twenty shares to scored outlets")
r <- cor(pol$nominate, pol$media_score)
fact("cor_nominate_media", round(r, 3),
     "correlation between how they vote and what they link to")
check("the two behaviours agree strongly but not perfectly", r > 0.7 && r < 0.99)

ov <- with(pol, {
  d <- media_score[party == "Democrat"]; r2 <- media_score[party == "Republican"]
  sum(d > min(r2)) + sum(r2 < max(d)) })
fact("party_overlap", ov, "politicians whose media score falls inside the other party's range")
fact("dem_median_media", round(median(pol$media_score[pol$party == "Democrat"]), 3),
     "the median Democrat's media score")
fact("rep_median_media", round(median(pol$media_score[pol$party == "Republican"]), 3),
     "and the median Republican's")

# --- how much rests on one person -------------------------------------------

fact("median_top_pct", round(median(K$top_sharer_pct), 1),
     "percent of a typical kept outlet's shares contributed by its single biggest sharer")
worst <- K[which.max(K$top_sharer_pct), ]
fact("worst_domain",  worst$domain,        "the kept outlet leaning hardest on one person")
fact("worst_top_pct", worst$top_sharer_pct, "the percent that one person contributes")
check("at least one kept outlet rests mostly on a single sharer",
      max(K$top_sharer_pct) > 40)

# ---------------------------------------------------------------------------

facts <- data.frame(key = names(FACTS),
                    value = vapply(FACTS, function(x) as.character(x$value), ""),
                    note  = vapply(FACTS, function(x) x$note, ""))
dd_write_csv(facts, "derived/facts.csv")
dd_write_csv(do.call(rbind, lapply(CHECKS, as.data.frame)), "derived/checks.csv")
say("done: %d facts, %d checks, %d outlets, %d politicians",
    nrow(facts), length(CHECKS), nrow(O), nrow(pol))

try(prov_stamp(), silent = TRUE)
