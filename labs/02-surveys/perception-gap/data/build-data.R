# ---------------------------------------------------------------------------
# The perception gap: what people think each party is made of.
#
# In March 2015 Ahler and Sood asked 1,000 Americans to estimate the percentage
# of each party's supporters belonging to four groups the party is stereotyped
# around. The answers are famously far from the truth, and the figure that says
# so has been reprinted everywhere.
#
# This chapter rebuilds that figure from the authors' own archive, then goes
# past it to the paper's actual argument, which the famous figure does not show:
# people are worse about the party they are NOT in. The respondent file carries
# party identification, so the eight points can be split by who was answering.
#
# Only after that does it ask what the perceived share is made of. It is the
# weighted MEAN of a thousand guesses, most of them round numbers, and the mean
# of a lumpy skewed pile is not the middle of it.
#
# ---------------------------------------------------------------------------
# WHAT IS WRITTEN
# ---------------------------------------------------------------------------
#
#   derived/perception_benchmarks.csv  one row per item: the true share and the
#                          national mean estimate, both with their standard
#                          errors. The table the class version of the block
#                          scores against; the items are printed in the brief.
#   derived/items.csv      the same eight items with the middle of the guesses
#                          beside the mean of them, and the gap computed both
#                          ways
#   derived/answers.csv    one row per person per item: 8,000 guesses, with the
#                          case weight and the respondent's party
#   derived/by_party.csv   the same eight items split by the respondent's own
#                          party, which is the paper's central claim
#   derived/dist.csv       the shape of the answers: how many people gave each
#                          value from 0 to 100, per item and pooled
#   derived/heaping.csv    how round the answers are, item by item
#   derived/facts.csv      single numbers the brief quotes
#   derived/checks.csv     what this script verified before it wrote anything
#
# Run this script from inside the data/ folder.
# ---------------------------------------------------------------------------

dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)
options(scipen = 999, stringsAsFactors = FALSE)
source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits

if (file.exists("../../../_lib/provenance.R")) {
  source("../../../_lib/provenance.R")
} else {
  prov_fetch  <- function(url, dest, ...) { download.file(url, dest, mode = "wb", quiet = TRUE); dest }
  prov_report <- function() invisible(FALSE)
}

FETCH_DATE <- "2026-08-29"
say <- function(...) cat(..., "\n", sep = "")
chk <- data.frame(check = character(), value = character())
note <- function(k, v) chk[nrow(chk) + 1, ] <<- c(k, as.character(v))

# --- SOURCE ----------------------------------------------------------------
#
# Ahler, Douglas J., and Gaurav Sood. 2018. "The Parties in Our Heads:
# Misperceptions About Party Composition and Their Consequences." *The Journal
# of Politics* 80(3): 964-981. Replication archive, Harvard Dataverse,
# doi:10.7910/DVN/CLMQ8E.
#
# WHAT IS TAKEN, AND WHY EACH ONE. The archive holds seventeen files covering
# five studies. Four are read here:
#
#   pcomp_yougov_data.tab    the 1,000 respondents themselves, one row each,
#                            with all eight estimates, the case weight and
#                            party identification. This is the file that makes
#                            the chapter possible: the published figure is a
#                            summary of it, and a summary cannot be asked what
#                            it is hiding.
#   fig_1_data_actual.tab    the true share of each group in each party, with a
#                            standard error, as the authors computed it
#   fig_1_data_perc.tab      the published perceived means, which this build
#                            recomputes from the respondent file and checks
#                            itself against
#   codebook.txt             what each column is
#
# GETTING THEM IS KEYLESS AND SCRIPTABLE, which is worth saying in a part where
# four of six survey archives will not answer a program at all -- see the
# survey-access chapter. There is one catch and it is silent: the file endpoint
# answers **303** and hands you a redirect to storage. A client that does not
# follow redirects writes a zero-byte file and reports success.

BASE <- "https://dataverse.harvard.edu/api/access/datafile/"
FILES <- c(readme            = "3047039",
           codebook          = "3047038",
           fig1_script       = "3047042",
           fig_1_data_actual = "3047048",
           fig_1_data_perc   = "3047049",
           fig_1_data        = "3047047",
           pcomp_yougov_data = "3047052")
EXT <- c(readme = ".txt", codebook = ".txt", fig1_script = ".R",
         fig_1_data_actual = ".tab", fig_1_data_perc = ".tab",
         fig_1_data = ".tab", pcomp_yougov_data = ".tab")
NAMES <- c(readme = "readme.txt", codebook = "codebook.txt",
           fig1_script = "02_fig1.R", fig_1_data_actual = "fig_1_data_actual.tab",
           fig_1_data_perc = "fig_1_data_perc.tab", fig_1_data = "fig_1_data.tab",
           pcomp_yougov_data = "pcomp_yougov_data.tab")

options(HTTPUserAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)")
for (k in names(FILES)) {
  dest <- file.path("raw", NAMES[[k]])
  invisible(prov_fetch(paste0(BASE, FILES[[k]]), dest,
                       label = paste("Ahler-Sood", NAMES[[k]])))
  if (file.size(dest) == 0) stop("zero bytes: the redirect was not followed for ", dest)
}
say("fetched ", length(FILES), " files, ",
    round(sum(file.size(file.path("raw", NAMES))) / 1024), " KB")

# ===========================================================================
# 1. THE EIGHT ITEMS
# ===========================================================================
#
# Each item asks for the percentage of one party's supporters belonging to one
# group. The four Democratic groups and the four Republican ones were chosen by
# the authors as the ones each party is stereotyped around.
#
# The column name in the respondent file and the group label in the published
# figure are different strings for the same item, and joining them is the one
# place this build could go quietly wrong. The map is written out rather than
# matched on a substring.

ITEM <- data.frame(
  item  = c("dem_black", "dem_union", "dem_aa", "dem_lgb",
            "rep_evang", "rep_rich", "rep_old", "rep_south"),
  party = c(rep("Democratic", 4), rep("Republican", 4)),
  group = c("Black", "Union", "Atheist/Agnostic", "LGB",
            "Evangelical", "$250K+ Income", "Age 65+", "Southern"),
  asked = c("Black", "a member of a labour union", "atheist or agnostic",
            "lesbian, gay or bisexual", "an evangelical Christian",
            "earning over $250,000 a year", "65 or older", "Southern"),
  # The field name for the class version of the block. The archive's column
  # names are the authors'; these are the ones the class form writes, and the
  # brief prints them so the form can be built from the chapter rather than
  # from a separate sheet that can drift away from it.
  field = c("percep_dem_black", "percep_dem_union", "percep_dem_atheist",
            "percep_dem_lgb", "percep_rep_evangelical", "percep_rep_250k",
            "percep_rep_65plus", "percep_rep_south"),
  # The question as the class form should ask it, written out rather than
  # assembled from the fragments above: "Democratic" is the party's adjective
  # and "Democrats" is what you call the people, and a rule that lowercases one
  # into the other produces a question no one would ask.
  question = c(
    "Out of every 100 Democrats, how many do you think are Black?",
    "Out of every 100 Democrats, how many do you think belong to a labour union?",
    "Out of every 100 Democrats, how many do you think are atheist or agnostic?",
    "Out of every 100 Democrats, how many do you think are lesbian, gay or bisexual?",
    "Out of every 100 Republicans, how many do you think are evangelical Christians?",
    "Out of every 100 Republicans, how many do you think earn more than $250,000 a year?",
    "Out of every 100 Republicans, how many do you think are 65 or older?",
    "Out of every 100 Republicans, how many do you think live in the South?"))

unq <- function(x) gsub('"', "", x)
act <- read.delim("raw/fig_1_data_actual.tab")
act$Group <- unq(act$Group); act$Party <- unq(act$Party)
pub <- read.delim("raw/fig_1_data_perc.tab")
pub$Group <- unq(pub$Group); pub$Party <- unq(pub$Party)
yg  <- read.delim("raw/pcomp_yougov_data.tab")

stopifnot(nrow(act) == 8, nrow(pub) == 8, nrow(yg) == 1000,
          all(ITEM$group %in% act$Group), all(ITEM$group %in% pub$Group),
          all(ITEM$item %in% names(yg)))
N_RESP <- nrow(yg)

# ===========================================================================
# 2. ONE ROW PER PERSON PER ITEM
# ===========================================================================
#
# The published figure is one point per item. The file underneath it is 8,000
# answers, and every question this chapter asks needs them rather than the
# eight numbers they were reduced to.

ans <- do.call(rbind, lapply(seq_len(nrow(ITEM)), function(i) {
  v <- ITEM$item[i]
  data.frame(respondent = yg$caseid, weight = yg$weight,
             pid7 = yg$pid7, item = v, party = ITEM$party[i],
             group = ITEM$group[i], estimate = yg[[v]])
}))
ans <- ans[!is.na(ans$estimate), ]
stopifnot(nrow(ans) == 8 * N_RESP,
          min(ans$estimate) >= 0, max(ans$estimate) <= 100)
dd_write_csv(ans, "derived/answers.csv")
N_ANS <- nrow(ans)

# ===========================================================================
# 3. REPRODUCING THE PUBLISHED FIGURE
# ===========================================================================
#
# The perceived share in the paper is the WEIGHTED mean, and weighting is the
# whole of the difference between reproducing the figure and nearly
# reproducing it. YouGov ships a case weight because its panel is not the
# country; the ces-class chapter is about what such a weight moves.
#
# This is the check that says the rest of the chapter is reading the same study
# the paper reported. If the recomputed means do not land on the published ones
# to the second decimal, nothing below is worth reading.

wmean <- function(x, w) sum(w * x) / sum(w)

items <- do.call(rbind, lapply(seq_len(nrow(ITEM)), function(i) {
  v <- ITEM$item[i]; g <- ITEM$group[i]
  x <- yg[[v]]; w <- yg$weight
  data.frame(
    item = v, party = ITEM$party[i], group = g, asked = ITEM$asked[i],
    field = ITEM$field[i],
    question = ITEM$question[i],
    actual_pct    = round(act$actual_pct[act$Group == g], 2),
    actual_se     = round(act$actual_se[act$Group == g], 2),
    published_pct = round(pub$coef[pub$Group == g], 2),
    perceived_pct = round(wmean(x, w), 2),
    # kept unrounded so the reproduction check is a real comparison rather
    # than a comparison of two numbers already rounded to the same place
    recomputed_raw = wmean(x, w),
    published_raw  = pub$coef[pub$Group == g],
    perceived_se  = round(pub$stderr[pub$Group == g], 2),
    perceived_n   = pub$N[pub$Group == g],
    plain_mean    = round(mean(x), 2),
    median        = median(x),
    q25           = as.numeric(quantile(x, 0.25)),
    q75           = as.numeric(quantile(x, 0.75)))
}))
items$gap_mean   <- round(items$perceived_pct - items$actual_pct, 2)
items$gap_median <- round(items$median        - items$actual_pct, 2)

MAXDIFF <- max(abs(items$recomputed_raw - items$published_raw))
stopifnot(MAXDIFF < 1e-4)           # the figure reproduces, before any rounding
items$recomputed_raw <- NULL
items$published_raw  <- NULL
dd_write_csv(items, "derived/items.csv")

# The benchmark table the class-survey block scores against keeps the shape it
# had before this script existed, so nothing downstream of it has to change.
bench <- data.frame(
  variable = items$field,
  party = items$party, group = items$group,
  actual_pct = items$actual_pct, actual_se = items$actual_se,
  perceived_pct = items$perceived_pct, perceived_se = items$perceived_se,
  perceived_n = items$perceived_n)
# The class form's field names, which are not the archive's column names.
bench$variable <- items$field
bench <- bench[order(bench$party, bench$variable), ]
dd_write_csv(bench, "derived/perception_benchmarks.csv")

GAP_MEAN   <- mean(items$gap_mean)
GAP_MEDIAN <- mean(items$gap_median)
R_AP       <- cor(items$actual_pct, items$perceived_pct)
SPAN_ACT   <- diff(range(items$actual_pct))
SPAN_PER   <- diff(range(items$perceived_pct))

say("figure reproduced, worst item off by ", sprintf("%.2e", MAXDIFF), " points")
say("average gap: ", round(GAP_MEAN, 1), " on means, ",
    round(GAP_MEDIAN, 1), " on medians")

note("respondents in the YouGov study", format(N_RESP, big.mark = ","))
note("answers behind the eight points", format(N_ANS, big.mark = ","))
note("worst item, recomputed against published (pts)", sprintf("%.2e", MAXDIFF))
note("average gap on weighted means (pts)", sprintf("%.1f", GAP_MEAN))
note("average gap on medians (pts)", sprintf("%.1f", GAP_MEDIAN))
note("correlation, true share against perceived", sprintf("%.3f", R_AP))

# ===========================================================================
# 4. HOW ROUND THE ANSWERS ARE
# ===========================================================================
#
# Respondents typed a percentage into a box. A percentage typed into a box is
# not a measurement on a fine scale: people reach for round numbers, and one
# round number in particular means "I have no idea".
#
# This is not a criticism of the study, which reports a mean of what people
# said and says so. It is a fact about what the mean is a mean OF, and it is
# invisible in every reprint of the figure.

roundness <- function(x) data.frame(
  n        = length(x),
  mult10   = round(100 * mean(x %%  10 == 0), 1),
  mult5    = round(100 * mean(x %%   5 == 0), 1),
  eq50     = round(100 * mean(x == 50), 1),
  eq0      = round(100 * mean(x ==  0), 1),
  eq100    = round(100 * mean(x == 100), 1))

heap <- do.call(rbind, lapply(seq_len(nrow(ITEM)), function(i) {
  cbind(data.frame(item = ITEM$item[i], group = ITEM$group[i]),
        roundness(yg[[ITEM$item[i]]]))
}))
heap <- rbind(heap, cbind(data.frame(item = "ALL", group = "all eight pooled"),
                          roundness(ans$estimate)))
dd_write_csv(heap, "derived/heaping.csv")

POOL <- heap[heap$item == "ALL", ]
say("pooled: ", POOL$mult10, "% are multiples of ten, ", POOL$mult5,
    "% multiples of five, ", POOL$eq50, "% exactly fifty")

note("answers that are a multiple of ten (%)", sprintf("%.1f", POOL$mult10))
note("answers that are a multiple of five (%)", sprintf("%.1f", POOL$mult5))
note("answers of exactly fifty (%)", sprintf("%.1f", POOL$eq50))

# ===========================================================================
# 5. WHOSE PARTY IS BEING GUESSED ABOUT
# ===========================================================================
#
# THIS IS THE PAPER'S ACTUAL ARGUMENT, and the famous figure cannot show it.
# Everyone overestimates. The claim is that you overestimate the OTHER party
# more, because you have no first-hand contact with it and learn about it from
# people describing it to you.
#
# Splitting the answers by the respondent's own party is what tests that, and
# the respondent file carries pid7 for every row.
#
# LEANERS COUNT AS PARTISANS HERE, which is a decision and not a fact. Somebody
# who calls themselves independent and then says they lean Democratic votes and
# thinks like a weak Democrat, which is the party-id chapter's subject. The
# alternative reading is computed too, so the sentence in the brief can say how
# much the decision is worth instead of hoping it is worth nothing.

side_of <- function(pid, leaners = TRUE) {
  if (leaners) ifelse(grepl("Democrat", pid), "D",
              ifelse(grepl("Republican", pid), "R", "I"))
  else ifelse(pid %in% c("Strong Democrat", "Not very strong Democrat"), "D",
       ifelse(pid %in% c("Strong Republican", "Not very strong Republican"), "R", "I"))
}
yg$side <- side_of(yg$pid7)

# One row per item per kind of respondent: what that group guessed, on average.
byp <- do.call(rbind, lapply(seq_len(nrow(ITEM)), function(i) {
  v <- ITEM$item[i]; g <- ITEM$group[i]
  a <- act$actual_pct[act$Group == g]
  do.call(rbind, lapply(c("D", "R", "I"), function(sd) {
    k <- yg$side == sd
    data.frame(item = v, party = ITEM$party[i], group = g,
               respondent = c(D = "Democrats", R = "Republicans",
                              I = "Independents")[[sd]],
               relation = if (sd == "I") "neither"
                          else if ((sd == "D") == (ITEM$party[i] == "Democratic"))
                            "own party" else "other party",
               n = sum(k),
               actual_pct = round(a, 2),
               guess = round(wmean(yg[[v]][k], yg$weight[k]), 2))
  }))
}))
byp$gap <- round(byp$guess - byp$actual_pct, 2)
dd_write_csv(byp, "derived/by_party.csv")

pgap <- function(rel) round(mean(byp$gap[byp$relation == rel]), 1)
OWN   <- pgap("own party")
OTHER <- pgap("other party")
NEITHER <- pgap("neither")

# The same two numbers with leaners pushed back out, so the brief can say what
# the coding decision costs.
yg$side_strict <- side_of(yg$pid7, leaners = FALSE)
strict <- do.call(rbind, lapply(seq_len(nrow(ITEM)), function(i) {
  v <- ITEM$item[i]; a <- act$actual_pct[act$Group == ITEM$group[i]]
  do.call(rbind, lapply(c("D", "R"), function(sd) {
    k <- yg$side_strict == sd
    data.frame(rel = if ((sd == "D") == (ITEM$party[i] == "Democratic"))
                       "own party" else "other party",
               gap = wmean(yg[[v]][k], yg$weight[k]) - a)
  }))
}))
OWN_S   <- round(mean(strict$gap[strict$rel == "own party"]), 1)
OTHER_S <- round(mean(strict$gap[strict$rel == "other party"]), 1)

WORST_CELL <- byp[which.max(byp$gap), ]
N_DEM <- sum(yg$side == "D"); N_REP <- sum(yg$side == "R"); N_IND <- sum(yg$side == "I")

say("own party: ", OWN, " points over.  other party: ", OTHER,
    ".  difference ", round(OTHER - OWN, 1))

note("respondents by side (D / R / neither)", paste(N_DEM, N_REP, N_IND, sep = " / "))
note("average overestimate of one's OWN party (pts)", sprintf("%.1f", OWN))
note("average overestimate of the OTHER party (pts)", sprintf("%.1f", OTHER))
note("the same, counting leaners as independents (pts)",
     sprintf("own %.1f, other %.1f", OWN_S, OTHER_S))

# ===========================================================================
# 6. THE SHAPE OF THE ANSWERS
# ===========================================================================
#
# The count of people who gave each value from 0 to 100, per item and pooled.
# The figure built on this is the one that shows what a mean is a mean of.

# ONE ANSWER IS NOT A WHOLE NUMBER. Somebody typed 0.5, on a scale where the
# other 7,999 answers are integers. It cannot go in a bin of whole numbers, and
# quietly rounding it away would be the small dishonesty this book is about, so
# it is counted and reported instead.
NONINT <- sum(ans$estimate %% 1 != 0)
dist <- do.call(rbind, lapply(c(ITEM$item, "ALL"), function(v) {
  x <- if (v == "ALL") ans$estimate else yg[[v]]
  x <- x[x %% 1 == 0]
  data.frame(item = v, value = 0:100,
             count = as.vector(table(factor(x, levels = 0:100))))
}))
stopifnot(sum(dist$count[dist$item == "ALL"]) + NONINT == N_ANS, NONINT >= 1)
note("answers that are not whole numbers", NONINT)
dd_write_csv(dist, "derived/dist.csv")

# ===========================================================================
# 7. THE SURVEY, AND THE COPY OF IT WE HAVE
# ===========================================================================
#
# THE SOURCE IS A SURVEY, NOT AN ARCHIVE. In March 2015 YouGov fielded this
# study to its online panel. That study is the thing that happened, and nobody
# outside it can have it: YouGov does not publish it, and the questionnaire as
# administered, the panel it was drawn from and everyone who did not finish it
# are not anywhere a reader can go.
#
# What exists in public is a COPY OF PART OF IT, deposited by the two authors
# so their results could be checked. That is a different object and the
# difference is measurable, because the deposit ships its own codebook.
#
# Reading the codebook's YouGov section against the file's header finds
# disagreement in both directions, which is the honest description of any
# deposit and is invisible unless you check.

cb <- readLines("raw/codebook.txt", warn = FALSE)
open_at <- grep("^## YouGov", cb)
next_at <- grep("^## ", cb)
end_at  <- min(c(next_at[next_at > open_at], length(cb) + 1)) - 1
sec <- cb[(open_at + 1):end_at]
documented <- trimws(sub("^\\s*-\\s*([A-Za-z0-9_]+)\\s*:.*$", "\\1",
                         grep("^\\s*-\\s*[A-Za-z0-9_]+\\s*:", sec, value = TRUE)))
carried <- names(yg)
carried <- carried[!carried %in% c("side", "side_strict")]

UNDOC   <- setdiff(carried, documented)      # in the file, not in the codebook
UNSHIPPED <- setdiff(documented, carried)    # in the codebook, not in the file

cov <- data.frame(
  column = c(sort(UNSHIPPED), sort(UNDOC)),
  where  = c(rep("codebook only", length(UNSHIPPED)),
             rep("file only", length(UNDOC))))
dd_write_csv(cov, "derived/coverage.csv")

say("codebook documents ", length(documented), ", file carries ", length(carried),
    "; ", length(UNSHIPPED), " documented and absent, ", length(UNDOC),
    " present and undocumented")

note("columns the codebook documents", length(documented))
note("columns the deposited file carries", length(carried))
note("documented but not in the file", paste(sort(UNSHIPPED), collapse = ", "))
note("in the file but not documented", paste(sort(UNDOC), collapse = ", "))

# ===========================================================================
# 8. FACTS THE BRIEF QUOTES
# ===========================================================================

worst <- items[which.max(items$gap_mean), ]
best  <- items[which.min(items$gap_mean), ]
biggest_drop <- items[which.max(items$gap_mean - items$gap_median), ]
widest_se    <- items[which.max(items$actual_se), ]

fx <- function(...) {
  a <- list(...)
  data.frame(key = names(a), value = unlist(lapply(a, as.character)))
}
facts <- fx(
  n_resp          = N_RESP,
  n_answers       = N_ANS,
  n_items         = nrow(items),
  maxdiff         = sprintf("%.2e", MAXDIFF),
  gap_mean        = round(GAP_MEAN, 1),
  gap_median      = round(GAP_MEDIAN, 1),
  gap_shrink_pct  = round(100 * (GAP_MEAN - GAP_MEDIAN) / GAP_MEAN, 1),
  r_actual_perc   = round(R_AP, 3),
  span_actual     = round(SPAN_ACT, 1),
  span_perceived  = round(SPAN_PER, 1),
  mult10          = POOL$mult10,
  mult5           = POOL$mult5,
  not_mult5       = round(100 - POOL$mult5, 1),
  eq50            = POOL$eq50,
  worst_group     = worst$group,
  worst_party     = worst$party,
  worst_actual    = worst$actual_pct,
  worst_perceived = worst$perceived_pct,
  worst_gap       = worst$gap_mean,
  best_group      = best$group,
  best_gap        = best$gap_mean,
  drop_group      = biggest_drop$group,
  drop_gap_mean   = biggest_drop$gap_mean,
  drop_gap_median = biggest_drop$gap_median,
  drop_median     = biggest_drop$median,
  drop_perceived  = biggest_drop$perceived_pct,
  se_group        = widest_se$group,
  se_actual       = widest_se$actual_pct,
  se_se           = widest_se$actual_se,
  se_lo           = round(widest_se$actual_pct - 1.96 * widest_se$actual_se, 1),
  se_hi           = round(widest_se$actual_pct + 1.96 * widest_se$actual_se, 1),
  own_gap         = OWN,
  other_gap       = OTHER,
  neither_gap     = NEITHER,
  own_other_diff  = round(OTHER - OWN, 1),
  own_gap_strict  = OWN_S,
  other_gap_strict = OTHER_S,
  diff_strict     = round(OTHER_S - OWN_S, 1),
  worst_cell_who  = WORST_CELL$respondent,
  worst_cell_group = WORST_CELL$group,
  # The party the worst cell is ABOUT, and the question's own phrasing for the
  # group. Assembling this sentence in the brief from the group label alone
  # produced "how many Democrats are $250k+ income", which is the wrong party
  # and not English.
  worst_cell_about_word = ifelse(WORST_CELL$party == "Democratic",
                                 "Democrats", "Republicans"),
  worst_cell_asked = ITEM$asked[ITEM$group == WORST_CELL$group],
  worst_cell_gap  = WORST_CELL$gap,
  n_dem           = N_DEM,
  n_rep           = N_REP,
  n_ind           = N_IND,
  nonint          = NONINT,
  n_documented    = length(documented),
  n_carried       = length(carried),
  n_unshipped     = length(UNSHIPPED),
  n_undoc         = length(UNDOC),
  unshipped       = paste(sort(UNSHIPPED), collapse = ", "),
  undoc           = paste(sort(UNDOC), collapse = ", "))
dd_write_csv(facts, "derived/facts.csv")
note("facts written", nrow(facts))

dd_write_csv(chk, "derived/checks.csv")

stopifnot(
  N_RESP == 1000,
  N_ANS == 8000,
  MAXDIFF < 1e-4,                  # the published figure reproduces
  GAP_MEDIAN < GAP_MEAN,           # the middle sits below the mean
  POOL$mult10 > 50,                # most answers are round to ten
  R_AP > 0 && R_AP < 1,
  SPAN_PER < SPAN_ACT,             # the guesses are compressed
  all(items$actual_se > 0),        # the truth is an estimate, not a constant
  nrow(bench) == 8,
  OTHER > OWN,                     # the paper's claim, in this file
  OTHER_S > OWN_S,                 # and it survives the leaner decision
  nrow(byp) == 24, nrow(dist) == 9 * 101,
  length(documented) > 15,         # the codebook section really was parsed
  "pid7" %in% UNDOC)               # the column the party split rests on

cat("\nbuilt ", FETCH_DATE, "\n\n", sep = "")
print(chk, row.names = FALSE)
cat("\nfiles written:\n")
for (f in list.files("derived", full.names = TRUE))
  cat(sprintf("  %-40s %5s rows %7.0f KB\n", f,
              format(nrow(read.csv(f)), big.mark = ","), file.size(f) / 1024))
prov_report()

if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_stamp()
}
