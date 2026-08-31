# ---------------------------------------------------------------------------
# Build the poll-weighting dataset: one poll, many answers.
#
# THE OBJECT. In September 2016 The New York Times and Siena College polled
# 867 Florida voters, published "Clinton +1", and then did something almost no
# pollster does: they released the respondent-level file and asked four outside
# pollsters to produce the estimate themselves. The four came back between
# Clinton +4 and Trump +1. Same 867 people. Five answers.
#
# SOURCE. TheUpshot/2016-upshot-siena-polls on GitHub, file
# `upshot-siena-polls.csv` -- 4,879 respondents across six state-waves, of
# which the September Florida wave is the 867 the article is about.
#   https://github.com/TheUpshot/2016-upshot-siena-polls
# Published alongside Nate Cohn, "We Gave Four Good Pollsters the Same Raw
# Data. They Had Four Different Results," The Upshot, 20 September 2016.
#
# ACQUISITION. One `download.file()` against a raw.githubusercontent.com
# address. No key, no account, no guestbook, 2.3 MB. That is the easiest
# acquisition in Part II and it is worth saying why it is unusual: a
# respondent-level poll file is normally the last thing a pollster releases,
# because it is the only thing that lets somebody else disagree with them.
#
# It is also a private compilation with no statute behind it. It exists at a
# newspaper's discretion on a platform that is not an archive, and nothing
# obliges anybody to keep it there. The derived files are committed for that
# reason.
#
# WHAT THIS SCRIPT DOES NOT DO. It does not reproduce the four pollsters'
# answers. Their weighting schemes were described in prose, not published as
# code, and two of the four used external targets (voter-file turnout scores,
# a census estimate of the 2016 electorate) that are not in this file. What it
# builds instead is a LADDER the file can support on its own terms:
#
#   as collected           no weights at all
#   registered voters      the Times' own rvweight, as published
#   likely voters          the Times' own lvweight -- the printed "Clinton +1"
#   ... then rakes to targets a reader sets, which is the chapter's exercise.
#
# The four published answers appear in the chapter as QUOTED figures from the
# article, in a table that says so, and are not computed here. Nothing in this
# folder claims to have recovered them.
#
# THE ONE TRICK WORTH KNOWING. The Times' assumed electorate is recoverable
# without being told. Weighted marginals ARE the target: if you weight the
# sample by lvweight and tabulate education, you get the education profile the
# Times assumed Florida's likely voters had. Those recovered targets are
# written to derived/targets.csv, and they are what the chapter's interactive
# starts from. No target anywhere in this chapter is typed.
# ---------------------------------------------------------------------------

source("../../../_lib/precision.R")
suppressWarnings(try(source("../../../_lib/provenance.R"), silent = TRUE))

dir.create("raw",     showWarnings = FALSE)
dir.create("derived", showWarnings = FALSE)

say <- function(...) cat(sprintf(...), "\n")

FACTS <- list(); CHECKS <- list()
fact <- function(key, value, note) {
  FACTS[[key]] <<- list(value = value, note = note); invisible(value)
}
check <- function(what, ok) {
  CHECKS[[length(CHECKS) + 1]] <<- list(check = what, passed = isTRUE(ok))
  if (!isTRUE(ok)) stop("FAILED: ", what)
  invisible(TRUE)
}

# --- acquire ---------------------------------------------------------------

URL <- paste0("https://raw.githubusercontent.com/TheUpshot/",
              "2016-upshot-siena-polls/master/upshot-siena-polls.csv")
RAW <- "raw/upshot-siena-polls.csv"
if (!file.exists(RAW)) {
  say("downloading %s", URL)
  download.file(URL, RAW, quiet = TRUE)
}
d <- read.csv(RAW, stringsAsFactors = FALSE)
fact("src_rows", nrow(d), "respondents in the whole released file")
fact("src_cols", ncol(d), "columns in it")

# The file holds six state-waves. This chapter is about exactly one of them.
fl <- d[d$state == "Florida" & d$poll == "September", ]
fact("n", nrow(fl), "respondents in the September Florida wave")
check("the wave is the 867 the article is about", nrow(fl) == 867)
check("both of the Times' own weights are present on every row",
      all(!is.na(fl$rvweight)) && all(!is.na(fl$lvweight)))

# --- the analysis columns ---------------------------------------------------
#
# Every recode below collapses categories, and every collapse is a decision.
# They are kept few and coarse on purpose: the chapter is about what weighting
# does, and a reader cannot turn a dial with eleven positions.

CLINTON <- "Hillary Clinton, the Democrat"
TRUMP   <- "Donald Trump, the Republican"

fl$vote <- ifelse(fl$vt_pres_2 == CLINTON, "Clinton",
           ifelse(fl$vt_pres_2 == TRUMP,   "Trump", NA))
fact("n_twoparty", sum(!is.na(fl$vote)), "of them naming one of the two major candidates")
fact("n_other", sum(is.na(fl$vote)),
     "who named somebody else, refused, or said they would not vote")

# Education, four bands. "Refused" is kept as its own level rather than
# dropped, because a refusal is not a missing value -- somebody was asked.
fl$educ4 <- c(
  "Grade school"                    = "HS or less",
  "High school"                     = "HS or less",
  "Some college or trade school"    = "Some college",
  "Bachelors' degree"               = "Bachelor's",
  "Graduate or Professional degree" = "Postgraduate",
  "[DO NOT READ] Refused"           = "Refused")[fl$educ]

# Race, from the VOTER FILE rather than from the interview, because the file's
# race is what a pollster weighting to registration data would use, and the two
# disagree. The survey's own race question is kept beside it for the chapter.
fl$race4 <- ifelse(fl$file_race %in% c("White", "Black", "Hispanic"),
                   fl$file_race, "Other")
fl$race_said <- c(
  "Caucasian/White"                              = "White",
  "African American/Black"                       = "Black",
  "Asian"                                        = "Other",
  "[DO NOT READ] Other/Something else (specify)" = "Other",
  "[DO NOT READ] Refused"                        = "Refused")[fl$race]

fl$party3   <- fl$file_party                 # Democratic / Republican / Other
fl$turnout3 <- fl$turnout_class              # High / Middle / Low
fl$region5  <- fl$region
fl$age4     <- cut(fl$file_age, c(17, 34, 49, 64, Inf),
                   labels = c("18-34", "35-49", "50-64", "65+"))

VARS <- c(educ4 = "education", race4 = "race (voter file)",
          party3 = "party registration", age4 = "age",
          turnout3 = "turnout history", region5 = "region")

for (v in names(VARS))
  check(sprintf("%s has no missing values to weight around", v),
        !any(is.na(fl[[v]])))

# --- the margin under a weight ---------------------------------------------

margin <- function(w) {
  k  <- !is.na(fl$vote)
  ww <- w[k]; ww[is.na(ww)] <- 0
  cl <- sum(ww[fl$vote[k] == "Clinton"])
  tr <- sum(ww[fl$vote[k] == "Trump"])
  round(100 * (cl - tr) / (cl + tr), 2)
}
share <- function(w, who) {
  k  <- !is.na(fl$vote)
  ww <- w[k]; ww[is.na(ww)] <- 0
  round(100 * sum(ww[fl$vote[k] == who]) / sum(ww), 2)
}

ONE <- rep(1, nrow(fl))
M_RAW <- margin(ONE); M_RV <- margin(fl$rvweight); M_LV <- margin(fl$lvweight)

fact("margin_raw", M_RAW, "Clinton's two-party margin with no weights at all")
fact("margin_rv",  M_RV,  "under the Times' registered-voter weight")
fact("margin_lv",  M_LV,  "under the Times' likely-voter weight -- the published figure")
fact("clinton_raw", share(ONE, "Clinton"), "Clinton's two-party share as collected")
fact("clinton_lv",  share(fl$lvweight, "Clinton"), "and as published")
fact("weighting_moves", round(M_RAW - M_LV, 2),
     "points of margin the weighting alone accounts for")

# The published poll said Clinton +1. If this does not round to it, something
# upstream has changed and every number in the chapter is wrong.
check("the published Clinton +1 reproduces from the released weights",
      round(M_LV) == 1)
check("weighting moves the margin by more than five points",
      abs(M_RAW - M_LV) > 5)

# --- recover the Times' assumed electorate ----------------------------------
#
# A weighted marginal IS a target. Tabulating each variable under lvweight
# returns the profile the Times assumed Florida's likely voters had -- without
# anybody having to state it, and without a number being typed here.

profile <- function(v, w) {
  t <- tapply(w, fl[[v]], sum)
  t[is.na(t)] <- 0
  round(100 * t / sum(t), 2)
}
targets <- do.call(rbind, lapply(names(VARS), function(v) {
  data.frame(variable = v, label = unname(VARS[v]),
             level = names(profile(v, ONE)),
             as_collected = as.numeric(profile(v, ONE)),
             registered   = as.numeric(profile(v, fl$rvweight)),
             likely       = as.numeric(profile(v, fl$lvweight)),
             stringsAsFactors = FALSE)
}))
dd_write_csv(targets, "derived/targets.csv")

ed  <- targets[targets$variable == "educ4", ]
BAC <- ed$as_collected[ed$level == "Bachelor's"] + ed$as_collected[ed$level == "Postgraduate"]
BAL <- ed$likely[ed$level == "Bachelor's"]       + ed$likely[ed$level == "Postgraduate"]
fact("bach_collected", BAC, "percent of the raw sample holding at least a bachelor's degree")
fact("bach_likely",    BAL, "and of the electorate the Times weighted to")
fact("bach_move", round(BAL - BAC, 2), "points the weighting moved that share, and in which direction")

# THIS IS NOT THE DIRECTION THE STANDARD STORY PREDICTS, and the check is
# written to record the direction rather than to assume it. The familiar
# account of 2016 is that state polls held too many college graduates and
# weighting should have pushed that share down. Here the Times' likely-voter
# weight pushes it UP, because likely voters really are better educated than
# registered voters and the target was a likely-voter electorate. A check that
# asserted the familiar direction would have failed on correct data -- as the
# first version of this script did.
check("the education profile is not left where it was found", abs(BAL - BAC) > 0.5)
fact("bach_direction", ifelse(BAL > BAC, "up", "down"),
     "whether weighting raised or lowered the college share")

# --- raking ----------------------------------------------------------------
#
# Iterative proportional fitting. Given a set of variables and a target share
# for every level of each, scale the weights until the weighted marginals match
# the targets on all of them at once. This is the workhorse of survey weighting
# and it is twelve lines. What it is NOT is a judgment about who votes: it
# hits whatever target it is handed.

rake <- function(vars, targets_list, w0 = rep(1, nrow(fl)), iters = 40) {
  w <- w0
  for (i in seq_len(iters))
    for (v in vars) {
      tg  <- targets_list[[v]]
      cur <- tapply(w, fl[[v]], sum); cur[is.na(cur)] <- 0
      cur <- cur / sum(cur)
      f   <- tg[names(cur)] / cur
      f[!is.finite(f)] <- 1
      w   <- w * f[as.character(fl[[v]])]
    }
  as.numeric(w * nrow(fl) / sum(w))
}
tget <- function(v, col) {
  t <- targets[targets$variable == v, ]
  setNames(t[[col]] / 100, t$level)
}

# Does raking to the Times' own recovered targets return the Times' own answer?
# It should not, exactly -- they weighted on more than these six variables and
# applied a likely-voter screen this does not -- but it should land close, and
# if it does not the recovery above is wrong.
TL <- lapply(names(VARS), function(v) tget(v, "likely")); names(TL) <- names(VARS)
w_rebuilt <- rake(names(VARS), TL)
fact("margin_rebuilt", margin(w_rebuilt),
     "raking the raw sample to the Times' own recovered targets")
check("the rebuilt weight lands within two points of the published margin",
      abs(margin(w_rebuilt) - M_LV) < 2)

# --- the schemes the chapter fixes -----------------------------------------

schemes <- data.frame(
  scheme = c("As collected",
             "Registered voters (Times)",
             "Likely voters (Times, published)",
             "Rebuilt from recovered targets",
             "Education only",
             "Party registration only",
             "Education and race",
             "Everything at once"),
  what_it_assumes = c(
    "that the people who answered are the electorate",
    "that the electorate looks like Florida's registered voters",
    "that it looks like the Times' model of who turns out",
    "the same targets, reached by raking rather than by the Times' procedure",
    "that only the education skew needs fixing",
    "that only the party mix needs fixing",
    "that education and race need fixing and nothing else",
    "that all six variables need fixing at once"),
  margin = c(
    M_RAW, M_RV, M_LV, margin(w_rebuilt),
    margin(rake("educ4",  TL["educ4"])),
    margin(rake("party3", TL["party3"])),
    margin(rake(c("educ4", "race4"), TL[c("educ4", "race4")])),
    margin(w_rebuilt)),
  stringsAsFactors = FALSE)
schemes$leader <- ifelse(schemes$margin > 0, "Clinton", "Trump")
dd_write_csv(schemes, "derived/schemes.csv")

fact("scheme_spread", round(max(schemes$margin) - min(schemes$margin), 2),
     "points between the widest and narrowest answer these schemes give")

# --- what actually happened -------------------------------------------------
#
# Read from a sibling chapter rather than typed here, so the two cannot
# disagree. Florida 2016, two-party.

HC <- "../../../03-elections/historical-campaigns/data/derived/pres_states_1864_2024.csv"
check("the sibling chapter's state returns are where this expects them",
      file.exists(HC))
h  <- read.csv(HC, stringsAsFactors = FALSE)
fl16 <- h[h$state_abbrev == "FL" & h$year == 2016, ]
check("exactly one Florida 2016 row", nrow(fl16) == 1)
ACT <- round(100 * (fl16$democrat - fl16$republican) /
                   (fl16$democrat + fl16$republican), 2)
fact("actual_margin", ACT, "the two-party margin Florida actually produced in November")
fact("actual_winner", ifelse(ACT > 0, "Clinton", "Trump"), "who carried the state")
fact("published_error", round(M_LV - ACT, 2),
     "points by which the published poll missed, on the two-party margin")
check("the poll and the result disagree about who won Florida",
      sign(M_LV) != sign(ACT))

# --- the respondent file the page ships ------------------------------------
#
# Nine columns of 867 rows. No name, no telephone number, no county -- the
# release carries none of those, which is why this can be committed at all.

keep <- data.frame(
  vote     = fl$vote,
  rvweight = round(fl$rvweight, 4),
  lvweight = round(fl$lvweight, 4),
  educ4    = fl$educ4,
  race4    = fl$race4,
  race_said = fl$race_said,
  party3   = fl$party3,
  age4     = as.character(fl$age4),
  turnout3 = fl$turnout3,
  region5  = fl$region5,
  stringsAsFactors = FALSE)
dd_write_csv(keep, "derived/fl_respondents.csv")
check("the committed respondent file is the whole wave, not a sample",
      nrow(keep) == nrow(fl))
check("it carries nothing that could identify anybody",
      !any(grepl("name|phone|address|county|zip", names(keep), ignore.case = TRUE)))

# The survey's race question and the voter file's race disagree about people.
# That is a fact worth a figure, and it is computed rather than asserted.
both <- keep$race_said != "Refused"
fact("race_disagree", sum(keep$race_said[both] != keep$race4[both]),
     "respondents whose voter-file race differs from the race they gave")
fact("race_disagree_pct",
     round(100 * sum(keep$race_said[both] != keep$race4[both]) / sum(both), 1),
     "percent of those who answered the race question")

# ---------------------------------------------------------------------------

facts <- data.frame(key = names(FACTS),
                    value = vapply(FACTS, function(x) as.character(x$value), ""),
                    note  = vapply(FACTS, function(x) x$note, ""))
dd_write_csv(facts, "derived/facts.csv")
dd_write_csv(do.call(rbind, lapply(CHECKS, as.data.frame)), "derived/checks.csv")
say("done: %d facts, %d checks", nrow(facts), length(CHECKS))

try(prov_stamp(), silent = TRUE)
