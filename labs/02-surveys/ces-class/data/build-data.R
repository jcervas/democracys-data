# ---------------------------------------------------------------------------
# Build the ces-class dataset: the class against the CES.
#
# Two files live in this folder:
#
#   derived/ces2024_benchmarks.csv     Every category of seven core CES variables, with
#                              its unweighted AND weighted share of the 2024
#                              Cooperative Election Study (n = 60,000).
#   derived/class_responses_EXAMPLE.csv  A stand-in so the lab runs before the class
#                              has taken the survey. REPLACE IT.
#
# ---------------------------------------------------------------------------
# BEFORE YOU TEACH THIS: replace the example file.
#
# Export the Google Form responses from the first-day CES replication, recode
# them to the CES numeric codes below, and save as raw/class_responses.csv with two
# columns: variable, code. One row per student per question. The lab reads
# raw/class_responses.csv if it exists and falls back to the example if it does not,
# printing a loud warning either way.
# ---------------------------------------------------------------------------
#
# SOURCE. Cooperative Election Study Common Content, 2024 (Data Release 2),
# Ansolabehere, Schaffner & Pope, Harvard Dataverse,
# doi:10.7910/DVN/X11EP6 -- file CCES24_Common_OUTPUT_vv_topost_final.csv
# (184 MB, 60,000 respondents, 694 variables). Weight: `commonweight`.
#
# VALUE LABELS WERE VERIFIED against the Guide to the 2024 CES (April 2025),
# Part III, not from memory. That mattered: the guide DISPLAYS categories in a
# different order than their numeric codes. For `race`, the guide lists Middle
# Eastern sixth, but the count that matches code 6 is "Two or more races";
# Middle Eastern is code 8. The mapping below is by COUNT, which is the only
# thing that can be checked.
#
#   gender4  1 Man  2 Woman  3 Non-binary  4 Other
#   educ     1 No HS  2 HS grad  3 Some college  4 2-year  5 4-year  6 Post-grad
#   race     1 White  2 Black  3 Hispanic  4 Asian  5 Native American
#            6 Two or more races  7 Other  8 Middle Eastern
#   pid7     1 Strong Dem  2 Not very strong Dem  3 Lean Dem  4 Independent
#            5 Lean Rep  6 Not very strong Rep  7 Strong Rep  8 Not sure
#   ideo5    1 Very liberal  2 Liberal  3 Moderate  4 Conservative
#            5 Very conservative  6 Not sure
#   newsint  1 Most of the time  2 Some of the time  3 Only now and then
#            4 Hardly at all
#   votereg  1 Yes  2 No  3 Don't know
#
# THE HEADLINE THE BENCHMARKS CARRY. Weighting moves "registered to vote" from
# 91.5% to 72.5% -- nineteen points, the largest single adjustment in the file.
# An opt-in online panel is far more politically engaged than the country, and
# the weights are doing that much work before anybody reports a result.
#
# Rebuilding the benchmarks requires the 184 MB CES file; the derived CSV is
# committed so nobody has to download it.
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

set.seed(84355)   # the example file must be reproducible, and obviously fake

b <- read.csv("derived/ces2024_benchmarks.csv", stringsAsFactors = FALSE)

# A stand-in class: n = 19, skewed the way a room of CMU undergraduates will
# actually be skewed (young, educated, more liberal, less registered).
n <- 19
draw <- function(v, p) data.frame(
  variable = v,
  code = sample(b$code[b$variable == v], n, replace = TRUE, prob = p))

ex <- rbind(
  draw("gender4", c(.45, .45, .08, .02)),
  draw("educ",    c(.00, .05, .80, .05, .10, .00)),
  draw("race",    c(.40, .10, .10, .30, .01, .06, .02, .01)),
  draw("pid7",    c(.25, .15, .20, .20, .08, .07, .03, .02)),
  draw("ideo5",   c(.20, .35, .30, .10, .03, .02)),
  draw("newsint", c(.35, .40, .18, .07)),
  draw("votereg", c(.70, .25, .05)))

write.csv(ex, "derived/class_responses_EXAMPLE.csv", row.names = FALSE)

cat("benchmarks:", nrow(b), "rows,", length(unique(b$variable)), "variables\n")
cat("example class file:", nrow(ex), "rows for n =", n, "students\n")
cat("\nlargest weighting adjustments in CES 2024:\n")
b$shift <- b$pct_weighted - b$pct_unweighted
print(head(b[order(-abs(b$shift)), c("variable","category","pct_unweighted","pct_weighted","shift")], 4),
      row.names = FALSE)

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
