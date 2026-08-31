# ---------------------------------------------------------------------------
# precision.R — stop derived tables claiming precision they do not have.
#
# WHY THIS EXISTS
#
# R computes in double precision and write.csv() prints all of it. A mean age
# comes out as 43.368932038835, a county's entropy as 0.917769220198584, a
# projected coordinate as 885.614812711957. Every one of those digits is real
# arithmetic — and after the fourth or fifth, none of them is information.
#
# Two costs, and the second is the one this course cares about. The small cost
# is noise: the digits change between builds when a summation order shifts, so
# every rebuild produces a diff nobody can read. The large cost is that a
# student opening the file sees a share of the vote given to thirteen decimal
# places and has no way to know that the last nine are an artifact of binary
# floating point rather than a measurement anybody made.
#
# The related-but-different case is a source stored as a 32-bit float, where
# the extra digits are not even arithmetic — they are storage residue. See
# house-competition/data/build-data.R, which cuts at seven significant digits
# because that is the width of a Stata `float`.
#
# WHAT IT DOES
#
#     dd_write_csv(d, "derived/facts.csv")          # 6 significant digits
#     dd_write_csv(d, "derived/probs.csv", 8)       # when 6 is not enough
#
# Every non-integer numeric column is rounded to `digits` SIGNIFICANT digits and
# the frame is then written exactly as write.csv() would write it.
#
# SIGNIFICANT DIGITS, NOT DECIMAL PLACES, because these tables mix quantities
# whose scales are nothing like each other. Six decimal places is absurd on a
# projected coordinate in kilometres and destroys a probability of 2.7e-3; six
# significant figures is right for both. The exceptions are quantities produced
# by SUBTRACTING two near-equal numbers — a margin, a residual — where the
# leading digits cancel and the error is absolute rather than relative. Round
# those to decimal places, at the point they are computed, and do not rely on
# this function to do it for you.
#
# INTEGER PRECISION IS NEVER LOST, and this is the whole safety argument for
# applying the thing at the write boundary. signif(145303625, 6) is 145304000 --
# it invents 375 citizens. The first version of this function tried to avoid
# that by skipping any column whose values were all whole numbers, which is not
# enough: `cps_turnout.csv` carries a population of 145,303,625 in the same
# frame as an over-report rate of 0.877629343527957, and one fractional value
# anywhere in a column exposed every integer beside it.
#
# So the rule is per value, not per column: **round to `digits` significant
# digits or to the value's own integer width, whichever is more.** A population
# keeps all nine of its digits; a rate keeps six; nothing is ever rounded to
# coarser than a whole number. Integers are therefore untouched by
# construction, and so are the integer parts of large non-integers.
#
# Character columns, factors, dates and logicals pass through untouched, so a
# GEOID stored as "01001" keeps its leading zero.
#
# Used by: any chapter. Source it with
#   source("../../../_lib/precision.R")  # from a chapter's data/ folder
#
# Three levels, not two: chapters live under a part directory, so the path out
# of <part>/<chapter>/data/ to labs/ is ../../.. — this comment said two until
# August 2026 and was the only place in the corpus still saying so.
# ---------------------------------------------------------------------------

#' The rule itself, for a bare vector. Use it where a number is turned into
#' something else -- a `fact()` table of key/value strings, a label, a caption --
#' because by the time such a value reaches the CSV it is character, and
#' dd_signif() below will rightly leave it alone.
dd_num <- function(x, digits = 6) {
  if (!is.numeric(x) || is.integer(x)) return(x)
  ok <- !is.na(x) & is.finite(x)
  if (!any(ok)) return(x)
  # digits to keep = the larger of `digits` and the value's integer width, so
  # nothing is ever rounded coarser than a whole number. See the header.
  width <- ifelse(abs(x[ok]) >= 1, floor(log10(abs(x[ok]))) + 1, 1)
  x[ok] <- signif(x[ok], pmax(digits, width))
  x
}

dd_signif <- function(d, digits = 6) {
  for (nm in names(d)) {
    if (!is.numeric(d[[nm]]) || is.integer(d[[nm]])) next
    d[[nm]] <- dd_num(d[[nm]], digits)
  }
  d
}

dd_write_csv <- function(d, path, digits = 6, ...) {
  write.csv(dd_signif(d, digits), path, row.names = FALSE, ...)
}
