# ---------------------------------------------------------------------------
# Build the election-returns-instrument dataset: the outcome, and nothing else.
#
# The fourth SOURCE chapter, after surveys, the census and voter files. Its
# subject is the instrument: what a certified election return establishes that
# nothing else can, the one thing it can never establish at any level of
# detail, and why there is no such thing as a national election return.
#
# Four files end up in this folder:
#
#   derived/ladder.csv       The aggregation ladder, from one ballot to one nation.
#   derived/ecological.csv   What aggregate returns say about who voted how, beside
#                    the truth from cast vote records. The estimates are not
#                    merely wrong; they are impossible.
#   derived/national.csv     What happens when you add up every county in the country
#                    and compare the sum to the certified national total.
#   derived/sources.csv      Who actually publishes returns, at what grain, and what
#                    each publisher is obliged to do.
#
# THE ARGUMENT. A survey elicits, a census enumerates, a voter file records an
# administrative status. An election return records an OUTCOME. It is the only
# source in this book that reports the thing democracy is for -- who won -- and
# it is complete rather than sampled, official rather than compiled, and
# available in some form back to the founding.
#
# It also has one absolute limit, and the limit is constitutional rather than
# technical. THE BALLOT IS SECRET. No return anywhere links a vote to a voter,
# and no amount of detail approaches it: even a precinct return is a total.
# Everything anyone claims about WHICH KINDS OF PEOPLE voted which way, from
# aggregate returns alone, is an inference across that gap.
#
# HOW BADLY THAT INFERENCE CAN FAIL, measured rather than asserted. The
# levels-of-aggregation chapter has something almost nothing else in political
# data has: an ANSWER KEY. Georgia's cast vote records report, ballot by
# ballot, how mail voters actually voted. So the standard ecological method can
# be run on the aggregate returns and then graded:
#
#   from 2,653 precincts : 194.0% of mail voters chose the Democrat
#   from   159 counties  : 233.4%
#   the truth            :  64.5%
#
# The estimates are above one hundred per cent. They are not near-misses; they
# are outside the range of things a percentage can be, and the method reports
# them without complaint. Note also that the COUNTY estimate is worse than the
# PRECINCT one: aggregating further makes it worse, not better.
#
# WHY THERE IS NO NATIONAL ELECTION RETURN. Elections in the United States are
# administered by states and, in practice, by several thousand counties and
# municipalities. No federal agency counts votes. What exists is compilation --
# by the FEC biennially, by the Clerk of the House for House races, and
# otherwise by researchers and newsrooms. national.csv shows the cost: sum
# every county in the country and the total does not match the certified
# national figure, and the same state's returns differ between the state's own
# publication and a national compilation of it.
#
# SOURCES. Everything here is read from files the corpus already built, so the
# provenance is a chapter rather than a URL:
#   ../../levels-of-aggregation/data/derived/ladder.csv, eco_estimates.csv, consistency.csv
#     -- built from Georgia SoS precinct returns, Georgia cast vote records,
#        and a national county compilation.
# Nothing is fetched. Run from this directory:  Rscript build-data.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

options(stringsAsFactors = FALSE)

L <- "../../levels-of-aggregation/data"
stopifnot(file.exists(file.path(L, "derived/ladder.csv")),
          file.exists(file.path(L, "derived/eco_estimates.csv")),
          file.exists(file.path(L, "derived/consistency.csv")))

# --- 1. the ladder ----------------------------------------------------------

lad <- read.csv(file.path(L, "derived/ladder.csv"), check.names = FALSE)
keep <- intersect(c("rung", "level", "units_nationally", "units_in_georgia",
                    "a_row_is", "published_for"), names(lad))
lad <- lad[, keep]
write.csv(lad, "derived/ladder.csv", row.names = FALSE)

# --- 2. the answer key ------------------------------------------------------

eco <- read.csv(file.path(L, "derived/eco_estimates.csv"), check.names = FALSE)
ec <- data.frame(
  computed_from = paste0(format(eco$units, big.mark = ","), " ",
                         ifelse(eco$level == "county", "counties",
                                paste0(eco$level, "s"))),
  estimate_mail = eco$est_by_mail,
  estimate_not_mail = eco$est_not_by_mail,
  truth_mail = eco$truth_by_mail,
  truth_not_mail = eco$truth_not_by_mail,
  error = round(eco$est_by_mail - eco$truth_by_mail, 1),
  impossible = ifelse(eco$est_by_mail > 100 | eco$est_by_mail < 0 |
                      eco$est_not_by_mail > 100 | eco$est_not_by_mail < 0,
                      "yes", "no"))
write.csv(ec, "derived/ecological.csv", row.names = FALSE)

# --- 3. the national total that is not a total ------------------------------
#
# consistency.csv arrives with embedded thousands separators inside quoted
# fields, so the numeric columns come back as text with commas in them. Strip
# them rather than trusting read.csv to have done something sensible.

con <- read.csv(file.path(L, "derived/consistency.csv"), check.names = FALSE)
num <- function(x) as.numeric(gsub("[^0-9.-]", "", as.character(x)))
nat <- data.frame(
  comparison = con$comparison,
  one_source = con$from,
  its_total = num(con$value_from),
  other_source = con$against,
  their_total = num(con$value_against),
  difference = num(con$difference))
nat$pct <- round(100 * nat$difference / nat$their_total, 4)
write.csv(nat, "derived/national.csv", row.names = FALSE)

# --- 4. who publishes returns -----------------------------------------------
#
# Hand-authored from the chapters that use each source. Every row names an
# obligation, because the obligation is what determines the grain.

src <- data.frame(
  publisher = c("A county election office",
                "A state's chief election officer",
                "The Clerk of the U.S. House",
                "The Federal Election Commission",
                "A university or newsroom compilation"),
  what_it_publishes = c(
    "Its own precinct and absentee returns, in its own format",
    "The certified statewide result, and usually precinct returns",
    "Official House results, biennially, as a PDF",
    "Federal Elections, a biennial workbook of federal races",
    "A national file assembled from the above"),
  finest_grain = c("Precinct", "Precinct", "District", "District or county",
                   "Precinct or county"),
  obliged_to = c("Certify a lawful count for its own jurisdiction",
                 "Certify the statewide result",
                 "Record the results of House elections",
                 "Report on federal elections",
                 "Nothing -- it is a voluntary act"),
  correction_path = c("Recount and amendment under state law",
                      "Recount and amendment under state law",
                      "Errata in a later edition",
                      "Errata in a later edition",
                      "Whatever the compiler decides"))
write.csv(src, "derived/sources.csv", row.names = FALSE)

# --- report -----------------------------------------------------------------

cat("\nladder.csv     : one ballot to one nation\n")
print(lad[, c("rung", "level", "units_nationally")], row.names = FALSE)
cat("\necological.csv : aggregate returns, graded against cast vote records\n")
print(ec[, c("computed_from", "estimate_mail", "truth_mail", "error",
             "impossible")], row.names = FALSE)
cat(sprintf("\n  Both estimates exceed 100%%. The county figure (%.1f) is worse\n",
            ec$estimate_mail[2]))
cat(sprintf("  than the precinct figure (%.1f): aggregating further hurts.\n",
            ec$estimate_mail[1]))
cat("\nnational.csv   : adding up the country\n")
print(nat[, c("comparison", "its_total", "their_total", "difference")],
      row.names = FALSE)
cat("\nsources.csv    :", nrow(src), "publishers, and what each is obliged to do\n")

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
