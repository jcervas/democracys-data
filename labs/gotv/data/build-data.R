# ---------------------------------------------------------------------------
# Build the gotv dataset.
#
# One file ends up in this folder:
#
#   derived/gotv_tactics.csv    Turnout effect and cost per additional vote for the
#                       main voter-mobilization tactics.
#
# SOURCE. Donald P. Green and Alan S. Gerber, "Get Out the Vote: How to
# Increase Voter Turnout", 5th ed. (Washington, DC: Brookings Institution
# Press, 2024). Table 12-1, pp. 172-73, for the printed price list; appendices
# A (canvassing, pp. 189-93), B (direct mail, pp. 195-99) and C (phone calls,
# pp. 201-04) for the pooled effects, confidence intervals and study counts.
# Sides et al. reproduce the Table 12-1 figures as their Table 12.1; we cite
# Green and Gerber directly because they are the people who ran the experiments.
#
# WHAT THE NUMBERS MEAN. Green and Gerber synthesise hundreds of randomised
# field experiments. "1 additional vote per 17 contacted" means: knock on 17
# doors and *speak to* 17 people, and on average one more person votes than
# would have otherwise. It is not "one of the 17 voted" -- 17 people contacted
# might produce 12 voters where 11 would have voted anyway. The denominator is
# people CONTACTED, not people approached, which matters enormously because
# most doors do not open.
#
# But "contacted" does not mean the same thing in every row, and the book says
# so in footnote b to Table 12-1: for canvassing and live calls it is talking to
# the target voter; for robocalls it is *attempting* to reach one, because
# robocalls typically leave voicemail; for mail it is a piece sent; for leaflets
# a leaflet dropped. For festivals, television and radio a "contact" is an
# entire precinct or media market. The `unit` column records which.
#
# TWELVE ROWS, NOT EIGHT. Table 12-1 prints twelve tactics. Earlier versions of
# this file carried eight, silently dropping text messages, election festivals,
# television and radio. Two of those matter for the argument: festivals cost $85
# per vote on Election Day and roughly $40 at early voting sites, which is
# cheaper than anything else in the table.
#
# THE BLANK CELLS ARE NOT BLANK IN THE BOOK. Where Table 12-1 prints an asterisk
# ("cost-effectiveness is not calculated for tactics that are not proven to
# raise turnout"), the effectiveness column often still carries a point
# estimate, and the appendices carry an interval. Leafleting is one vote per
# 189; television is 0.5 points; radio is 1 point; advocacy mail is 0.085 points
# with a 95% CI of -0.065 to 0.235. Those are recorded here rather than thrown
# away, because "we do not know" and "we know it is small and could be zero" are
# different claims that look identical as NA.
#
# UNCERTAINTY. Three appendices publish a standard error for every one of 229
# distinct experiments and a 95% confidence interval for every pooled estimate.
# effect_pp / ci_low / ci_high / n_studies carry those. They are NA only where
# the book runs no meta-analysis (text messages, festivals, television, radio,
# leaflets, e-mail).
#
# RESOLVED, and this replaces the note that used to say the robocall flag had
# never been checked. It has now been checked, against Green & Gerber 5th ed.
# pp. 172-73 and appendix C p. 204. The verdict is that BOTH the flag and the
# number are defensible only under a reading the table's own caption
# contradicts:
#
#   Appendix C, p. 204: pooled robocall ITT is 0.235 points, 95% CI 0.037 to
#   0.433, across nine studies. 1/0.00235 = 425, which is exactly the figure
#   Table 12-1 prints. That estimate INCLUDES the two robocall studies that
#   used social pressure scripts.
#
#   The same paragraph: excluding those two studies, the estimate "drops to
#   0.143 with a confidence interval ranging from -0.037 to 0.323". That
#   implies one vote per 699 targeted numbers, and the interval CONTAINS ZERO.
#
#   But the Table 12-1 cell reads: "One vote per 425 landlines targeted,
#   *without social pressure messages*." The number and the qualifier come from
#   different estimates. On the caption's own stated scope the reliability
#   column should read "not significantly greater than zero", as it does for
#   leafleting, television and radio.
#
# So the file carries both rows and lets the reader see the fork. Do not delete
# the second one to tidy the table: it is the finding.
#
# ELECTION FESTIVALS ARE FALSE, and this is a judgment call rather than a
# transcription. Table 12-1 prints a cost per vote for festivals ($85 on Election
# Day, roughly $40 at early voting sites), which is the book's own signal that it
# considers them proven -- the asterisk footnote says cost-effectiveness is *not*
# calculated for tactics that are not proven to raise turnout. But the
# reliability column for that row does not say "yes" in any form. It says
# "results vary widely across seven studies, two of which focus on early
# voting", which is the only entry in the whole table that describes dispersion
# instead of rendering a verdict, and chapter 8 hedges it further ("if the
# weather cooperates"). There is no appendix for festivals, so there is no
# interval to appeal to.
#
# Marking it FALSE therefore disagrees with the book's own pricing decision, on
# the grounds that a sentence about variability is not a finding of reliability.
# It makes festivals the only row in the file that is FALSE while carrying a
# price the book itself printed. (The robocall variant above is also FALSE with a
# price, but that price is one we computed, not one Green and Gerber published.)
# The oddity is deliberate, and the brief discusses it rather than hiding it.
#
# Flipping this line back to TRUE makes festivals the cheapest tactic in the
# table at $85, ahead of volunteer phone banking at $45, and changes what the
# chapter's central ranking says. Whoever changes it should say why here.
#
# Run from this directory:  Rscript build-data.R      (no internet needed)
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

options(stringsAsFactors = FALSE)

# Columns, in order:
#   tactic        as named in Table 12-1
#   unit          what one "contact" is for this row (footnote b, p. 173)
#   contacts_per_vote, cost_per_vote
#                 as PRINTED in Table 12-1. NA where the book prints "*".
#   reliability   the book's own words in the "Is effect statistically
#                 reliable?" column, condensed. This is a sentence, not a flag.
#   effective     the boolean this lab derives from that sentence. Every
#                 gradation between "large number of studies" and "results vary
#                 widely" is destroyed here, on purpose, so that the cost of
#                 doing it is visible.
#   effect_pp     pooled effect in percentage points from appendix A, B or C.
#   ci_low/ci_high 95% confidence interval on effect_pp.
#   n_studies     distinct experiments behind effect_pp.
tactics <- data.frame(
  tactic = c("Door-to-door canvassing",
             "Leaflets left on doors",
             "Direct mail, from a campaign",
             "Direct mail, nonpartisan",
             "Phone calls, volunteer",
             "Phone calls, commercial telemarketer",
             "Robocalls",
             "Robocalls, excluding social-pressure studies",
             "Email",
             "Text messages",
             "Election festivals",
             "Television GOTV",
             "Radio GOTV"),
  unit = c("conversation", "leaflet dropped", "mailer sent", "mailer sent",
           "conversation", "conversation", "number targeted", "number targeted",
           "message sent", "number targeted", "precinct", "media market",
           "media market"),
  contacts_per_vote = c(17, 189, NA, 260, 36, 106, 425, NA, NA, 381, NA, NA, NA),
  cost_per_vote     = c(57, NA, NA, 130, 45, 106, 64, NA, NA, 133, 85, NA, NA),
  reliability = c("Yes, large number of studies",
                  "Not significantly greater than zero",
                  "Yes, large number of studies (no detectable effect)",
                  "Yes, large number of studies",
                  "Yes, large number of studies",
                  "Yes, large number of studies",
                  "Yes, large number of studies",
                  "Interval contains zero (not printed in Table 12-1)",
                  "Average effect cannot be large",
                  "Yes, several large studies",
                  "Results vary widely across seven studies",
                  "Not significantly greater than zero",
                  "Not significantly greater than zero"),
  effective   = c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE,
                  FALSE, TRUE, FALSE, FALSE, FALSE),
  effect_pp   = c(4.0, NA, 0.085, 0.384, 2.78, 0.94, 0.235, 0.143,
                  NA, 0.26, 1.0, 0.5, 1.0),
  ci_low      = c(2.8, NA, -0.065, 0.117, 1.74, 0.52, 0.037, -0.037,
                  NA, NA, NA, NA, NA),
  ci_high     = c(5.2, NA, 0.235, 0.650, 3.83, 1.36, 0.433, 0.323,
                  NA, NA, NA, NA, NA),
  n_studies   = c(59, NA, NA, 65, 32, 22, 9, 7, NA, NA, 7, NA, NA)
)

# The robocall fork, worked out rather than asserted. The book supplies the
# effect (0.143) and the unit cost ($0.15 per targeted number for a series of
# three calls, Table 12-1); the arithmetic joining them is ours, and is the same
# arithmetic the book itself uses for the 425 / $64 row.
ROBO_UNIT_COST <- 0.15
i <- tactics$tactic == "Robocalls, excluding social-pressure studies"
tactics$contacts_per_vote[i] <- round(100 / tactics$effect_pp[i])
tactics$cost_per_vote[i]     <- round(tactics$contacts_per_vote[i] * ROBO_UNIT_COST)

# Derived. cost_per_contact is arithmetic, not evidence: it is cost_per_vote
# divided by contacts_per_vote and carries no information the two columns beside
# it do not already have.
tactics$cost_per_contact <- round(tactics$cost_per_vote / tactics$contacts_per_vote, 2)

# Also derived, and this one is a CHECK rather than a convenience. If a pooled
# effect of X percentage points is what stands behind a row, then the implied
# contacts per additional vote is 100/X. Where that disagrees with what Table
# 12-1 prints, the printed figure is resting on something other than the pooled
# all-studies estimate -- and the reader should be told which.
tactics$contacts_per_vote_pooled <- round(100 / tactics$effect_pp)

write.csv(tactics, "derived/gotv_tactics.csv", row.names = FALSE)
cat("wrote gotv_tactics.csv:", nrow(tactics), "tactics,",
    sum(tactics$effective), "with a measurable effect\n\n")

print(tactics[, c("tactic", "unit", "contacts_per_vote", "cost_per_vote",
                  "effective", "effect_pp", "ci_low", "ci_high")])

cat("\n--- the thing to notice ---\n")
e <- tactics[tactics$effective, ]
cat("Most effective per contact  :", e$tactic[which.min(e$contacts_per_vote)],
    "(1 vote per", min(e$contacts_per_vote, na.rm = TRUE), "contacts)\n")
cat("Cheapest per vote           :", e$tactic[which.min(e$cost_per_vote)],
    "($", min(e$cost_per_vote, na.rm = TRUE), "per vote)\n")
cat("These are not the same tactic. That is the lab.\n")

cat("\n--- does the printed figure match the pooled estimate? ---\n")
chk <- tactics[!is.na(tactics$contacts_per_vote) &
               !is.na(tactics$contacts_per_vote_pooled), ]
# Tolerance is proportional, not absolute: the pooled effects are printed to two
# or three significant figures, so back-computing 100/effect_pp lands a few
# contacts away from the printed figure on the larger denominators. 5% keeps
# rounding noise out of the way of a real disagreement.
chk$agrees <- ifelse(abs(chk$contacts_per_vote - chk$contacts_per_vote_pooled) /
                       chk$contacts_per_vote <= 0.05, "yes", "NO")
print(chk[, c("tactic", "contacts_per_vote", "contacts_per_vote_pooled", "agrees")],
      row.names = FALSE)
cat("\nEvery row agrees to within rounding except door-to-door canvassing, where\n",
    "Table 12-1's 17 comes from the 30-50% baseline-turnout subgroup (CACE 6.0,\n",
    "appendix A table A-2) rather than the all-studies pooled estimate of 4.0,\n",
    "which implies 25. The headline canvassing price is the best-targeting price.\n",
    sep = "")

cat("\n--- how much uncertainty the printed table drops ---\n")
u <- tactics[!is.na(tactics$ci_low), ]
cat(nrow(u), "of", nrow(tactics), "rows have a published 95% CI.",
    sum(u$ci_low < 0), "of those intervals contain zero.\n")
cat("None of it appears in Table 12-1.\n")

# ---------------------------------------------------------------------------
# Build stamp. Records which script produced what is now in this directory --
# every file under derived/ and raw/ with its size, hash and row count, and the
# date this ran -- into BUILD-STAMP.tsv beside the data. See
# ../../_lib/provenance.R. Two levels up, not three: this chapter sits at the
# labs root rather than inside a numbered part. Guarded, because a missing
# helper must not fail a build that was otherwise fine.
if (file.exists("../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../_lib/provenance.R")
  prov_stamp()
}
