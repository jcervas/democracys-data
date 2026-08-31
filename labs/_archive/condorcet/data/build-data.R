# ---------------------------------------------------------------------------
# Build the Condorcet jury theorem dataset.
#
# Five files end up in this folder:
#
#   theorem.csv       The theorem itself: for each competence p and jury size n,
#                     the probability the majority is correct. No source -- this
#                     is arithmetic, and the file says so.
#   independence.csv  Every pair of justices, 2020-2023: how often they actually
#                     voted together, and how often two INDEPENDENT voters with
#                     those same marginal rates would have. The gap is the
#                     assumption failing.
#   effective_n.csv   The nine justices re-expressed as a number of independent
#                     voters, using the design-effect formula.
#   splits.csv        Every term 1946-2023: how the nine divided. Unanimity is
#                     not rare and that is the point.
#   dispositions.csv  What the Court did to the decision below. A majority of
#                     appellate judges said one thing; nine more said another.
#
# WHY THIS DATASET FOR THIS THEOREM. Condorcet needs three things: a binary
# question, voters who are right more often than chance, and INDEPENDENCE. The
# Supreme Court supplies the first in abundance -- every case is a yes or no,
# recorded, for two hundred years. It supplies the third in a form we can
# actually measure, because we observe every justice on every case and can ask
# whether their votes look independent. It does not supply the second, and no
# dataset does. See THE MISSING COLUMN below.
#
# THE MISSING COLUMN. SCDB codes `direction` (1 = conservative, 2 = liberal),
# `majority` (1 = dissent, 2 = majority), `caseDisposition`, and 58 other
# variables -- 61 columns in the justice-centered Citation file, counted from
# raw_columns.txt. It does not code whether the Court was RIGHT, and the codebook
# never pretends to. There is no such column in SCDB, in Voteview, in any
# roll-call file, or in any election return. The theorem's central quantity --
# each voter's probability of choosing the correct alternative -- is not a
# measurement anyone knows how to take on a real political body. This chapter
# is largely about that fact, so the build script does not manufacture a
# stand-in for it. Reversal by a higher court is the closest thing available and
# it is NOT truth; it is authority, plus a severe selection effect, and
# dispositions.csv is labelled accordingly.
#
# INDEPENDENCE, AND HOW IT IS TESTED. For two justices with marginal
# conservative rates p_a and p_b, two independent voters agree with probability
#   p_a * p_b + (1 - p_a) * (1 - p_b)
# on cases they both hear. That is the `expected_indep` column. `observed` is
# what they did. Every pair in the modern court agrees more than independence
# predicts, which is why the theorem's engine -- more voters, more accuracy --
# stalls on a real court.
#
# EFFECTIVE N uses the standard design-effect correction. If n exchangeable
# voters have average pairwise correlation rho, the variance of their sum is
# inflated by 1 + (n - 1) * rho, so the panel carries the information of
#   n_eff = n / (1 + (n - 1) * rho)
# independent voters. rho here is the mean phi coefficient across all 36 pairs,
# computed on the 0/1 conservative vote. This is an ANALOGY, not an identity:
# the design effect is exact for a mean, and majority-correctness is not a mean.
# It is reported as an order-of-magnitude statement and the brief says so.
#
# CODING, from the SCDB codebook:
#   direction        1 = conservative, 2 = liberal (blank where not codeable)
#   majority         1 = dissent, 2 = majority
#   caseDisposition  2 = affirmed; 3 = reversed; 4 = reversed and remanded;
#                    5 = vacated and remanded; 1, 6-11 = other outcomes
#   Cases with blank direction are dropped wherever "voted the same way" has to
#   be defined, because it is undefined without a direction to compare.
#
# WINDOW. 2020-2023 terms for the pairwise work, matching the scdb chapter so
# the two can be read together. Breyer and Jackson each sat for part of it, so
# their pair counts are smaller -- `cases` is recorded rather than hidden.
#
# SOURCE. Supreme Court Database, Washington University, 2024 release 01,
# justice-centered, by citation. http://scdb.wustl.edu/data.php
# NOTE THE PROTOCOL: the SCDB site serves over **http only**. https fails.
#
# Run from this directory:  Rscript build-data.R   (downloads ~1.7 MB)
# ---------------------------------------------------------------------------

options(stringsAsFactors = FALSE)

# --- 1. the theorem, which needs no data ------------------------------------
#
# P(majority of n correct) when each of n is independently correct with prob p.
# n is odd throughout, so there are no ties to break.

maj_correct <- function(p, n) pbinom((n - 1) / 2, n, p, lower.tail = FALSE)

ps <- c(0.45, 0.49, 0.51, 0.55, 0.60, 0.70, 0.80)
ns <- c(1, 3, 5, 9, 15, 25, 51, 101, 501, 1001)

th <- expand.grid(n = ns, p = ps)
th$majority_correct <- round(maj_correct(th$p, th$n), 4)
th <- th[order(th$p, th$n), c("p", "n", "majority_correct")]
write.csv(th, "theorem.csv", row.names = FALSE)

# --- 2. the court ------------------------------------------------------------

URL <- paste0("http://scdb.wustl.edu/_brickFiles/2024_01/",
              "SCDB_2024_01_justiceCentered_Citation.csv.zip")

tmp <- tempfile(fileext = ".zip")
download.file(URL, tmp, mode = "wb", quiet = TRUE)
csv <- unzip(tmp, exdir = tempdir())
s <- read.csv(csv[1], fileEncoding = "UTF-8")

stopifnot(nrow(s) > 80000)

# --- 2a. a real capture of what arrives -------------------------------------
#
# The brief shows students the file as it comes off the server, so these are
# genuine rows, not a reconstruction. One case (Dobbs) across all nine seats,
# and the full column list, which is the point: 240-odd columns and not one of
# them says who was right.

writeLines(names(s), "raw_columns.txt")

dobbs <- s[grepl("DOBBS", toupper(s$caseName)), ]
if (nrow(dobbs) > 0) {
  keep <- c("caseId", "caseName", "term", "justiceName", "vote", "direction",
            "majority", "majVotes", "minVotes", "caseDisposition")
  write.csv(dobbs[, keep], "raw_sample.csv", row.names = FALSE)
} else {
  warning("no Dobbs rows found; raw_sample.csv not written")
}

s$term <- suppressWarnings(as.integer(s$term))

FROM <- 2020; TO <- 2023
w <- s[!is.na(s$term) & s$term >= FROM & s$term <= TO, ]

v <- w[w$direction %in% c(1, 2), c("caseId", "justiceName", "direction")]
v$cons <- as.integer(v$direction == 1)          # 1 = conservative, 0 = liberal
js <- sort(unique(v$justiceName))

vec_of <- function(j) setNames(v$cons[v$justiceName == j],
                               v$caseId[v$justiceName == j])
V <- lapply(js, vec_of); names(V) <- js

# --- 3. observed agreement vs. what independence predicts --------------------

pairs <- t(combn(js, 2))
ind <- data.frame(a = pairs[, 1], b = pairs[, 2],
                  cases = 0L, observed = NA_real_, expected_indep = NA_real_,
                  phi = NA_real_)

for (i in seq_len(nrow(ind))) {
  x <- V[[ind$a[i]]]; y <- V[[ind$b[i]]]
  both <- intersect(names(x), names(y))
  x <- x[both]; y <- y[both]
  n <- length(both)
  ind$cases[i] <- n

  # Jackson replaced Breyer, so that pair never heard a case together and
  # every quantity below is undefined for it. It stays in the file as a row of
  # NA with cases = 0, because a pair that cannot be compared is a fact about
  # the court, not a row to delete quietly.
  if (n < 2) next

  pa <- mean(x); pb <- mean(y)
  ind$observed[i]       <- mean(x == y)
  ind$expected_indep[i] <- pa * pb + (1 - pa) * (1 - pb)

  # phi = Pearson correlation of two binary vectors; undefined if either
  # justice voted the same direction in every shared case.
  ind$phi[i] <- if (sd(x) == 0 || sd(y) == 0) NA_real_ else cor(x, y)
}

ind$gap <- ind$observed - ind$expected_indep
ind <- ind[order(-ind$observed, na.last = TRUE), ]
ind$observed       <- round(100 * ind$observed, 1)
ind$expected_indep <- round(100 * ind$expected_indep, 1)
ind$gap            <- round(100 * ind$gap, 1)
ind$phi            <- round(ind$phi, 3)
write.csv(ind, "independence.csv", row.names = FALSE)

# --- 4. nine justices, expressed as independent voters -----------------------

# SEATS, NOT PEOPLE. Ten justices appear in a four-term window because Ginsburg
# died, Barrett was confirmed, Breyer retired and Jackson replaced him. Only
# nine ever sat at once, and the theorem is about the size of the deciding body,
# so n is the bench (9) and not the number of names in the file (10). Counting
# people here would inflate n with turnover and make the court look more
# independent than it is.
SEATS <- 9L
rho   <- mean(ind$phi, na.rm = TRUE)
eff   <- SEATS / (1 + (SEATS - 1) * rho)

en <- data.frame(
  quantity = c("Seats on the bench",
               "Justices appearing in the window",
               "Pairs",
               "Pairs that never shared a case",
               "Mean pairwise correlation (phi)",
               "Design-effect inflation, 1 + (n-1) * rho",
               "Effective number of independent voters"),
  value = c(SEATS, length(js), nrow(ind), sum(is.na(ind$phi)), round(rho, 3),
            round(1 + (SEATS - 1) * rho, 2), round(eff, 2)))
write.csv(en, "effective_n.csv", row.names = FALSE)

# --- 5. how the nine divide, every term --------------------------------------

sp <- unique(s[, c("caseId", "term", "majVotes", "minVotes")])
sp$majVotes <- suppressWarnings(as.integer(sp$majVotes))
sp$minVotes <- suppressWarnings(as.integer(sp$minVotes))
sp <- sp[!is.na(sp$term) & !is.na(sp$majVotes) & !is.na(sp$minVotes), ]
sp$seated <- sp$majVotes + sp$minVotes
sp <- sp[sp$seated == 9, ]                       # full bench only, so 9-0 means 9-0
sp$split <- paste0(sp$majVotes, "-", sp$minVotes)

tb <- table(sp$term, sp$split)
splits <- data.frame(term = as.integer(rownames(tb)),
                     cases = as.vector(rowSums(tb)))
for (k in c("9-0", "8-1", "7-2", "6-3", "5-4")) {
  share <- if (k %in% colnames(tb)) {
    round(100 * as.vector(tb[, k]) / splits$cases, 1)
  } else {
    rep(0, nrow(splits))
  }
  splits[[gsub("-", "_", k)]] <- share
}
write.csv(splits, "splits.csv", row.names = FALSE)

# --- 6. what happened to the decision below ---------------------------------
#
# NOT a measure of who was right. Cases reach the Court because four justices
# thought something was wrong with them, so the sample is selected on exactly
# the outcome being counted. The file exists to make that selection visible.

dp <- unique(s[, c("caseId", "term", "caseDisposition")])
dp <- dp[!is.na(dp$term), ]
lab <- c("1" = "Stay/petition denied", "2" = "Affirmed", "3" = "Reversed",
         "4" = "Reversed and remanded", "5" = "Vacated and remanded",
         "6" = "Affirmed and reversed in part", "7" = "Affirmed and reversed in part, remanded",
         "8" = "Vacated", "9" = "Petition denied", "10" = "Certification to a lower court",
         "11" = "No disposition")
dp$label <- lab[as.character(dp$caseDisposition)]
dp <- dp[!is.na(dp$label), ]

dd <- as.data.frame(table(dp$label), responseName = "cases")
names(dd)[1] <- "disposition"
dd$pct <- round(100 * dd$cases / sum(dd$cases), 1)
dd <- dd[order(-dd$cases), ]
write.csv(dd, "dispositions.csv", row.names = FALSE)

# "Reversed" is one code among several that all mean the decision below did not
# survive. Reporting the 22% code alone understates it by more than half, which
# is the kind of thing a codebook lets you get wrong.
undone   <- sum(dd$cases[grepl("Reversed|Vacated", dd$disposition)])
affirmed <- sum(dd$cases[dd$disposition == "Affirmed"])

# --- report ------------------------------------------------------------------

cat(sprintf("theorem.csv      : %d rows, p in [%.2f, %.2f], n in [%d, %d]\n",
            nrow(th), min(th$p), max(th$p), min(th$n), max(th$n)))
cat(sprintf("independence.csv : %d pairs, %d justice-votes, terms %d-%d\n",
            nrow(ind), nrow(v), FROM, TO))
cat(sprintf("  comparable pairs: %d   (undefined: %d)\n",
            sum(!is.na(ind$gap)), sum(is.na(ind$gap))))
cat(sprintf("  pairs agreeing MORE than independence predicts: %d of %d\n",
            sum(ind$gap > 0, na.rm = TRUE), sum(!is.na(ind$gap))))
cat(sprintf("effective_n.csv  : rho = %.3f, %d seats behave like %.2f independent voters\n",
            rho, SEATS, eff))
cat(sprintf("  (%d justices appear in the window; only %d sat at once)\n",
            length(js), SEATS))
cat(sprintf("splits.csv       : %d terms, full-bench cases only\n", nrow(splits)))
cat(sprintf("dispositions.csv : %d cases, %.1f%% affirmed, %.1f%% reversed or vacated\n",
            sum(dd$cases), 100 * affirmed / sum(dd$cases),
            100 * undone / sum(dd$cases)))

cat("\n--- the theorem, at a glance ---\n")
print(th[th$n %in% c(1, 9, 101, 1001), ], row.names = FALSE)

cat("\n--- most and least independent-looking pairs ---\n")
print(head(ind[, c("a", "b", "cases", "observed", "expected_indep", "gap")], 3),
      row.names = FALSE)
print(tail(ind[, c("a", "b", "cases", "observed", "expected_indep", "gap")], 3),
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
