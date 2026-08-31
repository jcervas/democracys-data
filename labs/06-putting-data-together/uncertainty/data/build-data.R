# ---------------------------------------------------------------------------
# Build the uncertainty dataset: one surname model, scored, with intervals.
#
# Six files end up in derived/:
#
#   derived/roc.csv       the ROC curve for P(Black), one row per threshold
#   derived/thresh.csv    the confusion counts at every threshold, so the
#                         figure can report them rather than recompute them
#   derived/by_group.csv  recall and precision per racial group, with intervals
#   derived/by_decile.csv the same quantity in ten strata of block composition
#   derived/calib.csv     predicted against observed, in bins
#   derived/facts.csv     single numbers the brief quotes
#
# Run this script from inside the data/ folder.
# ---------------------------------------------------------------------------

dir.create("derived", showWarnings = FALSE)
options(scipen = 999, stringsAsFactors = FALSE)

# --- Sources ----------------------------------------------------------------
#
# Three files, all already committed elsewhere in this book:
#
#   ../../bisg-check/data/derived/houston_voters.csv
#       49,771 registered voters in Houston County, Georgia, with a surname, a
#       census block, and a SELF-REPORTED race. Georgia is one of the few
#       states that records race on the registration form, which is what makes
#       this the only place in the book where an inferred quantity can be
#       graded. The bisg-check chapter's build script drops the rows whose race
#       was itself filled in by BISG; scoring BISG against BISG would produce a
#       beautiful and meaningless result.
#
#   ../../bisg-check/data/derived/houston_blocks.csv
#       the same county's census blocks, by race.
#
#   ../../../01-census-bureau/surnames/data/derived/census_surnames.csv
#       the Census Bureau's surname list: for each of 162,254 names, the share
#       of people carrying it who reported each race.
#
# This chapter does not argue about whether BISG works -- the bisg-check
# chapter does that. It uses BISG as the one worked example in the book where
# an estimate can be checked, and asks a different question: how much of any
# single number here is noise.

V <- "../../bisg-check/data/derived/houston_voters.csv"
B <- "../../bisg-check/data/derived/houston_blocks.csv"
S <- "../../../01-census-bureau/surnames/data/derived/census_surnames.csv"
stopifnot(file.exists(V), file.exists(B), file.exists(S))

vo <- read.csv(V, stringsAsFactors = FALSE, colClasses = c(GEOID20 = "character"))
bl <- read.csv(B, stringsAsFactors = FALSE, colClasses = c(GEOID20 = "character"))
sn <- read.csv(S, stringsAsFactors = FALSE)

R <- c("white", "black", "hispanic", "asian", "aian")

# --- BISG, in full ----------------------------------------------------------
#
# Bayes' rule with two inputs. u(r|s) is what the surname says; P(g|r) is what
# the geography says; the posterior is proportional to their product.
#
#     P(r | surname, block)  ∝  P(r | surname) × P(block | r)
#
# P(block | r) is the share of the county's members of race r who live in that
# block -- NOT the racial composition of the block, which is a different
# quantity and the most common way to get this wrong.

su <- as.matrix(sn[, c("pctwhite", "pctblack", "pcthispanic", "pctapi", "pctaian")])
colnames(su) <- R
su[is.na(su)] <- 0
su <- su / pmax(rowSums(su), 1e-9)
rownames(su) <- sn$name

bm <- as.matrix(bl[, R]); rownames(bm) <- bl$GEOID20
Nr <- colSums(bm)
gm <- sweep(bm, 2, pmax(Nr, 1), "/")          # P(block | race)

u <- su[match(toupper(vo$surname), rownames(su)), , drop = FALSE]
HIT <- !is.na(u[, 1])

# THE SURNAMES THAT ARE NOT ON THE LIST. The Census publishes a name only if at
# least 100 people carry it, so rare names -- and every misspelling -- are
# absent. Those voters get the national distribution of surnames instead, which
# is the standard fallback and is a much weaker prior. The count is written to
# facts.csv because it is 7.5% of the sample and it is not a random 7.5%.
marg <- colSums(su * sn$count, na.rm = TRUE); marg <- marg / sum(marg)
u[!HIT, ] <- matrix(marg, sum(!HIT), length(R), byrow = TRUE)

g <- gm[match(vo$GEOID20, rownames(gm)), , drop = FALSE]
post <- u * g
post <- post / pmax(rowSums(post), 1e-12)
colnames(post) <- R

vo$p_black <- post[, "black"]
vo$pred    <- R[max.col(post)]
vo$hit     <- HIT
TRUTH <- vo$race == "black"

# --- Wilson intervals -------------------------------------------------------
#
# A proportion near 0 or 1 has an asymmetric interval, and the textbook normal
# approximation puts one end outside [0,1] -- which is exactly the regime the
# smallest group here sits in. Wilson's interval does not. It is written out
# rather than imported so the arithmetic is visible.

wilson <- function(k, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  p <- k / n
  d <- 1 + z^2 / n
  c(centre = (p + z^2 / (2 * n)) / d,
    lo = (p + z^2 / (2 * n) - z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / d,
    hi = (p + z^2 / (2 * n) + z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / d)
}
# checked against binom.test's score interval on a few cases
stopifnot(abs(wilson(39, 100)["lo"] -
              prop.test(39, 100, correct = FALSE)$conf.int[1]) < 1e-8)

# --- 1. Per group -----------------------------------------------------------

grp <- do.call(rbind, lapply(R, function(rr) {
  n_true <- sum(vo$race == rr)
  k_rec  <- sum(vo$race == rr & vo$pred == rr)
  n_pred <- sum(vo$pred == rr)
  k_pre  <- k_rec
  wr <- wilson(k_rec, n_true)
  wp <- if (n_pred > 0) wilson(k_pre, n_pred) else c(centre = NA, lo = NA, hi = NA)
  data.frame(group = rr, n_true = n_true, n_pred = n_pred,
             recall = round(100 * k_rec / n_true, 2),
             rec_lo = round(100 * wr["lo"], 2), rec_hi = round(100 * wr["hi"], 2),
             precision = round(100 * k_pre / max(n_pred, 1), 2),
             pre_lo = round(100 * wp["lo"], 2), pre_hi = round(100 * wp["hi"], 2),
             stringsAsFactors = FALSE)
}))
ACC <- mean(vo$pred == vo$race)
wa  <- wilson(sum(vo$pred == vo$race), nrow(vo))
grp <- rbind(grp, data.frame(
  group = "ALL (pooled)", n_true = nrow(vo), n_pred = nrow(vo),
  recall = round(100 * ACC, 2),
  rec_lo = round(100 * wa["lo"], 2), rec_hi = round(100 * wa["hi"], 2),
  precision = NA, pre_lo = NA, pre_hi = NA, stringsAsFactors = FALSE))
write.csv(grp, "derived/by_group.csv", row.names = FALSE)

# --- 2. Per decile of block composition -------------------------------------
#
# The same estimate, computed in ten strata. Nothing about the model changes
# between rows; only the neighbourhood does.

bshare <- bm[, "black"] / pmax(rowSums(bm), 1)
vo$bshare <- bshare[match(vo$GEOID20, rownames(bm))]
brk <- quantile(vo$bshare, seq(0, 1, 0.1), na.rm = TRUE)
brk[1] <- -Inf; brk[length(brk)] <- Inf
vo$dec <- cut(vo$bshare, brk, labels = FALSE, include.lowest = TRUE)

dec <- do.call(rbind, lapply(sort(unique(vo$dec)), function(i) {
  z <- vo[vo$dec == i, ]
  n <- sum(z$race == "black"); k <- sum(z$race == "black" & z$pred == "black")
  w <- if (n > 0) wilson(k, n) else c(centre = NA, lo = NA, hi = NA)
  data.frame(decile = i,
             block_black_lo = round(100 * min(z$bshare, na.rm = TRUE), 1),
             block_black_hi = round(100 * max(z$bshare, na.rm = TRUE), 1),
             voters = nrow(z), black_voters = n,
             recall = round(100 * k / max(n, 1), 2),
             lo = round(100 * w["lo"], 2), hi = round(100 * w["hi"], 2),
             stringsAsFactors = FALSE)
}))
write.csv(dec, "derived/by_decile.csv", row.names = FALSE)

# --- 3. ROC and the threshold table -----------------------------------------
#
# One row per candidate threshold. The curve and the confusion counts come out
# of the same pass, so the figure cannot disagree with the table under it.

P <- sort(unique(round(vo$p_black, 3)))
P <- unique(c(0, P, 1))
np <- sum(TRUTH); nn <- sum(!TRUTH)
roc <- do.call(rbind, lapply(P, function(t) {
  tp <- sum(vo$p_black >= t &  TRUTH); fp <- sum(vo$p_black >= t & !TRUTH)
  fn <- np - tp;                       tn <- nn - fp
  data.frame(t = t, tp = tp, fp = fp, fn = fn, tn = tn,
             tpr = round(tp / np, 5), fpr = round(fp / nn, 5),
             prec = round(if (tp + fp > 0) tp / (tp + fp) else NA_real_, 5),
             stringsAsFactors = FALSE)
}))
write.csv(roc, "derived/roc.csv", row.names = FALSE)

# AUC by the rank identity, which needs no trapezoids
rk  <- rank(vo$p_black)
AUC <- (sum(rk[TRUTH]) - np * (np + 1) / 2) / (np * nn)

# thin the curve for the figure: keep every distinct (fpr,tpr) corner
keep <- !duplicated(round(roc[, c("fpr", "tpr")], 4))
write.csv(roc[keep, ], "derived/thresh.csv", row.names = FALSE)

# --- 4. Calibration ---------------------------------------------------------

cb <- cut(vo$p_black, seq(0, 1, 0.05), include.lowest = TRUE)
calib <- do.call(rbind, lapply(levels(cb), function(l) {
  z <- vo[!is.na(cb) & cb == l, ]
  if (!nrow(z)) return(NULL)
  w <- wilson(sum(z$race == "black"), nrow(z))
  data.frame(bin = l,
             mid = round(mean(z$p_black), 4), n = nrow(z),
             observed = round(mean(z$race == "black"), 4),
             lo = round(w["lo"], 4), hi = round(w["hi"], 4),
             stringsAsFactors = FALSE)
}))
write.csv(calib, "derived/calib.csv", row.names = FALSE)

# --- Facts ------------------------------------------------------------------

best <- roc[which.max(roc$tpr - roc$fpr), ]
half <- roc[which.min(abs(roc$t - 0.5)), ]
aian <- grp[grp$group == "aian", ]
wid  <- dec[which.max(dec$hi - dec$lo), ]
nar  <- dec[which.min(dec$hi - dec$lo), ]

facts <- data.frame(
  key = c("voters", "acc", "acc_lo", "acc_hi", "auc",
          "n_hit", "pct_hit", "n_miss", "pct_miss",
          "aian_n", "aian_recall", "aian_lo", "aian_hi", "aian_prec",
          "white_recall", "black_recall",
          "half_tpr", "half_fpr", "half_prec",
          "best_t", "best_tpr", "best_fpr",
          "wid_dec", "wid_n", "wid_w", "nar_dec", "nar_n", "nar_w",
          "dec_lo_recall", "dec_hi_recall"),
  value = c(nrow(vo), round(100 * ACC, 2), round(100 * wa["lo"], 2),
            round(100 * wa["hi"], 2), round(AUC, 4),
            sum(HIT), round(100 * mean(HIT), 1),
            sum(!HIT), round(100 * mean(!HIT), 1),
            aian$n_true, aian$recall, aian$rec_lo, aian$rec_hi, aian$precision,
            grp$recall[grp$group == "white"], grp$recall[grp$group == "black"],
            round(100 * half$tpr, 1), round(100 * half$fpr, 1),
            round(100 * half$prec, 1),
            best$t, round(100 * best$tpr, 1), round(100 * best$fpr, 1),
            wid$decile, wid$black_voters, round(wid$hi - wid$lo, 1),
            nar$decile, nar$black_voters, round(nar$hi - nar$lo, 1),
            min(dec$recall), max(dec$recall)),
  stringsAsFactors = FALSE)
write.csv(facts, "derived/facts.csv", row.names = FALSE)

cat("voters scored:", nrow(vo), "  surname on the list:",
    round(100 * mean(HIT), 1), "%\n")
cat("overall accuracy:", round(100 * ACC, 2), "%  [",
    round(100 * wa["lo"], 2), ",", round(100 * wa["hi"], 2), "]\n")
cat("AUC for P(Black):", round(AUC, 4), "\n")
cat("recall by group: white", grp$recall[grp$group == "white"],
    " black", grp$recall[grp$group == "black"],
    " aian", aian$recall, "[", aian$rec_lo, ",", aian$rec_hi, "]\n")
cat("recall across deciles:", min(dec$recall), "to", max(dec$recall), "\n")
cat("\ndone.\n")

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
