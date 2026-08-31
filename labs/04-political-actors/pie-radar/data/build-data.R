# ---------------------------------------------------------------------------
# Build the dataset for the pie-and-radar chapter: where 2024 candidates got
# their money, and the arithmetic of the two chart forms that are usually used
# to show it.
#
# Six files end up in derived/:
#
#   derived/candidates.csv one row per candidate: the six mutually exclusive
#                          sources, in dollars and as shares
#   derived/duplicates.csv the candidate records that report the same money
#                          twice under two candidate IDs
#   derived/featured.csv   the handful of candidates the figures draw
#   derived/areas.csv      for every featured candidate, the area of the radar
#                          polygon under all 60 distinct orderings of the axes
#   derived/lookalikes.csv the two candidates whose pies are indistinguishable
#   derived/facts.csv      single numbers the brief quotes
#
# Run this script from inside the data/ folder.
# ---------------------------------------------------------------------------

# dd_write_csv(): six significant digits on every non-integer column, so the
# share columns below do not claim fifteen. Dollars survive: they are rounded
# to the cent at the point they are computed, not here.
source("../../../_lib/precision.R")
dir.create("derived", showWarnings = FALSE)
options(scipen = 999, stringsAsFactors = FALSE)

# --- Source -----------------------------------------------------------------
#
# The Federal Election Commission's candidate summary file for the 2023-2024
# cycle, as captured and committed by this corpus's campaign-finance chapter:
#
#   ../../campaign-finance/data/derived/fec_candidates_2024.csv
#
# That chapter reads it for what it says about money. This one reads it for
# what happens when you try to draw it in a circle.

S <- "../../campaign-finance/data/derived/fec_candidates_2024.csv"
stopifnot(file.exists(S))
d <- read.csv(S)

# --- The categories, and why they are not the file's categories -------------
#
# The FEC publishes eight receipt fields, and THEY DO NOT FORM A PARTITION.
# Two problems have to be fixed before any pie is possible, and a pie drawn
# without fixing them is not a distorted picture of the data, it is a picture
# of something that does not exist.
#
# 1. OVERLAP. self_funding is exactly cand_contrib + cand_loans, in all 3,856
#    rows -- it is a total, not a sibling. Putting all three in one pie counts
#    the candidate's own money twice and shrinks every other slice to make room.
#    Checked below rather than asserted.
#
# 2. A RESIDUAL. The five remaining fields do not add up to ttl_receipts. What
#    is missing is real money -- offsets to operating expenditures, loans from
#    other sources, refunds and receipts the summary file does not itemise --
#    and it has to become a sixth slice called "other" or the pie will silently
#    rescale the five it has until they fill the circle.

stopifnot(all(abs(d$self_funding - (d$cand_contrib + d$cand_loans)) < 0.01))

SRC <- c(indiv_contrib = "From individuals",
         pac_contrib   = "From PACs",
         party_contrib = "From the party",
         self_funding  = "The candidate's own money",
         trans_from_auth = "Transferred from another committee",
         other         = "Everything else")
d$other <- round(d$ttl_receipts - rowSums(d[, names(SRC)[1:5]]), 2)
stopifnot(max(abs(d$ttl_receipts - rowSums(d[, names(SRC)]))) < 0.01)

# --- The same money, filed twice ---------------------------------------------
#
# A pie asserts that its slices are a whole. Before drawing one over this file
# it is worth knowing that the file contains the same dollars more than once:
# a person who has filed for two offices, or whose committee was renamed, gets
# a candidate record for each, and the FEC reports the committee's totals under
# both.
#
# Two records are treated as the same money when every RECEIPT figure matches
# to the cent. That is a deliberately strict rule and it still finds the
# largest campaign in the file twice: Biden's committee was renamed for Harris,
# and both candidate IDs carry the identical $1.18 billion. (Their disbursement
# totals differ by about seventy thousand dollars, which is why a rule over all
# financial columns would have missed it.)

RCPT <- c("ttl_receipts", names(SRC)[1:5], "cand_contrib", "cand_loans")
sig <- apply(round(as.matrix(d[, RCPT]), 2), 1, paste, collapse = "|")
# CANDIDATES WHO RAISED NOTHING ARE NOT DUPLICATES OF EACH OTHER. 983 records
# in this file are zero all the way across, so they share a signature made
# entirely of zeros. They are given unique keys instead: two people who each
# raised nothing are two people, and folding them together would turn the count
# below into a statement about how many candidates never filed.
sig[d$ttl_receipts <= 0] <- paste0("\rzero", seq_len(nrow(d)))[d$ttl_receipts <= 0]
tab <- table(sig)
dupsig <- names(tab[tab > 1])
dup <- d[sig %in% dupsig, ]
dup <- dup[order(-dup$ttl_receipts, dup$cand_name), ]
dup$group <- match(sig[sig %in% dupsig][order(-d$ttl_receipts[sig %in% dupsig],
                                                  d$cand_name[sig %in% dupsig])],
                   dupsig)
dd_write_csv(dup[, c("group", "cand_id", "cand_name", "office", "office_st",
                  "party", "ttl_receipts")],
          "derived/duplicates.csv")

# Keep one record per signature: the one whose office matches the candidate ID
# prefix, and failing that the first alphabetically, so the choice is a rule
# rather than whatever order the file arrived in.
pick <- function(z) {
  want <- c(H = "House", S = "Senate", P = "President")
  ok <- which(want[substr(z$cand_id, 1, 1)] == z$office)
  if (length(ok)) z[ok[1], ] else z[order(z$cand_id)[1], ]
}
keep <- rep(TRUE, nrow(d))
for (g in dupsig) {
  i <- which(sig == g)
  keep[i] <- FALSE
  keep[i[match(pick(d[i, ])$cand_id, d$cand_id[i])]] <- TRUE
}
stopifnot(sum(!keep) == nrow(dup) - length(dupsig))
dd <- d[keep, ]

# --- Candidates ---------------------------------------------------------------

MIN <- 3e6                       # the figures need candidates with real money
z <- dd[dd$ttl_receipts >= MIN, ]
for (k in names(SRC)) z[[paste0("p_", k)]] <- round(100 * z[[k]] / z$ttl_receipts, 4)
z <- z[order(-z$ttl_receipts), ]
dd_write_csv(z[, c("cand_id", "cand_name", "office", "office_st", "party",
                "ttl_receipts", names(SRC),
                paste0("p_", names(SRC)))],
          "derived/candidates.csv")

# --- Two pies nobody can tell apart -------------------------------------------
#
# The comparison a pie chart cannot survive. Search every pair of candidates
# above the threshold for the two whose six shares are closest, subject to one
# of them having raised at least six times as much as the other. A pie is drawn
# from shares, so a pair like this produces two identical circles for two
# campaigns of very different size.
#
# BOTH PROFILES HAVE TO HAVE STRUCTURE. Without the second constraint below the
# search wins by finding two candidates who each raised 98% from individuals:
# their pies are indeed identical and they are also both a single solid circle,
# which proves nothing anybody disputes. Requiring no slice above 70% and at
# least three slices above 5% forces the match to be between two pies a reader
# would take seriously.

P <- as.matrix(z[, paste0("p_", names(SRC))])
STRUCT <- apply(P, 1, max) <= 80 & rowSums(P >= 10) >= 3
stopifnot(sum(STRUCT) >= 20)
best <- NULL
for (i in which(STRUCT)) {
  if (i == nrow(z)) next
  r <- z$ttl_receipts[i] / z$ttl_receipts[-seq_len(i)]
  r <- pmax(r, 1 / r)
  cand <- which(r >= 6 & STRUCT[-seq_len(i)])
  if (!length(cand)) next
  j <- cand + i
  gap <- apply(abs(sweep(P[j, , drop = FALSE], 2, P[i, ])), 1, max)
  k <- which.min(gap)
  if (is.null(best) || gap[k] < best$gap)
    best <- list(i = i, j = j[k], gap = gap[k], ratio = r[cand][k])
}
look <- z[c(best$i, best$j), ]
look <- look[order(-look$ttl_receipts), ]
dd_write_csv(look[, c("cand_id", "cand_name", "office", "office_st", "party",
                   "ttl_receipts", names(SRC), paste0("p_", names(SRC)))],
          "derived/lookalikes.csv")

# --- The candidates the radar draws -------------------------------------------
#
# Chosen for contrast, not for size: one campaign that raised almost all of it
# from individuals, one that was almost entirely the candidate's own money, one
# that lives on transfers from another committee, one that lives on PACs, and
# the most evenly spread profile in the file -- which is the case a radar chart
# is supposed to be good at.

# Shannon entropy over the six shares, used only to find the most evenly
# spread profile. A share can be slightly negative (a refund larger than the
# receipts in a category), so the log is taken on the positive shares only
# rather than inside ifelse(), which would evaluate log(0) and log(-x) anyway.
shan <- function(p) { p <- p[p > 0] / 100; -sum(p * log(p)) }
z$even <- apply(P, 1, shan)
EVEN <- z$cand_name[which.max(z$even)]
FEAT <- c("HARRIS, KAMALA", "TRONE, DAVID", "SCALISE, STEVE MR",
          "SMITH, JASON T", EVEN)
stopifnot(all(FEAT %in% z$cand_name), !anyDuplicated(FEAT))
ft <- z[match(FEAT, z$cand_name), ]
ft$role <- c("almost all from individuals and transfers",
             "almost all the candidate's own money",
             "almost all transferred in",
             "the most PAC-funded campaign in the file",
             "the most evenly spread profile in the file")
dd_write_csv(ft[, c("cand_id", "cand_name", "office", "office_st", "party",
                 "role", "ttl_receipts", names(SRC),
                 paste0("p_", names(SRC)))],
          "derived/featured.csv")

# --- What the order of the axes does to the area ------------------------------
#
# THE POINT OF THE SECOND HALF OF THE CHAPTER. A radar polygon with n axes at
# equal angles and radii r has area
#
#     A = 1/2 * sin(2*pi/n) * sum_i r_i * r_(i+1)
#
# -- every term is a product of ADJACENT radii, so the area depends on which
# axes were put next to which. The axes carry no natural order (there is no
# sense in which PAC money comes "after" party money), so the area of the shape
# is a property of a decision nobody records.
#
# All 60 distinct cyclic orderings of six axes are enumerated here, exactly,
# rather than sampled: fixing the first axis and permuting the other five gives
# 120, and reversing an ordering gives the same polygon, so 60 remain.

perms <- function(v) {
  if (length(v) <= 1) return(list(v))
  out <- list()
  for (i in seq_along(v))
    for (p in perms(v[-i])) out[[length(out) + 1]] <- c(v[i], p)
  out
}
poly_area <- function(r) {
  n <- length(r)
  0.5 * sin(2 * pi / n) * sum(r * r[c(2:n, 1)])
}
NAX <- length(SRC)
ORDERS <- perms(2:NAX)
ORDERS <- lapply(ORDERS, function(p) c(1, p))
# drop each ordering's mirror image: it draws the same polygon
kk <- vapply(ORDERS, function(o) paste(o, collapse = ","), character(1))
mir <- vapply(ORDERS, function(o) paste(c(1, rev(o[-1])), collapse = ","),
              character(1))
ORDERS <- ORDERS[!duplicated(pmin(kk, mir))]
stopifnot(length(ORDERS) == factorial(NAX - 1) / 2)

areas <- do.call(rbind, lapply(seq_len(nrow(ft)), function(i) {
  r <- as.numeric(ft[i, paste0("p_", names(SRC))]) / 100
  do.call(rbind, lapply(seq_along(ORDERS), function(k) {
    o <- ORDERS[[k]]
    data.frame(cand_name = ft$cand_name[i], ordering = k,
               axes = paste(names(SRC)[o], collapse = " "),
               area = round(poly_area(r[o]), 8), stringsAsFactors = FALSE)
  }))
}))
dd_write_csv(areas, "derived/areas.csv")

# The identity is checked against the shoelace formula on the actual vertices,
# because the closed form is the whole argument and a typo in it would be
# invisible.
shoelace <- function(r) {
  n <- length(r); a <- 2 * pi * (seq_len(n) - 1) / n
  x <- r * sin(a); y <- r * cos(a)
  abs(sum(x * c(y[-1], y[1]) - c(x[-1], x[1]) * y)) / 2
}
set.seed(1)
for (i in 1:200) {
  r <- runif(NAX)
  stopifnot(abs(poly_area(r) - shoelace(r)) < 1e-12)
}

rng <- do.call(rbind, lapply(ft$cand_name, function(w) {
  a <- areas$area[areas$cand_name == w]
  data.frame(cand_name = w, min = min(a), max = max(a),
             ratio = round(max(a) / min(a), 2), stringsAsFactors = FALSE)
}))

# --- Facts -------------------------------------------------------------------

nz <- sum(d$indiv_contrib < 0)
big <- dup[which.max(dup$ttl_receipts), ]
once <- sum(tapply(dup$ttl_receipts, dup$group, function(x) x[1]))
widest <- dup[dup$group == as.integer(names(which.max(table(dup$group)))), ]
lk <- look
ev <- rng[rng$cand_name == EVEN, ]

facts <- data.frame(
  key = c("cycle_rows", "kept_rows", "categories", "orderings",
          "dup_groups", "dup_rows", "dup_extra", "dup_biggest",
          "dup_biggest_total", "dup_a", "dup_b",
          "dup_money_once", "dup_money_as_filed", "dup_overcount",
          "dup_widest", "dup_widest_ids", "dup_widest_total",
          "negative_indiv", "negative_indiv_min",
          "min_receipts", "candidates",
          "look_a", "look_a_total", "look_b", "look_b_total",
          "look_ratio", "look_gap", "look_structured",
          "even_name", "even_ratio", "even_min", "even_max",
          "max_ratio", "max_ratio_name", "min_ratio", "min_ratio_name",
          "median_other_pct", "max_other_pct", "max_other_name"),
  value = c(nrow(d), nrow(dd), length(SRC), length(ORDERS),
            length(dupsig), nrow(dup), nrow(dup) - length(dupsig),
            big$cand_name, round(big$ttl_receipts),
            dup$cand_name[dup$group == big$group][1],
            dup$cand_name[dup$group == big$group][2],
            round(once), round(sum(dup$ttl_receipts)),
            round(sum(dup$ttl_receipts) - once),
            widest$cand_name[1], nrow(widest), round(widest$ttl_receipts[1]),
            nz, round(min(d$indiv_contrib)),
            MIN, nrow(z),
            lk$cand_name[1], round(lk$ttl_receipts[1]),
            lk$cand_name[2], round(lk$ttl_receipts[2]),
            round(lk$ttl_receipts[1] / lk$ttl_receipts[2], 1),
            round(best$gap, 2), sum(STRUCT),
            EVEN, ev$ratio, signif(ev$min, 3), signif(ev$max, 3),
            max(rng$ratio), rng$cand_name[which.max(rng$ratio)],
            min(rng$ratio), rng$cand_name[which.min(rng$ratio)],
            round(median(z$p_other), 2), round(max(z$p_other), 1),
            z$cand_name[which.max(z$p_other)]),
  stringsAsFactors = FALSE)
dd_write_csv(facts, "derived/facts.csv")

cat("candidates.csv ->", nrow(z), "candidates at or above",
    format(MIN, big.mark = ","), "\n")
cat("duplicates.csv ->", nrow(dup), "records in", length(dupsig),
    "groups reporting the same receipts twice\n")
cat("areas.csv      ->", nrow(areas), "areas (", nrow(ft), "candidates x",
    length(ORDERS), "orderings )\n\n")
cat("the same pie, two campaigns:", lk$cand_name[1], "and", lk$cand_name[2],
    "-- no slice differs by more than", round(best$gap, 2), "points, and one",
    "raised", round(lk$ttl_receipts[1] / lk$ttl_receipts[2], 1),
    "times the other\n\n")
print(rng, row.names = FALSE)
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
