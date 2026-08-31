# ---------------------------------------------------------------------------
# Build the finance-network dataset: independent expenditures as a graph.
#
# Seven files end up in derived/:
#
#   derived/edges.csv       every spender-candidate-side pair, with dollars
#   derived/committees.csv  one row per spending committee, with its degree
#   derived/candidates.csv  one row per candidate targeted, with its degree
#   derived/graph_nodes.csv the drawable slice: nodes
#   derived/graph_edges.csv the drawable slice: edges
#   derived/identity.csv    the same file counted under three node rules
#   derived/facts.csv       single numbers the brief quotes
#
# Run this script from inside the data/ folder. It needs a network connection
# and downloads about 19 MB; the committed output means the lab does not.
# ---------------------------------------------------------------------------

# Downloads go through prov_fetch(), which records url, bytes, hash and row
# count in PROVENANCE.tsv and prints a banner when a source moves under us --
# a URL that still returns 200 and no longer means what it meant. See
# ../../../_lib/provenance.R. If the helper is missing the build still runs: the
# fallback is a plain download with the same signature, forwarding every
# argument so a source needing a redirect or a user agent still gets one.
if (file.exists("../../../_lib/provenance.R")) {
  source("../../../_lib/provenance.R")
} else {
  prov_fetch <- function(url, dest, label = NULL, mode = "wb", quiet = TRUE, ...) {
    download.file(url, dest, mode = mode, quiet = quiet, ...)
    invisible(dest)
  }
  prov_report <- function() invisible(FALSE)
}


dir.create("derived", showWarnings = FALSE)
options(scipen = 999, stringsAsFactors = FALSE)

# --- Source -----------------------------------------------------------------
#
# FEC bulk downloads, independent expenditures, 2024 cycle:
#   https://www.fec.gov/files/bulk-downloads/2024/independent_expenditure_2024.csv
# Column layout:
#   https://www.fec.gov/campaign-finance-data/independent-expenditures-file-description/
#
# This is the same download the independent-expenditures chapter uses. That
# chapter aggregates it to totals; this one keeps the pairs, because the pairs
# are the thing the file actually records and the thing every published summary
# throws away.
#
# THE SIXTEEN ROWS. The sibling chapter established that sixteen rows over
# $100 million each account for most of the raw total and are junk filings --
# committees named "THE COURT OF DIVINE JUSTICE" and the like. The same crude
# cut is applied here, for the same reason and with the same caveat: it is a
# threshold, not a diagnosis. In a graph it matters more than in a total,
# because one $9.98bn edge would dominate every layout drawn from these data.

URL <- paste0("https://www.fec.gov/files/bulk-downloads/2024/",
              "independent_expenditure_2024.csv")
tmp <- tempfile(fileext = ".csv")
prov_fetch(URL, tmp, mode = "wb", quiet = TRUE)
ie <- read.csv(tmp, stringsAsFactors = FALSE, colClasses = "character")

ie$amount <- suppressWarnings(as.numeric(ie$exp_amo))
ie$amount[is.na(ie$amount)] <- 0
stopifnot(nrow(ie) > 70000)

CUT <- 1e8
cl  <- ie[ie$amount <= CUT, ]
RAW_ROWS <- nrow(ie); CLEAN_ROWS <- nrow(cl); DROPPED <- RAW_ROWS - CLEAN_ROWS

cl$sid  <- trimws(cl$spe_id)
cl$cid  <- trimws(cl$cand_id)
cl$snm  <- trimws(cl$spe_nam)
cl$cnm  <- toupper(trimws(cl$cand_name))

# --- Three ways to count the nodes ------------------------------------------
#
# A graph needs nodes. This file does not supply them. It supplies an
# identifier that is sometimes blank and sometimes wrong, and a name that is
# typed by whoever filed the row. Counting the same 73,433 rows under three
# defensible rules gives three different graphs, and the difference is not
# small. The brief prints this table before it draws anything.

letters_only <- function(x) gsub("[^A-Z]", "", toupper(trimws(x)))

ident <- data.frame(
  rule = c("FEC candidate ID", "candidate name, upper-cased",
           "candidate name, letters only",
           "FEC committee ID", "committee name"),
  side = c("candidates", "candidates", "candidates",
           "committees", "committees"),
  nodes = c(
    length(unique(cl$cid[nzchar(cl$cid)])),
    length(unique(cl$cnm)),
    length(unique(letters_only(cl$cnm))),
    length(unique(cl$sid)),
    length(unique(cl$snm))),
  stringsAsFactors = FALSE)
write.csv(ident, "derived/identity.csv", row.names = FALSE)

# Rows with no candidate identifier at all. They are not a rounding error and
# they are not dropped quietly: the count and the dollars go into facts.csv so
# the brief has to account for them.
NOID_ROWS <- sum(!nzchar(cl$cid))
NOID_USD  <- sum(cl$amount[!nzchar(cl$cid)])

# The two principals, as a worked example of the same problem.
count_forms <- function(pat) {
  z <- cl[grepl(pat, cl$cnm) & cl$can_office == "P", ]
  c(spellings = length(unique(z$cnm)),
    ids       = length(unique(z$cid[nzchar(z$cid)])),
    no_id     = sum(!nzchar(z$cid)))
}
TR <- count_forms("TRUMP"); HA <- count_forms("HARRIS")

# --- The graph itself -------------------------------------------------------
#
# From here on the FEC identifier is the node, because it is the only field in
# the file that was meant to be one. Rows without it cannot join to anything
# and are therefore absent from every figure -- which is a statement about the
# figures, not about the money.

g <- cl[nzchar(cl$cid), ]

e <- aggregate(amount ~ sid + cid + sup_opp, g, sum)
e$amount <- round(e$amount, 2)
names(e)[names(e) == "sup_opp"] <- "side"
e <- e[order(-e$amount), ]
write.csv(e, "derived/edges.csv", row.names = FALSE)

# one name per identifier: the most frequently filed spelling, so the label is
# the one a reader is most likely to have seen
modal <- function(v) names(sort(table(v), decreasing = TRUE))[1]

cm <- data.frame(sid = unique(g$sid), stringsAsFactors = FALSE)
cm$name    <- vapply(cm$sid, function(i) modal(g$snm[g$sid == i]), character(1))
cm$total   <- round(vapply(cm$sid, function(i) sum(g$amount[g$sid == i]), numeric(1)), 2)
cm$against <- round(vapply(cm$sid, function(i)
                    sum(g$amount[g$sid == i & g$sup_opp == "O"]), numeric(1)), 2)
cm$pct_against <- round(100 * cm$against / cm$total, 1)
cm$candidates  <- vapply(cm$sid, function(i)
                         length(unique(g$cid[g$sid == i])), integer(1))
cm <- cm[order(-cm$total), ]
write.csv(cm, "derived/committees.csv", row.names = FALSE)

cd <- data.frame(cid = unique(g$cid), stringsAsFactors = FALSE)
cd$name   <- vapply(cd$cid, function(i) modal(g$cnm[g$cid == i]), character(1))
cd$office <- vapply(cd$cid, function(i) modal(g$can_office[g$cid == i]), character(1))
cd$party  <- vapply(cd$cid, function(i) modal(g$cand_pty_aff[g$cid == i]), character(1))
cd$total  <- round(vapply(cd$cid, function(i) sum(g$amount[g$cid == i]), numeric(1)), 2)
cd$against <- round(vapply(cd$cid, function(i)
                    sum(g$amount[g$cid == i & g$sup_opp == "O"]), numeric(1)), 2)
cd$committees <- vapply(cd$cid, function(i)
                        length(unique(g$sid[g$cid == i])), integer(1))
cd <- cd[order(-cd$total), ]
write.csv(cd, "derived/candidates.csv", row.names = FALSE)

# --- The drawable slice -----------------------------------------------------
#
# 995 committees and 745 candidates joined by 4,000-odd edges is a hairball,
# and a hairball is a picture of nothing. The slice drawn in the brief is the
# thirty largest committees and every candidate they spent at least a million
# dollars on. The share of cleaned dollars it covers is written to facts.csv,
# so the figure can say how much of the money it is showing.

TOPN <- 30; MINE <- 1e6
top  <- head(cm$sid, TOPN)
pair <- aggregate(amount ~ sid + cid, g, sum)
sl   <- pair[pair$sid %in% top & pair$amount >= MINE, ]

opp  <- aggregate(amount ~ sid + cid, g[g$sup_opp == "O", ], sum)
sl$against <- opp$amount[match(paste(sl$sid, sl$cid), paste(opp$sid, opp$cid))]
sl$against[is.na(sl$against)] <- 0
sl$mostly_against <- sl$against / sl$amount > 0.5
sl$amount  <- round(sl$amount, 2)
sl$against <- round(sl$against, 2)
names(sl)[1:2] <- c("source", "target")
write.csv(sl[, c("source", "target", "amount", "against", "mostly_against")],
          "derived/graph_edges.csv", row.names = FALSE)

nodes <- rbind(
  data.frame(id = unique(sl$source), kind = "committee",
             name = cm$name[match(unique(sl$source), cm$sid)],
             total = cm$total[match(unique(sl$source), cm$sid)],
             pct_against = cm$pct_against[match(unique(sl$source), cm$sid)],
             degree = NA_integer_, office = "", party = "",
             stringsAsFactors = FALSE),
  data.frame(id = unique(sl$target), kind = "candidate",
             name = cd$name[match(unique(sl$target), cd$cid)],
             total = cd$total[match(unique(sl$target), cd$cid)],
             pct_against = round(100 * cd$against[match(unique(sl$target), cd$cid)] /
                                 cd$total[match(unique(sl$target), cd$cid)], 1),
             degree = NA_integer_,
             office = cd$office[match(unique(sl$target), cd$cid)],
             party  = cd$party[match(unique(sl$target), cd$cid)],
             stringsAsFactors = FALSE))
deg <- table(c(sl$source, sl$target))
nodes$degree <- as.integer(deg[nodes$id])

# --- The layout, computed once, here ---------------------------------------
#
# A force layout is a physics simulation with a random start, so running one in
# the browser and a different one for print would put the same graph in two
# different arrangements and invite a reader to read meaning into the
# difference. There is no meaning to read: in a force layout POSITION IS NOT
# DATA. Only adjacency is. So the layout is solved once, with a fixed seed, and
# both renderers draw the same coordinates.
#
# Plain spring-and-repulsion, which is all d3.forceSimulation is: every node
# pushes every other node away, every edge pulls its two ends together, and the
# whole thing is damped until it stops moving.

set.seed(84355)
N  <- nrow(nodes)
ix <- setNames(seq_len(N), nodes$id)
ei <- cbind(ix[sl$source], ix[sl$target])

ang <- seq(0, 2 * pi, length.out = N + 1)[-(N + 1)]
rad <- ifelse(nodes$kind == "committee", 90, 210)
px  <- rad * cos(ang) + rnorm(N, 0, 6)
py  <- rad * sin(ang) + rnorm(N, 0, 6)

K <- 46                                  # natural spring length
for (it in 1:600) {
  cool <- 1 - it / 700
  dx <- outer(px, px, "-"); dy <- outer(py, py, "-")
  d2 <- dx^2 + dy^2; diag(d2) <- 1
  d  <- sqrt(d2)
  rep_ <- (K^2) / d2                     # repulsion falls off with distance^2
  diag(rep_) <- 0
  fx <- rowSums(rep_ * dx / d, na.rm = TRUE)
  fy <- rowSums(rep_ * dy / d, na.rm = TRUE)
  # springs
  sx <- px[ei[, 1]] - px[ei[, 2]]; sy <- py[ei[, 1]] - py[ei[, 2]]
  sd <- pmax(sqrt(sx^2 + sy^2), 1e-6)
  f  <- (sd - K) / sd * 0.9
  fx <- fx - as.vector(tapply(f * sx, factor(ei[, 1], seq_len(N)), sum,
                              default = 0))
  fy <- fy - as.vector(tapply(f * sy, factor(ei[, 1], seq_len(N)), sum,
                              default = 0))
  fx <- fx + as.vector(tapply(f * sx, factor(ei[, 2], seq_len(N)), sum,
                              default = 0))
  fy <- fy + as.vector(tapply(f * sy, factor(ei[, 2], seq_len(N)), sum,
                              default = 0))
  fx <- fx - px * 0.030                  # gentle pull to the centre
  fy <- fy - py * 0.030
  st <- pmin(sqrt(fx^2 + fy^2), 14) / pmax(sqrt(fx^2 + fy^2), 1e-9)
  px <- px + fx * st * cool
  py <- py + fy * st * cool
}
# centre and scale into a 960 x 640 frame
px <- px - mean(px); py <- py - mean(py)
s  <- min(430 / max(abs(px)), 290 / max(abs(py)))
nodes$x <- round(480 + px * s, 1)
nodes$y <- round(320 + py * s, 1)

write.csv(nodes, "derived/graph_nodes.csv", row.names = FALSE)

SLICE_PCT <- round(100 * sum(sl$amount) / sum(g$amount), 1)

# --- The numbers the prose quotes -------------------------------------------

BIG <- cm[cm$total > 5e6, ]
facts <- data.frame(
  key = c("raw_rows", "clean_rows", "dropped", "clean_usd",
          "noid_rows", "noid_usd", "noid_pct",
          "committees", "candidates", "edges",
          "cand_median_deg", "cand_max_deg", "comm_median_deg", "comm_max_deg",
          "widest_committee", "widest_n", "most_targeted", "most_targeted_n",
          "trump_spellings", "trump_ids", "trump_noid",
          "harris_spellings", "harris_ids", "harris_noid",
          "big_committees", "big_attack", "slice_nodes", "slice_edges",
          "slice_pct", "names_per_id_max", "ids_with_many_names"),
  value = c(RAW_ROWS, CLEAN_ROWS, DROPPED, round(sum(cl$amount), 2),
            NOID_ROWS, round(NOID_USD, 2), round(100 * NOID_ROWS / CLEAN_ROWS, 1),
            nrow(cm), nrow(cd), nrow(e),
            median(cd$committees), max(cd$committees),
            median(cm$candidates), max(cm$candidates),
            cm$name[which.max(cm$candidates)], max(cm$candidates),
            cd$name[which.max(cd$committees)], max(cd$committees),
            TR["spellings"], TR["ids"], TR["no_id"],
            HA["spellings"], HA["ids"], HA["no_id"],
            nrow(BIG), sum(BIG$pct_against > 90),
            nrow(nodes), nrow(sl), SLICE_PCT,
            max(tapply(g$snm, g$sid, function(v) length(unique(v)))),
            sum(tapply(g$snm, g$sid, function(v) length(unique(v))) > 1)),
  stringsAsFactors = FALSE)
write.csv(facts, "derived/facts.csv", row.names = FALSE)

cat("edges.csv       ->", nrow(e), "spender-candidate-side pairs\n")
cat("committees.csv  ->", nrow(cm), "committees\n")
cat("candidates.csv  ->", nrow(cd), "candidates\n")
cat("graph slice     ->", nrow(nodes), "nodes,", nrow(sl), "edges,",
    SLICE_PCT, "% of cleaned dollars\n")
cat("\nrows with no candidate id:", NOID_ROWS, "carrying $",
    round(NOID_USD / 1e6, 1), "M\n")
cat("done.\n")

# ---------------------------------------------------------------------------
# Build stamp. Records which script produced what is now in this directory --
# every file under derived/ and raw/ with its size, hash and row count, and the
# date this ran -- into BUILD-STAMP.tsv beside the data. See
# ../../../_lib/provenance.R. Guarded, because a missing helper must not fail a
# build that was otherwise fine.
if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_report()
  prov_stamp()
}
