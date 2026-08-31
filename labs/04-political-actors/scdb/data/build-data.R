# ---------------------------------------------------------------------------
# Build the scdb dataset: the Supreme Court's votes.
#
# Four files end up in this folder:
#
#   derived/agreement.csv     Every pair of justices, 2020-2023 terms: how often they
#                     voted the same direction, and on how many cases.
#   derived/justices.csv      Per justice: cases, share of votes in the conservative
#                     direction, share of the time in the majority.
#   derived/close_cases.csv   The one-vote decisions, and who was in the majority.
#   derived/by_term.csv       Share of decisions in the conservative direction, every
#                     term from 1946 to 2023.
#
# WHY SCDB AND NOT MARTIN-QUINN. The obvious source for "where is each justice
# on the spectrum" is the published Martin-Quinn score. Two reasons not to use
# it. First, practical: mqscores.wustl.edu is behind a bot check and cannot be
# fetched by a script. Second, and better: **published scores are the answer,
# and this session is about how the answer is made.** SCDB gives the votes the
# scores are computed from, exactly as the roll-call half of this session gives
# the votes behind DW-NOMINATE.
#
# NOTE THE PROTOCOL: the SCDB site serves over **http only**. https fails.
#
# WHAT THE LAB FINDS. Feed nothing but a matrix of "how often did these two
# agree" into a one-dimensional scaling and out comes:
#
#   Thomas  Alito  Gorsuch  Barrett  Roberts  Kavanaugh | Jackson Kagan
#   Sotomayor  Breyer
#
# which is the conventional ideological ordering of the Roberts Court,
# recovered from votes alone with no ideology supplied. It correlates -0.975
# with each justice's share of conservative votes -- a variable the scaling
# never saw.
#
# AND THE SECOND FINDING, which students tend to like more: **Kavanaugh is in
# the majority 95.5% of the time and Roberts 94.3%**, the two highest on the
# court, while Thomas (79.3%) and Sotomayor (70.7%) are lowest. The middle
# wins. Being moderate is not a description of your opinions here; it is a
# description of how often you are on the winning side.
#
# CODING, from the SCDB codebook:
#   direction  1 = conservative, 2 = liberal   (blank where not codeable)
#   majority   1 = dissent, 2 = majority
#   Cases with blank direction are dropped from agreement, because "voted the
#   same way" is undefined when there is no direction to compare.
#
# WINDOW. 2020-2023 terms, which is the Barrett court. Breyer (122 cases) and
# Jackson (110) each sat for part of it, so their pair counts are smaller --
# the agreement file records `cases` so this is visible rather than hidden.
#
# SOURCE. Supreme Court Database, Washington University, 2024 release 01,
# justice-centered, by citation. http://scdb.wustl.edu/data.php
#
# Run from this directory:  Rscript build-data.R   (downloads ~1.7 MB)
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
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

URL <- paste0("http://scdb.wustl.edu/_brickFiles/2024_01/",
              "SCDB_2024_01_justiceCentered_Citation.csv.zip")

tmp <- tempfile(fileext = ".zip")
prov_fetch(URL, tmp, mode = "wb", quiet = TRUE)
csv <- unzip(tmp, exdir = tempdir())
# ENCODING. The file is UTF-8: the apostrophe in "DOBBS v. JACKSON WOMEN'S
# HEALTH ORGANIZATION" is bytes E2 80 99, a curly right single quote. Reading it
# as latin1 turns that into "WOMENâS" and the damage is silent -- it survives
# into any case name written out. Nothing in the 2020-2023 one-vote decisions
# happens to carry such a character, so close_cases.csv was clean by luck, not
# by construction.
s <- read.csv(csv[1], stringsAsFactors = FALSE, fileEncoding = "UTF-8")

stopifnot(nrow(s) > 80000)
s$term <- suppressWarnings(as.integer(s$term))

FROM <- 2020; TO <- 2023
w <- s[!is.na(s$term) & s$term >= FROM & s$term <= TO, ]

# --- 1. pairwise agreement --------------------------------------------------
v <- w[w$direction %in% c(1, 2), c("caseId", "justiceName", "direction")]
js <- sort(unique(v$justiceName))

dir_of <- function(j) setNames(v$direction[v$justiceName == j],
                               v$caseId[v$justiceName == j])
D <- lapply(js, dir_of); names(D) <- js

pairs <- t(combn(js, 2))
ag <- data.frame(a = pairs[, 1], b = pairs[, 2], agree = 0L, cases = 0L,
                 stringsAsFactors = FALSE)
for (i in seq_len(nrow(ag))) {
  x <- D[[ag$a[i]]]; y <- D[[ag$b[i]]]
  both <- intersect(names(x), names(y))
  ag$cases[i] <- length(both)
  ag$agree[i] <- sum(x[both] == y[both])
}
ag$pct <- round(100 * ag$agree / ag$cases, 1)
write.csv(ag, "derived/agreement.csv", row.names = FALSE)

# --- 2. per justice ---------------------------------------------------------
pct <- function(num, den) round(100 * num / den, 1)
jj <- data.frame(justice = js, stringsAsFactors = FALSE)
jj$cases <- sapply(js, function(j) sum(v$justiceName == j))
jj$pct_conservative <- sapply(js, function(j)
  pct(sum(v$justiceName == j & v$direction == 1), sum(v$justiceName == j)))
m <- w[w$majority %in% c(1, 2), ]
jj$pct_in_majority <- sapply(js, function(j)
  pct(sum(m$justiceName == j & m$majority == 2), sum(m$justiceName == j)))
write.csv(jj, "derived/justices.csv", row.names = FALSE)

# --- 3. one-vote decisions --------------------------------------------------
cc <- unique(w[, c("caseId", "caseName", "term", "majVotes", "minVotes")])
cc$majVotes <- suppressWarnings(as.integer(cc$majVotes))
cc$minVotes <- suppressWarnings(as.integer(cc$minVotes))
cc <- cc[!is.na(cc$majVotes) & !is.na(cc$minVotes) &
         (cc$majVotes - cc$minVotes) == 1, ]
inmaj <- function(id) paste(sort(w$justiceName[w$caseId == id & w$majority == 2]),
                            collapse = " ")
cc$majority_bloc <- sapply(cc$caseId, inmaj)
cc <- cc[order(cc$term), c("term", "caseName", "majVotes", "minVotes", "majority_bloc")]
write.csv(cc, "derived/close_cases.csv", row.names = FALSE)

# --- 4. the long view -------------------------------------------------------
d <- unique(s[, c("caseId", "term", "decisionDirection")])
d <- d[d$decisionDirection %in% c(1, 2) & !is.na(d$term), ]
bt <- data.frame(term = sort(unique(d$term)))
bt$cases <- as.vector(table(d$term))
bt$pct_conservative <- round(100 * as.vector(tapply(d$decisionDirection == 1,
                                                    d$term, mean)), 1)
write.csv(bt, "derived/by_term.csv", row.names = FALSE)

cat(sprintf("terms %d-%d: %d justice-votes, %d justices\n",
            FROM, TO, nrow(v), length(js)))
cat(sprintf("pairs: %d   one-vote decisions: %d\n", nrow(ag), nrow(cc)))
cat(sprintf("long series: %d terms, %d-%d\n", nrow(bt), min(bt$term), max(bt$term)))
cat("\nhighest and lowest agreement:\n")
print(head(ag[order(-ag$pct), c("a","b","pct","cases")], 3), row.names = FALSE)
print(head(ag[order( ag$pct), c("a","b","pct","cases")], 3), row.names = FALSE)

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
