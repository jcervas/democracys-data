# ---------------------------------------------------------------------------
# Build the dw-nominate dataset.
#
# One file ends up in this folder:
#
#   derived/nominate_members.csv   every member of the House and Senate since the 46th
#                          Congress (1879), with their DW-NOMINATE coordinates
#
# Run this script from inside the data/ folder. It needs a network connection;
# the committed output means the lab does not.
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)

# --- Source -----------------------------------------------------------------
#
# Lewis, Jeffrey B., Keith Poole, Howard Rosenthal, Adam Boche, Aaron Rudkin
# and Luke Sonnet, "Voteview: Congressional Roll-Call Votes Database"
#   https://voteview.com/  —  static/data/out/members/HSall_members.csv
#
# DW-NOMINATE places each member on a scale estimated ONLY from how they voted
# relative to everyone else. Nobody read the bills. Nobody coded a policy as
# liberal or conservative. The scale falls out of the pattern of agreement and
# disagreement across thousands of roll calls, and dimension 1 turns out to
# line up with what we usually call left and right.
#
# That is the fact worth pausing on before any of the trends: this is a
# measurement of ideology built without anybody's judgement about ideology.

url <- "https://voteview.com/static/data/out/members/HSall_members.csv"
raw <- read.csv(url, stringsAsFactors = FALSE)
cat("downloaded", nrow(raw), "member-congress records\n")

stopifnot(all(c("congress","chamber","party_code","bioname",
                "nominate_dim1","nominate_dim2") %in% names(raw)))

# Keep the two chambers, the two major parties, and members who actually have
# coordinates. Presidents are in this file too and are dropped: they are scored
# on their announced positions, not roll calls, and mixing them into a chamber
# median would be wrong.
d <- raw[raw$chamber %in% c("House","Senate") &
         raw$party_code %in% c(100, 200) &
         !is.na(raw$nominate_dim1), ]

# 1879 onward. Before the 46th Congress the party system is different enough
# that a Democrat-Republican comparison stops meaning the same thing, and the
# early scores rest on far fewer roll calls.
d <- d[d$congress >= 46, ]

d$party <- ifelse(d$party_code == 100, "Democrat", "Republican")
d$year  <- 1789 + 2 * (d$congress - 1)

out <- d[, c("congress","year","chamber","state_abbrev","district_code",
             "bioname","party","nominate_dim1","nominate_dim2")]
names(out)[names(out) == "state_abbrev"]   <- "state"
names(out)[names(out) == "nominate_dim1"]  <- "dim1"
names(out)[names(out) == "nominate_dim2"]  <- "dim2"
out <- out[order(out$congress, out$chamber, -out$dim1), ]

cat("kept", nrow(out), "rows,", min(out$congress), "-", max(out$congress), "\n")
cat("chambers:", paste(table(out$chamber), names(table(out$chamber)), collapse = ", "), "\n")
write.csv(out, "derived/nominate_members.csv", row.names = FALSE)

# --- Report the findings the lab is built on --------------------------------

h <- out[out$chamber == "House", ]
med <- function(cg, p) median(h$dim1[h$congress == cg & h$party == p])

cat("\nHouse party medians:\n")
for (cg in c(90, 100, 110, max(h$congress))) {
  cat(sprintf("  %3d (%d): D %+.3f  R %+.3f  gap %.3f\n",
              cg, 1789 + 2*(cg-1), med(cg,"Democrat"), med(cg,"Republican"),
              med(cg,"Republican") - med(cg,"Democrat")))
}

last <- max(h$congress)
cat(sprintf("\nasymmetry 90th -> %dth: D %+.3f, R %+.3f (R moved %.1fx further)\n",
            last, med(last,"Democrat") - med(90,"Democrat"),
            med(last,"Republican") - med(90,"Republican"),
            abs(med(last,"Republican")-med(90,"Republican")) /
            abs(med(last,"Democrat")-med(90,"Democrat"))))

overlap <- function(cg) {
  x <- h[h$congress == cg, ]
  sum(x$dim1[x$party == "Republican"] < max(x$dim1[x$party == "Democrat"]))
}
ov <- sapply(46:last, overlap); names(ov) <- 46:last
cat("last Congress with any party overlap:",
    max(as.integer(names(ov)[ov > 0])), "\n")
cat("congresses with zero overlap:",
    paste(names(ov)[ov == 0], collapse = ", "), "\n")

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
