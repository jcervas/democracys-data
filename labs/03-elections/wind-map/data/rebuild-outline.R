# ---------------------------------------------------------------------------
# Rewrite us_outline.csv from the shared base map (labs/_lib/geo).
#
# The outline is the one output of build-data.R that depends on no fetch at
# all -- it comes from a file already in this corpus -- so it can be rebuilt on its own
# when the way it is SIMPLIFIED changes, without asking a dozen servers for
# votes this folder already holds.
#
# Run from inside data/:  Rscript rebuild-outline.R
# ---------------------------------------------------------------------------
# raw/ holds the sources as they arrive; derived/ is what this script writes.
source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)
source("projection.R")

old <- read.csv("derived/us_outline.csv")
out <- state_outline()
dd_write_csv(out, "derived/us_outline.csv")

cat("points: ", nrow(old), " -> ", nrow(out), "\n", sep = "")
cat("parts:  ", length(unique(old$part)), " -> ", length(unique(out$part)), "\n", sep = "")
cat("bbox x: ", paste(round(range(out$x)), collapse = " "), "\n", sep = "")
cat("bbox y: ", paste(round(range(out$y)), collapse = " "), "\n", sep = "")

ff <- read.csv("derived/facts.csv", colClasses = "character")
ff$value[ff$key == "us_outline_points"] <- as.character(nrow(out))
dd_write_csv(ff, "derived/facts.csv")
cat("us_outline_points updated\n")
