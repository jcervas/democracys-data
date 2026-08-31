# ---------------------------------------------------------------------------
# Build the datasets for the PART VI opener.
#
# One of six part openers, all built to the same shape. A part opener is not a table of contents. It has the same obligation
# as every other chapter in this book: say something the reader can check, from
# data this script computed. What makes an opener different is only its
# SUBJECT -- the part itself, and what its chapters have in common.
#
# THE ARGUMENT.
#
#   NOBODY RECORDED ANY OF THESE NUMBERS. A DW-NOMINATE score is in no file
#   until somebody computes it; neither is an imputed race, an ideal point, or
#   a confidence interval. This part is about data that is MANUFACTURED from
#   other data, which makes provenance a chain rather than a source -- and
#   every assumption in the chain arrives silently attached to the output.
#
# ---------------------------------------------------------------------------
# SOURCES
# ---------------------------------------------------------------------------
#
# The corpus itself, which is why nothing here is downloaded.
#
#   ../../INDEX.md            Written by _lib/make-index.py. Supplies the part
#                             membership, the order, and each chapter's title
#                             and subtitle. Reading the index rather than the
#                             PARTS map means this opener cannot disagree with
#                             the book it opens: reorder a part and rebuild,
#                             and the opener follows.
#   ../../<slug>/...          Every other chapter's build script, scanned to
#                             see which parts read which.
#
# THE BEATS ARE A JUDGMENT and are stated here, the same way PARTS states the
# order rather than inferring it. A chapter's beat is not recoverable from its
# files.
#
# ---------------------------------------------------------------------------
# WHAT IS WRITTEN
# ---------------------------------------------------------------------------
# Three tables in derived/:
#
#   derived/chapters.csv   The part, in teaching order, with each chapter's
#                          beat and what it is about.
#   derived/beats.csv      The beats this part runs, and how many chapters each holds.
#   derived/reuse.csv      Where this part sits: what it exports, what it
#                          imports.
#
# Run from this directory:  Rscript build-data.R      (no internet needed)
# ---------------------------------------------------------------------------

dir.create("derived", showWarnings = FALSE)
source("../../../_lib/precision.R")

options(scipen = 999, stringsAsFactors = FALSE)

PART  <- "VI putting data together"
LABS  <- "../../.."
INDEX <- file.path(LABS, "INDEX.md")
stopifnot(file.exists(INDEX))

# Chapter folders live under a part directory (01-counting-people, ...), so a
# slug does not resolve against LABS on its own. Build the map from the
# directories themselves, once, so it survives the next reorganisation.
PARTDIRS <- list.dirs(LABS, recursive = FALSE)
PARTDIRS <- PARTDIRS[grepl("/[0-9]{2}-[a-z0-9-]+$", PARTDIRS)]
CAND  <- c(unlist(lapply(PARTDIRS, function(d) {
             x <- list.dirs(d, recursive = FALSE); x[x != d] })),
           setdiff(list.dirs(LABS, recursive = FALSE), c(LABS, PARTDIRS)))
ALLCH <- CAND[file.exists(file.path(CAND, paste0(basename(CAND), "-brief.Rmd")))]
names(ALLCH) <- basename(ALLCH)
path_of <- function(s) unname(ALLCH[s])
stopifnot(length(ALLCH) > 50)


# --- 1. Read the part out of the index --------------------------------------
#
# INDEX.md rows are pipe-delimited: part | kind | `slug` | [title](href) | ...
# The order of the rows IS the teaching order, because make-index sorts by it.

rows <- readLines(INDEX, warn = FALSE)
rows <- rows[startsWith(rows, "| ")]
f    <- strsplit(rows, "\\s*\\|\\s*")
part <- vapply(f, function(x) if (length(x) > 1) x[[2]] else "", "")
keep <- f[part == PART]
stopifnot(length(keep) > 3)   # the part was found and is not a stub

slug  <- vapply(keep, function(x) gsub("`", "", x[[4]]), "")
# Drop the opener itself. It is a chapter in this part, but it is the chapter
# the reader is holding, and listing "you are here" in the part's own contents
# is noise -- it would also sit ahead of the source chapter and break the
# contiguity check below for a reason that is not a real ordering problem.
drop  <- startsWith(slug, "part-")
keep  <- keep[!drop]
slug  <- slug[!drop]
title <- vapply(keep, function(x) sub("^\\[(.*?)\\].*", "\\1", x[[5]]), "")
topic <- vapply(keep, function(x) if (length(x) >= 7) x[[7]] else "", "")

# --- 2. The beats -----------------------------------------------------------
#
# The beats, in the order a part runs them. Assigning a chapter to one is
# a judgment about what the chapter is FOR, so the map is explicit and a
# chapter missing from it is reported rather than guessed at.

# THIS PART RUNS ONE BEAT, AND THE ABSENCE OF THE OTHERS IS THE POINT. It
# introduces no instrument, because it introduces no new data: every file it
# touches was met in Parts I-V. What it adds is COMBINATION -- an imputed race
# joined to a precinct return, a confidence interval laid over an estimate, a
# map graded against a rule -- and every chapter here takes a number somebody
# built and grades it. That is the critique beat, start to finish.
BEAT <- c(
  "uncertainty"   = "5 critique",
  "cost-of-voting"= "5 critique",
  "bisg-check"    = "5 critique",
  "rpv"           = "5 critique",
  "vote-dilution" = "5 critique",
  "sweet-spot"    = "5 critique",
  "redistricting" = "5 critique")
beat <- unname(ifelse(slug %in% names(BEAT), BEAT[slug], "4 use"))


# TECHNIQUE COMPANIONS, AND WHY THEY ARE EXEMPT FROM THE ORDERING TEST BELOW.
#
# A display chapter is taught beside the chapter whose data it draws --
# pie-radar after campaign-finance, sparklines after historical-campaigns --
# because that is the moment it is worth twenty minutes. make-index.py states
# that rule: "Technique chapters sit beside the substantive chapter they serve
# rather than being collected at the end, because they are taught as
# companions."
#
# That collides with the contiguity rule. A companion is a "4 use" chapter and
# its parent may be an instrument, so the companion lands inside an earlier
# beat's block and the check below fails on an order that is deliberately
# right. Both rules are right, so the companion is MARKED and exempted rather
# than moved to the end.
#
# The set is narrow on purpose: a companion here is a display or technique
# chapter that draws no data of its own. Chapters that merely reuse a sibling's
# file to answer their own question -- primary-defeats, poll-simulation,
# uncertainty -- are substantive and stay in the test.
COMPANION <- character(0)
companion <- slug %in% COMPANION

chapters <- data.frame(position = seq_along(slug), beat = beat,
                       companion = companion,
                       chapter = slug, title = title, topic = topic)
dd_write_csv(chapters, "derived/chapters.csv")

# The beats must be CONTIGUOUS among the substantive chapters -- a part whose
# access chapter sits in the middle of the applied ones is a part in the wrong
# order, and this is the check that catches it. Companions are exempt, above.
stopifnot(!is.unsorted(match(beat[!companion], sort(unique(beat[!companion])))))

beats <- as.data.frame(table(beat), stringsAsFactors = FALSE)
names(beats) <- c("beat", "chapters")
# Looked up by NAME, not by position: no part runs every beat, and
# indexing a fixed vector would silently mislabel the ones that do not.
WHAT <- c(
  "1 source"     = "What this kind of data is, and what it structurally cannot say",
  "2 instrument" = "One chapter per instrument that produces it",
  "3 instrument" = "One chapter per instrument that produces it",
  "2 access"     = "How you actually get hold of it, and what each route hands you",
  "3 access"     = "How you actually get hold of it, and what each route hands you",
  "2 use"        = "Chapters that use the data to answer a question",
  "3 use"        = "Chapters that use the data to answer a question",
  "4 use"        = "Chapters that use the data to answer a question",
  "5 critique"   = "Chapters that turn on the instrument after it has been used")
stopifnot(all(beats$beat %in% names(WHAT)))
beats$what_it_does <- unname(WHAT[beats$beat])
dd_write_csv(beats, "derived/beats.csv")

# --- 3. Where this part sits in the book ------------------------------------
#
# Two directions, and for most parts both numbers are SMALL. That is the
# finding, not a disappointment: this book is a set of independent
# investigations that share a method, not a dependency tree. A part that
# exports to nobody is not peripheral -- it is terminal, which is what a part
# made of applications should be.

mine   <- slug
others <- setdiff(names(ALLCH), mine)
others <- others[file.exists(file.path(path_of(others), paste0(others, "-brief.Rmd")))]

txt_of <- function(s) {
  p <- c(file.path(path_of(s), "data", "build-data.R"),
         file.path(path_of(s), paste0(s, "-brief.Rmd")))
  paste(unlist(lapply(p[file.exists(p)], readLines, warn = FALSE)), collapse = "\n")
}
reads <- function(t, set) any(vapply(set, function(m)
  grepl(paste0("\\.\\./(?:[^\"' ]*/)?", m, "/data/"), t, perl = TRUE), logical(1)))

tt  <- vapply(others, txt_of, "")
mt  <- vapply(mine[file.exists(file.path(path_of(mine), paste0(mine, "-brief.Rmd")))],
              txt_of, "")
exp_ <- sum(vapply(tt, reads, logical(1), set = mine))
imp_ <- sum(vapply(mt, reads, logical(1), set = others))

reuse <- data.frame(
  quantity = c("Chapters in this part",
               "Chapters elsewhere that read this part's files",
               "Chapters here that read another part's files"),
  value = c(length(mine), exp_, imp_),
  unit = "chapters")
dd_write_csv(reuse, "derived/reuse.csv")

# --- report -----------------------------------------------------------------

cat(sprintf("\nchapters.csv : %d chapters in %s\n", nrow(chapters), PART))
print(beats, row.names = FALSE)
cat("\nreuse.csv\n")
print(reuse, row.names = FALSE)
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
