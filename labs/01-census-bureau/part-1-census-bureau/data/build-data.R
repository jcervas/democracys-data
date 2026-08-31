# ---------------------------------------------------------------------------
# Build the datasets for the PART I opener: "Counting People".
#
# TEMPLATE NOTE. This is the first of six part openers, and it is meant to be
# copied. A part opener is not a table of contents. It has the same obligation
# as every other chapter in this book: say something the reader can check, from
# data this script computed. What makes an opener different is only its
# SUBJECT -- the part itself, and what its chapters have in common.
#
# THE ARGUMENT.
#
#   THE PART IS DEFINED BY PROVENANCE, NOT TOPIC. Everything in Part I exists
#   because somebody was ORDERED to produce it. Article I orders a headcount;
#   P.L. 94-171 orders it delivered to the states; Section 203 of the Voting
#   Rights Act orders language determinations computed from it. Nobody in this
#   part collected data because they wanted a dataset. That is the difference
#   between this part and Part III, where every number exists because a
#   researcher chose to ask.
#
#   THE PART HAS A SHAPE, AND IT IS THE SAME SHAPE IN EVERY PART. Source, then
#   instruments, then access, then use. The opener names the shape so a reader
#   knows where they are, and the beats are read from the part's own order
#   rather than retyped here.
#
#   YOU WILL MEET THIS DATA AGAIN, AND MOSTLY NOT THROUGH THIS PART. Eleven of
#   the sixty-six chapters outside Part I go to the Census Bureau themselves.
#   That is the honest version of a claim it would be easy to overstate: this
#   is NOT the part everything else divides by -- only three chapters elsewhere
#   read this part's derived files, and Part II exports to more -- it is the
#   SOURCE everything else keeps going back to, independently.
#
#   The two counts are both written out for exactly that reason. Quoting the
#   larger one alone would make the part sound load-bearing in a way the
#   dependency graph does not support.
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
#   ../../<slug>/...          Every other chapter's build script and brief,
#                             scanned to count who reaches back to the Census
#                             Bureau on their own.
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
#   derived/reuse.csv      Who outside this part uses this part's source.
#
# Run from this directory:  Rscript build-data.R      (no internet needed)
# ---------------------------------------------------------------------------

dir.create("derived", showWarnings = FALSE)
source("../../../_lib/precision.R")

options(scipen = 999, stringsAsFactors = FALSE)

PART  <- "I the census bureau"
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

BEAT <- c(
  "census-source"    = "1 source",
  "census-decennial" = "2 instrument", "census-acs" = "2 instrument",
  "census-pep"       = "2 instrument", "census-geography" = "2 instrument",
  "census-access"    = "3 access",     "census-api" = "3 access")
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
COMPANION <- c("overplotting", "chord")
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

# --- 3. Who else goes to this source ----------------------------------------
#
# Two different questions, and the difference is the point:
#   (a) who READS THIS PART'S FILES -- a hard dependency, three chapters;
#   (b) who GOES TO THE BUREAU THEMSELVES -- a shared source, twelve.
# Reporting only (b) would overstate the part's centrality; reporting only (a)
# would understate how often a reader meets this data again.

mine  <- slug
others <- setdiff(names(ALLCH), mine)
others <- others[file.exists(file.path(path_of(others), paste0(others, "-brief.Rmd")))]

txt_of <- function(s) {
  p <- c(file.path(path_of(s), "data", "build-data.R"),
         file.path(path_of(s), paste0(s, "-brief.Rmd")))
  paste(unlist(lapply(p[file.exists(p)], readLines, warn = FALSE)), collapse = "\n")
}
BUREAU <- "census\\.gov|P\\.L\\. 94-171|acsdt|popest|American Community Survey"
reads_mine <- function(t) any(vapply(mine, function(m)
  grepl(paste0("\\.\\./(?:[^\"' ]*/)?", m, "/data/"), t, perl = TRUE), logical(1)))

tt   <- vapply(others, txt_of, "")
goes <- vapply(tt, function(t) grepl(BUREAU, t, ignore.case = TRUE), logical(1))
deps <- vapply(tt, reads_mine, logical(1))

reuse <- data.frame(
  quantity = c("Chapters outside this part",
               "Of those, that go to the Census Bureau themselves",
               "Of those, that read this part's own derived files"),
  value = c(length(others), sum(goes), sum(deps)),
  unit = "chapters")
dd_write_csv(reuse, "derived/reuse.csv")

# --- report -----------------------------------------------------------------

cat(sprintf("\nchapters.csv : %d chapters in %s\n", nrow(chapters), PART))
print(beats, row.names = FALSE)
cat("\nreuse.csv\n")
print(reuse, row.names = FALSE)
cat(sprintf("\n  goes to the Bureau : %s\n",
            paste(sort(names(goes)[goes]), collapse = ", ")))
cat(sprintf("  reads our files    : %s\n",
            paste(sort(names(deps)[deps]), collapse = ", ")))
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
