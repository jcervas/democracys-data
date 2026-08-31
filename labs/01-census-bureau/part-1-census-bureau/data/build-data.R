# ---------------------------------------------------------------------------
# Build the datasets for the SECTION I opener: "Data About the Population".
#
# TEMPLATE NOTE. This is the first of the section openers, and it is meant to
# be copied. An opener is not a table of contents. It has the same obligation
# as every other document in this book: say something the reader can check,
# from data this script computed. What makes an opener different is only its
# SUBJECT -- the section itself, and what its documents have in common.
#
# THE ARGUMENT.
#
#   THE SECTION IS DEFINED BY PROVENANCE, NOT TOPIC. Everything in Section I
#   exists because somebody was ORDERED to produce it. Article I orders a
#   headcount; P.L. 94-171 orders it delivered to the states; Section 203 of
#   the Voting Rights Act orders language determinations computed from it.
#   Nobody in this section collected data because they wanted a dataset. That
#   is the difference between this section and Section II, where every number
#   exists because a researcher chose to ask.
#
#   THE SECTION IS A RUN OF CLUSTERS. Each cluster is one data-type chapter --
#   the reading -- and then the briefs, the labs, that use that chapter's
#   data. The opener names the clusters so a reader knows where they are, and
#   the clusters are read from the book's own section map rather than retyped
#   here.
#
#   YOU WILL MEET THIS DATA AGAIN, AND MOSTLY NOT THROUGH THIS SECTION. The
#   honest version of a claim it would be easy to overstate: this is NOT the
#   section everything else divides by -- few documents elsewhere read this
#   section's derived files -- it is the SOURCE everything else keeps going
#   back to, independently. The two counts are both written out for exactly
#   that reason. Quoting the larger one alone would make the section sound
#   load-bearing in a way the dependency graph does not support.
#
# ---------------------------------------------------------------------------
# SOURCES
# ---------------------------------------------------------------------------
#
# The corpus itself, which is why nothing here is downloaded.
#
#   ../../_lib/make-index.py  The book's section map, SECTIONS: which cluster
#                             each document belongs to, and in what order.
#                             make-index.py is authoritative for placement
#                             (INDEX.md is generated from it), so reading the
#                             map means this opener cannot disagree with the
#                             book it opens: move a document and rebuild, and
#                             the tables follow.
#   ../../<slug>/...          Every document's own front matter, read for its
#                             title and subtitle; and every other document's
#                             build script and brief, scanned to count who
#                             reaches back to the Census Bureau on their own.
#
# ---------------------------------------------------------------------------
# WHAT IS WRITTEN
# ---------------------------------------------------------------------------
# Three tables in derived/:
#
#   derived/contents.csv   The section, in reading order: cluster, role,
#                          document, title, subtitle.
#   derived/clusters.csv   One row per cluster: its reading and its labs.
#   derived/reuse.csv      Who outside this section uses this section's source.
#
# Run from this directory:  Rscript build-data.R      (no internet needed)
# ---------------------------------------------------------------------------

dir.create("derived", showWarnings = FALSE)
source("../../../_lib/precision.R")

options(scipen = 999, stringsAsFactors = FALSE)

SECTION <- "I"                              # the roman numeral of this section
LABS    <- "../../.."

# Doc folders live under a top-level directory (01-census-bureau, ...), so a
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

# --- 1. Read the section out of the book's section map ----------------------
#
# _lib/make-index.py holds SECTIONS, the authoritative placement of every
# document: sections, clusters, and roles. INDEX.md is generated FROM it, and
# can lag behind it, so the map itself is what is read here. Each cluster is a
# tuple: ("Cluster Name", [chapter slugs], [brief slugs]).

MK <- readLines(file.path(LABS, "_lib", "make-index.py"), warn = FALSE)
start <- grep(sprintf('dict\\(name="%s\\. ', SECTION), MK)
stopifnot(length(start) == 1)
end <- grep("^\\s*\\]\\),\\s*$", MK)
end <- min(end[end > start])
block <- paste(MK[start:end], collapse = " ")

tup <- regmatches(block, gregexpr(
  '\\("([^"]+)",\\s*\\[[^]]*\\],\\s*\\[[^]]*\\]\\s*\\)', block))[[1]]
stopifnot(length(tup) > 1)

slugs_in <- function(s) regmatches(s, gregexpr('"[a-z0-9-]+"', s))[[1]]

cluster <- character(0); cluster_name <- character(0)
type <- character(0); slug <- character(0)
for (i in seq_along(tup)) {
  cname    <- sub('^\\("([^"]+)".*', "\\1", tup[i])
  parts    <- strsplit(tup[i], "\\[")[[1]]
  chapters <- gsub('"', "", slugs_in(paste0("[", parts[2])))
  briefs   <- gsub('"', "", slugs_in(paste0("[", parts[3])))
  n        <- length(chapters) + length(briefs)
  cluster      <- c(cluster, rep(sprintf("%s.%d", SECTION, i), n))
  cluster_name <- c(cluster_name, rep(cname, n))
  type         <- c(type, rep("chapter", length(chapters)),
                          rep("brief",   length(briefs)))
  slug         <- c(slug, chapters, briefs)
}
stopifnot(all(slug %in% names(ALLCH)))

# Titles and subtitles come from each document's own front matter, so a
# retitled chapter retitles itself here on the next build.
meta <- function(s, key) {
  y <- readLines(file.path(path_of(s), paste0(s, "-brief.Rmd")),
                 n = 30, warn = FALSE)
  v <- grep(sprintf("^%s:", key), y, value = TRUE)
  if (!length(v)) return("")
  gsub('^"|"$', "", sub(sprintf("^%s:\\s*", key), "", v[1]))
}
title <- vapply(slug, meta, "", key = "title")
topic <- vapply(slug, meta, "", key = "subtitle")
stopifnot(all(nchar(title) > 0))

contents <- data.frame(position = seq_along(slug), cluster = cluster,
                       cluster_name = cluster_name, type = type,
                       doc = slug, title = title, topic = topic)
dd_write_csv(contents, "derived/contents.csv")

# One row per cluster: the reading that leads it, and how many labs follow.
# A cluster may hold no chapter yet (the map allows zero or two); the opener's
# table prints a dash for those rather than inventing a reading.
ord <- unique(cluster)
stopifnot(!is.unsorted(match(cluster, ord)))
first_reading <- vapply(ord, function(k) {
  t <- title[cluster == k & type == "chapter"]
  if (length(t)) t[1] else ""
}, "")
clusters <- data.frame(
  cluster = ord,
  name    = cluster_name[match(ord, cluster)],
  reading = first_reading,
  briefs  = vapply(ord, function(k) sum(cluster == k & type == "brief"), 0L))
dd_write_csv(clusters, "derived/clusters.csv")

# --- 2. Who else goes to this source ----------------------------------------
#
# Two different questions, and the difference is the point:
#   (a) who READS THIS SECTION'S FILES -- a hard dependency, very few;
#   (b) who GOES TO THE BUREAU THEMSELVES -- a shared source, many.
# Reporting only (b) would overstate the section's centrality; reporting only
# (a) would understate how often a reader meets this data again.

mine   <- slug
# The opener itself is neither inside a cluster nor "outside the section":
# it is the page being built, so it is excluded from both sides of the count.
others <- setdiff(names(ALLCH), c(mine, "part-1-census-bureau"))
others <- others[file.exists(file.path(path_of(others),
                                       paste0(others, "-brief.Rmd")))]

txt_of <- function(s) {
  p <- c(file.path(path_of(s), "data", "build-data.R"),
         file.path(path_of(s), paste0(s, "-brief.Rmd")))
  paste(unlist(lapply(p[file.exists(p)], readLines, warn = FALSE)),
        collapse = "\n")
}
BUREAU <- "census\\.gov|P\\.L\\. 94-171|acsdt|popest|American Community Survey"
reads_mine <- function(t) any(vapply(mine, function(m)
  grepl(paste0("\\.\\./(?:[^\"' ]*/)?", m, "/data/"), t, perl = TRUE),
  logical(1)))

tt   <- vapply(others, txt_of, "")
goes <- vapply(tt, function(t) grepl(BUREAU, t, ignore.case = TRUE),
               logical(1))
deps <- vapply(tt, reads_mine, logical(1))

reuse <- data.frame(
  quantity = c("Docs outside this section",
               "Of those, that go to the Census Bureau themselves",
               "Of those, that read this section's own files"),
  value = c(length(others), sum(goes), sum(deps)),
  unit = "docs")
dd_write_csv(reuse, "derived/reuse.csv")

# --- report -----------------------------------------------------------------

cat(sprintf("\ncontents.csv : %d docs in section %s\n", nrow(contents),
            SECTION))
print(clusters, row.names = FALSE)
cat("\nreuse.csv\n")
print(reuse, row.names = FALSE)
cat(sprintf("\n  goes to the Bureau : %s\n",
            paste(sort(names(goes)[goes]), collapse = ", ")))
cat(sprintf("  reads our files    : %s\n",
            paste(sort(names(deps)[deps]), collapse = ", ")))
cat("\ndone.\n")

# ---------------------------------------------------------------------------
# Build stamp. Records which script produced what is now in this directory --
# every file under derived/ and raw/ with its size, hash and row count, and
# the date this ran -- into BUILD-STAMP.tsv beside the data. See
# ../../../_lib/provenance.R. Guarded, because a missing helper must not fail
# a build that was otherwise fine.
if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_stamp()
}
