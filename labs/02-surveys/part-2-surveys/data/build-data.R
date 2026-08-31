# ---------------------------------------------------------------------------
# Build the datasets for the SECTION II opener: "Survey Data".
#
# TEMPLATE NOTE. Copied from the Section I opener, and meant to stay in step
# with it. An opener is not a table of contents. It has the same obligation as
# every other document in this book: say something the reader can check, from
# data this script computed. What makes an opener different is only its
# SUBJECT -- the section itself, and what its documents have in common.
#
# THE ARGUMENT.
#
#   THE SECTION IS DEFINED BY PROVENANCE, NOT TOPIC. Everything in Section II
#   exists because a researcher chose to ask, and a respondent chose to
#   answer. Nobody was ordered to do either. That is the difference from
#   Section I, where the law compels the answer, and from Section III, where
#   nobody asked anybody anything.
#
#   THE SECTION IS A RUN OF CLUSTERS. Each cluster is one data-type chapter --
#   the reading -- and then the briefs, the labs, that use that chapter's
#   data. The opener names the clusters so a reader knows where they are, and
#   the clusters are read from the book's own section map rather than retyped
#   here.
#
#   THIS SECTION IS A DESTINATION, NOT A SUPPLY LINE. Survey answers do not
#   become inputs to anything else in this book, and the export count in
#   reuse.csv is written out so the opener can say so from a measured number
#   rather than an impression.
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
#                             title and subtitle; and every document's build
#                             script and brief, scanned for who reads whose
#                             files.
#
# ---------------------------------------------------------------------------
# WHAT IS WRITTEN
# ---------------------------------------------------------------------------
# Three tables in derived/:
#
#   derived/contents.csv   The section, in reading order: cluster, role,
#                          document, title, subtitle.
#   derived/clusters.csv   One row per cluster: its reading and its labs.
#   derived/reuse.csv      Where this section sits in the book's dependency
#                          graph, in both directions.
#
# Run from this directory:  Rscript build-data.R      (no internet needed)
# ---------------------------------------------------------------------------

dir.create("derived", showWarnings = FALSE)
source("../../../_lib/precision.R")

options(scipen = 999, stringsAsFactors = FALSE)

SECTION <- "II"                             # the roman numeral of this section
LABS    <- "../../.."

# Doc folders live under a top-level directory (02-surveys, ...), so a slug
# does not resolve against LABS on its own. Build the map from the
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

# --- 2. Where this section sits in the book ---------------------------------
#
# Two directions, and both numbers are expected to be SMALL. That is the
# finding, not a disappointment: this book is a set of independent
# investigations that share a method, not a dependency tree. A section that
# exports to nobody is not peripheral -- it is terminal, which is what a
# section about what people think should be.

mine   <- slug
# The opener itself is neither inside a cluster nor "outside the section":
# it is the page being built, so it is excluded from both sides of the count.
others <- setdiff(names(ALLCH), c(mine, "part-2-surveys"))
others <- others[file.exists(file.path(path_of(others),
                                       paste0(others, "-brief.Rmd")))]

txt_of <- function(s) {
  p <- c(file.path(path_of(s), "data", "build-data.R"),
         file.path(path_of(s), paste0(s, "-brief.Rmd")))
  paste(unlist(lapply(p[file.exists(p)], readLines, warn = FALSE)),
        collapse = "\n")
}
reads <- function(t, set) any(vapply(set, function(m)
  grepl(paste0("\\.\\./(?:[^\"' ]*/)?", m, "/data/"), t, perl = TRUE),
  logical(1)))

tt  <- vapply(others, txt_of, "")
mt  <- vapply(mine, txt_of, "")
exp_ <- sum(vapply(tt, reads, logical(1), set = mine))
imp_ <- sum(vapply(mt, reads, logical(1), set = others))

reuse <- data.frame(
  quantity = c("Docs in this section",
               "Docs elsewhere that read this section's files",
               "Docs here that read another section's files"),
  value = c(length(mine), exp_, imp_),
  unit = "docs")
dd_write_csv(reuse, "derived/reuse.csv")

# --- report -----------------------------------------------------------------

cat(sprintf("\ncontents.csv : %d docs in section %s\n", nrow(contents),
            SECTION))
print(clusters, row.names = FALSE)
cat("\nreuse.csv\n")
print(reuse, row.names = FALSE)
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
