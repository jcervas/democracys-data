# introduction-code.R -- chunk bodies for introduction-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.6,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

pc <- function(x, k = 2) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",")

# ---- render every data.frame in this document as a TABLE, not code output ---
# These are front-facing documents. A data.frame printed the ordinary way comes
# out as a "##"-prefixed code block, which reads as machinery rather than as a
# result. Registering knit_print for data.frame turns all of them into real
# tables in both HTML and PDF without touching a single chunk.
knit_print.data.frame <- function(x, ...) {
  nm <- names(x)
  nm <- gsub("_", " ", nm)                        # fails_when -> fails when
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)    # sentence case the first letter
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

# ---- THE CORPUS, COUNTED BY READING IT ------------------------------------
# An introduction that asserts a discipline and then exempts itself from it is
# not worth reading. Every claim below about the size and shape of this book is
# measured from the book, at the moment this page is built, by opening the
# chapter files and counting. Adding or removing a chapter changes these
# sentences with no editing at all.
# Chapters sit at labs/<NN-part>/<slug>/, and a chapter with no part sits at
# the labs root, so both depths are globbed. This file lives one level further
# down than it used to, hence "../..".
LABS <- normalizePath("../..", mustWork = TRUE)
BRF  <- c(Sys.glob(file.path(LABS, "*", "*", "*-brief.Rmd")),
          Sys.glob(file.path(LABS, "*", "*-brief.Rmd")))
# Exclude _archive and _lib by looking at the FOLDER NAMES only. Testing the
# whole path for "/_" would match this corpus's own absolute path, which
# contains "_teaching" and "_Democracy's Data", and would discard everything.
BRF  <- BRF[!startsWith(basename(dirname(BRF)), "_") &
            !startsWith(basename(dirname(dirname(BRF))), "_")]
BRF  <- BRF[basename(dirname(BRF)) != "introduction"]   # not this file
if (!length(BRF)) stop("introduction-brief: found no chapters beside me")

# Chapters this file reads by name are addressed through a lookup rather than
# a fixed path, because which part a chapter is filed under can change.
CH <- setNames(dirname(BRF), basename(dirname(BRF)))
chap <- function(slug, ...) {
  if (is.na(CH[slug])) stop("introduction-brief: no chapter named ", slug)
  file.path(CH[slug], ...)
}
SRC  <- lapply(BRF, readLines, warn = FALSE, encoding = "UTF-8")

NCH  <- length(BRF)
NDIR <- length(unique(dirname(BRF)))
NDAT <- sum(dir.exists(file.path(unique(dirname(BRF)), "data")))

# The data-directory claim is a fraction only when it is really a fraction.
# While every chapter carries its own data, "96 of the 96" states a universal
# as though it were a count, which reads as though some chapter had been left
# out. Both phrasings live here and the sentence picks one at build time, so a
# chapter added without its data changes the wording rather than hiding in it.
DATC <- if (NDAT == NDIR) "Every chapter folder carries its own" else
        paste0(n(NDAT), " of the ", n(NDIR), " chapter folders carry their own")

# The same treatment for the build scripts, and for the same reason: the
# sentence that follows DATC used to assert the script as a flat universal, and
# a chapter whose data arrived without one would not have disturbed it. Counted
# by extension rather than by filename, because the name varies -- build-data.R
# in most folders, but also build-data.py, assemble.R, build-maps.R, and in a
# few folders a pair of them. What the sentence claims is that the code which
# produced the files sits beside the files, not that it is always called the
# same thing.
NBLD <- sum(vapply(file.path(unique(dirname(BRF)), "data"), function(d)
             dir.exists(d) && length(list.files(d, pattern = "\\.(R|py|sh)$")) > 0,
             logical(1)))
SCRC <- if (NBLD == NDAT) "and each of those directories also holds" else
        paste0("and ", n(NBLD), " of them also hold")

# And the build stamp, counted the same way. BUILD-STAMP.tsv is written by
# `_lib/provenance.R`; it names the script that produced the directory and
# lists every file with its size, hash and modification date. What it does NOT
# always carry is the date the script ran: a stamp taken by reading a directory
# after the fact says so in its `stamp_source` column, and only a chapter that
# has been rebuilt since the stamps were introduced can claim a real run date.
# So the sentence below stops at what every stamp does carry.
NSTM <- sum(file.exists(file.path(unique(dirname(BRF)), "data", "BUILD-STAMP.tsv")))
STMC <- if (NSTM == NDAT)
          "and beside both a stamp listing every one of those files" else
          paste0("and ", n(NSTM), " of them a stamp listing every file")

# ---- THE SECTIONS, READ OUT OF THE INDEX ------------------------------------
# The section names, their order and their sizes come from INDEX.md, which
# _lib/make-index.py generates from the corpus. Reorganise the book and this
# table follows; it cannot describe a structure the book does not have. The
# one-line description of each section is editorial and lives here, keyed by
# the section's numeral so renaming a section does not silently orphan its
# blurb. INDEX.md's first column is "Section / Cluster" ("I. Census &
# Geography" on intro rows, "I.1 The Census and Its Products" on cluster
# rows); the leading roman numeral is the section. Its second column is the
# doc's type: intro, chapter, or brief.
IDX  <- readLines(file.path(LABS, "INDEX.md"), warn = FALSE)
IDX  <- IDX[startsWith(IDX, "| ")]
fld  <- strsplit(IDX, "\\s*\\|\\s*")
sec  <- vapply(fld, function(x) if (length(x) > 1) x[[2]] else "", "")
typ  <- vapply(fld, function(x) if (length(x) > 2) x[[3]] else "", "")
keep <- grepl("^[IV]+[. ]", sec) & typ %in% c("intro", "chapter", "brief")
ABOUT <- c(
  I   = "Counting people and places: the enumeration, the rolling survey, the estimates between, and the geography they are published on",
  II  = "Asking a sample of people questions, and weighting the answers until they describe the country",
  III = "Records kept to run a system: returns, rolls, filings, votes, stops",
  IV  = "Combining the kinds, which is where the hard questions and the wrong answers are")
num  <- sub("^([IV]+)[. ].*$", "\\1", sec)
pn   <- factor(num[keep], levels = unique(num[keep]))
# Display names are spelled out rather than harvested from the index, because
# a proper noun is not something a string function can find.
NAME <- c(I   = "Data About the Population",
          II  = "Survey Data",
          III = "Administrative Data",
          IV  = "Putting Data Together")
PT   <- data.frame(numeral = levels(pn), docs = as.integer(table(pn)),
                   row.names = NULL)
PT$section <- paste0(PT$numeral, ". ", unname(NAME[PT$numeral]))
PT$about   <- unname(ABOUT[PT$numeral])
stopifnot(nrow(PT) == 4, !any(is.na(PT$about)), !any(is.na(NAME[PT$numeral])),
          sum(PT$docs) > 100)
# Chapters and briefs, counted from the same column of the same index.
NCHAP <- sum(typ[keep] == "chapter")
NBRF  <- sum(typ[keep] == "brief")
stopifnot(NCHAP >= 10, NBRF >= 80)

has <- function(x, p) any(grepl(p, x, perl = TRUE))
# A prediction prompt, in either of the two forms the chapters use: a set-off
# blockquote, or a bolded instruction in the running text.
P_BQ <- "^> ?\\*\\*(Before|Write down|Stop here|Predict|Answer this|Commit)"
P_IN <- "\\*\\*Before (you|turning|Step|reading)"
NPRED <- sum(vapply(SRC, function(x) has(x, P_BQ) || has(x, P_IN), logical(1)))
# The closing move: a late section that says what the evidence will and will not
# bear. The chapters phrase that heading five ways -- "can and cannot support",
# "can testify to", "will bear", "will and will not carry", and once "what the
# crosswalk licenses" -- so the pattern has to admit all five rather than the
# one this introduction happens to like.
P_LIM <- "^#{2,3} .*(cannot|can testify|will bear|will not|licenses)"
NLIM  <- sum(vapply(SRC, has, logical(1), P_LIM))
NSRC  <- sum(vapply(SRC, has, logical(1), "^#{2,3} Sources"))

# ---- THE MOVES, COUNTED THE SAME WAY --------------------------------------
# The table below used to assert that a chapter is "always the same four
# moves". It was not true of two of the four, and the table had no way of
# noticing: nothing connected it to the chapters it described. So each move is
# now counted, by the same reading of the same files, and the table prints the
# count beside the claim. A move that stops being usual will say so here
# without anybody editing this file.
#
# The address. Every chapter that reads data names where the data came from
# and prints the address, which _lib/check-sources.py enforces. The count
# falls short of NCH by the part openers, which carry no data of their own,
# and by the one chapter whose source is a printed book with an ISBN and no
# URL.
NADDR <- sum(vapply(SRC, has, logical(1), "https?://"))
# The exhibit: one real record shown before anything is summarised. Detected
# by the chunk that prints it or by the heading above it. This is the move the
# old table was most wrong about -- it is the practice of about half the
# chapters, not all of them, and saying "always" taught a reader to expect
# something the book does not always do.
P_EXH <- paste0("^```\\{r (one-row|onerow|raw|peek|exhibit|record)\\b",
                "|^#{2,3} .*(One row|one row|What arrives|What a row",
                "|actually looks like)")
NEXH  <- sum(vapply(SRC, has, logical(1), P_EXH))
# Numbers computed from the file rather than typed into the prose. This is the
# house rule the whole book rests on, so it should be universal, and it is.
NNUM  <- sum(vapply(SRC, has, logical(1), "`r [^`]"))
# The tables handed over, and the questions asked of them. The tables live at
# the foot of ## Sources; the questions live either there under **Your turn**
# (2nd-edition chapters) or in a ## Extensions section (3rd edition), and
# during the rewrite the corpus holds both forms, so both are counted.
NGIVE <- sum(vapply(SRC, has, logical(1), "\\*\\*The data itself\\*\\*"))
NTURN <- sum(vapply(SRC, function(x)
             has(x, "\\*\\*Your turn\\*\\*") || has(x, "^#{2,3} Extensions"),
             logical(1)))
# Every doc that leaves exercises also hands over its tables. The reverse is
# not universal: a section intro may hand over the tables behind its own
# exhibits without assigning exercises, so NGIVE may exceed NTURN by the few
# intros that do.
stopifnot(NTURN <= NGIVE, NGIVE - NTURN <= 4,
          NADDR > 0, NEXH > 0, NLIM > 0, NNUM == NCH)

# ---- THE ONE WORKED EXHIBIT, from the chapter it belongs to ----------------
# The traffic-stop chapter's two denominators. Neither is dishonest, and they
# disagree about the sign of the finding, which is the point being made.
PBY <- read.csv(chap("policing", "data", "derived", "by_race.csv"),
                stringsAsFactors = FALSE)
PDN <- read.csv(chap("policing", "data", "derived", "acs_denominators.csv"),
                stringsAsFactors = FALSE)
prel <- function(dn, race) {
  m <- merge(PBY, PDN[PDN$denominator == dn, ], by = "race")
  r <- m$stops / m$count
  r[m$race == race] / r[m$race == "white"]
}
HRES <- prel("residents", "hispanic")
HDRV <- prel("drives to work", "hispanic")

## ---- sectiontab
data.frame(Section = PT$section, Docs = n(PT$docs), What_it_covers = PT$about)

## ---- movetab
data.frame(
  the_move = c("Where it came from, and why",
               "What it actually looks like",
               "What it says",
               "What it cannot say",
               "Extensions"),
  what_that_means = c(
    "Who produced this file, under what obligation, and for whose purpose — which was almost never yours. The address is printed, so you can go and get it yourself",
    "One real record, shown in full before anything is summarised — because a row is where the surprises are. The chapters that skip it have nothing small enough to print",
    "Summary numbers and figures, built from the file in front of you, with the wrong reading shown as well as the right one. Every number is computed from the data as the page is built, never typed in",
    "The question this source will not answer however carefully you ask — a limit built into the file, not into the analysis",
    "The tables the figures rest on, linked so you can open them in a spreadsheet, and questions the document left for you — answerable in a spreadsheet, plus a stretch or two beyond it"),
  chapters = c(NADDR, NEXH, NNUM, NLIM, NTURN),
  check.names = FALSE)

## ---- four
data.frame(
  the_question = c(
    "What is the source?",
    "Why is it appropriate to this question?",
    "What are the challenges — with the data, or with measuring the thing at all?",
    "Is it clean, or does it need cleaning?"),
  what_it_catches = c(
    "A file built for somebody else's purpose, and nobody's obligation to keep it right",
    "A unit of analysis that cannot carry the question being asked of it",
    "A quantity that was never observable, dressed as one that was",
    "Decisions made on your behalf, upstream, that move the answer"),
  check.names = FALSE)
