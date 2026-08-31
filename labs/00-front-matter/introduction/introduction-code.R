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

# ---- THE PARTS, READ OUT OF THE INDEX ---------------------------------------
# The part names, their order and their sizes come from INDEX.md, which
# _lib/make-index.py generates from the corpus. Reorganise the book and this
# table follows; it cannot describe a structure the book does not have. The
# one-line description of each part is editorial and lives here, keyed by the
# part's numeral so that renaming a part does not silently orphan its blurb.
IDX  <- readLines(file.path(LABS, "INDEX.md"), warn = FALSE)
IDX  <- IDX[startsWith(IDX, "| ")]
fld  <- strsplit(IDX, "\\s*\\|\\s*")
prt  <- vapply(fld, function(x) if (length(x) > 1) x[[2]] else "", "")
kind <- vapply(fld, function(x) if (length(x) > 2) x[[3]] else "", "")
keep <- prt != "Part" & prt != "---" & prt != "front matter" &
        prt != "unassigned" & kind != "part opener"
ABOUT <- c(
  I   = "One agency that counts people and places, and where population numbers come from",
  II  = "Asking a sample of people questions: public opinion, and polls that predict elections",
  III = "The certified result, and the machinery that produced it",
  IV  = "What a member of Congress, a campaign or a lobbyist leaves on the record",
  V   = "Records made to run a system, about people who never volunteered for anything",
  VI  = "Combining the sources, which is where the hard questions and the wrong answers are")
pn   <- factor(prt[keep], levels = unique(prt[keep]))
PT   <- data.frame(part = levels(pn), chapters = as.integer(table(pn)),
                   row.names = NULL)
# Display names are spelled out rather than title-cased from the index, because
# title-casing turns "the census bureau" into "The census bureau" -- a proper
# noun is not something a string function can find.
NAME <- c(I   = "The Census Bureau",
          II  = "Surveys",
          III = "Elections",
          IV  = "Records of Political Actors",
          V   = "Records of Ordinary People",
          VI  = "Putting Data Together")
PT$numeral <- sub(" .*$", "", PT$part)
PT$part    <- paste0(PT$numeral, ". ", unname(NAME[PT$numeral]))
PT$about   <- unname(ABOUT[PT$numeral])
stopifnot(nrow(PT) >= 5, !any(is.na(PT$about)), !any(is.na(NAME[PT$numeral])),
          sum(PT$chapters) > 50)

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
# The tables handed over, and the questions asked of them. Both live at the
# foot of ## Sources; both arrived on 29 Aug 2026, and before that the
# introduction promised a move no chapter made.
NGIVE <- sum(vapply(SRC, has, logical(1), "\\*\\*The data itself\\*\\*"))
NTURN <- sum(vapply(SRC, has, logical(1), "\\*\\*Your turn\\*\\*"))
stopifnot(NTURN == NGIVE, NADDR > 0, NEXH > 0, NLIM > 0, NNUM == NCH)

# ---- THE OPENING EXHIBIT, from the election-returns chapter's own data -----
# Read from that chapter's folder rather than copied here, so this page cannot
# disagree with the chapter it is introducing. The FIPS column is text: read as
# a number it loses the leading zero on every state from Alabama to
# Connecticut, which is the second thing this file is about.
DS   <- chap("data-sources", "data", "derived")
o20  <- read.csv(file.path(DS, "pres2020_counties.csv"), stringsAsFactors = FALSE,
                 colClasses = c(county_fips = "character"))
o24  <- read.csv(file.path(DS, "pres2024_counties.csv"), stringsAsFactors = FALSE,
                 colClasses = c(county_fips = "character"))
naive <- read.csv(file.path(DS, "pres2024_counties.csv"), stringsAsFactors = FALSE)
sw <- merge(o20[, c("county_fips", "votes_dem", "votes_gop")],
            o24[, c("county_fips", "votes_dem", "votes_gop")],
            by = "county_fips", suffixes = c("_20", "_24"))
sw$r20   <- 100 * sw$votes_gop_20 / (sw$votes_gop_20 + sw$votes_dem_20)
sw$r24   <- 100 * sw$votes_gop_24 / (sw$votes_gop_24 + sw$votes_dem_24)
sw$swing <- round(sw$r24 - sw$r20, 2)
DCSW  <- sw$swing[sw$county_fips == "11001"]
MEDSW <- median(sw$swing)
DC20  <- sum(o20$total_votes[o20$state_name == "District of Columbia"])
DC24  <- o24$total_votes[o24$county_fips == "11001"]
DCSHR <- 100 * DC24 / DC20
NBAD  <- sum(nchar(naive$county_fips) == 4)     # rows a numeric read would break

# ---- three more exhibits, each from the chapter it belongs to --------------
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

fv <- function(file, key, col = "key") {
  d <- read.csv(file, stringsAsFactors = FALSE)
  v <- d$value[d[[col]] == key]
  if (!length(v)) stop("introduction-brief: no key '", key, "' in ", file)
  v[1]
}
FM   <- chap("false-matches", "data", "derived", "facts.csv")
JAN1 <- as.numeric(fv(FM, "jan1_n"))
JAN1M <- as.numeric(fv(FM, "jan1_mult"))
NJN  <- as.numeric(fv(FM, "nj_n"))
DEMO <- chap("demographics", "data", "derived", "facts.csv")
STEP <- as.numeric(fv(DEMO, "prof_jump_black", col = "name"))

## ---- parttab
data.frame(Part = PT$part, Chapters = n(PT$chapters), What_it_covers = PT$about)

## ---- movetab
data.frame(
  the_move = c("Where it came from, and why",
               "What it actually looks like",
               "What it says",
               "What it cannot say",
               "Your turn"),
  what_that_means = c(
    "Who produced this file, under what obligation, and for whose purpose — which was almost never yours. The address is printed, so you can go and get it yourself",
    "One real record, shown in full before anything is summarised — because a row is where the surprises are. The chapters that skip it have nothing small enough to print",
    "Summary numbers and figures, built from the file in front of you, with the wrong reading shown as well as the right one. Every number is computed from the data as the page is built, never typed in",
    "The question this source will not answer however carefully you ask — a limit built into the file, not into the analysis",
    "The tables the figures rest on, linked so you can open them in a spreadsheet, and questions the chapter did not answer"),
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
