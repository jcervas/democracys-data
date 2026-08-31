# ---------------------------------------------------------------------------
# Build the Clerk-of-the-House dataset: the official congressional returns, and
# what it costs to read them.
#
# The third SOURCE chapter in Part II. county-returns established that no
# federal agency counts votes and that assembling a national file means going
# to fifty-one officers. There is one exception, for one office: the Clerk of
# the House publishes "Statistics of the Presidential and Congressional
# Election" after every general election. It is official, free, complete, and
# it is a DOCUMENT rather than a dataset.
#
# ---------------------------------------------------------------------------
# SOURCES
# ---------------------------------------------------------------------------
#
# Nothing is downloaded here. This chapter reads the house-competition
# chapter's own folder -- the PDFs it fetched, the text pdftotext produced from
# them, and the tables its parse wrote -- because the subject of this chapter
# is that parse.
#
#   ../../house-competition/data/raw/clerk_YYYY.pdf       the documents
#   ../../house-competition/data/derived/clerk_YYYY.txt   what pdftotext made of them
#   ../../house-competition/data/derived/clerk_house.csv  what the parse made of that
#   ../../house-competition/data/derived/races.csv        the spliced 1946-2024 series
#
# The parse itself -- ../../house-competition/data/parse-clerk.py -- carries a
# 150-line header recording every edge case, every bug it hit, and every
# disagreement that survived validation. The stated tables below are read from
# that record rather than reinvented, and it is cited in the brief as the
# source it is.
#
# THE ARGUMENT. An official document is not an official dataset, and the gap
# between them is a program somebody had to write. The Clerk prints one block
# per district, typeset for a reader:
#
#      1. Barry Moore, Republican ...........................   258,619
#         Tom Holmes, Democrat ..............................    70,929
#
# Every property that makes that readable -- the leader dots, the superscript
# footnotes, the indentation that means "same candidate, another party line" --
# is a decision the typesetter made for a human, and every one of them is a
# place a parser can go wrong while still producing a number.
#
# THE POINT OF THE CHAPTER is not that parsing is hard. It is that the three
# bugs this parse hit were all INVISIBLE IN THE AGGREGATE. Each produced a
# plausible district, a plausible state and a plausible national total. They
# were found only because an independent file covering the same years existed
# to disagree with -- and chasing the disagreements found errors in that file
# too.
#
# ---------------------------------------------------------------------------
# WHAT IS WRITTEN
# ---------------------------------------------------------------------------
# Six tables in derived/:
#
#   derived/documents.csv  the eleven publications, and how much text each is
#   derived/edges.csv      the four ways the typesetting is not a table
#   derived/bugs.csv       three parse bugs, what each did, and how big
#   derived/survivors.csv  the disagreements that outlived validation, and whose
#   derived/coverage.csv   what the spliced series can and cannot report, by year
#   derived/checks.csv     the validation results the chapter prints verbatim
#
# Run from this directory:  Rscript build-data.R      (no internet needed)
# ---------------------------------------------------------------------------

source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)

HC  <- file.path("..", "..", "house-competition", "data")
RAW <- file.path(HC, "raw")
DER <- file.path(HC, "derived")

stopifnot(dir.exists(RAW), dir.exists(DER))

# --- 1. The documents -------------------------------------------------------

pdfs <- sort(list.files(RAW, pattern = "^clerk_[0-9]{4}\\.pdf$", full.names = TRUE))
stopifnot(length(pdfs) > 5)

yr    <- as.integer(sub(".*clerk_([0-9]{4})\\.pdf$", "\\1", pdfs))
bytes <- file.info(pdfs)$size
lines <- vapply(yr, function(y) {
  f <- file.path(DER, sprintf("clerk_%d.txt", y))
  if (file.exists(f)) length(readLines(f, warn = FALSE)) else NA_integer_
}, integer(1))

documents <- data.frame(year = yr, kilobytes = bytes / 1024,
                        extracted_lines = lines)
dd_write_csv(documents, "derived/documents.csv")

# --- 2. The four edge cases -------------------------------------------------
#
# Stated, from the parse script's own header. These are properties of the
# printed page, not quantities in a file.

edges <- data.frame(
  edge_case = c("Fusion", "Top-two", "Louisiana", "At-large"),
  where = c("NY, CT, SC", "CA, WA", "LA", "single-district states"),
  what_the_page_does = c(
    "One candidate printed on several party lines, the extra lines carrying no name",
    "Two candidates of the same party facing each other in the general election",
    "Every candidate on one November ballot, with a December runoff if nobody clears 50%",
    "Printed as \"AT LARGE\" rather than \"1.\""),
  if_mishandled = c(
    "Splits a winner into pieces and invents uncontested races",
    "Counts a contested race as uncontested",
    "Mixes two elections into one row",
    "Drops the district"))
dd_write_csv(edges, "derived/edges.csv")

# --- 3. The three bugs ------------------------------------------------------

bugs <- data.frame(
  bug = c("Footnote markers read as vote totals",
          "Fusion lines listing several parties",
          "Unopposed candidates with no printed total"),
  cause = c(
    "The Clerk prints footnote references as superscripts; pdftotext drops them to the end of the line and pushes the real total onto a line of its own",
    "New York prints cross-endorsements under a candidate with no name, often as a list (\"Conservative, Libertarian\"); matching one party name missed them",
    "Florida keeps unopposed candidates off the ballot, so the PDF shows \"(1)\" where a number would go"),
  worst_case = c(
    "Ralph Abraham (LA-05, 2014) recorded with 2 votes instead of 134,616",
    "Thousands of votes unattributed; four New York districts became outliers",
    "Those districts dropped entirely"),
  scale = c("24 rows across five years", "4 districts, all resolved",
            "undercounted the headline measure, in the wrong direction"),
  status = c("fixed", "fixed", "fixed"))
dd_write_csv(bugs, "derived/bugs.csv")

# --- 4. What survived validation --------------------------------------------

survivors <- data.frame(
  disagreement = c("Louisiana", "Texas 22, 2006", "Texas 23, 2006",
                   "Connecticut, 2008 (5 districts)",
                   "Nebraska 3 (2004), Utah 1 (2006)",
                   "South Carolina 6 (2004)"),
  whose = c("Nobody's", "Nobody's", "Nobody's",
            "The academic file's", "The academic file's", "Ours"),
  why = c(
    "The Clerk substitutes runoff totals for the two finalists while leaving eliminated candidates' November numbers in place, so one row mixes two elections",
    "Tom DeLay resigned and Republicans ran a write-in campaign; write-in lines carry no party, so the Republican vote is genuinely unrecoverable",
    "Court-ordered mid-decade redistricting forced a December runoff -- same class as Louisiana",
    "He counts fusion ballot lines in New York and South Carolina but not in Connecticut; this parse counts every line in every state, as the Clerk's own Recapitulation does",
    "Both the candidate blocks and the Recapitulation give figures that disagree with him -- two independent presentations inside the primary source agreeing with each other",
    "\"Constitution\" was missing from the fusion party list, so 4,157 votes went unadded"),
  resolution = c("flagged, not fixed", "not a parse failure", "flagged, not fixed",
                 "we correct him", "we keep ours", "fixed; now matches exactly"))
dd_write_csv(survivors, "derived/survivors.csv")

# --- 5. What the parse flagged, and what the series can report ---------------

ch <- read.csv(file.path(DER, "clerk_house.csv"))
rc <- read.csv(file.path(DER, "races.csv"))

flags <- c(la_primary = sum(ch$la_primary, na.rm = TRUE),
           runoff_mixed = sum(ch$runoff_mixed, na.rm = TRUE),
           top_two = sum(ch$top_two, na.rm = TRUE),
           uncontested = sum(ch$uncontested, na.rm = TRUE))

# Presidential-by-district figures do not exist for every year, and a year with
# none must be MASKED rather than reported as complete. This is the coverage
# the spliced series can actually support.
rc$has_split <- !is.na(rc$split_district)
cov <- aggregate(has_split ~ year, data = rc, FUN = function(x) c(sum(x), length(x)))
coverage <- data.frame(year = cov$year,
                       districts_with = cov$has_split[, 1],
                       districts_total = cov$has_split[, 2])
coverage$status <- ifelse(coverage$districts_with == 0, "none",
                   ifelse(coverage$districts_with == coverage$districts_total,
                          "complete", "partial"))
dd_write_csv(coverage, "derived/coverage.csv")

NONE <- coverage$year[coverage$status == "none"]
PART <- coverage$year[coverage$status == "partial"]
FULL <- coverage$year[coverage$status == "complete"]

# The headline series the parse exists to extend.
sd <- rc[rc$has_split, ]
sd$split <- as.logical(sd$split_district)
byyr <- aggregate(split ~ year, data = sd, FUN = function(x) c(sum(x), length(x)))
split_year <- data.frame(year = byyr$year, split = byyr$split[, 1],
                         covered = byyr$split[, 2])
split_year$pct <- 100 * split_year$split / split_year$covered
PEAK <- split_year[which.max(split_year$pct), ]
Y2012 <- split_year[split_year$year == 2012, ]

src <- table(rc$source)

# --- 6. Checks --------------------------------------------------------------

f1 <- function(x) formatC(x, format = "f", digits = 1)
f2 <- function(x) formatC(x, format = "f", digits = 2)
cm <- function(x) format(round(x), big.mark = ",")

chk <- data.frame(
  check = c(
    "Publications read",
    "Elections they cover",
    "Total size of the documents, MB",
    "Lines of text pdftotext produced from them",
    "District-elections the parse recovered",
    "Typesetting edge cases the parse must handle",
    "Parse bugs found, and fixed",
    "Districts in the overlap used to validate the parse",
    "Median disagreement in two-party Democratic share, points",
    "Districts differing by more than 1 point",
    "Districts differing by more than 2 points",
    "Disagreements that survived, by whose they are",
    "Rows flagged la_primary",
    "Rows flagged runoff_mixed",
    "Rows flagged top_two",
    "Uncontested races the parse recovered",
    "Spliced series: rows",
    "Spliced series: elections",
    "Rows from the academic file",
    "Rows from the Clerk",
    "Years with no presidential-by-district figure at all",
    "Years with partial coverage",
    "Years with complete coverage",
    "Split districts at their peak",
    "Split districts in 2012"),
  value = c(
    nrow(documents),
    paste0(min(documents$year), "-", max(documents$year)),
    f1(sum(documents$kilobytes) / 1024),
    cm(sum(documents$extracted_lines, na.rm = TRUE)),
    cm(nrow(ch)),
    nrow(edges),
    nrow(bugs),
    "2,260",
    "0.03",
    "14",
    "8",
    paste0(sum(survivors$whose == "Nobody's"), " nobody's, ",
           sum(survivors$whose == "The academic file's"), " the academic file's, ",
           sum(survivors$whose == "Ours"), " ours"),
    cm(flags[["la_primary"]]),
    cm(flags[["runoff_mixed"]]),
    cm(flags[["top_two"]]),
    cm(flags[["uncontested"]]),
    cm(nrow(rc)),
    cm(length(unique(rc$year))),
    cm(src[["Jacobson"]]),
    cm(src[["Clerk of the House"]]),
    paste(NONE, collapse = ", "),
    length(PART),
    length(FULL),
    paste0(PEAK$split, " of ", PEAK$covered, " in ", PEAK$year,
           " (", f1(PEAK$pct), "%)"),
    paste0(Y2012$split, " of ", Y2012$covered, " (", f1(Y2012$pct), "%)")))
dd_write_csv(chk, "derived/checks.csv")

# Hard stops. Each is a way this chapter could be quietly wrong.
stopifnot(
  nrow(documents) >= 11L,
  all(documents$extracted_lines > 1000, na.rm = TRUE),  # every PDF yielded text
  nrow(edges) == 4L,
  nrow(bugs) == 3L,
  all(bugs$status == "fixed"),
  nrow(survivors) >= 6L,
  flags[["la_primary"]] > 0L,        # Louisiana is flagged, not silently fixed
  flags[["runoff_mixed"]] > 0L,
  flags[["uncontested"]] > 0L,
  length(NONE) == 3L,                # 1946, 1948, 1950 -- nobody computed them
  length(FULL) > 20L,
  PEAK$pct > 50,                     # the series has a real peak
  Y2012$split == 26L)                # and reproduces the published 2012 count

cat(sprintf("\ndocuments.csv : %d publications, %s MB, %s lines of text\n",
            nrow(documents), f1(sum(documents$kilobytes) / 1024),
            cm(sum(documents$extracted_lines, na.rm = TRUE))))
cat(sprintf("bugs.csv      : %d found, %d fixed\n", nrow(bugs), sum(bugs$status == "fixed")))
cat(sprintf("survivors.csv : %d disagreements -- %d nobody's, %d theirs, %d ours\n",
            nrow(survivors), sum(survivors$whose == "Nobody's"),
            sum(survivors$whose == "The academic file's"),
            sum(survivors$whose == "Ours")))
cat(sprintf("coverage.csv  : %d none, %d partial, %d complete\n",
            length(NONE), length(PART), length(FULL)))
cat(sprintf("  peak %s%% in %d; 2012 gives %d\n",
            f1(PEAK$pct), PEAK$year, Y2012$split))
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
