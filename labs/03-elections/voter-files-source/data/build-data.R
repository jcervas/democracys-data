# ---------------------------------------------------------------------------
# Build the voter-file-instrument dataset: an operational record, borrowed.
#
# The third SOURCE chapter, after surveys and the census. Its subject is the
# instrument: what a voter file is FOR, what it can establish that neither a
# survey nor a census can, and the three things that stand between a person
# registering and a number being published.
#
# Six files end up in this folder:
#
#   raw/two-states.txt  A real capture: the column names of two states' files,
#                       side by side, from the actual extracts.
#   derived/schemas.csv         The same comparison as data -- what each state records
#                       and what it does not.
#   derived/validated.csv       Recorded turnout in one county, by race, from the
#                       state's own history files.
#   derived/selfreport.csv      What survey respondents say their turnout was, the
#                       same years. The gap is the chapter.
#   derived/attrition.csv       People who voted in an election and are no longer in
#                       the file. This is why a current file cannot measure a
#                       past election.
#   derived/status.csv          Active and inactive registrations, with the reasons a
#                       state gives for the distinction.
#
# THE ARGUMENT. A survey elicits and a census enumerates. A voter file does
# neither: it is an ADMINISTRATIVE record, built to run elections, and every
# research use of it is a borrowing. It exists so that a poll worker can
# determine whether the person in front of them may vote today. Nothing about
# its design anticipated anybody studying it, and its most useful properties
# for research are side effects of that operational purpose.
#
# WHAT IT ALONE CAN DO. Three things, and the first is the one that matters
# most for this book:
#
#   1. IT RECORDS WHETHER YOU ACTUALLY VOTED. Not whether you say you did.
#      Surveys overstate turnout, consistently and by a great deal, because
#      voting is a socially desirable act and misremembering is easy. In the
#      most recent ANES about 83% of respondents said they voted. In one
#      Georgia county's official history files, about 62% of the people
#      currently registered are recorded as having voted in the same election.
#      The two numbers have different denominators and are not directly
#      comparable -- see the chapter, which is careful about this -- but only
#      one of the two instruments can be checked against a record at all.
#
#   2. IT IS INDIVIDUAL AND COMPLETE, not a sample. Every registrant, with a
#      history attached. No margin of error, because nothing was sampled.
#
#   3. IT GOES DOWN TO THE PRECINCT AND THE BLOCK, which a survey cannot
#      reach and which a census reaches only every ten years.
#
# WHAT IT CANNOT DO. It contains the REGISTERED, who are not the eligible and
# not the population. It records no attitudes. It has no past tense: the file
# is a snapshot of today's status, and yesterday's status is simply gone.
# Roughly thirty states record party and only a handful record race, so the
# variables available depend on which state you are standing in.
#
# FIFTY STATES, FIFTY FILES. There is no national voter file. HAVA (2002)
# required each state to build a single statewide computerised list, which is
# why a modern voter file exists at all, but it required nothing about format,
# fields, or vocabulary. Georgia's extract has 53 columns and records race.
# New Jersey's has 28 and does not, but carries a full date of birth where
# Georgia carries only a birth year. Four column names match if you ignore
# capitalisation and none match if you do not, so the obvious join finds
# nothing and reports no error while doing it. A national analysis is fifty
# parsing problems before it is one dataset.
#
# SOURCES (public records; neither file is redistributed here)
#   Georgia: Houston County registration extract 71754 and the state's voter
#   history files, from the working data for the 2026 Houston County
#   redistricting matter. 127,560 registrations, 53 columns.
#   New Jersey: the 21 county extracts from the statewide voter list.
#   Derived counts are read from the voter-files chapter, which built them.
#   Self-reported turnout: ANES Time Series Cumulative Data File 1948-2024.
#
# Run from this directory:  Rscript build-data.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)

options(stringsAsFactors = FALSE)

DRIVE <- file.path(path.expand("~"),
  "Library/CloudStorage/GoogleDrive-jcervas@andrew.cmu.edu/My Drive")
GAFILE <- file.path(DRIVE, "Redistricting/2026/Houston County/Superseding Report",
                    "data/voter-reg", "71754 - Houston County.csv")
NJDIR  <- file.path(DRIVE, "Redistricting/2026/New Jersey/State Voter File")

VF <- "../../voter-files/data"          # counts this chapter reuses
stopifnot(file.exists(file.path(VF, "turnout.csv")))

# --- 1. two states, two schemas ---------------------------------------------

ga_cols <- if (file.exists(GAFILE)) {
  names(read.csv(GAFILE, nrows = 1, check.names = FALSE))
} else stop("Georgia extract not found at ", GAFILE)

nj_zip <- if (dir.exists(NJDIR)) {
  list.files(NJDIR, pattern = "^vlist_.*\\.csv\\.zip$", full.names = TRUE)[1]
} else {
  NA_character_
}
nj_cols <- if (!is.na(nj_zip)) {
  inner <- unzip(nj_zip, list = TRUE)$Name[1]
  names(read.csv(unz(nj_zip, inner), nrows = 1, check.names = FALSE))
} else stop("New Jersey extracts not found in ", NJDIR)

has <- function(cols, pat) any(grepl(pat, cols, ignore.case = TRUE))
sch <- data.frame(
  field = c("Columns in the extract",
            "Name", "Residence address", "Mailing address",
            "Party", "Race", "Birth year", "Full date of birth",
            "Registration date", "Status (active or inactive)",
            "Precinct", "Congressional district"),
  georgia = c(length(ga_cols),
    has(ga_cols, "last name"), has(ga_cols, "residence street"),
    has(ga_cols, "mailing"), has(ga_cols, "party"), has(ga_cols, "^race$"),
    has(ga_cols, "birth year"), has(ga_cols, "birth date|date of birth"),
    has(ga_cols, "registration date"), has(ga_cols, "^status$"),
    has(ga_cols, "precinct"), has(ga_cols, "congressional")),
  new_jersey = c(length(nj_cols),
    has(nj_cols, "^last$"), has(nj_cols, "street_"),
    has(nj_cols, "mail"), has(nj_cols, "^party$"), has(nj_cols, "^race$"),
    has(nj_cols, "birth year"), has(nj_cols, "^dob$"),
    has(nj_cols, "reg_date"), has(nj_cols, "^status$"),
    has(nj_cols, "district|ward"), has(nj_cols, "congressional")))
sch$georgia    <- ifelse(sch$field == "Columns in the extract",
                         sch$georgia, ifelse(sch$georgia == 1, "yes", "no"))
sch$new_jersey <- ifelse(sch$field == "Columns in the extract",
                         sch$new_jersey, ifelse(sch$new_jersey == 1, "yes", "no"))
write.csv(sch, "derived/schemas.csv", row.names = FALSE)

cap <- file("raw/two-states.txt", "w")
writeLines(c(
"There is no national voter file. HAVA required every state to build a",
"single statewide list; it required nothing about what the list should",
"look like. Here are the column names of two states' extracts, exactly",
"as they arrive.",
"",
paste0("GEORGIA -- ", length(ga_cols), " columns"),
""), cap)
writeLines(paste(strwrap(paste(ga_cols, collapse = " | "), 68), collapse = "\n"), cap)
writeLines(c("", paste0("NEW JERSEY -- ", length(nj_cols), " columns"), ""), cap)
writeLines(paste(strwrap(paste(nj_cols, collapse = " | "), 68), collapse = "\n"), cap)
shared_ci <- intersect(tolower(ga_cols), tolower(nj_cols))
shared_cs <- intersect(ga_cols, nj_cols)
writeLines(c("",
sprintf("Four names are shared if you ignore capitalisation -- %s --",
        paste(sort(shared_ci), collapse = ", ")),
sprintf("and %s are shared if you do not. Georgia capitalises its headers",
        if (length(shared_cs)) as.character(length(shared_cs)) else "none"),
"and New Jersey does not, so a join written the obvious way matches",
"nothing at all and reports no error while doing it.",
"",
"Georgia records race and a birth YEAR; New Jersey records neither race",
"nor a birth year, but a full date of birth. A national file is fifty",
"parsing problems before it is one dataset -- and which variables you",
"may study at all depends on which state you are standing in."), cap)
close(cap)

# --- 2. recorded turnout -----------------------------------------------------

t <- read.csv(file.path(VF, "turnout.csv"), check.names = FALSE)
els <- setdiff(names(t), c("race", "in_file_2026"))
val <- data.frame(election = els,
                  in_file = sum(t$in_file_2026),
                  recorded_voting = sapply(els, function(e) sum(t[[e]])))
val$pct_of_current_file <- round(100 * val$recorded_voting / val$in_file, 1)
rownames(val) <- NULL
write.csv(val, "derived/validated.csv", row.names = FALSE)

# --- 3. what people say ------------------------------------------------------

CAND <- path.expand(c(
  "~/Downloads/anes_timeseries_cdf_csv_20260205.zip",
  "~/Downloads/anes_timeseries_cdf_csv_20260205.csv"))
hit <- CAND[file.exists(CAND)]
if (length(hit)) {
  src <- hit[1]
  if (grepl("\\.zip$", src)) {
    inner <- grep("\\.csv$", unzip(src, list = TRUE)$Name, value = TRUE)[1]
    hdr <- names(read.csv(unz(src, inner), nrows = 1, check.names = FALSE))
    a <- read.csv(unz(src, inner),
                  colClasses = ifelse(hdr %in% c("VCF0004", "VCF0702"), NA, "NULL"),
                  check.names = FALSE)
  } else {
    hdr <- names(read.csv(src, nrows = 1, check.names = FALSE))
    a <- read.csv(src, colClasses = ifelse(hdr %in% c("VCF0004", "VCF0702"), NA, "NULL"),
                  check.names = FALSE)
  }
  v <- a$VCF0702; v[!v %in% 1:2] <- NA
  ys <- c(2020, 2024)
  sr <- do.call(rbind, lapply(ys, function(y) {
    k <- !is.na(a$VCF0004) & a$VCF0004 == y & !is.na(v)
    data.frame(year = y, respondents = sum(k),
               pct_said_they_voted = round(100 * mean(v[k] == 2), 1))
  }))
  write.csv(sr, "derived/selfreport.csv", row.names = FALSE)
} else {
  stop("ANES file not found; needed for the self-report comparison")
}

# --- 4. the file has no past tense ------------------------------------------

l <- read.csv(file.path(VF, "lost.csv"))
names(l) <- c("election", "voters_no_longer_in_file")
l$pct_of_recorded <- round(
  100 * l$voters_no_longer_in_file /
    (val$recorded_voting[match(l$election, val$election)] +
     l$voters_no_longer_in_file), 1)
write.csv(l, "derived/attrition.csv", row.names = FALSE)

# --- 5. active and inactive --------------------------------------------------

st <- read.csv(file.path(VF, "derived/status.csv"))
write.csv(st, "derived/status.csv", row.names = FALSE)

# --- report -----------------------------------------------------------------

cat(sprintf("\nschemas.csv   : Georgia %d columns, New Jersey %d columns\n",
            length(ga_cols), length(nj_cols)))
cat(sprintf("  names shared ignoring case: %d (%s)\n",
            length(shared_ci), paste(sort(shared_ci), collapse = ", ")))
cat(sprintf("  names shared exactly      : %d\n", length(shared_cs)))
cat("\nvalidated.csv : recorded turnout among people currently in the file\n")
print(val, row.names = FALSE)
cat("\nselfreport.csv: what survey respondents said\n")
print(sr, row.names = FALSE)
cat(sprintf("\n  %d: file records %.1f%%, survey respondents claim %.1f%%\n",
            2024, val$pct_of_current_file[val$election == "2024 general"],
            sr$pct_said_they_voted[sr$year == 2024]))
cat("\nattrition.csv : people who voted and are no longer in the file\n")
print(l, row.names = FALSE)
cat(sprintf("\n  %.1f%% of the people recorded voting in %s have since left the file.\n",
            l$pct_of_recorded[1], l$election[1]))
cat(sprintf("\nstatus.csv    : %s active, %s inactive across %d reasons\n",
            format(st$voters[st$status == "ACTIVE"][1], big.mark = ","),
            format(sum(st$voters[st$status == "INACTIVE"]), big.mark = ","),
            sum(st$status == "INACTIVE")))

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
