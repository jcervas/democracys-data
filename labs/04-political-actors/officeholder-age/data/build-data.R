# ---------------------------------------------------------------------------
# Build the officeholder-age lab dataset.
#
# FETCHED 2026-08-10.  Sources, with what each returned on that date:
#
#   1. Voteview congressional member file
#      https://voteview.com/static/data/out/members/HSall_members.csv
#      HTTP 200 - 6,201,247 bytes - 51,062 rows - Congresses 1-119
#      Every member of every Congress since 1789, plus the presidents, one row
#      per member per Congress. Carries `born` and `died` as YEARS, not dates.
#
#   2. Life expectancy AT BIRTH, annual 1900-2018
#      https://data.cdc.gov/api/views/w9j2-ggv5/rows.csv?accessType=DOWNLOAD
#      HTTP 200 - 34,059 bytes - 1,071 rows (3 races x 3 sexes x 119 years)
#      NCHS/CDC open data. This is the NAIVE series - see the note below.
#
#   3. Life expectancy AT AGE 65 and AT AGE 75, selected years 1950-2018
#      https://www.cdc.gov/nchs/data/hus/2019/004-508.pdf
#      HTTP 200 - 127,616 bytes - "Health, United States, 2019", Trend Table 4
#      A published PDF with a real text layer; parsed here with `pdftotext
#      -layout` and validated against source 2 below. This is the series the
#      whole lab turns on and it is not in CDC's open-data catalogue: only
#      at-birth series are there. NCHS does not publish at-65 before 1950 in
#      this table, and skips 2011.
#
# WHY TWO LIFE-EXPECTANCY SERIES
#
# Life expectancy AT BIRTH rose about 31 years across the 20th century, and
# most of that rise is the collapse of infant and child mortality. None of it
# is relevant to a 60-year-old senator, who has already survived all of it.
# The honest comparison for an officeholder is REMAINING life expectancy
# conditional on having reached an age they have actually reached - e(65).
# Both series come from NCHS; the lab is about which one you pick.
#
# REQUIRES `pdftotext` (poppler). On macOS: brew install poppler.
#
# OUTPUTS (all written next to this script)
#
#   derived/members.csv            one row per member per Congress, with age, the
#                          Congress they first entered that chamber, their age
#                          then, and years served so far
#   derived/age_by_congress.csv    one row per Congress per chamber: median/mean age,
#                          quartiles, share over 65/70/80, and the additive
#                          decomposition mean_age = mean_entry_age + mean_years_since_entry
#   derived/life_expectancy.csv    year, e0 (annual), e0/e65/e75 (selected years)
#   derived/entry_tenure.csv       one row per member per chamber: entry age, years
#                          served in total, whether still serving
#   derived/presidents.csv         one row per president: age on taking office
#   derived/checks.csv             the validation results printed at the end
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)

# Run this script from inside this data/ folder. `../../../_lib/provenance.R`
# records url, bytes, hash and row count for every download and prints a loud
# banner when a source moves under us. If it is missing the build still runs.
if (file.exists("../../../_lib/provenance.R")) {
  source("../../../_lib/provenance.R")
} else {
  prov_fetch  <- function(url, dest, ...) { download.file(url, dest, mode = "wb", quiet = TRUE); dest }
  prov_report <- function() invisible(FALSE)
}

FETCH_DATE <- "2026-08-10"
dir.create("raw", showWarnings = FALSE)

# --- 1. Voteview -----------------------------------------------------------
#
# WARNING: this is quoted CSV and `bioname` contains commas and embedded
# quotes ("WASHINGTON, George", 'Jr."'). Splitting it with awk -F, corrupts
# the file silently. Use a real CSV parser.

U_VV <- "https://voteview.com/static/data/out/members/HSall_members.csv"
invisible(prov_fetch(U_VV, "raw/HSall_members.csv", label = "Voteview members"))
raw <- read.csv("raw/HSall_members.csv", colClasses = "character")

EXPECT_ROWS <- 51062L
stopifnot(all(c("congress", "chamber", "icpsr", "bioname", "born", "died",
                "state_abbrev", "party_code") %in% names(raw)))
for (v in c("congress", "icpsr", "born", "died", "party_code", "district_code"))
  raw[[v]] <- suppressWarnings(as.integer(raw[[v]]))

# Congress n begins in year 1787 + 2n. Verified against two fixed points
# below; the check is in checks.csv, not in a comment.
raw$year <- 1787L + 2L * raw$congress
raw$age  <- raw$year - raw$born          # approximate to +/- 1 year: `born`
                                         # is a year, not a date

# --- 2. members.csv --------------------------------------------------------

keep <- c("congress", "year", "chamber", "icpsr", "bioname", "state_abbrev",
          "party_code", "born", "died", "age")
m <- raw[, keep]

# A member's career in a chamber: the Congress they first appear in there.
# Done per chamber, so a senator who served in the House first has a Senate
# entry age measured from the Senate. That is deliberate and is flagged in the
# lab: it means "entering older" for the Senate partly reflects House service.
cham <- m[m$chamber %in% c("House", "Senate"), ]
cham <- cham[order(cham$icpsr, cham$congress), ]
k    <- paste(cham$icpsr, cham$chamber)
firstc <- tapply(cham$congress, k, min)
lastc  <- tapply(cham$congress, k, max)
nterm  <- tapply(cham$congress, k, function(z) length(unique(z)))

m$entry_congress <- NA_integer_
m$entry_congress[m$chamber %in% c("House", "Senate")] <-
  firstc[paste(m$icpsr, m$chamber)[m$chamber %in% c("House", "Senate")]]
m$entry_year <- 1787L + 2L * m$entry_congress
m$entry_age  <- m$entry_year - m$born
m$years_since_entry <- m$year - m$entry_year   # elapsed years since first
                                               # seated in this chamber

dd_write_csv(m, "derived/members.csv")

# --- 3. age_by_congress.csv ------------------------------------------------

sh <- function(a, cut) 100 * mean(a >= cut)
agg <- function(z) {
  a <- z$age[!is.na(z$age)]
  data.frame(
    congress   = z$congress[1],
    year       = z$year[1],
    chamber    = z$chamber[1],
    n          = nrow(z),
    n_age      = length(a),
    median_age = median(a),
    mean_age   = mean(a),
    p25_age    = unname(quantile(a, 0.25)),
    p75_age    = unname(quantile(a, 0.75)),
    min_age    = min(a),
    max_age    = max(a),
    pct_65plus = sh(a, 65),
    pct_70plus = sh(a, 70),
    pct_80plus = sh(a, 80),
    # The decomposition. mean_age = mean_entry_age + mean_served is an exact
    # identity member by member, so it survives averaging. Computed only on
    # members whose entry age is known, so the three columns stay consistent.
    mean_entry_age = mean(z$entry_age[!is.na(z$entry_age) & !is.na(z$age)]),
    mean_years_since_entry = mean(z$years_since_entry[!is.na(z$entry_age) & !is.na(z$age)]),
    median_entry_age = median(z$entry_age[!is.na(z$entry_age) & !is.na(z$age)]),
    median_years_since_entry = median(z$years_since_entry[!is.na(z$entry_age) & !is.na(z$age)]))
}

hs <- m[m$chamber %in% c("House", "Senate"), ]
by_ch <- do.call(rbind, lapply(split(hs, list(hs$congress, hs$chamber),
                                     drop = TRUE), agg))
both  <- hs; both$chamber <- "Congress"
by_bo <- do.call(rbind, lapply(split(both, both$congress, drop = TRUE), agg))
ac <- rbind(by_ch, by_bo)
ac <- ac[order(ac$chamber, ac$congress), ]
dd_write_csv(ac, "derived/age_by_congress.csv")

# --- 4. entry_tenure.csv ---------------------------------------------------

MAXC <- max(raw$congress)
u <- unique(cham[, c("icpsr", "chamber", "bioname", "born")])
u <- u[!duplicated(paste(u$icpsr, u$chamber)), ]
ku <- paste(u$icpsr, u$chamber)
u$entry_congress <- firstc[ku]
u$last_congress  <- lastc[ku]
u$n_congresses   <- as.integer(nterm[ku])
u$entry_year     <- 1787L + 2L * u$entry_congress
u$entry_age      <- u$entry_year - u$born
u$years_served   <- 2L * u$n_congresses    # a partial term counts as a full
                                           # two years; there is no finer
                                           # resolution in this file
u$span_years     <- 2L * (u$last_congress - u$entry_congress + 1L)
u$still_serving  <- u$last_congress == MAXC     # right-censored: these careers
                                                # are not over

# Did this person sit in the House before entering the Senate? Senators
# arriving older is partly senators arriving from somewhere, and the somewhere
# is usually the House. Flagged so the lab can say so with a number.
hfirst <- tapply(cham$congress[cham$chamber == "House"],
                 cham$icpsr[cham$chamber == "House"], min)
u$prior_house <- u$chamber == "Senate" &
  !is.na(hfirst[as.character(u$icpsr)]) &
  hfirst[as.character(u$icpsr)] < u$entry_congress

u <- u[order(u$entry_congress, u$icpsr), ]
dd_write_csv(u, "derived/entry_tenure.csv")

# --- 5. presidents.csv -----------------------------------------------------
#
# Presidents are in the same file, chamber == "President", one row per
# Congress they overlap. Four have no birth year anywhere in the file
# (Washington, John Adams, Zachary Taylor, Herbert Hoover) and are therefore
# missing an age. They are kept with age NA rather than dropped, so the gap is
# visible in the data rather than hidden by a filter.
#
# One row per PRESIDENCY, not per person: a new presidency starts wherever the
# name changes from the previous Congress. That keeps Cleveland's two
# non-consecutive terms and Trump's two non-consecutive terms as separate
# rows, which a `!duplicated(bioname)` filter would silently collapse.

p <- raw[raw$chamber == "President", ]
p <- p[order(p$congress), ]
newterm <- c(TRUE, p$bioname[-1] != p$bioname[-nrow(p)])
pres <- data.frame(bioname  = p$bioname[newterm],
                   icpsr    = p$icpsr[newterm],
                   congress = p$congress[newterm],
                   year     = p$year[newterm],
                   born     = p$born[newterm],
                   died     = p$died[newterm],
                   age_at_term_start = p$age[newterm])
dd_write_csv(pres, "derived/presidents.csv")

# --- 6. Life expectancy at birth, annual ------------------------------------

U_CDC <- paste0("https://data.cdc.gov/api/views/w9j2-ggv5/rows.csv",
                "?accessType=DOWNLOAD")
invisible(prov_fetch(U_CDC, "raw/cdc_le_birth.csv", label = "NCHS life expectancy at birth"))
cdc <- read.csv("raw/cdc_le_birth.csv", check.names = FALSE)
names(cdc)[names(cdc) == "Average Life Expectancy (Years)"] <- "e0"
cdc <- cdc[cdc$Race == "All Races" & cdc$Sex == "Both Sexes", c("Year", "e0")]
names(cdc) <- c("year", "e0_annual")
cdc <- cdc[order(cdc$year), ]

# --- 7. Life expectancy at 65 and 75, from the HUS trend-table PDF ----------

U_HUS <- "https://www.cdc.gov/nchs/data/hus/2019/004-508.pdf"
invisible(prov_fetch(U_HUS, "raw/hus2019_table4.pdf", label = "NCHS HUS 2019 Table 4"))
if (nchar(Sys.which("pdftotext")) == 0)
  stop("pdftotext not found. Install poppler (macOS: brew install poppler).")

txt <- system("pdftotext -layout -f 1 -l 2 raw/hus2019_table4.pdf -", intern = TRUE)
# Pages 1-2 carry the All races / White / Black panel; the moment the
# "not Hispanic" panel header appears we are into a different table and stop.
stop_at <- grep("not Hispanic", txt)[1]
if (!is.na(stop_at)) txt <- txt[seq_len(stop_at - 1L)]

sect <- NA_character_; rows <- list()
for (ln in txt) {
  if (grepl("^\\s*At birth",    ln)) { sect <- "e0";  next }
  if (grepl("^\\s*At 65 years", ln)) { sect <- "e65"; next }
  if (grepl("^\\s*At 75 years", ln)) { sect <- "e75"; next }
  # Year labels carry footnote digits glued on: "19004,5", "20186". Take the
  # leading four digits, drop the rest, then require the leader dots.
  mm <- regmatches(ln, regexec("^\\s*(1[89][0-9]{2}|20[0-9]{2})[0-9,]*\\s*[. ]{4,}\\s*(.*)$", ln))[[1]]
  if (length(mm) == 0 || is.na(sect)) next
  vals <- regmatches(mm[3], gregexpr("[0-9]+\\.[0-9]", mm[3]))[[1]]
  if (!length(vals)) next
  rows[[length(rows) + 1L]] <- data.frame(year = as.integer(mm[2]),
                                          section = sect,
                                          value = as.numeric(vals[1]))
}
hus <- do.call(rbind, rows)
hus <- reshape(hus, idvar = "year", timevar = "section", direction = "wide")
names(hus) <- sub("^value\\.", "", names(hus))
hus <- hus[order(hus$year), ]

le <- merge(cdc, hus, by = "year", all = TRUE)
le <- le[order(le$year), ]
names(le)[names(le) == "e0"] <- "e0_hus"
dd_write_csv(le, "derived/life_expectancy.csv")

# --- 8. Validation ---------------------------------------------------------
#
# The PDF parse is the fragile step, so it is checked against a completely
# separate NCHS publication of the same quantity: the at-birth column of the
# printed table must equal the at-birth value in CDC's open-data file, year by
# year. If a column ever shifts, this fails.

# One person in the file has a birth year on some of their rows and not on
# others. Nothing in the file flags it, and a mean or a median would simply
# skip the blank rows. The build does not repair it; it counts it and names it.
bsp   <- split(raw$born, raw$icpsr)
mixed <- names(bsp)[vapply(bsp, function(b) any(is.na(b)) && any(!is.na(b)),
                           logical(1))]
mixed_nm <- unique(raw$bioname[raw$icpsr %in% as.integer(mixed)])
twoborn  <- sum(vapply(bsp, function(b) length(unique(b[!is.na(b)])) > 1,
                       logical(1)))

ov <- le[!is.na(le$e0_hus) & !is.na(le$e0_annual), ]
ov$gap <- ov$e0_hus - ov$e0_annual
e0_gap  <- max(abs(ov$gap))
e0_same <- sum(ov$gap == 0)
e0_diff <- paste(ov$year[ov$gap != 0], collapse = ", ")

# 24 of the 26 overlapping years agree to the tenth. Two do not, and they do
# not look like rounding: the open-data file gives 2003 = 77.6 and 2004 = 77.5
# where the printed table gives 77.2 and 77.6. One NCHS series is monotone
# there and the other is not, which is what a transposed pair looks like.
# The build does not adjudicate. It records the disagreement and carries on,
# and the lab shows students both numbers.

chk <- data.frame(
  check = c(
    "Voteview rows",
    "Congress 1 begins in 1789",
    "Congress 119 begins in 2025",
    "member-Congress rows with no birth year",
    "presidents in the file",
    "separate presidencies (Cleveland and Trump twice)",
    "presidents with no birth year",
    "people with a birth year on some rows and not others",
    "who that is",
    "people carrying two different birth years",
    "years with a published e(65)",
    "e(65) series first year",
    "e(65) series last year",
    "years where HUS and CDC both give e(0)",
    "of those, years where the two agree exactly",
    "years where they disagree",
    "largest HUS-vs-CDC disagreement on e(0), years"),
  value = c(
    format(nrow(raw), big.mark = ","),
    as.character(unique(raw$year[raw$congress == 1])),
    as.character(unique(raw$year[raw$congress == 119])),
    format(sum(is.na(raw$born)), big.mark = ","),
    as.character(length(unique(p$bioname))),
    as.character(nrow(pres)),
    as.character(sum(is.na(pres$born[!duplicated(pres$bioname)]))),
    as.character(length(mixed)),
    paste(mixed_nm, collapse = "; "),
    as.character(twoborn),
    as.character(sum(!is.na(le$e65))),
    as.character(min(le$year[!is.na(le$e65)])),
    as.character(max(le$year[!is.na(le$e65)])),
    as.character(nrow(ov)),
    as.character(e0_same),
    e0_diff,
    format(e0_gap, nsmall = 1)))
dd_write_csv(chk, "derived/checks.csv")

if (nrow(raw) != EXPECT_ROWS)
  cat(sprintf("\n  *** Voteview row count moved: expected %d, got %d ***\n",
              EXPECT_ROWS, nrow(raw)))
stopifnot(unique(raw$year[raw$congress == 1]) == 1789,
          unique(raw$year[raw$congress == 119]) == 2025,
          e0_gap <= 0.5,          # a column shift in the PDF parse would be
          e0_same >= 20,          # far larger than this, and far more common
          sum(!is.na(le$e65)) >= 25)

cat("\nbuilt on", FETCH_DATE, "\n")
print(chk, row.names = FALSE)
cat("\nfiles written:\n")
for (f in c("derived/members.csv", "derived/age_by_congress.csv", "derived/life_expectancy.csv",
            "derived/entry_tenure.csv", "derived/presidents.csv", "derived/checks.csv"))
  cat(sprintf("  %-22s %8s rows  %7.0f KB\n", f,
              format(nrow(read.csv(f)), big.mark = ","), file.size(f) / 1024))
prov_report()

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
