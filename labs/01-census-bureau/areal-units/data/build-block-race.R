# ---------------------------------------------------------------------------
# build-block-race.R -- 2020 Census race counts for every Georgia census block.
#
# WRITES
#   derived/ga_block_race.csv    GEOID20 (character, 15 chars), pop, black_alone,
#                        black_any, vap, black_any_vap
#
# SOURCE (fetched 2026-08-10)
#   U.S. Census Bureau, 2020 Census Redistricting Data (P.L. 94-171) Summary
#   File, legacy format, Georgia:
#   https://www2.census.gov/programs-surveys/decennial/2020/data/01-Redistricting_File--PL_94-171/Georgia/ga2020.pl.zip
#   35,831,859 bytes.
#
#   This is the file the Census Bureau is required by law to deliver to every
#   state so its legislature can draw districts. It is the legal basis of the
#   whole redistricting cycle, which is why this lab uses it rather than the
#   API: no key, no rate limit, and the exact numbers the map-drawers had.
#
#   Four pipe-delimited files, no headers:
#     gageo2020.pl        geographic header, one row per geographic unit
#     ga000012020.pl      segment 1: Table P1 (71 cols) then P2 (73 cols)
#     ga000022020.pl      segment 2: Table P3 (71) then P4 (73) -- voting age
#     ga000032020.pl      segment 3: Table P5 (group quarters)
#   Segments join to the header on LOGRECNO.
#
# GEOID DISCIPLINE
#   Block GEOIDs are 15-character strings, not numbers. Everything here reads
#   and writes them as character and the script stops if any is not 15 wide.
#   (A prior lab in this course read tract GEOIDs as numeric and silently lost
#   the leading zero on 1,549 of 4,489 of them.)
#
# BUILD SCRIPT -- may use packages. The student documents are base R.
# Run from this directory:  Rscript build-block-race.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
# Downloads go through prov_fetch(), which records url, bytes, hash and row
# count in PROVENANCE.tsv and prints a banner when a source moves under us --
# a URL that still returns 200 and no longer means what it meant. See
# ../../../_lib/provenance.R. If the helper is missing the build still runs: the
# fallback is a plain download with the same signature, forwarding every
# argument so a source needing a redirect or a user agent still gets one.
if (file.exists("../../../_lib/provenance.R")) {
  source("../../../_lib/provenance.R")
} else {
  prov_fetch <- function(url, dest, label = NULL, mode = "wb", quiet = TRUE, ...) {
    download.file(url, dest, mode = mode, quiet = quiet, ...)
    invisible(dest)
  }
  prov_report <- function() invisible(FALSE)
}


source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)

URL <- paste0("https://www2.census.gov/programs-surveys/decennial/2020/data/",
              "01-Redistricting_File--PL_94-171/Georgia/ga2020.pl.zip")
ZIP <- "raw/ga2020.pl.zip"

if (!file.exists(ZIP)) {
  cat("downloading", URL, "\n")
  prov_fetch(URL, ZIP, mode = "wb", quiet = TRUE)
}
cat(sprintf("zip bytes: %s\n", format(file.size(ZIP), big.mark = ",")))

# Unpacked INTO raw/, because that is what it is: the PL 94-171 zip opened up.
# It used to land in data/pl -- neither raw/ nor derived/, 342 MB of it, and
# invisible to the layout check, which reads the files at the top of data/ and
# not the directories.
ex <- file.path("raw", "pl")
if (!dir.exists(ex)) utils::unzip(ZIP, exdir = ex)
f_geo  <- file.path(ex, "gageo2020.pl")
f_seg1 <- file.path(ex, "ga000012020.pl")
f_seg2 <- file.path(ex, "ga000022020.pl")
stopifnot(file.exists(f_geo), file.exists(f_seg1), file.exists(f_seg2))

# ---- geographic header -----------------------------------------------------
# Field 3 SUMLEV, field 8 LOGRECNO, field 10 GEOCODE.  SUMLEV 750 = block.
# The header is latin-1 and contains embedded quotes in place names, so it is
# read with quote = "" and everything as character.
cat("reading geographic header ...\n")
geo <- read.delim(f_geo, sep = "|", header = FALSE, quote = "",
                  colClasses = "character", fileEncoding = "latin1")
cat(sprintf("  header rows: %s   columns: %d\n",
            format(nrow(geo), big.mark = ","), ncol(geo)))
names(geo)[c(3, 8, 10)] <- c("SUMLEV", "LOGRECNO", "GEOCODE")
bg <- geo[geo$SUMLEV == "750", c("LOGRECNO", "GEOCODE")]
cat(sprintf("  blocks (SUMLEV 750): %s\n", format(nrow(bg), big.mark = ",")))
stopifnot(nrow(bg) > 0, all(nchar(bg$GEOCODE) == 15))

# ---- which P1 columns are "any part Black" ---------------------------------
# P1 enumerates the 63 race combinations in a fixed order with a header line
# before each group size.  Rather than hard-code 32 magic numbers, rebuild the
# layout from that order and take every cell whose combination contains race 2
# (Black or African American).  Races are 1 White, 2 Black, 3 AIAN, 4 Asian,
# 5 NHPI, 6 Some Other Race.
#
# The cells are interleaved with subtotal lines, so the k-race block starts at:
#   k=1 at 3 (after 1 Total, 2 "one race"), k=2 at 11 (after 9 "two or more",
#   10 "two races"), k=3 at 27, k=4 at 48, k=5 at 64, k=6 at 71.
START <- c(3L, 11L, 27L, 48L, 64L, 71L)
lay <- list()
for (k in 1:6) {
  cb <- combn(6, k, simplify = FALSE)
  for (i in seq_along(cb))
    lay[[length(lay) + 1L]] <- list(pos = START[k] + i - 1L, races = cb[[i]])
}
pos_last <- max(vapply(lay, function(z) z$pos, 1L))
stopifnot(pos_last == 71L, length(lay) == 63L,
          !anyDuplicated(vapply(lay, function(z) z$pos, 1L)))
IDX_BLACK_ANY <- vapply(lay, function(z) if (2L %in% z$races) z$pos else NA_integer_, 1L)
IDX_BLACK_ANY <- IDX_BLACK_ANY[!is.na(IDX_BLACK_ANY)]
IDX_BLACK_ALONE <- lay[[which(vapply(lay, function(z) identical(z$races, 2L), TRUE))]]$pos
stopifnot(length(IDX_BLACK_ANY) == 32L, IDX_BLACK_ALONE == 4L)
cat(sprintf("  'any part Black' = %d of the 63 P1 cells; first four: %s\n",
            length(IDX_BLACK_ANY), paste(head(IDX_BLACK_ANY, 4), collapse = ", ")))

# ---- segment 1 (P1, total population by race) ------------------------------
# Field 5 LOGRECNO; P1 starts at field 6, so P1 cell i is field 5 + i.
read_seg <- function(path, want, expect) {
  # colClasses shorter than the file's width is RECYCLED by read.delim, which
  # would silently mangle everything, so measure the width first.
  #   segment 1 = 5 header fields + P1 (71) + P2 (73)            = 149
  #   segment 2 = 5 header fields + P3 (71) + P4 (73) + H1 (3)   = 152
  ncol_file <- length(strsplit(readLines(path, n = 1L), "|", fixed = TRUE)[[1]])
  stopifnot(ncol_file == expect)
  cls <- rep("NULL", ncol_file)
  cls[5L] <- "character"                # LOGRECNO
  cls[5L + want] <- "integer"
  d <- read.delim(path, sep = "|", header = FALSE, quote = "", colClasses = cls)
  names(d)[1] <- "LOGRECNO"
  d
}
cat("reading segment 1 (P1) ...\n")
want1 <- sort(unique(c(1L, IDX_BLACK_ALONE, IDX_BLACK_ANY)))
s1 <- read_seg(f_seg1, want1, 149L)
colnames(s1)[-1] <- paste0("c", want1)

cat("reading segment 2 (P3, voting age) ...\n")
want2 <- sort(unique(c(1L, IDX_BLACK_ANY)))
s2 <- read_seg(f_seg2, want2, 152L)
colnames(s2)[-1] <- paste0("c", want2)

# ---- assemble --------------------------------------------------------------
sum_cols <- function(d, idx) rowSums(d[, paste0("c", idx), drop = FALSE])

m1 <- merge(bg, s1, by = "LOGRECNO", all.x = TRUE)
m2 <- merge(bg, s2, by = "LOGRECNO", all.x = TRUE)
m2 <- m2[match(m1$GEOCODE, m2$GEOCODE), ]
stopifnot(identical(m1$GEOCODE, m2$GEOCODE), !anyNA(m1$c1), !anyNA(m2$c1))

out <- data.frame(
  GEOID20       = m1$GEOCODE,
  pop           = m1$c1,
  black_alone   = m1[[paste0("c", IDX_BLACK_ALONE)]],
  black_any     = sum_cols(m1, IDX_BLACK_ANY),
  vap           = m2$c1,
  black_any_vap = sum_cols(m2, IDX_BLACK_ANY))
out <- out[order(out$GEOID20), ]

# ---- verification ----------------------------------------------------------
stopifnot(all(nchar(out$GEOID20) == 15))
stopifnot(!anyDuplicated(out$GEOID20))
stopifnot(all(out$black_alone <= out$black_any),
          all(out$black_any   <= out$pop),
          all(out$black_any_vap <= out$vap),
          all(out$vap <= out$pop))
cat("\n---- statewide totals (compare to published Georgia 2020 figures) ----\n")
cat(sprintf("blocks            %s\n", format(nrow(out), big.mark = ",")))
cat(sprintf("total population  %s\n", format(sum(out$pop), big.mark = ",")))
cat(sprintf("Black alone       %s  (%.2f%%)\n", format(sum(out$black_alone), big.mark = ","),
            100 * sum(out$black_alone) / sum(out$pop)))
cat(sprintf("any part Black    %s  (%.2f%%)\n", format(sum(out$black_any), big.mark = ","),
            100 * sum(out$black_any) / sum(out$pop)))
cat(sprintf("voting age pop    %s\n", format(sum(out$vap), big.mark = ",")))
cat(sprintf("any part Black VAP  %s  (%.2f%%)\n", format(sum(out$black_any_vap), big.mark = ","),
            100 * sum(out$black_any_vap) / sum(out$vap)))
stopifnot(sum(out$pop) == 10711908L)   # Georgia's 2020 apportionment count

dd_write_csv(out, "derived/ga_block_race.csv")
cat(sprintf("\nwrote ga_block_race.csv  %s rows\n", format(nrow(out), big.mark = ",")))

# ---------------------------------------------------------------------------
# Build stamp. Records which script produced what is now in this directory --
# every file under derived/ and raw/ with its size, hash and row count, and the
# date this ran -- into BUILD-STAMP.tsv beside the data. See
# ../../../_lib/provenance.R. Guarded, because a missing helper must not fail a
# build that was otherwise fine.
if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_report()
  prov_stamp()
}
