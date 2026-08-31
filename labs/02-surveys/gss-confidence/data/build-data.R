# ---------------------------------------------------------------------------
# Build the GSS dataset: confidence in institutions, 1973-2024.
#
#   derived/confidence.csv   One row per institution per survey year: the weighted
#                    share saying "a great deal", "only some", "hardly any".
#   derived/institutions.csv The 13 institutions, with first and last year asked.
#
# ---------------------------------------------------------------------------
# THIS SCRIPT USES ONE PACKAGE, AND ONLY HERE. The GSS cumulative file is a
# Stata 13+ .dta, which base R's `foreign` cannot read ("not a Stata version
# 5-12 .dta file"). `haven::read_dta` can. The LAB is base R and reads only the
# CSVs written below -- the package dependency stops at this file.
#
# THE SOURCE IS 570 MB. The cumulative file has ~6,000 variables and 75,699
# respondents across 35 survey years. We pull 20 columns.
#
# THE QUESTION, UNCHANGED SINCE 1973:
#   "I am going to name some institutions in this country. As far as the people
#    running these institutions are concerned, would you say you have a great
#    deal of confidence, only some confidence, or hardly any confidence at all
#    in them?"
#   Coded 1 = a great deal, 2 = only some, 3 = hardly any.
#
# Fifty-one years of identical wording is the whole point. Nothing else in this
# course lets you watch an attitude move without wondering whether the question
# moved too.
#
# THE WEIGHT. GSS ships several. `wtssall` covers 1972-2018 only; `wtssps`
# covers every year in the file, so it is used throughout -- one weight, no
# splicing. Unweighted results differ slightly and the lab does not use them.
#
# WHAT WAS FOUND (see the lab):
#   Confidence in Congress: 24.1% "a great deal" in 1973, 5.6% in 2024.
#   Medicine halved. The military ROSE. Organized labor barely moved.
#
# SOURCE. General Social Survey cumulative file, NORC at the University of
# Chicago, release 3a (2024). https://gss.norc.org/us/en/gss/get-the-data.html
#   https://gss.norc.org/content/dam/gss/get-the-data/documents/stata/GSS_stata.zip
#
# Run from this directory:  Rscript build-data.R   (downloads ~48 MB)
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


dir.create("derived", showWarnings = FALSE)
dir.create("raw",     showWarnings = FALSE)

if (!requireNamespace("haven", quietly = TRUE))
  stop("This build script needs 'haven' to read a Stata 13+ file. install.packages('haven')")

URL <- paste0("https://gss.norc.org/content/dam/gss/get-the-data/",
              "documents/stata/GSS_stata.zip")

# THE SOURCE IS KEPT, NOT DISCARDED. This script used to fetch to a
# tempfile() and unzip into tempdir(), which meant the 570 MB file it was
# built from vanished the moment R exited. Nothing could be re-derived
# without going back to NORC, and the subset below could not be checked
# against the thing it came out of. The project standard is: preserve the
# complete source under raw/, subset into derived/, and never let size be the
# reason a source is thrown away.
#
# The same file backs `gender-gap`. Rather than hold two 570 MB copies, look
# there before downloading -- the same borrowing `bisg-check` does with the
# surnames tables. Whichever chapter is built first pays for the download.
SIB <- "../../gender-gap/data/raw"
dta <- c(Sys.glob("raw/*.dta"), Sys.glob(file.path(SIB, "*.dta")))[1]

if (is.na(dta)) {
  zf <- "raw/GSS_stata.zip"
  prov_fetch(URL, zf, mode = "wb", quiet = TRUE)
  fs  <- unzip(zf, exdir = "raw", junkpaths = TRUE)
  dta <- grep("\\.dta$", fs, value = TRUE)[1]
  unlink(zf)          # the .dta is the source; the zip is packaging
}
stopifnot(!is.na(dta), file.exists(dta))
message("reading ", dta)

INST <- c(conlegis = "Congress",
          confed   = "Executive branch of federal government",
          conjudge = "U.S. Supreme Court",
          conpress = "Press",
          contv    = "Television",
          conbus   = "Major companies",
          confinan = "Banks and financial institutions",
          conmedic = "Medicine",
          consci   = "Scientific community",
          conclerg = "Organized religion",
          coneduc  = "Education",
          conarmy  = "Military",
          conlabor = "Organized labor")

d <- haven::read_dta(dta, col_select = c("year", "wtssps", names(INST)))
d <- as.data.frame(lapply(d, as.numeric))
stopifnot(nrow(d) > 70000)

rows <- list()
for (v in names(INST)) {
  for (yr in sort(unique(d$year))) {
    k <- d$year == yr & !is.na(d[[v]]) & !is.na(d$wtssps)
    if (sum(k) < 50) next                    # too few to report
    w <- d$wtssps[k]; x <- d[[v]][k]
    rows[[length(rows) + 1]] <- data.frame(
      institution = INST[[v]], variable = v, year = yr, n = sum(k),
      great_deal = round(100 * sum(w * (x == 1)) / sum(w), 1),
      only_some  = round(100 * sum(w * (x == 2)) / sum(w), 1),
      hardly_any = round(100 * sum(w * (x == 3)) / sum(w), 1),
      stringsAsFactors = FALSE)
  }
}
cf <- do.call(rbind, rows)
write.csv(cf, "derived/confidence.csv", row.names = FALSE)

ag <- function(f) as.vector(tapply(cf$year, cf$institution, f))
inst <- data.frame(institution = sort(unique(cf$institution)),
                   first_year = ag(min), last_year = ag(max),
                   survey_years = as.vector(table(cf$institution)),
                   stringsAsFactors = FALSE)
first_last <- function(i, yr) cf$great_deal[cf$institution == i & cf$year == yr]
inst$change <- mapply(function(i, a, b) {
  x <- first_last(i, a); y <- first_last(i, b)
  if (length(x) && length(y)) round(y - x, 1) else NA
}, inst$institution, inst$first_year, inst$last_year)
write.csv(inst, "derived/institutions.csv", row.names = FALSE)

cat(sprintf("respondents: %s   survey years: %d\n",
            format(nrow(d), big.mark = ","), length(unique(d$year))))
cat(sprintf("series rows: %d across %d institutions\n", nrow(cf), nrow(inst)))
cat("\nlargest falls and rises, first year to last:\n")
print(head(inst[order(inst$change), c("institution", "first_year", "last_year", "change")], 3),
      row.names = FALSE)
print(head(inst[order(-inst$change), c("institution", "first_year", "last_year", "change")], 3),
      row.names = FALSE)

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
