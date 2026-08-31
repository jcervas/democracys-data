# ---------------------------------------------------------------------------
# Build the datasets for the "Population Estimates Program" chapter.
#
# Third and shortest of the instrument chapters. The decennial counts everyone
# once a decade. The ACS samples continuously. The Population Estimates Program
# does neither: it takes the last census and CARRIES IT FORWARD with arithmetic.
#
# THE ARGUMENT.
#
#   IT IS A MODEL, AND THE MODEL IS AN ACCOUNTING IDENTITY. Last year's
#   population, plus births, minus deaths, plus everyone who moved in, minus
#   everyone who moved out. That is the whole method, and it is the reason the
#   estimates exist: nobody is going to enumerate the country every year.
#
#   THE IDENTITY IS CHECKABLE, SO THIS BUILD CHECKS IT. For every county in
#   the file, last year's estimate plus natural change plus net migration plus
#   the residual equals this year's estimate -- to the person, with zero
#   violations. That is worth verifying rather than believing, because it tells
#   you exactly what kind of object an estimate is: a sum, not a measurement.
#
#   AND THERE IS A RESIDUAL. The identity does not close on its own. The
#   Bureau publishes a column called RESIDUAL, which is the part of the change
#   that the components do not account for. It is small, it is documented, and
#   it is a plug -- an honest one, printed rather than buried, but a plug.
#
#   WHAT THE COMPONENTS SAY IS NOT SMALL. In 2024 more than half of American
#   counties recorded MORE DEATHS THAN BIRTHS. Most counties that grew at all
#   grew because people moved in, and a large number of them would have shrunk
#   without international migration specifically. The instrument that looks
#   like bookkeeping is carrying the most consequential demographic facts in
#   the country.
#
#   AND IT IS NOT A SIDESHOW. The ACS county population you look up is
#   CONTROLLED to this program -- forced to agree with it -- which is why 3,090
#   of 3,222 ACS county rows print no margin of error. The estimates are not an
#   alternative to the census data most people use. They are inside it.
#
# ---------------------------------------------------------------------------
# SOURCES
# ---------------------------------------------------------------------------
#
# U.S. Census Bureau, Population Estimates Program, county totals with
# components of change, vintage 2024:
#
#   https://www2.census.gov/programs-surveys/popest/datasets/
#     2020-2024/counties/totals/co-est2024-alldata.csv        1.8 MB
#
# Plain CSV over HTTPS, no key or account. 3,195 rows: 51 state rows
# (SUMLEV 040) and 3,144 county rows (SUMLEV 050). Every quantity appears
# twice, once as a count (BIRTHS2024) and once as a rate per thousand
# (RBIRTH2024); this build uses the counts.
#
# STATE AND COUNTY MUST BE READ AS TEXT. They are zero-padded FIPS codes, and
# as numbers Alabama's "01" becomes 1 and stops joining to anything.
#
# ---------------------------------------------------------------------------
# WHAT IS WRITTEN
# ---------------------------------------------------------------------------
# Five tables in derived/ and one raw capture:
#
#   raw/arrives.txt        One county's row, as the file stores it.
#   derived/identity.csv   The accounting identity, checked on every county.
#   derived/components.csv What the components sum to, nationally, in 2024.
#   derived/natural.csv    Counties with more deaths than births.
#   derived/growth.csv     What carried the counties that grew.
#   derived/residual.csv   The size of the part that does not add up.
#
# Run from this directory:  Rscript build-data.R
# (Downloads 1.8 MB on first run; nothing after that.)
# ---------------------------------------------------------------------------

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


dir.create("raw",     showWarnings = FALSE)
dir.create("derived", showWarnings = FALSE)
source("../../../_lib/precision.R")    # dd_write_csv(): six significant digits

options(scipen = 999, stringsAsFactors = FALSE)

SRC <- paste0("https://www2.census.gov/programs-surveys/popest/datasets/",
              "2020-2024/counties/totals/co-est2024-alldata.csv")
DEST <- "raw/co-est2024-alldata.csv"
if (!file.exists(DEST)) prov_fetch(SRC, DEST, quiet = TRUE)

# FIPS as text, everything else numeric. read.csv would happily turn STATE
# into an integer and quietly destroy the join key.
p <- read.csv(DEST, colClasses = c(STATE = "character", COUNTY = "character"))
p$fips <- paste0(p$STATE, p$COUNTY)

st  <- p[p$SUMLEV == 40, ]          # the 50 states and DC
cty <- p[p$SUMLEV == 50, ]          # counties and equivalents
stopifnot(nrow(st) == 51, nrow(cty) > 3100, !any(duplicated(cty$fips)))

Y <- 2024                            # the year this vintage estimates to
g <- function(d, stem, year = Y) d[[paste0(stem, year)]]

# --- 1. The identity, checked -----------------------------------------------
#
# The Bureau's own method, stated as arithmetic:
#
#   this year = last year + (births - deaths) + net migration + residual
#
# Every term is published. So the claim is checkable on all 3,144 counties at
# once, and a build that merely repeated the formula would be worth nothing.

lhs <- g(cty, "POPESTIMATE")
rhs <- g(cty, "POPESTIMATE", Y - 1) + g(cty, "NATURALCHG") +
       g(cty, "NETMIG") + g(cty, "RESIDUAL")
off <- lhs - rhs

# and net migration is itself the sum of its two halves
mig_off <- g(cty, "NETMIG") -
           (g(cty, "INTERNATIONALMIG") + g(cty, "DOMESTICMIG"))

# and natural change is births minus deaths
nat_off <- g(cty, "NATURALCHG") - (g(cty, "BIRTHS") - g(cty, "DEATHS"))

identity <- data.frame(
  check = c("Last year + natural change + net migration + residual = this year",
            "International + domestic migration = net migration",
            "Births - deaths = natural change"),
  counties_checked = nrow(cty),
  violations = c(sum(off != 0), sum(mig_off != 0), sum(nat_off != 0)))
dd_write_csv(identity, "derived/identity.csv")
stopifnot(all(identity$violations == 0))

# --- 2. The components, nationally ------------------------------------------
#
# Summed over counties rather than read off a national row, so the number in
# the chapter is one this build actually computed.

tot <- function(stem) sum(g(cty, stem))
components <- data.frame(
  component = c("Births", "Deaths", "Natural change",
                "International migration", "Domestic migration",
                "Net migration", "Residual", "Total change in 2024"),
  people = c(tot("BIRTHS"), -tot("DEATHS"), tot("NATURALCHG"),
             tot("INTERNATIONALMIG"), tot("DOMESTICMIG"), tot("NETMIG"),
             tot("RESIDUAL"), tot("NPOPCHG")))
dd_write_csv(components, "derived/components.csv")

# Domestic migration nets to about zero across the whole country by
# construction -- everyone who leaves one county arrives in another. It moves
# people around; it does not add any.
DOM_NET <- tot("DOMESTICMIG")

# --- 3. More deaths than births ---------------------------------------------

dying <- g(cty, "DEATHS") > g(cty, "BIRTHS")
natural <- data.frame(
  quantity = c("Counties with more deaths than births, 2024",
               "Share of all counties",
               "People living in those counties",
               "Share of the country living in them"),
  value = c(sum(dying),
            100 * mean(dying),
            sum(lhs[dying]),
            100 * sum(lhs[dying]) / sum(lhs)),
  unit = c("counties", "%", "people", "%"))
dd_write_csv(natural, "derived/natural.csv")

# --- 4. What carried the counties that grew ---------------------------------
#
# THE COUNTERFACTUAL IS ARITHMETIC, NOT A MODEL. "Would have shrunk without
# international migration" means only: this county's total change was positive,
# and total change minus its international migration is negative. Nothing is
# re-estimated; a published component is subtracted from a published total.

grew  <- g(cty, "NPOPCHG") > 0
wo_int <- g(cty, "NPOPCHG") - g(cty, "INTERNATIONALMIG")
wo_nat <- g(cty, "NPOPCHG") - g(cty, "NATURALCHG")

growth <- data.frame(
  quantity = c("Counties that grew in 2024",
               "Of those, would have shrunk without international migration",
               "Of those, would have shrunk without natural change",
               "Counties that grew despite more deaths than births"),
  value = c(sum(grew), sum(grew & wo_int < 0), sum(grew & wo_nat < 0),
            sum(grew & dying)),
  unit = "counties")
dd_write_csv(growth, "derived/growth.csv")

# --- 5. The residual --------------------------------------------------------
#
# Not an error term in any statistical sense, and not noise: it is the amount
# by which the components fail to explain the change, published rather than
# absorbed silently into one of them.

r <- g(cty, "RESIDUAL")
residual <- data.frame(
  quantity = c("Counties with a residual of zero",
               "Counties with a non-zero residual",
               "Largest residual, in people",
               "Total residual, all counties",
               "Total residual as a share of total change"),
  value = c(sum(r == 0), sum(r != 0), max(abs(r)), sum(r),
            100 * abs(sum(r)) / abs(tot("NPOPCHG"))),
  unit = c("counties", "counties", "people", "people", "%"))
dd_write_csv(residual, "derived/residual.csv")

# --- 6. What arrives --------------------------------------------------------

ex <- cty[cty$fips == "13089", ]          # DeKalb County, Georgia
stopifnot(nrow(ex) == 1)
show <- c("POPESTIMATE2023", "BIRTHS2024", "DEATHS2024", "NATURALCHG2024",
          "INTERNATIONALMIG2024", "DOMESTICMIG2024", "NETMIG2024",
          "RESIDUAL2024", "NPOPCHG2024", "POPESTIMATE2024")
writeLines(c(
sprintf("One row of the file: %s, %s.", ex$CTYNAME, ex$STNAME),
"The file is 1.8 MB of plain CSV and every county carries the same",
"columns. These are the ten that make up one year's arithmetic:",
"",
paste0("  ", format(show, width = 22), format(unlist(ex[show]), big.mark = ",",
                                              width = 12)),
"",
"Read down: last year's estimate, then births and deaths and the",
"difference between them, then the two halves of migration and their",
"sum, then the residual, then the total change, then this year's",
"estimate. The last line is the sum of everything above it.",
"",
"STATE and COUNTY are zero-padded FIPS codes and must be read as",
"text. As numbers they lose their leading zeros and stop joining to",
"anything else in this book."), "raw/arrives.txt")

# --- report -----------------------------------------------------------------

cat("\nidentity.csv  : the published arithmetic, checked on every county\n")
print(identity, row.names = FALSE)
cat("\ncomponents.csv: 2024, summed over counties\n")
print(components, row.names = FALSE)
cat(sprintf("\n  domestic migration nets to %s across the whole country\n",
            format(DOM_NET, big.mark = ",")))
cat("\nnatural.csv   : more deaths than births\n")
print(natural, row.names = FALSE)
cat("\ngrowth.csv    : what carried the counties that grew\n")
print(growth, row.names = FALSE)
cat("\nresidual.csv  : the part that does not add up\n")
print(residual, row.names = FALSE)
cat("\ndone.\n")

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
