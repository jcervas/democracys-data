# ---------------------------------------------------------------------------
# Build the year-by-year incumbency-advantage series -- the classic
# Gelman-King regression Jacobson uses for his own Figure 1, extended from
# 1946 through 2024.
#
#   derived/by_year.csv            One row per election year, 1946-2024: n,
#                          the estimated incumbency advantage that year
#                          ("jump"), its standard error, which source it
#                          came from, and whether the year is a
#                          redistricting gap (no estimate at all).
#   derived/jacobson_validation.csv  The overlap check described below: one row.
#
# Run AFTER build-data.R -- this script reads derived/races.csv, which that
# script writes.
#
# ---------------------------------------------------------------------------
# TWO SOURCES, NOT ONE
#
# 1946-2014: Gary Jacobson's own replication data for "It's Nothing
# Personal: The Decline of the Incumbency Advantage in U.S. House
# Elections" (Journal of Politics, 2015) -- his life's-work file, with a
# hand-curated incumbency variable (`inc3`) and, critically, a per-district
# `redist` flag marking which seats were redrawn since the last election,
# not just which YEARS were. It is freely downloadable, no guestbook, and
# fetched below.
#
# 2016-2024: Jacobson's file stops in 2014, so the extension is built the
# same way the rest of this chapter was -- from the raw MEDSL returns,
# using `personal_incumbent_running` from races.csv as the incumbency
# signal. THESE ARE NOT COMPUTED THE SAME WAY, and checking that matters
# turned up a real gap: see below.
#
# ---------------------------------------------------------------------------
# WHY THE TWO SOURCES ARE NOT SPLICED BLIND
#
# For the sixteen years both files cover (1976-2014, minus the redistricting
# years), the underlying vote shares agree almost exactly -- a correlation
# above 0.9999 and a mean absolute difference of about half a point. But
# Jacobson's `inc3` and this chapter's own `personal_incumbent_running`
# disagree on whether an incumbent is running in roughly 1 race in 11.
#
# The reason survives inspection. California's 17th in 1994: Leon Panetta
# won it in 1992, then left mid-term to become Bill Clinton's OMB director;
# Sam Farr won the June 1993 SPECIAL election to replace him, and was the
# sitting incumbent by November 1994. This file has no row for that special
# election at all -- of 90 special-election rows in the whole 1976-2024
# file, none falls in an odd year, so an off-cycle special that resolves
# before the next regular general simply leaves no trace here. Every such
# case reads as an open seat, understating the incumbency effect a little
# in whatever years it falls. Jacobson's file, curated by hand against
# contemporary sources, does not have this blind spot, which is the reason
# his numbers are used through 2014 rather than this chapter's own.
#
# ---------------------------------------------------------------------------

dir.create("derived", showWarnings = FALSE)

# ---- fetch Jacobson's own replication file ---------------------------------
if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_fetch")) source("../../../_lib/provenance.R")
} else {
  prov_fetch <- function(url, dest, ...) { download.file(url, dest, mode = "wb", quiet = TRUE); invisible(dest) }
}
JAC_URL <- "https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/29559/19HFER"
prov_fetch(JAC_URL, "raw/JOPrepfile1.tab")

jac <- read.delim("raw/JOPrepfile1.tab", stringsAsFactors = FALSE)
jac <- jac[!is.na(jac$year), ]
cat(sprintf("Jacobson file: %s real rows, %d-%d\n", format(nrow(jac), big.mark = ","),
            min(jac$year), max(jac$year)))
stopifnot(nrow(jac) == 15228)

# ---- Jacobson's own documented recipe for his Figure 1 ---------------------
# "sort year / by year: reg dv dvp ptynow inc3 if redist==0 & year!=1972 &
#  year!=1982 & year!=1992 & year!=2002 & year!=2012" -- coefficient on
# inc3 is the incumbency advantage. (Documentation for JOPrepfile1, part
# II.) Replicated here exactly: a row-level filter on redist, PLUS five
# years excluded outright because too little survives that filter to
# estimate anything from.
JAC_GAP_YEARS <- c(1972, 1982, 1992, 2002, 2012)

fit_year <- function(sub, formula) {
  sub <- sub[stats::complete.cases(sub[, all.vars(formula)]), ]
  if (nrow(sub) < 30) return(c(n = nrow(sub), jump = NA_real_, se = NA_real_))
  f <- tryCatch(lm(formula, data = sub), error = function(e) NULL)
  if (is.null(f)) return(c(n = nrow(sub), jump = NA_real_, se = NA_real_))
  co <- summary(f)$coefficients
  if (!"inc3" %in% rownames(co)) return(c(n = nrow(sub), jump = NA_real_, se = NA_real_))
  c(n = nrow(sub), jump = unname(co["inc3", "Estimate"]), se = unname(co["inc3", "Std. Error"]))
}

jac_years <- sort(unique(jac$year))
jac_out <- do.call(rbind, lapply(jac_years, function(y) {
  if (y %in% JAC_GAP_YEARS) return(data.frame(year = y, n = 0L, jump = NA, se = NA))
  sub <- jac[jac$year == y & jac$redist == 0, c("dv", "dvp", "ptynow", "inc3")]
  r <- fit_year(sub, dv ~ dvp + ptynow + inc3)
  data.frame(year = y, n = as.integer(r["n"]), jump = r["jump"], se = r["se"])
}))
jac_out$source <- "Jacobson (JOP replication)"
cat(sprintf("Jacobson series: %d years, %d of them redistricting gaps\n",
            nrow(jac_out), sum(is.na(jac_out$jump))))

# ---- the same design, 2016-2024, built from this chapter's own races.csv --
races <- read.csv("derived/races.csv", stringsAsFactors = FALSE)
vp <- races[races$valid_pair & races$standard & races$lag_standard %in% TRUE &
            !is.na(races$lag_dv) & races$year > max(jac_out$year, na.rm = TRUE), ]
vp$ptynow <- ifelse(vp$lag_dv > 50, 1, ifelse(vp$lag_dv < 50, -1, 0))
vp$inc3   <- ifelse(vp$personal_incumbent_running & vp$lag_dv > 50, 1,
             ifelse(vp$personal_incumbent_running & vp$lag_dv < 50, -1, 0))

MEDSL_GAP_YEARS <- 2022   # the first election under the 2020s lines
medsl_years <- sort(unique(c(vp$year, MEDSL_GAP_YEARS)))
medsl_out <- do.call(rbind, lapply(medsl_years, function(y) {
  if (y %in% MEDSL_GAP_YEARS) return(data.frame(year = y, n = 0L, jump = NA, se = NA))
  sub <- vp[vp$year == y, c("dv", "lag_dv", "ptynow", "inc3")]
  names(sub)[names(sub) == "lag_dv"] <- "dvp"
  r <- fit_year(sub, dv ~ dvp + ptynow + inc3)
  data.frame(year = y, n = as.integer(r["n"]), jump = r["jump"], se = r["se"])
}))
medsl_out$source <- "This chapter (MEDSL returns)"
cat(sprintf("MEDSL extension: %d years\n", sum(!is.na(medsl_out$jump))))

by_year <- rbind(jac_out, medsl_out)
by_year <- by_year[order(by_year$year), ]
by_year$redistricting_gap <- is.na(by_year$jump)
by_year$jump <- round(by_year$jump, 4); by_year$se <- round(by_year$se, 4)
# wide columns so the figure can style the two sources differently without
# the brief doing that split itself
by_year$jump_jacobson <- ifelse(by_year$source == "Jacobson (JOP replication)", by_year$jump, NA)
by_year$jump_medsl     <- ifelse(by_year$source == "This chapter (MEDSL returns)", by_year$jump, NA)
write.csv(by_year, "derived/by_year.csv", row.names = FALSE)

cat("\nfull series:\n"); print(by_year[, c("year","n","jump","source","redistricting_gap")], row.names = FALSE)

# ---- the overlap check: do the two sources agree where both cover it? -----
state50 <- sort(unique(races$state))
smap <- setNames(seq_along(state50), state50)
races$state_rank <- smap[races$state]
races$stcd <- races$state_rank * 100 + races$district
races$my_inc3 <- ifelse(races$personal_incumbent_running & races$lag_dv > 50, 1,
                 ifelse(races$personal_incumbent_running & races$lag_dv < 50, -1, 0))

overlap_years <- setdiff(intersect(unique(jac$year), unique(races$year)), JAC_GAP_YEARS)
agree_n <- 0L; total_n <- 0L
for (y in overlap_years) {
  j <- jac[jac$year == y, c("stcd", "inc3")]
  m <- races[races$year == y & races$standard & races$lag_standard %in% TRUE,
             c("stcd", "my_inc3")]
  mrg <- merge(j, m, by = "stcd")
  mrg <- mrg[!is.na(mrg$inc3), ]
  total_n <- total_n + nrow(mrg)
  agree_n <- agree_n + sum(mrg$inc3 == mrg$my_inc3)
}
validation <- data.frame(
  overlap_years = length(overlap_years),
  races_compared = total_n,
  races_agree = agree_n,
  agreement_pct = round(100 * agree_n / total_n, 1))
cat("\noverlap validation:\n"); print(validation, row.names = FALSE)
write.csv(validation, "derived/jacobson_validation.csv", row.names = FALSE)

if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_stamp()
}
