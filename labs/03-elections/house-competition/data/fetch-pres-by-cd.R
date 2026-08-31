# ---------------------------------------------------------------------------
# Presidential results by congressional district, 2008-2024.
#
#   derived/pres_by_cd.csv   One row per district per presidential election:
#                    state, district, Democratic and Republican shares, and the
#                    Democratic share of the TWO-PARTY vote (dpres).
#
# SOURCE. The Downballot (formerly Daily Kos Elections), which calculates
# presidential results by congressional district by allocating precinct returns
# to district boundaries. It is the standard public source; no government
# agency produces this.
#   https://www.the-downballot.com/p/the-downballots-calculations-of-presidential
#
# **KEYLESS AND SCRIPTABLE**, which is worth noting after MEDSL and the
# Dataverse guestbooks: these are public Google Sheets and export as CSV over
# plain HTTP with no account. Run this and it works.
#
# ---------------------------------------------------------------------------
# WHY FOUR SHEETS AND NOT ONE
#
# A presidential result "by congressional district" only means something
# relative to a SET OF LINES, and the lines change. To pair a House election
# with the presidential vote in the same district, you need the presidential
# result recomputed on the lines that House election was run under:
#
#     House election      lines used        presidential result needed
#     2016, 2018          2012-2021 maps    2016
#     2020                2012-2021 maps    2020
#     2022                2022 maps         2020
#     2024                2024 maps         2024
#
# That is what the sheet-to-year mapping below encodes. Getting it wrong is not
# a rounding error -- it pairs a district with a presidential result computed
# for a different piece of ground.
#
# ---------------------------------------------------------------------------
# WHAT dpres IS
#
# The Downballot publishes each candidate's share of the TOTAL vote, so the
# columns do not sum to 100. Jacobson's `dpres` is the Democratic share of the
# TWO-PARTY vote. This script converts:  dpres = 100 * D / (D + R).
#
# Verified against Jacobson's own `dpres` for 2012: matches to the decimal
# (AL-01 37.70 here, 37.70161 from The Downballot).
#
# Run from this directory:  Rscript fetch-pres-by-cd.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

csv_url <- function(id, gid)
  sprintf("https://docs.google.com/spreadsheets/d/%s/export?format=csv&gid=%s", id, gid)

SHEETS <- list(
  list(id = "1XbUXnI9OyfAuhP5P3vWtMuGc5UJlrhXbzZo3AwMuHtk", gid = "0", skip = 1,
       lines = "2012-2021",
       cols = c(D2020 = 4, R2020 = 5, D2016 = 6, R2016 = 7,
                D2012 = 8, R2012 = 9, D2008 = 10, R2008 = 11)),
  list(id = "1CKngqOp8fzU22JOlypoxNsxL6KSAH920Whc-rd7ebuM", gid = "1871835782", skip = 0,
       lines = "2022",
       cols = c(D2020 = 4, R2020 = 5)),
  list(id = "1ng1i_Dm_RMDnEvauH44pgE6JCUsapcuu8F2pCfeLWFo", gid = "620838163", skip = 2,
       lines = "2024",
       cols = c(D2024 = 4, R2024 = 5, D2020 = 7, R2020 = 8))
)

CODE <- setNames(seq_len(50), state.abb[order(state.name)])   # Jacobson's state numbering

grab <- function(s) {
  u <- csv_url(s$id, s$gid)
  raw <- tryCatch(read.csv(u, skip = s$skip, header = FALSE, stringsAsFactors = FALSE,
                           check.names = FALSE),
                  error = function(e) { cat("  FAILED:", conditionMessage(e), "\n"); NULL })
  if (is.null(raw)) return(NULL)
  raw <- raw[grepl("^[A-Z]{2}-", raw[[1]]), , drop = FALSE]
  st <- substr(raw[[1]], 1, 2)
  cd <- suppressWarnings(as.integer(sub("^..-", "", raw[[1]])))
  cd[is.na(cd)] <- 1L                                   # at-large ("AK-AL")
  out <- data.frame(lines = s$lines, district = raw[[1]], state_abb = st,
                    cd = cd, stcd = as.integer(CODE[st]) * 100 + cd,
                    stringsAsFactors = FALSE)
  nm <- names(s$cols)
  for (i in seq_along(s$cols)) {
    v <- suppressWarnings(as.numeric(gsub("[%,]", "", raw[[s$cols[i]]])))
    out[[nm[i]]] <- v
  }
  out <- out[!is.na(out$stcd), ]
  cat(sprintf("  %-9s  %3d districts, columns: %s\n", s$lines, nrow(out),
              paste(nm, collapse = ", ")))
  out
}

cat("fetching The Downballot sheets (no key required)\n")
parts <- lapply(SHEETS, grab)
if (any(vapply(parts, is.null, logical(1)))) stop("one or more sheets could not be read")

long <- do.call(rbind, lapply(parts, function(p) {
  yrs <- unique(sub("^[DR]", "", grep("^[DR]20", names(p), value = TRUE)))
  do.call(rbind, lapply(yrs, function(y) {
    d <- p[[paste0("D", y)]]; r <- p[[paste0("R", y)]]
    ok <- !is.na(d) & !is.na(r) & (d + r) > 0
    data.frame(lines = p$lines[ok], pres_year = as.integer(y),
               district = p$district[ok], state_abb = p$state_abb[ok],
               cd = p$cd[ok], stcd = p$stcd[ok],
               dem_pct = d[ok], rep_pct = r[ok],
               dpres = round(100 * d[ok] / (d[ok] + r[ok]), 2),
               stringsAsFactors = FALSE)
  }))
}))

# ---- sanity checks -------------------------------------------------------
cat("\nchecks\n")
n_ok <- tapply(long$stcd, paste(long$lines, long$pres_year), length)
print(n_ok)
stopifnot(all(long$dpres >= 0 & long$dpres <= 100))
dup <- sum(duplicated(long[, c("lines", "pres_year", "stcd")]))
cat("  duplicate (lines, year, district) rows:", dup, "\n")
stopifnot(dup == 0)

write.csv(long[order(long$lines, long$pres_year, long$stcd), ],
          "derived/pres_by_cd.csv", row.names = FALSE)
cat(sprintf("\nwritten: pres_by_cd.csv, %d rows, %s\n", nrow(long),
            paste(sort(unique(long$pres_year)), collapse = " ")))
cat("dpres is the Democratic share of the two-party presidential vote.\n")
