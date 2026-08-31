# Build the electoral-map dataset: 2024 presidential results by state,
# with 2020-census electoral votes and a tile-grid layout.

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

src <- "https://github.com/jaytimm/PresElectionResults/raw/master/data/pres_by_state.rda"
tmp <- tempfile(fileext = ".rda")
prov_fetch(src, tmp, mode = "wb", quiet = TRUE)
e <- new.env(); load(tmp, envir = e)
d <- as.data.frame(e$pres_by_state)
d24 <- subset(d, year == 2024)

# Electoral votes, 2024 (2020 census apportionment)
ev <- c(AL=9, AK=3, AZ=11, AR=6, CA=54, CO=10, CT=7, DE=3, DC=3, FL=30,
        GA=16, HI=4, ID=4, IL=19, IN=11, IA=6, KS=6, KY=8, LA=8, ME=4,
        MD=10, MA=11, MI=15, MN=10, MS=6, MO=10, MT=4, NE=5, NV=6, NH=4,
        NJ=14, NM=5, NY=28, NC=16, ND=3, OH=17, OK=7, OR=8, PA=19, RI=4,
        SC=9, SD=3, TN=11, TX=40, UT=6, VT=3, VA=13, WA=12, WV=4, WI=10,
        WY=3)
stopifnot(sum(ev) == 538, length(ev) == 51)

# Tile-grid layout: read from the shared base map rather than kept here, so
# this chapter's grid is the same grid every chapter draws. The layout began
# life in this file and now lives in ../../../_lib/geo/us-grid.geojson, one
# square per state with its col/row; only the layout columns are used here.
gj <- jsonlite::fromJSON("../../../_lib/geo/us-grid.geojson",
                         simplifyVector = FALSE)
grid <- do.call(rbind, lapply(gj$features, function(f)
  data.frame(abbrev = f$properties$st, col = f$properties$col,
             row = f$properties$row, stringsAsFactors = FALSE)))
stopifnot(nrow(grid) == 51, !any(duplicated(grid$abbrev)),
          !any(duplicated(paste(grid$col, grid$row))))

state_names <- c(AL="Alabama", AK="Alaska", AZ="Arizona", AR="Arkansas",
  CA="California", CO="Colorado", CT="Connecticut", DE="Delaware",
  DC="District of Columbia", FL="Florida", GA="Georgia", HI="Hawaii",
  ID="Idaho", IL="Illinois", IN="Indiana", IA="Iowa", KS="Kansas",
  KY="Kentucky", LA="Louisiana", ME="Maine", MD="Maryland",
  MA="Massachusetts", MI="Michigan", MN="Minnesota", MS="Mississippi",
  MO="Missouri", MT="Montana", NE="Nebraska", NV="Nevada",
  NH="New Hampshire", NJ="New Jersey", NM="New Mexico", NY="New York",
  NC="North Carolina", ND="North Dakota", OH="Ohio", OK="Oklahoma",
  OR="Oregon", PA="Pennsylvania", RI="Rhode Island", SC="South Carolina",
  SD="South Dakota", TN="Tennessee", TX="Texas", UT="Utah", VT="Vermont",
  VA="Virginia", WA="Washington", WV="West Virginia", WI="Wisconsin",
  WY="Wyoming")

out <- data.frame(
  state    = unname(state_names[d24$state_abbrev]),
  abbrev   = d24$state_abbrev,
  ev       = unname(ev[d24$state_abbrev]),
  harris   = d24$democrat,
  trump    = d24$republican,
  other    = ifelse(is.na(d24$other), 0, d24$other),
  winner   = ifelse(d24$party_win == "democrat", "Harris", "Trump"),
  col      = grid$col[match(d24$state_abbrev, grid$abbrev)],
  row      = grid$row[match(d24$state_abbrev, grid$abbrev)],
  stringsAsFactors = FALSE
)
out$margin <- round(out$trump - out$harris, 2)
out <- out[order(out$state), ]

stopifnot(!any(is.na(out)))

# Verification against the known result
wta <- tapply(out$ev, out$winner, sum)
cat("Winner-take-all EV:  Harris", wta[["Harris"]], " Trump", wta[["Trump"]], "\n")
cat("Actual certified:    Harris 226  Trump 312\n")
cat("Total EV:", sum(out$ev), "\n")
cat("States won: Harris", sum(out$winner == "Harris"),
    " Trump", sum(out$winner == "Trump"), "\n\n")
cat("Closest five states by margin:\n")
print(head(out[order(abs(out$margin)), c("state", "harris", "trump", "margin", "ev")], 5))

write.csv(out, "derived/pres2024_states.csv", row.names = FALSE)
cat("\nwrote pres2024_states.csv:", nrow(out), "rows\n")

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
