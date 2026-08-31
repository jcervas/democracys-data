# ---------------------------------------------------------------------------
# Carry Georgia's 2020 precinct votes onto the 2024 precinct boundaries.
#
#   derived/crosswalk.csv            from_2020, to_2024, weight  (areal share)
#   derived/assign_point.csv         from_2020, to_2024          (interior-point, 1-to-1)
#   derived/precincts_2024_est.csv   2020 votes re-expressed on 2024 boundaries
#   derived/crosswalk_check.csv      what survived the carry
#
# ---------------------------------------------------------------------------
# WHY THIS EXISTS
#
# Precinct boundaries are not stable. Between 2020 and 2024 Georgia's counties
# redrew enough of them that **only 75.1% of precinct identifiers appear in both
# years** -- 660 disappear, 703 are new. Comparing 2020 to 2024 precinct by
# precinct compares different pieces of ground wearing the same label.
#
# ---------------------------------------------------------------------------
# THE MISTAKE THIS SCRIPT USED TO MAKE, AND WHY IT IS WORTH KEEPING IN THE LAB
#
# An earlier version read the votes from the parsed SoS returns and joined them
# to the shapefile **by precinct name**. That join reached 90.4%, and the
# missing 9.6% -- 255 precincts, concentrated in Chatham and DeKalb -- took
# 489,876 votes with them. Nearly a tenth of the state, gone, in a file that
# otherwise looked fine.
#
# Two attempts to rescue the join by normalising names ("01 BETHLEHEM", "01C
# FIRST PRESBYTERIAN") moved the loss from 11.1% to 9.8% and then not at all.
#
# **The join was never necessary.** The 2020 precinct shapefile already carries
# the vote totals -- TRUMP20, BIDEN20, PURDUE20, OSSOFF20, WARNOCK20 and the
# rest -- in its own attribute table, attached to the geometry by construction.
# No name matching, nothing to lose. And they are the same numbers: on the 2,401
# precincts where the name join did succeed, the shapefile's totals and the
# Secretary of State's returns agree in **every single precinct**.
#
# The lesson is the lab's: when a join is losing rows, ask whether you needed
# the join.
#
# ---------------------------------------------------------------------------
# TWO WAYS TO ASSIGN OLD GEOGRAPHY TO NEW, BOTH COMPUTED HERE
#
# 1. INTERIOR POINTS (following Cervas, R-Functions/assignPolys). Reduce each
#    2020 precinct to a point guaranteed to lie inside it, and ask which 2024
#    precinct contains that point. Gives a clean many-to-one assignment with no
#    sliver polygons and no fractional weights. Robust, and the right tool when
#    the lower geography is small relative to the target.
#
# 2. AREAL WEIGHTS. Intersect the two layers and weight by share of the source
#    precinct's area. Keeps the fact that **78.3% of 2020 precincts fall across
#    more than one 2024 precinct**, which the point method has to discard.
#
# Both are written out so the lab can compare them. Neither is "right":
# **areal weighting assumes population is spread uniformly inside a precinct**,
# which it is not. Disaggregating to census blocks and weighting by block
# population is better still, and needs block geography this lab does not carry.
#
# THIS IS A BUILD SCRIPT and may use packages. The student lab is base R and
# reads only the CSVs written here.
#
# Run from this directory:  Rscript build-crosswalk.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)

suppressPackageStartupMessages(library(sf))
sf_use_s2(FALSE)

for (z in c("raw/ga_precincts_2020.zip", "raw/ga_precincts_2024.zip")) stopifnot(file.exists(z))
if (!dir.exists("raw/shp2020")) utils::unzip("raw/ga_precincts_2020.zip", exdir = "raw/shp2020")
if (!dir.exists("raw/shp2024")) utils::unzip("raw/ga_precincts_2024.zip", exdir = "raw/shp2024")

p20 <- st_read(list.files("raw/shp2020", "\\.shp$", full.names = TRUE)[1], quiet = TRUE)
p24 <- st_read(list.files("raw/shp2024", "\\.shp$", full.names = TRUE)[1], quiet = TRUE)
p20 <- st_transform(st_make_valid(p20), 5070)   # equal area, so weights are areas
p24 <- st_transform(st_make_valid(p24), 5070)
cat(sprintf("2020 precincts %d   2024 precincts %d\n", nrow(p20), nrow(p24)))

p20$src <- toupper(trimws(paste(p20$CTYNAME, p20$PRECINCT_N, sep = "|")))
p24$dst <- toupper(trimws(paste(p24$CTYNAME, p24$PRECINCT_N, sep = "|")))

# ---- votes come from the shapefile itself; no join, nothing to lose --------
CAND <- intersect(c("TRUMP20","BIDEN20","JORGENSON2","PURDUE20","OSSOFF20","HAZEL20",
                    "LOEFFLER20","COLLINS20","WARNOCK20","REG20","VOTED20"), names(p20))
v20 <- st_drop_geometry(p20[, c("src", CAND)])
for (v in CAND) { v20[[v]] <- as.numeric(v20[[v]]); v20[[v]][is.na(v20[[v]])] <- 0 }
cat(sprintf("vote columns carried from the shapefile: %s\n", paste(CAND, collapse = ", ")))

# ---- method 1: interior points -------------------------------------------
pt <- suppressWarnings(st_point_on_surface(p20))          # a point inside each precinct
hit <- st_within(pt, p24)
assign_pt <- data.frame(
  from_2020 = p20$src,
  to_2024   = ifelse(lengths(hit) > 0, p24$dst[vapply(hit, function(i) if (length(i)) i[1] else NA_integer_, 1L)], NA),
  stringsAsFactors = FALSE)
dd_write_csv(assign_pt, "derived/assign_point.csv")
cat(sprintf("interior-point assignment: %d of %d precincts landed in a 2024 precinct (%.1f%%)\n",
            sum(!is.na(assign_pt$to_2024)), nrow(assign_pt),
            100 * mean(!is.na(assign_pt$to_2024))))

# ---- method 2: areal weights ---------------------------------------------
p20$src_area <- as.numeric(st_area(p20))
cat("intersecting ...\n")
ix <- suppressWarnings(st_intersection(p20[, c("src", "src_area")], p24[, "dst"]))
ix$piece <- as.numeric(st_area(ix)); ix <- st_drop_geometry(ix); ix <- ix[ix$piece > 0, ]
ix$weight <- ix$piece / as.numeric(tapply(ix$piece, ix$src, sum)[ix$src])
ix <- ix[ix$weight > 1e-6, ]
cw <- data.frame(from_2020 = ix$src, to_2024 = ix$dst, weight = round(ix$weight, 6))
dd_write_csv(cw[order(cw$from_2020, -cw$weight), ], "derived/crosswalk.csv")
cat(sprintf("areal crosswalk: %d rows; %.1f%% of 2020 precincts split across >1 target\n",
            nrow(cw), 100 * mean(table(cw$from_2020) > 1)))

# ---- carry the votes ------------------------------------------------------
m <- merge(v20, cw, by.x = "src", by.y = "from_2020")
for (v in CAND) m[[v]] <- m[[v]] * m$weight
est <- aggregate(m[, CAND], by = list(to_2024 = m$to_2024),
                 FUN = function(x) sum(x, na.rm = TRUE))
for (v in CAND) est[[v]] <- round(est[[v]])
dd_write_csv(est, "derived/precincts_2024_est.csv")

VOTE <- setdiff(CAND, c("REG20", "VOTED20"))
before <- sum(v20[, VOTE], na.rm = TRUE); after <- sum(est[, VOTE], na.rm = TRUE)
chk <- data.frame(
  check = c("2020 precincts (shapefile)", "carried into the crosswalk",
            "2024 precincts receiving votes",
            "votes before", "votes after", "votes lost", "share lost (%)"),
  value = c(nrow(v20), length(unique(m$src)), nrow(est),
            format(before, big.mark = ","), format(after, big.mark = ","),
            format(before - after, big.mark = ","),
            sprintf("%.3f", 100 * (before - after) / before)))
dd_write_csv(chk, "derived/crosswalk_check.csv")
print(chk, row.names = FALSE)
if ((before - after) / before > 0.005)
  cat("\n*** more than 0.5% of votes did not survive -- do not use the estimates.\n")
