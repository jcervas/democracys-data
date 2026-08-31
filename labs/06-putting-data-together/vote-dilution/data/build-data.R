# ---------------------------------------------------------------------------
# Build the vote-dilution dataset: Houston County, Georgia.
#
# THE QUESTION THIS FOLDER SERVES. Vote dilution comes in two legal flavours.
# One is MALAPPORTIONMENT -- districts of unequal population, so that a vote in
# a small district weighs more than a vote in a large one. Reynolds v. Sims,
# 377 U.S. 533 (1964) settled it, and it is measurable by subtraction. The
# other is RACIAL VOTE DILUTION under Section 2 of the Voting Rights Act,
# 52 U.S.C. sec. 10301, where the test comes from Thornburg v. Gingles,
# 478 U.S. 30 (1986). Its FIRST precondition -- that the minority group be
# "sufficiently large and geographically compact to constitute a majority in a
# single-member district" -- is a claim about a map that does not exist. This
# folder assembles what is needed to try to build that map, and to show that
# the answer moves when the analyst's choices move.
#
# THE DOCTRINE MOVED AFTER THIS SCRIPT WAS WRITTEN, AND THE DATA DID NOT.
# Louisiana v. Callais, 608 U.S. ___ (2026), decided 29 April 2026, left the
# three Gingles preconditions standing and "update[d]" what satisfies them. The
# Court now phrases precondition one as a majority in a "reasonably configured"
# district, and imposes two conditions on the illustrative map offered to prove
# it: the plaintiff "cannot use race as a districting criterion," and the map
# "must meet all the State's legitimate districting objectives, including
# traditional districting criteria and the State's specified political goals,"
# achieving them "just as well" as the State's own map (slip op., at 29).
#
# NOTHING IN THIS SCRIPT CHANGED IN RESPONSE, DELIBERATELY. The grow() family
# below optimises on race at every setting of `reach` above 1, which is exactly
# the practice the first condition disqualifies -- and the lab and brief teach
# that as the point rather than hiding it. The second condition cannot be
# evaluated with these inputs at all: it needs a complete partition of the
# county, a statement of Georgia's stated objectives, and election results, and
# this folder has none of the three. If that ever changes, it changes what the
# lab COMPUTES, not just what it says, and it should be a deliberate rebuild.
#
# WHY THIS COUNTY. It is the county the RPV lab and the BISG lab already use,
# so the arc is continuous: `bisg-check` graded a guess about who somebody is,
# `rpv` graded a guess about how they voted, and this lab asks whether the
# districting plan turns that voting into a denial of opportunity. Houston
# County also has the two things this lab needs and almost nowhere else has at
# once: block-level census race counts, and a voter registration file that
# records self-reported race, so the same district can be scored against three
# different definitions of "the people."
#
# A NOTE ON THE LIVE CASE. Houston County's method of electing its Board of
# Commissioners is the subject of Driver v. Houston County, 5:25-cv-00025
# (M.D. Ga.), in which the instructor has been retained by plaintiffs. NOTHING
# from any expert report is used here. The three inputs are (1) the 2020
# decennial census, (2) the Board of Education district plan enacted by the
# Georgia General Assembly and currently in use, and (3) the Georgia voter
# registration file. Every district this lab evaluates other than the enacted
# one is drawn by an algorithm printed in the lab itself. The lab reaches no
# legal conclusion and takes no position on the case.
#
# NINE FILES END UP IN THIS FOLDER:
#
#   derived/blocks.csv         One row per 2020 census block in Houston County: the
#                      enacted Board of Education district, population and
#                      voting-age population by race, registered voters by
#                      race, centroid, and land area.
#   derived/adjacency.csv      Which blocks touch which. The lab needs this to grow a
#                      contiguous district; base R cannot compute it.
#   derived/block_shapes.csv   Simplified block outlines in kilometres, long format,
#                      so that base R and D3 can both draw the county.
#   derived/plan_shapes.csv    Outlines of the enacted districts and of the districts
#                      this script's algorithms build.
#   derived/districts.csv      Every district considered, with its Black share under
#                      three definitions of the population, and its
#                      compactness on two measures.
#   derived/tradeoff.csv       The same algorithm run at fifteen settings of one dial,
#                      tracing what compactness costs in minority share.
#   derived/seed_sweep.csv     The algorithm restarted from every plausible seed
#                      block, as a robustness check on the starting point.
#   derived/ga_county_units.csv  Georgia's pre-1963 county unit rule applied to 2020
#                      population, so the enacted plan's deviation has
#                      something to be compared against.
#   derived/segregation.csv    The county's index of dissimilarity.
#
# THREE DEFINITIONAL CHOICES, ALL OF WHICH MOVE THE ANSWER, ALL OF THEM MADE
# EXPLICITLY BECAUSE THE LAB IS ABOUT THEM:
#
#   * BLACK is ANY-PART BLACK (alone or in combination, including Hispanic
#     Black), which matches Voting Rights Act practice. The BISG lab documents
#     what the alternative costs: 51,992 people against 56,520 statewide for
#     the same county, a gap of 8.7%.
#   * The three population bases -- total population, voting-age population,
#     registered voters -- are all carried, never collapsed. Which one the
#     first Gingles precondition is measured against is a contested question
#     and the lab's pivot turns on it.
#   * Registered voters use SELF-REPORTED race only. Rows whose race had been
#     filled in by BISG were already dropped upstream, in the file the BISG
#     lab commits.
#
# Run from this directory:  Rscript build-data.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

options(stringsAsFactors = FALSE, scipen = 999)
suppressPackageStartupMessages(library(sf))
sf_use_s2(FALSE)

CO   <- "13153"                       # Houston County, Georgia
LABS <- normalizePath("../../..", mustWork = TRUE)  # .../labs

# WHERE EVERY INPUT COMES FROM ----------------------------------------------
#
# 1. Census blocks, population and voting-age population by race. 2020 PL
#    94-171. Read from the same working directory the BISG lab reads, which is
#    where the decennial tables for Georgia were assembled; see
#    labs/bisg-check/data/build-data.R, which documents the table codes
#    and the any-part-Black decision. NOT COMMITTED THERE, so it is read here
#    and the derived county file is written into this folder.
SRC <- file.path("/Users/cervas/Library/CloudStorage/GoogleDrive-jcervas@andrew.cmu.edu",
                 "My Drive/Redistricting/2026/Houston County/Superseding Report/data")

# 2. The enacted Board of Education plan, as a block equivalency file. This is
#    the plan the Georgia General Assembly enacted and the county uses; the
#    boundary file is the county's own.
ENACTED <- file.path(SRC, "block_equiv", "boe-5.csv")

# 3. Registered voters with self-reported race and census block. Committed in
#    the course repository by the BISG lab. READ ONLY -- not modified.
VOTERS <- file.path(LABS, "06-putting-data-together", "bisg-check", "data",
                    "derived", "houston_voters.csv")

# 4. Block geometry. TIGER/Line 2020 tabular blocks for Georgia, downloaded by
#    the precincts lab. READ ONLY -- not modified.
SHP <- file.path(LABS, "03-elections", "ga-precinct-returns", "data", "raw",
                 "blocks", "tl_2020_13_tabblock20.shp")

for (f in c(SRC, ENACTED, VOTERS, SHP))
  if (!file.exists(f)) stop("missing input: ", f, call. = FALSE)

# --- 1. census demographics -------------------------------------------------

pb <- read.csv(file.path(SRC, "census", "pop_blocks.csv"),
               colClasses = c(GEOID20 = "character"))
pb <- pb[substr(pb$GEOID20, 1, 5) == CO, ]
stopifnot(nrow(pb) > 3000, !any(duplicated(pb$GEOID20)))

# --- 2. the enacted plan ----------------------------------------------------

en <- read.csv(ENACTED, colClasses = c(GEOID20 = "character"))
names(en)[names(en) == "District"] <- "boe5"
stopifnot(setequal(en$GEOID20, pb$GEOID20), all(en$boe5 %in% 1:5))

# --- 3. registered voters ---------------------------------------------------

vo <- read.csv(VOTERS, colClasses = c(GEOID20 = "character"))
# Every registered voter is one row. Collapse to blocks. Any-part Black is
# already how the upstream file codes race.
vo$is_black <- vo$race == "black"
reg <- aggregate(cbind(reg = rep(1, nrow(vo)), reg_black = vo$is_black),
                 by = list(GEOID20 = vo$GEOID20), FUN = sum)
cat(sprintf("registered voters: %d in %d blocks; %d blocks are outside the\n",
            nrow(vo), nrow(reg), sum(!reg$GEOID20 %in% pb$GEOID20)))
cat("  county's census block list and are dropped\n")
reg <- reg[reg$GEOID20 %in% pb$GEOID20, ]

# --- 4. geometry ------------------------------------------------------------

g <- st_read(SHP, quiet = TRUE)
g <- g[substr(g$GEOID20, 1, 5) == CO, c("GEOID20", "ALAND20", "POP20")]
g <- st_transform(g, 26917)                       # UTM zone 17N, metres
stopifnot(setequal(g$GEOID20, pb$GEOID20))
g <- g[match(pb$GEOID20, g$GEOID20), ]

# TIGER's own population count is an independent check on the tables above.
stopifnot(sum(g$POP20) == sum(pb$pop))
cat(sprintf("TIGER POP20 and the PL tables agree: %d people\n", sum(pb$pop)))

ctr <- st_coordinates(st_centroid(st_geometry(g)))

# --- 5. the block table -----------------------------------------------------

b <- data.frame(
  GEOID20   = pb$GEOID20,
  boe5      = en$boe5[match(pb$GEOID20, en$GEOID20)],
  pop       = pb$pop,
  pop_black = pb$pop_black,
  pop_white = pb$pop_white,
  vap       = pb$vap,
  vap_black = pb$vap_black,
  vap_white = pb$vap_white,
  reg       = 0L,
  reg_black = 0L,
  # kilometres, origin at the county's south-west corner, so the numbers in
  # the committed file are small and readable
  x_km      = round((ctr[, 1] - min(ctr[, 1])) / 1000, 4),
  y_km      = round((ctr[, 2] - min(ctr[, 2])) / 1000, 4),
  area_km2  = round(as.numeric(st_area(g)) / 1e6, 5))

i <- match(reg$GEOID20, b$GEOID20)
b$reg[i]       <- reg$reg
b$reg_black[i] <- reg$reg_black

ORIG <- c(min(ctr[, 1]), min(ctr[, 2]))            # for every other geometry

# --- 6. adjacency -----------------------------------------------------------
#
# Rook or queen? Queen (touching at a single point counts) is the usual
# redistricting convention, because a plan whose districts meet at a corner is
# normally treated as contiguous. st_touches gives queen.

nb <- st_touches(g)
ij <- do.call(rbind, lapply(seq_along(nb), function(k)
  if (length(nb[[k]])) cbind(k, nb[[k]]) else NULL))
ij <- ij[ij[, 1] < ij[, 2], , drop = FALSE]        # each pair once
adj <- data.frame(a = ij[, 1], b = ij[, 2])
cat(sprintf("adjacency: %d blocks, %d touching pairs, %d blocks with no neighbour\n",
            nrow(b), nrow(adj), sum(lengths(nb) == 0)))

# --- 7. shapes for drawing --------------------------------------------------
#
# Long format, kilometres, two decimal places. 25 m of simplification takes the
# county from 158,458 coordinates to about 31,000, which is small enough to
# knit and still shows every block.

long <- function(geom, ids) {
  out <- vector("list", length(geom))
  for (k in seq_along(geom)) {
    xy <- st_coordinates(geom[[k]])
    out[[k]] <- data.frame(
      id   = ids[k],
      part = if ("L2" %in% colnames(xy)) paste(xy[, "L1"], xy[, "L2"], sep = "-")
             else as.character(xy[, "L1"]),
      x    = round((xy[, "X"] - ORIG[1]) / 1000, 3),
      y    = round((xy[, "Y"] - ORIG[2]) / 1000, 3))
  }
  do.call(rbind, out)
}

gs <- st_simplify(st_geometry(g), dTolerance = 25, preserveTopology = TRUE)
bs <- long(gs, b$GEOID20)

# --- 8. the algorithms the lab runs -----------------------------------------
#
# These are written out here as well as in the lab, because the seed sweep is
# 800-odd restarts and nobody wants that inside a knit. The lab's code is the
# same code; the numbers it prints reproduce the ones in seed_sweep.csv.

NB <- nb                                            # list of integer vectors

# ONE ALGORITHM WITH ONE DIAL. Start at `seed` and annex blocks one at a time
# until the district holds `target` people. At each step the mapmaker looks at
# the `reach` nearest available neighbouring blocks -- nearest to the centre of
# what has been built so far -- and takes whichever of those leaves the Black
# share of voting-age population highest.
#
# `reach` is the whole argument of this lab in a single integer.
#
#   reach = 1    take the nearest block, always. Race never enters the
#                decision, because there is only ever one candidate. This is
#                the roundest district the geography allows.
#   reach = Inf  consider every block on the frontier, however far out it
#                sits, and take the Blackest. This is a mapmaker pursuing one
#                objective and nothing else.
#
# Everything in between is a mapmaker willing to reach a certain distance for
# race and no further. There is no correct value. That is the point.
grow <- function(seed, target, reach = Inf,
                 pop = b$pop, num = b$vap_black, den = b$vap) {
  inn <- logical(length(pop)); inn[seed] <- TRUE
  P <- pop[seed]; N <- num[seed]; D <- den[seed]
  cx <- b$x_km[seed]; cy <- b$y_km[seed]                # district centre
  fr <- NB[[seed]]
  while (P < target && length(fr)) {
    if (is.finite(reach) && reach < length(fr)) {
      d <- (b$x_km[fr] - cx)^2 + (b$y_km[fr] - cy)^2
      cand <- fr[order(d)[seq_len(reach)]]
    } else cand <- fr
    pick <- cand[which.max((N + num[cand]) / pmax(D + den[cand], 1))]
    inn[pick] <- TRUE
    P <- P + pop[pick]; N <- N + num[pick]; D <- D + den[pick]
    w <- b$pop[which(inn)] + 1                          # population-weighted
    cx <- sum(b$x_km[inn] * w) / sum(w); cy <- sum(b$y_km[inn] * w) / sum(w)
    fr <- setdiff(unique(c(fr, NB[[pick]])), which(inn))
  }
  which(inn)
}

# The concentration ceiling: contiguity dropped entirely. Take the most heavily
# Black blocks in the county, in order, until the population target is met.
# This is a greedy heuristic, not a proven maximum, and the lab says so.
ceiling_set <- function(target, num = b$vap_black, den = b$vap, pop = b$pop) {
  o <- order(num / pmax(den, 1), num, decreasing = TRUE)
  o[seq_len(sum(cumsum(pop[o]) < target) + 1)]
}

# --- 9. the districts the lab compares --------------------------------------

TOT  <- sum(b$pop)
mk   <- function(name, kind, idx) {
  data.frame(plan = name, kind = kind,
             blocks = length(idx),
             pop = sum(b$pop[idx]),
             pop_black_pct = 100 * sum(b$pop_black[idx]) / sum(b$pop[idx]),
             vap = sum(b$vap[idx]),
             vap_black_pct = 100 * sum(b$vap_black[idx]) / sum(b$vap[idx]),
             reg = sum(b$reg[idx]),
             reg_black_pct = 100 * sum(b$reg_black[idx]) / max(sum(b$reg[idx]), 1))
}

rows <- list()
for (d in 1:5)
  rows[[length(rows) + 1]] <- mk(paste0("Enacted BOE district ", d), "enacted",
                                 which(b$boe5 == d))

id5 <- TOT / 5; id4 <- TOT / 4

# the seed the lab uses: the block with the most Black adults in the county
SEED <- which.max(b$vap_black)
cat(sprintf("seed block: %s, %d Black adults of %d\n",
            b$GEOID20[SEED], b$vap_black[SEED], b$vap[SEED]))

sets <- list(
  `Ceiling, 5 districts`     = list("ceiling", ceiling_set(id5)),
  `Ceiling, 4 districts`     = list("ceiling", ceiling_set(id4)),
  `Reach 1, 5 districts`     = list("reach",   grow(SEED, id5, 1)),
  `Reach 40, 5 districts`    = list("reach",   grow(SEED, id5, 40)),
  `Reach unlimited, 5 districts` = list("reach", grow(SEED, id5, Inf)),
  `Reach 1, 4 districts`     = list("reach",   grow(SEED, id4, 1)),
  `Reach unlimited, 4 districts` = list("reach", grow(SEED, id4, Inf)))
for (nm in names(sets))
  rows[[length(rows) + 1]] <- mk(nm, sets[[nm]][[1]], sets[[nm]][[2]])

dis <- do.call(rbind, rows)

# --- 10. compactness --------------------------------------------------------
#
# Polsby-Popper: 4*pi*area / perimeter^2. One for a circle, towards zero for
# anything spidery. Reock: area / area of the smallest circle that contains the
# district. Both are computed on the UNSIMPLIFIED geometry, because
# simplification shortens perimeters and would flatter every district.

dissolve <- function(idx) st_union(st_geometry(g)[idx])

compact <- function(geom) {
  a <- as.numeric(st_area(geom))
  p <- as.numeric(st_length(st_cast(st_boundary(geom), "MULTILINESTRING")))
  mbc <- st_minimum_bounding_circle(geom)
  c(pp = 4 * pi * a / p^2, reock = a / as.numeric(st_area(mbc)),
    area_km2 = a / 1e6, perim_km = p / 1000)
}

geoms <- c(lapply(1:5, function(d) dissolve(which(b$boe5 == d))),
           lapply(sets, function(s) dissolve(s[[2]])))
cm <- do.call(rbind, lapply(geoms, compact))
dis <- cbind(dis, round(as.data.frame(cm), 5))

# --- 11. shapes of every district -------------------------------------------

ps <- do.call(rbind, Map(function(gg, nm)
  long(st_simplify(gg, dTolerance = 25, preserveTopology = TRUE), nm),
  geoms, dis$plan))

# county outline, for a frame to draw things inside
county <- st_union(st_geometry(g))
cs <- long(st_simplify(county, dTolerance = 40, preserveTopology = TRUE), "COUNTY")
ps <- rbind(ps, cs)

# --- 12. the trade-off curve ------------------------------------------------
#
# THE CENTRE OF THE LAB. Turn the dial. For each value of `reach`, grow the
# district, then measure two things that pull against each other: how Black the
# district is, and how compact it is. The result is not an answer to "can a
# majority-Black district be drawn." It is a frontier, and every point on it is
# a district somebody could defend.

REACH <- c(1, 2, 3, 4, 6, 8, 12, 16, 24, 40, 60, 100, 200, 500, Inf)

trade <- do.call(rbind, lapply(REACH, function(k) do.call(rbind, lapply(
  list(c(n = 5, t = id5), c(n = 4, t = id4)), function(cfg) {
    idx <- grow(SEED, cfg[["t"]], k)
    cm  <- compact(st_union(st_geometry(g)[idx]))
    data.frame(reach = k, districts = cfg[["n"]], blocks = length(idx),
               pop = sum(b$pop[idx]),
               pop_black_pct = 100 * sum(b$pop_black[idx]) / sum(b$pop[idx]),
               vap_black_pct = 100 * sum(b$vap_black[idx]) / sum(b$vap[idx]),
               reg_black_pct = 100 * sum(b$reg_black[idx]) / max(sum(b$reg[idx]), 1),
               pp = cm[["pp"]], reock = cm[["reock"]])
  }))))

# --- 13. does the starting point matter? ------------------------------------
#
# A robustness check, and the lab reports it whichever way it comes out.
# Restart the unlimited-reach algorithm from every block where Black adults are
# already a majority and there are at least twenty of them.

cand <- which(b$vap >= 20 & b$vap_black / pmax(b$vap, 1) > 0.5)
cat(sprintf("seed sweep: %d candidate seed blocks\n", length(cand)))

sweep <- do.call(rbind, lapply(cand, function(s) {
  i5 <- grow(s, id5, Inf); i4 <- grow(s, id4, Inf)
  data.frame(seed = b$GEOID20[s],
             seed_vap_black = b$vap_black[s],
             d5_vap_black_pct = 100 * sum(b$vap_black[i5]) / sum(b$vap[i5]),
             d5_reg_black_pct = 100 * sum(b$reg_black[i5]) / max(sum(b$reg[i5]), 1),
             d4_vap_black_pct = 100 * sum(b$vap_black[i4]) / sum(b$vap[i4]),
             d4_reg_black_pct = 100 * sum(b$reg_black[i4]) / max(sum(b$reg[i4]), 1))
}))

# --- 14. how segregated is the county, on one number? -----------------------
#
# The index of dissimilarity: the share of one group that would have to move to
# a different block for the two groups to be spread identically across the
# county. Duncan and Duncan (1955). It is the standard single number for
# residential segregation, and it is what "geographically compact" is quietly
# asking about.

dis_index <- 50 * sum(abs(b$vap_black / sum(b$vap_black) -
                          (b$vap - b$vap_black) / sum(b$vap - b$vap_black)))

# --- 15. what malapportionment used to look like, in this state -------------
#
# Georgia elected statewide officers under the COUNTY UNIT SYSTEM until Gray v.
# Sanders, 372 U.S. 368 (1963). The rule, as it stood, was: the 8 most populous
# counties cast 6 unit votes each, the next 30 cast 4, and the remaining 121
# cast 2. A candidate needed a majority of unit votes, not of people.
#
# This applies that rule to 2020 population. It is a RECONSTRUCTION, not
# history: the counties and their populations are today's. It exists so that
# the enacted plan's population deviation has something to be compared with.

gg <- st_read(SHP, quiet = TRUE)
cnty <- aggregate(list(pop = gg$POP20),
                  by = list(fips = substr(gg$GEOID20, 1, 5)), FUN = sum)
cnty <- cnty[order(-cnty$pop), ]
stopifnot(nrow(cnty) == 159)
cnty$units <- c(rep(6, 8), rep(4, 30), rep(2, 121))
cnty$people_per_unit <- cnty$pop / cnty$units
cat(sprintf("Georgia 2020: %s people in 159 counties; county-unit rule gives\n",
            format(sum(cnty$pop), big.mark = ",")))
cat(sprintf("  %.0f people per unit vote at the top and %.0f at the bottom, a ratio of %.0f to 1\n",
            max(cnty$people_per_unit), min(cnty$people_per_unit),
            max(cnty$people_per_unit) / min(cnty$people_per_unit)))

# --- write ------------------------------------------------------------------

r3 <- function(d, cols) { for (k in cols) d[[k]] <- round(d[[k]], 3); d }
PCT <- c("pop_black_pct", "vap_black_pct", "reg_black_pct")

write.csv(b,     "derived/blocks.csv",       row.names = FALSE)
write.csv(adj,   "derived/adjacency.csv",    row.names = FALSE)
write.csv(bs,    "derived/block_shapes.csv", row.names = FALSE)
write.csv(ps,    "derived/plan_shapes.csv",  row.names = FALSE)
write.csv(r3(dis,   PCT),                "derived/districts.csv", row.names = FALSE)
write.csv(r3(trade, c(PCT, "pp", "reock")), "derived/tradeoff.csv", row.names = FALSE)
write.csv(r3(sweep, c("d5_vap_black_pct", "d5_reg_black_pct",
                      "d4_vap_black_pct", "d4_reg_black_pct")),
          "derived/seed_sweep.csv", row.names = FALSE)
write.csv(data.frame(fips = cnty$fips, pop = cnty$pop, units = cnty$units,
                     people_per_unit = round(cnty$people_per_unit, 1)),
          "derived/ga_county_units.csv", row.names = FALSE)
write.csv(data.frame(quantity = "index of dissimilarity, Black vs non-Black adults",
                     value = round(dis_index, 2)),
          "derived/segregation.csv", row.names = FALSE)

cat("\n--- county ---------------------------------------------------------\n")
cat(sprintf("population %s, voting age %s, registered %s\n",
            format(sum(b$pop), big.mark = ","), format(sum(b$vap), big.mark = ","),
            format(sum(b$reg), big.mark = ",")))
cat(sprintf("Black share: %.2f%% of population, %.2f%% of voting age, %.2f%% of registered\n",
            100 * sum(b$pop_black) / sum(b$pop),
            100 * sum(b$vap_black) / sum(b$vap),
            100 * sum(b$reg_black) / sum(b$reg)))
cat(sprintf("index of dissimilarity: %.2f\n", dis_index))
cat("\n--- districts ------------------------------------------------------\n")
print(dis[, c("plan", "pop", "pop_black_pct", "vap_black_pct", "reg_black_pct",
              "pp", "reock")], digits = 4, row.names = FALSE)
cat("\n--- trade-off, five-district configuration -------------------------\n")
print(trade[trade$districts == 5, c("reach", "vap_black_pct", "reg_black_pct",
                                    "pp", "reock")], digits = 4, row.names = FALSE)
cat("\n--- trade-off, four-district configuration -------------------------\n")
print(trade[trade$districts == 4, c("reach", "vap_black_pct", "reg_black_pct",
                                    "pp", "reock")], digits = 4, row.names = FALSE)
cat("\n--- seed sweep -----------------------------------------------------\n")
print(summary(sweep[, c("d5_vap_black_pct", "d4_vap_black_pct")]))
cat(sprintf("\nfiles written: blocks.csv %d rows, adjacency.csv %d, block_shapes.csv %d,\n",
            nrow(b), nrow(adj), nrow(bs)))
cat(sprintf("plan_shapes.csv %d, districts.csv %d, tradeoff.csv %d, seed_sweep.csv %d\n",
            nrow(ps), nrow(dis), nrow(trade), nrow(sweep)))

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
