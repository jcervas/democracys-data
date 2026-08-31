# ---------------------------------------------------------------------------
# Build the seat-forecast dataset: a working House and Senate forecast for
# November 2026, every piece of it computed here so a reader can take it apart.
# The two chambers are simulated together, sharing one national error, which is
# the only way the joint probability comes out right.
#
# WHAT THIS WRITES
#
#   derived/generic_ballot.csv   Six polling aggregators' national House
#                                averages, plus their own average row.
#   derived/baseline.csv         435 districts: the 2024 House result, the
#                                2024 presidential vote inside the district,
#                                and the forecast's starting share for each.
#   derived/calibration.csv      One row per pair of consecutive elections:
#                                how far district swings departed from the
#                                national swing, and how many seats a
#                                uniform-swing model would have missed by.
#   derived/curve.csv            Seats against national swing, with no noise:
#                                the seats-votes curve for the 2024 map.
#   derived/simulation.csv       Summary of 20,000 simulated elections at each
#                                point on a grid of national environments.
#   derived/sim_default.csv      The full seat distribution at the environment
#                                the polls currently imply.
#   derived/models.csv           Five ways of answering the same question.
#   derived/redistricting.csv    Which states redrew their maps before 2026,
#                                and which way.
#   derived/senate_seats.csv     The 33 seats up in 2026 and their baseline.
#   derived/senate_calibration.csv  Past Senate races against their state's
#                                presidential vote, and what is left over.
#   derived/senate_curve.csv     Senate seats against the national vote.
#   derived/joint.csv            Both chambers, with and without a shared error.
#   derived/joint_cloud.csv      The two chambers' seat counts, binned, under
#                                each error structure.
#
# SOURCES
#
# 1. Wikipedia, "2026 United States House of Representatives elections",
#    fetched as wikitext through the MediaWiki API. Two tables are taken from
#    it: the generic-ballot aggregator comparison, and the summary of
#    mid-decade redistricting. Wikipedia is not the authority on either --
#    it is a place where six aggregators who do not publish a common file are
#    put in one table, which is the only reason it is used. Every aggregator
#    is named and linked in the chapter.
#
# 2. This book's own house-competition chapter, for the district-level
#    returns: derived/clerk_house.csv (the Clerk of the House's official
#    returns, 2004-2024) and derived/pres_by_cd.csv (the presidential vote
#    inside each congressional district).
#
# 3. This book's own midterm-loss chapter, for the base-rate model, and its
#    historical-campaigns chapter, for presidential returns by state.
#
# 4. @unitedstates/congress-legislators, for every sitting senator's party and
#    Senate class, and Wikipedia's Senate election pages for 2014, 2018, 2020
#    and 2022, used only to measure how far a Senate race lands from its
#    state's presidential vote.
#
# TWO SEATS THE OFFICIAL RETURNS DO NOT NUMBER
#
# The Clerk publishes "(1)" instead of a vote total where a candidate ran
# unopposed in some states. Two 2024 districts come through with zero votes
# for both parties -- FL-20 and OK-03 -- so the winner cannot be read off the
# arithmetic. They are assigned here to the party that carried the district's
# presidential vote. That rule is stated because it is a rule and not a
# lookup: it would be wrong in a district that split its ticket. Neither of
# these is within twenty points of the line, which is why it is safe here and
# would not be somewhere else.
#
# Run from this directory:  Rscript build-data.R      (needs internet)
# ---------------------------------------------------------------------------

source("../../../_lib/precision.R")     # dd_write_csv(): six significant digits

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
options(stringsAsFactors = FALSE, scipen = 999, timeout = 600)
set.seed(20261103)          # election day; the simulations must be reproducible

HC  <- "../../house-competition/data/derived"
HC2 <- "../../historical-campaigns/data/derived"
ML  <- "../../midterm-loss/data/derived"
EN  <- "../../election-night/data/derived"

say <- function(...) cat(..., "\n", sep = "")

# ===========================================================================
# 1. THE POLLS
# ===========================================================================
# Wikipedia wraps the aggregator table in <section begin="GenericBallotAgg"/>
# markers, which is a transclusion device rather than anything meant for a
# parser -- and it is the reason this table can be pulled out of a 370 KB page
# without guessing where it starts.

wiki_file <- "raw/wikipedia-house-2026.wikitext"
if (!file.exists(wiki_file)) {
  say("fetching the Wikipedia page as wikitext ...")
  api <- paste0("https://en.wikipedia.org/w/api.php?action=parse",
                "&page=2026_United_States_House_of_Representatives_elections",
                "&prop=wikitext&format=json&formatversion=2")
  tmp <- file.path(tempdir(), "wp.json")
  prov_fetch(api, tmp, quiet = TRUE, method = "curl",
             extra = '-L -A "Mozilla/5.0 (84-355 Democracys Data, course material)"')
  j <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  # The wikitext is one JSON string field. Pull it out without a JSON library:
  # everything between the "wikitext":" key and the closing quote of the
  # object, then undo the two escapes JSON actually uses here.
  w <- sub('.*"wikitext"\\s*:\\s*"', "", j)
  w <- sub('"\\s*\\}\\s*\\}\\s*$', "", w)
  w <- gsub('\\\\n', "\n", w)
  w <- gsub('\\\\"', '"', w)
  w <- gsub('\\\\\\\\', "\\\\", w)
  writeLines(w, wiki_file)
}
wiki <- paste(readLines(wiki_file, warn = FALSE), collapse = "\n")

gb_block <- regmatches(wiki, regexpr(
  '<section begin="GenericBallotAgg".*?<section end="GenericBallotAgg"', wiki))
stopifnot(length(gb_block) == 1)

# Strip the citation apparatus, then split the table on its row separators.
# SELF-CLOSING REFS COME OUT FIRST, AND THE ORDER IS NOT COSMETIC.
# <ref[^>]*> also matches a self-closing <ref name=x />. Strip paired refs
# first and that opening match hunts forward to the next real </ref>, deleting
# everything in between -- which in the Senate tables was whole rows of the
# results table. It cost 26 of 35 races before anyone noticed, because what
# survived still parsed. Remove the self-closing form before the paired form
# and the pattern has nothing to overshoot.
strip_refs <- function(x) {
  x <- gsub("<ref[^>]*/>", "", x)          # self-closing FIRST -- see above
  gsub("<ref[^>]*>.*?</ref>", "", x)
}
declutter <- function(x) {
  x <- strip_refs(x)
  x <- gsub("\\{\\{efn.*?\\}\\}", "", x)
  x <- gsub("\\{\\{Party shading/Democratic\\}\\}|\\{\\{Party shading/Republican\\}\\}",
            "", x)
  x <- gsub("\\{\\{cite[^}]*\\}\\}", "", x)
  x <- gsub("'''|''", "", x)
  x <- gsub("\\[\\[([^]|]*)\\|([^]]*)\\]\\]", "\\2", x)
  x <- gsub("\\[\\[([^]]*)\\]\\]", "\\1", x)
  trimws(x)
}
pct <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.]", "", x)))

rows <- strsplit(gb_block, "\n\\|-\n")[[1]]
gb <- list()
for (r in rows[-1]) {
  cells <- trimws(strsplit(sub("^\\|", "", r), "\n\\|")[[1]])
  cells <- declutter(cells)
  cells <- sub("^\\s*\\|\\s*", "", cells)          # "{{shading}} |value"
  if (length(cells) < 6) next
  # The average row merges its first two cells, so it is one short at the front.
  if (grepl("^colspan", cells[1])) {
    src <- "Average"; cells <- c("", "", cells[-1])
  } else src <- cells[1]
  n <- length(cells)
  rep_p <- pct(cells[n - 3]); dem_p <- pct(cells[n - 2])
  if (is.na(rep_p) || is.na(dem_p)) next
  gb[[length(gb) + 1]] <- data.frame(
    source = src, updated = cells[3], rep_pct = rep_p, dem_pct = dem_p,
    other_pct = pct(cells[n - 1]),
    margin = round(dem_p - rep_p, 1),
    two_party_dem = round(100 * dem_p / (dem_p + rep_p), 2))
}
gb <- do.call(rbind, gb)
gb$source[gb$source == ""] <- "Average"
stopifnot(nrow(gb) >= 5, "Average" %in% gb$source)
dd_write_csv(gb, "derived/generic_ballot.csv")
say("wrote generic_ballot.csv: ", nrow(gb) - 1, " aggregators, average D+",
    gb$margin[gb$source == "Average"])

POLL_2P <- gb$two_party_dem[gb$source == "Average"]   # the model's one poll input

# ===========================================================================
# 2. WHICH STATES REDREW THEIR MAPS
# ===========================================================================
# The same page carries a state-by-state summary of mid-decade redistricting.
# {{increase}} / {{decrease}} / {{steady}} / {{Same}} are the templates that
# carry the direction, so the count is read off the template name plus the
# number beside it.

rd_block <- regmatches(wiki, regexpr(
  "\\|\\+Summary of mid-decade changes.*?\\n\\|\\}", wiki))
stopifnot(length(rd_block) == 1)

# A count cell is a template and nothing else: {{increase}} 3, {{decrease}} 1,
# {{steady}}, {{Same}}. A cell that carries no such template is not a count
# cell, and the first version of this parser read three of them off the end of
# the notes column instead -- which is how Texas came out at fifteen seats and
# Wisconsin at fourteen. Requiring the template is what makes the cell
# identifiable at all, so a cell without one is refused rather than guessed at.
rdir <- function(cell) {
  if (!grepl("\\{\\{(increase|decrease|steady|Same)\\}\\}", cell,
             ignore.case = TRUE)) return(NA_integer_)
  after <- sub(".*\\{\\{(increase|decrease|steady|Same)\\}\\}", "", cell,
               ignore.case = TRUE)
  n <- suppressWarnings(as.integer(gsub("[^0-9]", "", after)))
  if (is.na(n)) n <- 0L
  if (grepl("\\{\\{increase\\}\\}", cell, ignore.case = TRUE)) return(n)
  if (grepl("\\{\\{decrease\\}\\}", cell, ignore.case = TRUE)) return(-n)
  0L                                       # {{steady}} or {{Same}}: no change
}
rd <- list()
for (r in strsplit(rd_block, "\n\\|-\n")[[1]][-1]) {
  cells <- strsplit(sub("^!", "", r), "\n\\|")[[1]]
  if (length(cells) < 6) next
  st <- declutter(sub("^\\[\\[#[^|]*\\|", "", cells[1]))
  st <- gsub("\\[|\\]|#", "", st)
  if (!st %in% state.name) next
  n <- length(cells)
  trio <- vapply(cells[(n - 2):n], rdir, integer(1), USE.NAMES = FALSE)
  if (any(is.na(trio))) next
  rd[[length(rd) + 1]] <- data.frame(
    state = st,
    enacted = grepl("New districts enacted|New map|redistricting", cells[2]),
    dem_seats = trio[1], comp_seats = trio[2], rep_seats = trio[3])
}
rd <- do.call(rbind, rd)
# No state redrew more than a handful of seats. A double-digit figure here is a
# parse that wandered into the notes, not a map.
stopifnot(max(abs(c(rd$dem_seats, rd$comp_seats, rd$rep_seats))) <= 8)
dd_write_csv(rd, "derived/redistricting.csv")
say("wrote redistricting.csv: ", nrow(rd), " states considered, ",
    sum(rd$dem_seats != 0 | rd$rep_seats != 0 | rd$comp_seats != 0),
    " changed a map; net R-leaning seats ", sprintf("%+d", sum(rd$rep_seats)),
    ", net D-leaning ", sprintf("%+d", sum(rd$dem_seats)))

# ===========================================================================
# 3. THE DISTRICT BASELINE
# ===========================================================================
ch <- read.csv(file.path(HC, "clerk_house.csv"))
pb <- read.csv(file.path(HC, "pres_by_cd.csv"))

# Which presidential file describes which House year's districts. The lines
# changed in 2022 and again in 2024, so the pairing is not automatic.
lines_for <- function(y) if (y >= 2024) "2024" else if (y >= 2022) "2022" else "2012-2021"
pres_for  <- function(y) if (y >= 2024) 2024 else if (y >= 2022) 2020 else
                         if (y >= 2018) 2016 else if (y >= 2014) 2012 else 2008

# National two-party Democratic share of the House vote, by year, straight out
# of the Clerk's totals. This is the quantity the generic ballot is trying to
# measure, and the one the model takes as its input.
natl <- round(100 * tapply(ch$dem_votes, ch$year, sum) /
              (tapply(ch$dem_votes, ch$year, sum) +
               tapply(ch$rep_votes, ch$year, sum)), 3)

build_year <- function(y) {
  p <- pb[pb$lines == lines_for(y) & pb$pres_year == pres_for(y),
          c("stcd", "dpres", "district")]
  h <- ch[ch$year == y, c("stcd", "state", "district", "dv", "dem_votes",
                          "rep_votes", "uncontested", "top_two")]
  z <- merge(h, p[, c("stcd", "dpres", "district")], by = "stcd",
             suffixes = c("", "_cd"))
  z$dwin <- z$dem_votes > z$rep_votes
  # The footnote seats: no votes published for either party.
  z$no_totals <- z$dem_votes == 0 & z$rep_votes == 0
  z$dwin[z$no_totals] <- z$dpres[z$no_totals] > 50
  # Seats with no two-party share -- unopposed, or a same-party top-two final.
  # Their starting share is predicted from the presidential vote in the
  # district, using the line fitted on the seats that did have two candidates.
  z$imputed <- is.na(z$dv)
  fit <- lm(dv ~ dpres, z[!z$imputed, ])
  z$base <- z$dv
  z$base[z$imputed] <- predict(fit, z[z$imputed, ])
  list(z = z, fit = fit, r2 = summary(fit)$r.squared,
       sigma = summary(fit)$sigma)
}

b24 <- build_year(2024)
base <- b24$z
stopifnot(nrow(base) == 435, !any(is.na(base$base)))
# The imputation must not overturn a result that is actually known.
wrong_side <- base$imputed & !base$no_totals & ((base$base > 50) != base$dwin)
stopifnot(sum(wrong_side) == 0)

base$district_id <- base$district_cd      # "AL-01", already in pres_by_cd.csv
stopifnot(!any(is.na(base$district_id)), !any(duplicated(base$district_id)))
base$party_2024 <- ifelse(base$dwin, "D", "R")
keep <- c("stcd", "district_id", "state", "district", "dv", "dpres", "base",
          "imputed", "no_totals", "uncontested", "top_two", "party_2024")
dd_write_csv(base[order(-base$base), keep], "derived/baseline.csv")

DSEATS_2024 <- sum(base$dwin)
NAT_2024    <- as.numeric(natl["2024"])
say("wrote baseline.csv: 435 districts, ", sum(base$imputed),
    " starting shares imputed, ", sum(base$no_totals),
    " seats the Clerk published no totals for")
say("  2024: D ", DSEATS_2024, " R ", 435 - DSEATS_2024,
    "; national two-party Democratic House vote ", NAT_2024, "%")
stopifnot(sum(base$base > 50) == DSEATS_2024)   # the baseline reproduces 2024

# ===========================================================================
# 4. CALIBRATION: HOW WRONG IS UNIFORM SWING?
# ===========================================================================
# The model moves every district by the same amount. Districts do not move by
# the same amount. The size of that failure is measurable: take two
# consecutive elections on the same map, subtract the national swing, and look
# at what is left in each district.
#
# Only pairs on stable district lines can be compared. 2010->2012 and
# 2020->2022 are redistricting boundaries and are left out, which is itself
# the point of the redistricting caveat in the chapter.
pairs <- list(c(2004, 2006), c(2006, 2008), c(2008, 2010),
              c(2012, 2014), c(2014, 2016), c(2016, 2018), c(2018, 2020),
              c(2022, 2024))

cal <- list(); devs <- list()
for (p in pairs) {
  a <- build_year(p[1])$z; b <- build_year(p[2])$z
  m <- merge(a[, c("stcd", "base")], b[, c("stcd", "base", "dwin")],
             by = "stcd", suffixes = c("_a", "_b"))
  sw <- as.numeric(natl[as.character(p[2])] - natl[as.character(p[1])])
  d  <- m$base_b - m$base_a - sw
  pred <- sum(m$base_a + sw > 50)
  nochange <- sum(m$base_a > 50)     # the baseline every forecast has to beat
  devs[[length(devs) + 1]] <- data.frame(pair = paste0(p[1], "-", p[2]), dev = d)
  cal[[length(cal) + 1]] <- data.frame(
    from = p[1], to = p[2], n = nrow(m), swing = round(sw, 2),
    sd_dev = round(sd(d), 2), pred_seats = pred, actual_seats = sum(m$dwin),
    err = pred - sum(m$dwin), nochange_err = nochange - sum(m$dwin))
}
cal <- do.call(rbind, cal)
devs <- do.call(rbind, devs)
dd_write_csv(cal, "derived/calibration.csv")

SD_DIST <- round(mean(cal$sd_dev[cal$to >= 2018]), 2)  # the recent era only
say("wrote calibration.csv: ", nrow(cal), " election pairs")
say("  mean |seat error| knowing the national swing: ",
    round(mean(abs(cal$err)), 1), " seats")
say("  district departure from uniform swing, sd: all pairs ",
    round(sd(devs$dev), 2), ", 2018 onward ", SD_DIST)

# ===========================================================================
# 5. THE MODEL
# ===========================================================================
# Three lines.
#
#   swing        = V - 48.65    the national two-party House vote you assume,
#                               minus what it actually was in 2024
#   share[d]     = base[d] + swing + e_national + e_district[d]
#   seats        = how many districts came out above 50
#
# e_national is one draw shared by all 435 districts: the polls being off, the
# late break, the thing that moves the whole country. e_district is drawn
# separately for each: a retirement, a scandal, a candidate who cannot raise
# money. Only the first of them matters much, and the chapter is largely about
# why.
NSIM <- 20000

# The curve with the noise switched off: seats as a pure function of the
# national vote. This is the object the phrase "the map" refers to.
grid <- seq(38, 62, by = 0.1)
curve <- data.frame(
  national_2p = grid,
  margin = round(2 * (grid - 50), 1),
  seats = sapply(grid, function(V) sum(base$base + (V - NAT_2024) > 50)))
dd_write_csv(curve, "derived/curve.csv")

TIP <- min(curve$national_2p[curve$seats >= 218])
say("wrote curve.csv: Democrats reach 218 seats at a national two-party vote ",
    "of ", TIP, "% (a margin of ", sprintf("%+.1f", 2 * (TIP - 50)), ")")

# The tipping-point district: sort every seat by the Democratic share it starts
# from, and take the 218th. Under uniform swing that seat decides the chamber,
# because the 217 above it are already won by the time it is.
ord <- base[order(-base$base), ]
tipd <- ord[218, ]
say("  the 218th seat from the Democratic end is ", tipd$district_id,
    ", starting at ", round(tipd$base, 1), "% -- it decides the House")
near <- sum(abs(base$base - 50) <= 5)
say("  ", near, " of 435 seats start within 5 points of the line; the other ",
    435 - near, " decide nothing")

# --- how steep the curve is where the answer is being decided ---------------
# Seats per point of national vote, over the range the 2026 forecast lives in.
# This converts a seat error into a vote error and back, which is what lets the
# backtest calibrate the model's own uncertainty in the units the model uses.
in_range <- curve$national_2p >= 50 & curve$national_2p <= 56
SLOPE <- round(as.numeric(coef(lm(seats ~ national_2p,
                                  curve[in_range, ]))[2]), 2)
say("  the curve runs at ", SLOPE, " seats per point of national two-party vote")

# --- the two error terms ----------------------------------------------------
# SD_MODEL is measured. The backtest above ran this exact model on eight pairs
# of past elections while TELLING it the national vote, and it still missed the
# seat count. Divide that seat error by the slope of the curve and it becomes
# an error in the model's own units -- points of national vote.
SD_MODEL <- round(sd(cal$err) / SLOPE, 2)

# SD_POLL is not measured, and this is the only quantity in the build that is
# not. It stands for two things a file cannot supply: how far a polling average
# lands from the vote, and how far the country moves between August and
# November. Published post-mortems put the final generic-ballot average's miss
# at roughly two to three points of margin, with 2022 nearer four -- and this
# average is not final, it is three months early. Two points of SHARE, which is
# four points of margin, is the default. It is the number the answer is most
# sensitive to, the chapter says so, and the figure lets a reader move it.
SD_POLL <- 2.0

# One national draw, so they combine in the usual way.
SD_NAT <- round(sqrt(SD_MODEL^2 + SD_POLL^2), 2)
say("  national error: ", SD_MODEL, " measured + ", SD_POLL,
    " assumed = ", SD_NAT, " points of share (", 2 * SD_NAT, " of margin)")

simulate <- function(V, sd_nat = SD_NAT, sd_dist = SD_DIST, nsim = NSIM,
                     b = base$base) {
  swing <- V - NAT_2024
  e_nat <- rnorm(nsim, 0, sd_nat)
  m <- matrix(b, nrow = nsim, ncol = length(b), byrow = TRUE)
  m <- m + swing + e_nat + rnorm(nsim * length(b), 0, sd_dist)
  rowSums(m > 50)
}

# How much of the spread each term is responsible for, at the polling average.
# The answer is the chapter's main methodological point, so it is written out.
q <- function(x, p) as.integer(round(quantile(x, p, type = 1)))
one <- function(label, ...) {
  s <- simulate(POLL_2P, ...)
  data.frame(terms = label, lo80 = q(s, .10), hi80 = q(s, .90),
             p_house = round(mean(s >= 218), 3))
}
spread <- rbind(one("national error only", sd_dist = 0),
                one("district error only", sd_nat  = 0),
                one("both"))
spread$width <- spread$hi80 - spread$lo80

# What the assumed polling error is worth. SD_POLL is the one input nobody
# measured, so how far the answer travels when it is turned is the honest
# statement of how much that guess matters.
sens <- do.call(rbind, lapply(c(0, 1, 2, 3, 4), function(pm) {
  sn <- sqrt(SD_MODEL^2 + (pm / 2)^2)     # pm is in points of MARGIN
  s <- simulate(POLL_2P, sd_nat = sn)
  data.frame(assumed_poll_error_margin = pm,
             national_error_share = round(sn, 2),
             median = q(s, .5), lo80 = q(s, .10), hi80 = q(s, .90),
             width = q(s, .90) - q(s, .10),
             p_house = round(mean(s >= 218), 3))
}))
dd_write_csv(sens, "derived/sensitivity.csv")
say("  turning the assumed polling error from 0 to ", 2 * SD_POLL,
    " points of margin: P(House) ", sprintf("%.0f%%", 100 * sens$p_house[1]),
    " -> ", sprintf("%.0f%%", 100 * sens$p_house[nrow(sens)]),
    ", interval ", sens$width[1], " -> ", sens$width[nrow(sens)], " seats wide")
dd_write_csv(spread, "derived/spread.csv")
say("  80% interval width: national error alone ", spread$width[1],
    " seats, district error alone ", spread$width[2], ", both ", spread$width[3])

# The simulation, over the same grid, coarser.
sgrid <- seq(44, 58, by = 0.25)
sim <- do.call(rbind, lapply(sgrid, function(V) {
  s <- simulate(V)
  data.frame(national_2p = V, margin = round(2 * (V - 50), 2),
             median = q(s, .5), mean = round(mean(s), 1),
             lo80 = q(s, .10), hi80 = q(s, .90),
             lo95 = q(s, .025), hi95 = q(s, .975),
             p_house = round(mean(s >= 218), 4))
}))
row.names(sim) <- NULL
dd_write_csv(sim, "derived/simulation.csv")
say("wrote simulation.csv: ", nrow(sim), " environments x ", NSIM, " simulations")

# The full distribution at the environment the polls currently imply.
s0 <- simulate(POLL_2P)
d0 <- as.data.frame(table(factor(s0, levels = min(s0):max(s0))))
names(d0) <- c("seats", "n")
d0$seats <- as.integer(as.character(d0$seats))
d0$share <- round(d0$n / NSIM, 5)
dd_write_csv(d0, "derived/sim_default.csv")
say("  at the polling average (two-party ", POLL_2P, "%): median ", q(s0, .5),
    " D seats, 80% interval ", q(s0, .10), "-", q(s0, .90),
    ", P(House) ", sprintf("%.0f%%", 100 * mean(s0 >= 218)))

# ===========================================================================
# 6. THE COMPETING MODELS
# ===========================================================================
mt <- read.csv(file.path(ML, "house_midterms.csv"))
mt <- mt[mt$midterm & !is.na(mt$pres_party_change), ]
# The interval here is the 10th to 90th percentile of past midterm seat
# changes, not the middle half. Every other row in this table reports an 80%
# interval, and putting a 50% interval beside them would make the oldest and
# crudest model look like the most confident one.
med_loss <- median(mt$pres_party_change)
lo_loss  <- as.numeric(quantile(mt$pres_party_change, .90))   # smallest loss
hi_loss  <- as.numeric(quantile(mt$pres_party_change, .10))   # largest loss

models <- rbind(
  data.frame(
    model = "No change",
    what_it_uses = "Nothing. Last election repeats.",
    point = DSEATS_2024, lo = NA, hi = NA, p_house = NA),
  data.frame(
    model = "The midterm penalty",
    what_it_uses = paste0("One sentence and ", nrow(mt), " past midterms."),
    point = DSEATS_2024 - med_loss,
    lo = DSEATS_2024 - lo_loss, hi = DSEATS_2024 - hi_loss,
    p_house = round(mean(DSEATS_2024 - mt$pres_party_change >= 218), 3)),
  data.frame(
    model = "Uniform swing",
    what_it_uses = "The polling average and the 2024 map.",
    point = sum(base$base + (POLL_2P - NAT_2024) > 50),
    lo = NA, hi = NA, p_house = NA),
  data.frame(
    model = "Uniform swing, simulated",
    what_it_uses = "The same, plus a national and a district error term.",
    point = q(s0, .5), lo = q(s0, .10), hi = q(s0, .90),
    p_house = round(mean(s0 >= 218), 3)),
  data.frame(
    model = "FiftyPlusOne",
    what_it_uses = paste("Polls, fundraising, past returns, candidate data",
                         "and race ratings, House and Senate together."),
    point = 230, lo = 211, hi = 253, p_house = 0.85))
models$point <- round(models$point)
dd_write_csv(models, "derived/models.csv")
say("wrote models.csv: ", nrow(models), " models")

# ===========================================================================
# 7. THE SENATE
# ===========================================================================
# Same three lines as the House, on 33 seats instead of 435. What differs is
# the baseline. A House district's starting share is what it did in 2024,
# because the whole House ran in 2024. A Senate seat's last outing was 2020,
# six years and two very different elections ago, so the starting share is the
# state's 2024 PRESIDENTIAL vote instead.
#
# That choice has a consequence the chapter states rather than hides: the
# Senate model does not reproduce the Senate. At a swing of zero it hands the
# Democrats the states Harris carried, which is not the same as the seats
# Democrats hold. The House model dodged this by starting from an actual House
# result. The Senate model cannot, so the incumbency the House model quietly
# inherited is missing here in plain view.

leg_url <- "https://unitedstates.github.io/congress-legislators/legislators-current.csv"
leg_file <- "raw/legislators-current.csv"
if (!file.exists(leg_file)) prov_fetch(leg_url, leg_file, quiet = TRUE)
leg <- read.csv(leg_file)
sens <- leg[leg$type == "sen", ]
stopifnot(nrow(sens) == 100)

# Presidential vote by state, from the historical-campaigns chapter.
ps <- read.csv(file.path(HC2, "pres_states_1864_2024.csv"))
ps$d2p <- 100 * ps$democrat / (ps$democrat + ps$republican)
p24 <- ps[ps$year == 2024, c("state_abbrev", "d2p")]
PRES_NAT_2024 <- round(100 * sum(ps$democrat[ps$year == 2024]) /
                       sum(ps$democrat[ps$year == 2024] +
                           ps$republican[ps$year == 2024]), 3)

# The Senate is in three classes and one class faces the voters every two
# years. Class 2 is up in 2026. Which seats those are is the calendar, not a
# forecast.
up    <- sens[sens$senate_class == 2, ]
notup <- sens[sens$senate_class != 2, ]
# The two independents caucus with the Democrats, so they count toward a
# Democratic majority. This is a fact about the Senate that no party column
# records, and it decides the arithmetic below.
DEM_FLOOR <- sum(notup$party != "Republican")
REP_FLOOR <- sum(notup$party == "Republican")
stopifnot(DEM_FLOOR + REP_FLOOR == 67)

# The Vice President is a Republican until January 2029 and breaks ties, so a
# Democratic Senate needs 51 and a Republican one needs 50.
DEM_TARGET <- 51 - DEM_FLOOR

sup <- merge(up[, c("state", "last_name", "first_name", "party")],
             p24, by.x = "state", by.y = "state_abbrev")
names(sup)[names(sup) == "d2p"] <- "pres24_2p"
sup$held <- ifelse(sup$party == "Republican", "R", "D")
sup <- sup[order(-sup$pres24_2p), ]
stopifnot(nrow(sup) == 33)

SEN_TIP <- sort(sup$pres24_2p, decreasing = TRUE)[DEM_TARGET]
say("wrote senate seats: ", nrow(sup), " up (D ", sum(sup$held == "D"),
    ", R ", sum(sup$held == "R"), "); not up D+I ", DEM_FLOOR,
    ", R ", REP_FLOOR)
say("  Democrats need ", DEM_TARGET, " of the ", nrow(sup),
    " up for 51, a net gain of ", DEM_TARGET - sum(sup$held == "D"))
say("  the ", DEM_TARGET, "th state by 2024 presidential lean is ",
    sup$state[DEM_TARGET], " at ", round(SEN_TIP, 2),
    "% -- a swing of ", round(50.001 - SEN_TIP, 2), " points")

# --- how far Senate races run from their state's presidential vote ----------
# For each past Senate race, the Democratic share of the two-party Senate vote
# is compared with the Democratic share of the two-party presidential vote in
# the same state, and the year's national offset is taken out. What is left is
# the state-level error this model has to carry.
#
# Each Senate year is paired with the presidential election a forecaster would
# have had in hand: the concurrent one in a presidential year, the one two
# years earlier at a midterm. 2026 is a midterm working from 2024, so the
# midterm years are the ones that describe its problem. Four years are read
# rather than only Class 2's own two, because 33 seats is a small sample and
# the quantity being measured is not specific to a class.
parse_senate_year <- function(year) {
  f <- sprintf("raw/wikipedia-senate-%d.wikitext", year)
  if (!file.exists(f)) {
    api <- paste0("https://en.wikipedia.org/w/api.php?action=parse&page=",
                  year, "_United_States_Senate_elections",
                  "&prop=wikitext&format=json&formatversion=2")
    tmp <- file.path(tempdir(), paste0("sen", year, ".json"))
    prov_fetch(api, tmp, quiet = TRUE, method = "curl",
               extra = '-L -A "Mozilla/5.0 (84-355 Democracys Data, course material)"')
    j <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
    w <- sub('.*"wikitext"\\s*:\\s*"', "", j)
    w <- sub('"\\s*\\}\\s*\\}\\s*$', "", w)
    w <- gsub('\\\\n', "\n", w); w <- gsub('\\\\"', '"', w)
    w <- gsub('\\\\\\\\', "\\\\", w)
    writeLines(w, f)
  }
  t <- paste(readLines(f, warn = FALSE), collapse = "\n")
  i <- regexpr("===\\s*Elections leading to the next Congress\\s*===", t)
  # End the block at the next second-level heading rather than after a fixed
  # number of characters. A fixed window runs on into the per-state sections,
  # which carry their own tables and their own refs.
  rest <- substring(t, i + 10)
  e <- regexpr("\n==[^=]", rest)
  stopifnot(i > 0, e > 0)
  # strip_refs only: declutter() would also unwrap [[link|text]], and the state
  # is identified by the wiki-link in the header cell.
  blk <- strip_refs(substring(rest, 1, e))
  out <- list()
  for (r in strsplit(blk, "\n\\|-\n")[[1]][-1]) {
    m <- regmatches(r, regexec("^!\\s*\\[\\[(.*?)\\]\\]", r))[[1]]
    if (length(m) < 2) next
    st <- trimws(sub(".*\\|", "", m[2]))    # "…|Alabama" -> "Alabama"
    if (!st %in% state.name) next
    d <- regmatches(r, gregexpr("\\(Democratic\\)\\s*([0-9.]+)%", r))[[1]]
    p <- regmatches(r, gregexpr("\\(Republican\\)\\s*([0-9.]+)%", r))[[1]]
    num <- function(x) as.numeric(gsub("[^0-9.]", "", x))
    # Exactly one Democrat and one Republican with a published share. A race
    # with none of one, or with two rounds, has no two-party share at all --
    # the Senate version of an uncontested House seat -- and is refused here
    # rather than guessed at.
    ok <- length(d) == 1 && length(p) == 1
    out[[length(out) + 1]] <- data.frame(
      year = year, state = st, usable = ok,
      dem = if (ok) num(d) else NA, rep = if (ok) num(p) else NA)
  }
  o <- do.call(rbind, out)
  # The presidential election a forecaster would have had: the concurrent one
  # in a presidential year, the previous one at a midterm. There is no 2014
  # presidential row anywhere, and an earlier version of this joined on the
  # Senate year itself and silently kept only the presidential years.
  o$pres_year <- if (year %% 4 == 0) year else year - 2
  o$midterm   <- year %% 4 != 0
  o
}

SEN_YEARS <- c(2014, 2018, 2020, 2022)
senhist <- do.call(rbind, lapply(SEN_YEARS, parse_senate_year))
# Class 2 is 33 seats, plus whatever specials shared the ballot, so each year
# should yield about 33 rows. A parser that quietly returns eight of them still
# produces a standard deviation, and that number would be wrong in a way
# nothing downstream could detect.
stopifnot(all(table(senhist$year) >= 30))
senhist$sen_2p <- 100 * senhist$dem / (senhist$dem + senhist$rep)
abb <- setNames(state.abb, state.name)
senhist$abbrev <- abb[senhist$state]
senhist <- merge(senhist, ps[, c("year", "state_abbrev", "d2p")],
                 by.x = c("pres_year", "abbrev"),
                 by.y = c("year", "state_abbrev"), all.x = TRUE)
names(senhist)[names(senhist) == "d2p"] <- "pres_2p"
u <- senhist[senhist$usable & !is.na(senhist$pres_2p), ]
# An earlier version joined on the Senate year itself. There is no 2014
# presidential row anywhere, so every midterm silently dropped out and the
# whole calibration rested on one concurrent presidential year.
stopifnot(nrow(u) > 60, all(SEN_YEARS %in% u$year))
u$gap <- u$sen_2p - u$pres_2p
# Take out each year's national offset, so what is left is the STATE-level
# scatter rather than the national environment.
off <- tapply(u$gap, u$year, mean)
u$resid <- u$gap - off[as.character(u$year)]

# 2026 is a midterm working from a presidential result two years old, so the
# midterm years are the ones that describe its problem. Both are written out,
# because the gap between them is worth seeing on its own.
SD_STATE     <- round(sd(u$resid[u$midterm]), 2)
SD_STATE_ALL <- round(sd(u$resid), 2)

sencal <- u[order(u$year, -abs(u$resid)),
            c("year", "pres_year", "midterm", "state", "sen_2p", "pres_2p",
              "gap", "resid")]
sencal[, 5:8] <- round(sencal[, 5:8], 2)
dd_write_csv(sencal, "derived/senate_calibration.csv")

sen_sd <- data.frame(
  year  = sort(unique(u$year)),
  kind  = ifelse(sort(unique(u$year)) %% 4 == 0, "presidential year", "midterm"),
  races = as.integer(table(u$year)),
  sd    = round(as.numeric(tapply(u$resid, u$year, sd)), 2))
dd_write_csv(sen_sd, "derived/senate_sd_by_year.csv")

# How many races the parser saw and how many it could use. A Senate race with a
# major party missing, a jungle primary or a runoff has no two-party share, and
# the count of those is a fact about Senate elections worth reporting rather
# than a number to bury.
dd_write_csv(data.frame(
  races_found = nrow(senhist), usable = nrow(u),
  refused = nrow(senhist) - nrow(u)), "derived/senate_parse.csv")

# The same measure on House districts, so the two chambers compare on like
# terms: the seat's own vote against the presidential vote in that seat, with
# the year's national offset removed. SD_DIST is a different quantity -- the
# departure from uniform swing between two elections -- and putting the two
# beside each other would be comparing unlike things.
rr <- read.csv(file.path(HC, "races.csv"))
rr <- rr[!is.na(rr$dv) & !is.na(rr$dpres) & rr$year %in% SEN_YEARS, ]
ho <- tapply(rr$dv - rr$dpres, rr$year, mean)
SD_DIST_LIKE <- round(sd(rr$dv - rr$dpres - ho[as.character(rr$year)]), 2)

say("  calibration: ", nrow(u), " of ", nrow(senhist),
    " past races had a two-party share for both parties, across ",
    length(SEN_YEARS), " elections")
say("  a Senate race lands ", SD_STATE,
    " points from its state's presidential vote at a midterm (all years ",
    SD_STATE_ALL, "); a House district, ", SD_DIST_LIKE)

sup$baseline <- sup$pres24_2p
dd_write_csv(sup[, c("state", "last_name", "first_name", "party", "held",
                     "pres24_2p", "baseline")], "derived/senate_seats.csv")

# --- the Senate seats-votes curve -------------------------------------------
sen_seats <- function(V) DEM_FLOOR +
  sum(sup$baseline + (V - PRES_NAT_2024) > 50)
sen_curve <- data.frame(
  national_2p = grid,
  margin = round(2 * (grid - 50), 1),
  seats = sapply(grid, sen_seats))
dd_write_csv(sen_curve, "derived/senate_curve.csv")
SEN_TIP_V <- min(sen_curve$national_2p[sen_curve$seats >= 51])
say("  Democrats reach 51 Senate seats at a national two-party vote of ",
    SEN_TIP_V, "% (a margin of ", sprintf("%+.1f", 2 * (SEN_TIP_V - 50)),
    "); the House needs ", TIP, "%")

# ===========================================================================
# 7b. BOTH CHAMBERS, ONE SIMULATION
# ===========================================================================
# The whole reason to carry the Senate. Each simulated election draws ONE
# national error and gives it to both chambers, because a polling miss is a
# polling miss everywhere. Run them separately and the joint probability comes
# out wrong -- the chapter shows by how much.
simulate_both <- function(V, sd_nat = SD_NAT, sd_dist = SD_DIST,
                          sd_state = SD_STATE, nsim = NSIM, shared = TRUE) {
  e_nat <- rnorm(nsim, 0, sd_nat)
  # A second, independent draw for the Senate is what "modeling the chambers
  # separately" amounts to. Passing shared = FALSE builds that straw man on
  # purpose, so the two can be compared.
  e_nat_sen <- if (shared) e_nat else rnorm(nsim, 0, sd_nat)

  hm <- matrix(base$base, nrow = nsim, ncol = 435, byrow = TRUE)
  hm <- hm + (V - NAT_2024) + e_nat + rnorm(nsim * 435, 0, sd_dist)
  house <- rowSums(hm > 50)

  sm <- matrix(sup$baseline, nrow = nsim, ncol = 33, byrow = TRUE)
  sm <- sm + (V - PRES_NAT_2024) + e_nat_sen + rnorm(nsim * 33, 0, sd_state)
  senate <- DEM_FLOOR + rowSums(sm > 50)

  data.frame(house = house, senate = senate)
}

jb <- simulate_both(POLL_2P)
ji <- simulate_both(POLL_2P, shared = FALSE)

joint_row <- function(d, label) {
  h <- d$house >= 218; s <- d$senate >= 51
  data.frame(model = label,
             p_house  = round(mean(h), 4),
             p_senate = round(mean(s), 4),
             p_both   = round(mean(h & s), 4),
             p_neither = round(mean(!h & !s), 4),
             p_split  = round(mean(h != s), 4),
             p_independent = round(mean(h) * mean(s), 4))
}
joint <- rbind(
  joint_row(jb, "one national error, shared by both chambers"),
  joint_row(ji, "a separate national error for each chamber"))
dd_write_csv(joint, "derived/joint.csv")

sen_dist <- as.data.frame(table(factor(jb$senate,
                                       levels = min(jb$senate):max(jb$senate))))
names(sen_dist) <- c("seats", "n")
sen_dist$seats <- as.integer(as.character(sen_dist$seats))
sen_dist$share <- round(sen_dist$n / NSIM, 5)
dd_write_csv(sen_dist, "derived/senate_sim.csv")

# The joint table as counts, for the two-by-two figure.
grid2 <- expand.grid(house = c("Democratic", "Republican"),
                     senate = c("Democratic", "Republican"))
grid2$share <- c(mean(jb$house >= 218 & jb$senate >= 51),
                 mean(jb$house <  218 & jb$senate >= 51),
                 mean(jb$house >= 218 & jb$senate <  51),
                 mean(jb$house <  218 & jb$senate <  51))
grid2$share <- round(grid2$share, 4)
dd_write_csv(grid2, "derived/joint_grid.csv")

# The two chambers plotted against each other, binned. Shared error and
# separate errors produce clouds of different SHAPE, and the shape is the
# argument: correlated error tilts it.
binned <- function(d, label) {
  hb <- 4 * floor(d$house / 4)
  sb <- d$senate
  tb <- as.data.frame(table(house = hb, senate = sb), stringsAsFactors = FALSE)
  tb <- tb[tb$Freq > 0, ]
  data.frame(mode = label,
             house = as.integer(tb$house), senate = as.integer(tb$senate),
             n = tb$Freq, share = round(tb$Freq / nrow(d), 6))
}
cloud <- rbind(binned(jb, "shared"), binned(ji, "separate"))
dd_write_csv(cloud, "derived/joint_cloud.csv")
say("  correlation between the chambers' seat counts: shared ",
    round(cor(jb$house, jb$senate), 2), ", separate ",
    round(cor(ji$house, ji$senate), 2))

say("wrote joint.csv: P(House) ", sprintf("%.0f%%", 100 * joint$p_house[1]),
    ", P(Senate) ", sprintf("%.0f%%", 100 * joint$p_senate[1]),
    ", P(both) ", sprintf("%.0f%%", 100 * joint$p_both[1]),
    " -- independence would say ",
    sprintf("%.0f%%", 100 * joint$p_independent[1]))
say("  Senate median ", q(jb$senate, .5), " seats, 80% interval ",
    q(jb$senate, .10), "-", q(jb$senate, .90))

# The published forecast this chapter is modeled on, for comparison. Quoted,
# not recomputed.
f51 <- data.frame(
  quantity = c("House", "Senate", "Both chambers"),
  fiftyplusone = c(0.85, 0.55, 0.53),
  this_model = c(joint$p_house[1], joint$p_senate[1], joint$p_both[1]))
f51$independence_would_say <- c(NA, NA, round(0.85 * 0.55, 4))
dd_write_csv(f51, "derived/joint_compare.csv")



# ===========================================================================
# 8. CHECKS THAT CAN FAIL
# ===========================================================================
checks <- rbind(
  data.frame(check = "baseline reproduces the 2024 House",
             value = sum(base$base > 50), expected = DSEATS_2024,
             ok = sum(base$base > 50) == DSEATS_2024),
  data.frame(check = "every district has a starting share",
             value = sum(!is.na(base$base)), expected = 435,
             ok = sum(!is.na(base$base)) == 435),
  data.frame(check = "the seats-votes curve is non-decreasing",
             value = sum(diff(curve$seats) < 0), expected = 0,
             ok = all(diff(curve$seats) >= 0)),
  data.frame(check = "simulated seats never leave 0-435",
             value = max(s0), expected = 435, ok = max(s0) <= 435 && min(s0) >= 0),
  data.frame(check = "aggregators agree to within 1.5 points of margin",
             value = round(diff(range(gb$margin[gb$source != "Average"])), 1),
             expected = 1.5,
             ok = diff(range(gb$margin[gb$source != "Average"])) <= 1.5),
  data.frame(check = "uniform swing beats assuming no change, in the backtest",
             value = round(mean(abs(cal$err)), 1),
             expected = round(mean(abs(cal$nochange_err)), 1),
             ok = mean(abs(cal$err)) < mean(abs(cal$nochange_err))),
  data.frame(check = "the redistricting table parsed as seats, not as notes",
             value = max(abs(c(rd$dem_seats, rd$rep_seats, rd$comp_seats))),
             expected = 8,
             ok = max(abs(c(rd$dem_seats, rd$rep_seats, rd$comp_seats))) <= 8),
  data.frame(check = "the Senate adds up to 100",
             value = nrow(sup) + DEM_FLOOR + REP_FLOOR, expected = 100,
             ok = nrow(sup) + DEM_FLOOR + REP_FLOOR == 100),
  data.frame(check = "every Senate election yielded about a full class",
             value = min(as.integer(table(u$year))), expected = 20,
             ok = min(as.integer(table(u$year))) >= 20),
  data.frame(check = "the Senate is the harder chamber for the Democrats",
             value = SEN_TIP_V, expected = TIP, ok = SEN_TIP_V > TIP),
  data.frame(check = "sharing the national error raises P(both chambers)",
             value = joint$p_both[1], expected = joint$p_both[2],
             ok = joint$p_both[1] > joint$p_both[2]),
  data.frame(check = "P(both) lies between its bounds",
             value = joint$p_both[1],
             expected = min(joint$p_house[1], joint$p_senate[1]),
             ok = joint$p_both[1] <= min(joint$p_house[1], joint$p_senate[1]) &&
                  joint$p_both[1] >= joint$p_house[1] + joint$p_senate[1] - 1))
dd_write_csv(checks, "derived/checks.csv")
say("\nchecks:")
for (i in seq_len(nrow(checks)))
  say("  ", ifelse(checks$ok[i], "ok  ", "FAIL"), "  ", checks$check[i],
      ": ", checks$value[i], " (expected ", checks$expected[i], ")")
stopifnot(all(checks$ok))

if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_report()
  prov_stamp()
}
