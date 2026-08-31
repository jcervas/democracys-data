# ---------------------------------------------------------------------------
# Build the chord dataset: a year of interstate migration, aggregated from a
# 52 x 52 matrix down to something a circle can hold, and laid out here so that
# print and screen draw the same ribbons.
#
# Six files end up in derived/:
#
#   derived/matrix.csv   the aggregated flow matrix, both groupings, with
#                        margins of error and a count of suppressed cells
#   derived/groups.csv   one row per group per grouping: totals and rim angles
#   derived/arcs.csv     one row per ordered pair: the sub-arc it occupies on
#                        its origin's rim -- the chord layout, precomputed
#   derived/pairs.csv    one row per unordered pair: gross both ways, net, and
#                        whether the net clears its own margin of error
#   derived/states.csv   the state-to-group assignment, so the aggregation is
#                        auditable rather than buried in this script
#   derived/facts.csv    single numbers the brief quotes
#
# Run this script from inside the data/ folder.
# ---------------------------------------------------------------------------

dir.create("derived", showWarnings = FALSE)
options(scipen = 999, stringsAsFactors = FALSE)

# --- Source -----------------------------------------------------------------
#
# The American Community Survey's 2024 state-to-state migration table, as
# captured, parsed and committed by this corpus's migration chapter:
#
#   ../../migration/data/derived/flows.csv    2,652 ordered state pairs
#   ../../migration/data/derived/states.csv   52 rows: 50 states, DC, Puerto Rico
#   ../../migration/data/derived/meta.csv     that chapter's published totals
#
# That chapter draws these flows as arcs on a map, one hub state at a time,
# and shows net migration by state. This one draws the whole matrix at once,
# which a map cannot do, and it has to destroy most of the matrix to do it.
#
# Seats come from the 2020 apportionment, as committed by:
#   ../../apportionment/data/derived/apportionment_2020.csv
#
# The state-to-division map comes from the regional-shift chapter, which builds
# it from the Bureau's Geographic Areas Reference Manual AND THEN CHECKS IT --
# it aggregates its own state populations to the four regions and refuses to
# build unless they reproduce the Bureau's published region totals for every
# decade. Retyping the nine lists here would be a second copy that nothing
# validates, so this chapter reads that one:
#   ../../regional-shift/data/derived/states.csv

F <- "../../migration/data/derived/flows.csv"
S <- "../../migration/data/derived/states.csv"
M <- "../../migration/data/derived/meta.csv"
A <- "../../apportionment/data/derived/apportionment_2020.csv"
G <- "../../regional-shift/data/derived/states.csv"
stopifnot(file.exists(F), file.exists(S), file.exists(M), file.exists(A),
          file.exists(G))
fl <- read.csv(F); st <- read.csv(S); mt <- read.csv(M); ap <- read.csv(A)
rs <- unique(read.csv(G)[, c("name", "region", "division")])

# --- The grouping, which is the whole chapter -------------------------------
#
# A 52 x 52 matrix has 2,652 ordered pairs. A chord diagram with 2,652 ribbons
# is a disc of ink. So the states are grouped, and the grouping is a decision
# that the figure will then present as though it were a fact about the country.
#
# These are the Census Bureau's own four REGIONS and nine DIVISIONS, used here
# rather than invented for two reasons: they are the units the Bureau publishes
# its own migration summaries in, so a reader can check this figure against the
# source; and they are stable, so the figure means the same thing next year.
#
# They are still a choice. The Bureau's divisions were fixed in 1910 and put
# Delaware and Florida in one group and Pennsylvania and New York in another;
# Maryland is South, Missouri is Midwest, Texas sits with Arkansas. Any
# statement this chapter makes about "the South" is a statement about that
# list of states and nothing more.
#
# PUERTO RICO IS IN NO DIVISION. The Bureau's regions cover the fifty states
# and the District of Columbia. Puerto Rico appears in the migration table --
# 38,257 people left it for a state in the year measured -- and dropping it to
# make the circle tidy would delete those people. It is carried as its own
# group, labelled, in both groupings.

# The Bureau's own groupings, read from regional-shift rather than retyped.
ORD_DIV <- c("New England", "Middle Atlantic", "East North Central",
             "West North Central", "South Atlantic", "East South Central",
             "West South Central", "Mountain", "Pacific")
ORD_REG <- c("Northeast", "Midwest", "South", "West")
stopifnot(setequal(rs$division, ORD_DIV), setequal(rs$region, ORD_REG),
          nrow(rs) == 51)

PR <- "Puerto Rico"
div_of <- setNames(c(rs$division, PR), c(rs$name, PR))
reg_of <- setNames(c(rs$region,   PR), c(rs$name, PR))
DIVS <- c(ORD_DIV, PR)
REGS <- c(ORD_REG, PR)

# Every state in the migration file must land in exactly one group, and every
# group must be non-empty. A typo in the lists above is otherwise invisible: it
# just makes a division slightly smaller.
UNITS <- sort(unique(c(fl$from_state, fl$to_state)))
stopifnot(length(UNITS) == 52,
          setequal(UNITS, names(div_of)),
          !anyDuplicated(names(div_of)),
          all(ORD_DIV %in% div_of), all(ORD_REG %in% reg_of))

sa <- data.frame(state = UNITS, division = unname(div_of[UNITS]),
                 region = unname(reg_of[UNITS]))
sa <- merge(sa, st[, c("state", "fips", "pop1")], by = "state")
sa <- merge(sa, setNames(ap[, c("state", "seats")], c("state", "seats_2020")),
            by = "state", all.x = TRUE)
sa$seats_2020[is.na(sa$seats_2020)] <- 0   # DC and Puerto Rico have no seats
sa <- sa[order(sa$division, sa$state), ]
write.csv(sa, "derived/states.csv", row.names = FALSE)

# --- Aggregating the matrix --------------------------------------------------
#
# Two things happen to a cell on the way into a group, and only one of them is
# arithmetic.
#
# SUPPRESSED CELLS. 279 of the 2,652 ordered pairs are published as "N" rather
# than a number: too few sample households to release. Summing with na.rm
# treats every one of them as a zero, which is the only thing you can do and is
# also wrong, because the true values are small but not zero. They are counted
# per aggregated cell and written out, so that a group flow assembled mostly
# from suppressed pairs cannot pass as a measurement.
#
# MARGINS OF ERROR. The Bureau's own instruction for a sum of published
# estimates is to add the margins in quadrature -- sqrt of the sum of squares.
# That assumes the errors are independent, and cells drawn from the same survey
# are not exactly independent, so the aggregated margins below are approximate
# and are, if anything, too small. They are still worth computing: they are how
# you find out that a net flow between two divisions is smaller than the
# uncertainty in either direction of it.

agg <- function(key) {
  g <- if (key == "division") div_of else reg_of
  d <- data.frame(from = unname(g[fl$from_state]), to = unname(g[fl$to_state]),
                  est = fl$est, moe = fl$moe,
                  supp = fl$flag == "N", stringsAsFactors = FALSE)
  k <- paste(d$from, d$to, sep = "\r")
  out <- do.call(rbind, lapply(split(seq_len(nrow(d)), k), function(i) {
    z <- d[i, ]
    data.frame(grouping = key, from = z$from[1], to = z$to[1],
               est = sum(z$est, na.rm = TRUE),
               moe = round(sqrt(sum(z$moe^2, na.rm = TRUE))),
               cells = nrow(z), suppressed = sum(z$supp),
               stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}
mx <- rbind(agg("division"), agg("region"))
mx$within <- mx$from == mx$to
write.csv(mx, "derived/matrix.csv", row.names = FALSE)

ORD <- list(division = DIVS, region = REGS)

# --- The chord layout, computed once ----------------------------------------
#
# A chord diagram is one piece of arithmetic. Each group gets a slice of the
# circle proportional to how many people LEFT it, that slice is divided into
# sub-arcs in group order, and the ribbon for a pair joins the (i -> j) sub-arc
# on i's rim to the (j -> i) sub-arc on j's rim.
#
# The consequence is the reason to draw one at all: THE TWO ENDS OF A RIBBON
# ARE DIFFERENT WIDTHS, and the difference is the net flow. It is the one
# quantity this figure states better than a table.
#
# It is computed here rather than in the browser so that the PDF and the HTML
# place every ribbon identically, instead of relying on two implementations of
# the same layout agreeing.

PAD <- 0.045                                  # radians of gap between groups

layout <- function(key, within) {
  gs <- ORD[[key]]
  z  <- mx[mx$grouping == key, ]
  if (!within) z <- z[!z$within, ]
  tot <- vapply(gs, function(g) sum(z$est[z$from == g]), numeric(1))
  span <- 2 * pi - PAD * length(gs)
  a <- 0; arcs <- list(); grp <- list()
  for (g in gs) {
    a0 <- a
    sub <- z[z$from == g, ]
    sub <- sub[order(match(sub$to, gs)), ]
    for (r in seq_len(nrow(sub))) {
      w <- sub$est[r] / sum(tot) * span
      arcs[[length(arcs) + 1]] <- data.frame(
        grouping = key, within = within, from = g, to = sub$to[r],
        est = sub$est[r], ang0 = a, ang1 = a + w, stringsAsFactors = FALSE)
      a <- a + w
    }
    grp[[length(grp) + 1]] <- data.frame(
      grouping = key, within = within, group = g,
      out_est = tot[[g]], in_est = sum(z$est[z$to == g]),
      ang0 = a0, ang1 = a, stringsAsFactors = FALSE)
    a <- a + PAD
  }
  list(arcs = do.call(rbind, arcs), groups = do.call(rbind, grp))
}

L <- lapply(list(c("division", TRUE), c("division", FALSE),
                 c("region", TRUE),   c("region", FALSE)),
            function(p) layout(p[1], as.logical(p[2])))
arcs <- do.call(rbind, lapply(L, `[[`, "arcs"))
grps <- do.call(rbind, lapply(L, `[[`, "groups"))
arcs$ang0 <- round(arcs$ang0, 6); arcs$ang1 <- round(arcs$ang1, 6)
grps$ang0 <- round(grps$ang0, 6); grps$ang1 <- round(grps$ang1, 6)
grps$net <- grps$in_est - grps$out_est

# The rim must close: the arcs of a group have to fill its span exactly, and
# the groups plus the padding have to fill the circle.
for (i in seq_len(nrow(grps))) {
  g <- grps[i, ]
  z <- arcs[arcs$grouping == g$grouping & arcs$within == g$within &
            arcs$from == g$group, ]
  stopifnot(nrow(z) > 0,
            abs(min(z$ang0) - g$ang0) < 1e-5, abs(max(z$ang1) - g$ang1) < 1e-5)
}
for (k in unique(paste(grps$grouping, grps$within))) {
  z <- grps[paste(grps$grouping, grps$within) == k, ]
  stopifnot(abs(max(z$ang1) + PAD - 2 * pi) < 1e-5)
}
write.csv(arcs, "derived/arcs.csv", row.names = FALSE)
write.csv(grps, "derived/groups.csv", row.names = FALSE)

# --- Gross both ways, and the net between them -------------------------------
#
# The chapter's finding lives in this table. For each unordered pair of groups,
# how many people went each way, what the difference is, and whether that
# difference is bigger than its own uncertainty.
#
# The margin on a DIFFERENCE of two estimates is also added in quadrature, so a
# net between two large flows carries the uncertainty of both. That is why a
# net can be a large number and still not be a finding.

pairs <- do.call(rbind, lapply(c("division", "region"), function(key) {
  gs <- ORD[[key]]; z <- mx[mx$grouping == key, ]
  do.call(rbind, lapply(seq_along(gs), function(i) {
    if (i == length(gs)) return(NULL)
    do.call(rbind, lapply((i + 1):length(gs), function(j) {
      ab <- z[z$from == gs[i] & z$to == gs[j], ]
      ba <- z[z$from == gs[j] & z$to == gs[i], ]
      net <- ab$est - ba$est
      nmoe <- sqrt(ab$moe^2 + ba$moe^2)
      data.frame(grouping = key, a = gs[i], b = gs[j],
                 a_to_b = ab$est, b_to_a = ba$est,
                 gross = ab$est + ba$est, net = net,
                 net_moe = round(nmoe),
                 net_sig = abs(net) > nmoe,
                 net_pct_of_gross = round(100 * abs(net) / (ab$est + ba$est), 1),
                 suppressed = ab$suppressed + ba$suppressed,
                 stringsAsFactors = FALSE)
    }))
  }))
}))
write.csv(pairs, "derived/pairs.csv", row.names = FALSE)

# --- Facts -------------------------------------------------------------------

f <- function(k) mt$value[mt$key == k]
dv <- mx[mx$grouping == "division", ]
gd <- grps[grps$grouping == "division" & grps$within, ]
pd <- pairs[pairs$grouping == "division", ]
pr <- pairs[pairs$grouping == "region", ]

within_est <- sum(dv$est[dv$within])
between    <- sum(dv$est[!dv$within])
big  <- pd[which.max(pd$gross), ]
netb <- pd[which.max(abs(pd$net)), ]
big2 <- pd[pd$gross > quantile(pd$gross, 0.5), ]
tiny <- big2[!big2$net_sig, ]
tiny <- tiny[which.max(tiny$gross), ]          # largest exchange with no net
canc <- big2[which.min(abs(big2$net)), ]       # the flattest exchange of all

seats <- tapply(sa$seats_2020, sa$division, sum)
pop   <- tapply(sa$pop1, sa$division, sum)
gd$seats <- as.numeric(seats[gd$group])
gd$pop   <- as.numeric(pop[gd$group])
gd$net_per1k <- round(1000 * gd$net / gd$pop, 2)
gd <- gd[order(gd$net_per1k), ]
losers  <- gd[gd$net < 0 & gd$group != "Puerto Rico", ]
winners <- gd[gd$net > 0, ]

facts <- data.frame(
  key = c("units", "ordered_pairs", "suppressed_pairs", "sig_pairs",
          "movers_between_states", "movers_within_state", "movers_abroad",
          "acs_year",
          "divisions", "regions", "div_ribbons", "reg_ribbons",
          "within_div", "between_div", "within_pct",
          "big_a", "big_b", "big_gross", "big_ab", "big_ba", "big_net",
          "big_net_pct",
          "netb_a", "netb_b", "netb_net", "netb_gross", "netb_net_pct",
          "tiny_a", "tiny_b", "tiny_gross", "tiny_net", "tiny_net_moe",
          "canc_a", "canc_b", "canc_gross", "canc_net", "canc_net_moe",
          "ne_south_net", "ne_south_gross", "south_west_net", "south_west_gross",
          "div_cells", "div_cells_supp", "div_cells_zero",
          "div_pairs", "div_pairs_sig", "reg_pairs", "reg_pairs_sig",
          "median_net_pct", "overall_net_pct",
          "loser_seats", "winner_seats", "loser_divs", "winner_divs",
          "top_gainer", "top_gainer_per1k", "top_loser", "top_loser_per1k",
          "pr_out", "pr_in"),
  value = c(52, nrow(fl), f("n_suppressed"), f("n_sig"),
            f("us_movers_between_states"), f("us_movers_within_state"),
            f("us_movers_from_abroad"), f("acs_year"),
            length(DIVS), length(REGS),
            sum(mx$grouping == "division"), sum(mx$grouping == "region"),
            within_est, between, round(100 * within_est / (within_est + between), 1),
            big$a, big$b, big$gross, big$a_to_b, big$b_to_a, big$net,
            big$net_pct_of_gross,
            netb$a, netb$b, netb$net, netb$gross, netb$net_pct_of_gross,
            tiny$a, tiny$b, tiny$gross, tiny$net, tiny$net_moe,
            canc$a, canc$b, canc$gross, canc$net, canc$net_moe,
            pr$net[pr$a == "Northeast" & pr$b == "South"],
            pr$gross[pr$a == "Northeast" & pr$b == "South"],
            pr$net[pr$a == "South" & pr$b == "West"],
            pr$gross[pr$a == "South" & pr$b == "West"],
            nrow(dv), sum(dv$suppressed > 0), sum(dv$est == 0),
            nrow(pd), sum(pd$net_sig), nrow(pr), sum(pr$net_sig),
            median(pd$net_pct_of_gross),
            round(100 * sum(abs(pd$net)) / sum(pd$gross), 1),
            sum(losers$seats), sum(winners$seats),
            nrow(losers), nrow(winners),
            gd$group[nrow(gd)], gd$net_per1k[nrow(gd)],
            gd$group[1], gd$net_per1k[1],
            sum(dv$est[dv$from == "Puerto Rico" & !dv$within]),
            sum(dv$est[dv$to == "Puerto Rico" & !dv$within])),
  stringsAsFactors = FALSE)
write.csv(facts, "derived/facts.csv", row.names = FALSE)

cat("matrix.csv ->", nrow(mx), "aggregated cells (",
    sum(mx$grouping == "division"), "division,",
    sum(mx$grouping == "region"), "region )\n")
cat("arcs.csv   ->", nrow(arcs), "sub-arcs across 4 layouts\n")
cat("pairs.csv  ->", nrow(pd), "division pairs,", sum(pd$net_sig),
    "with a net larger than its own margin\n\n")
cat("2,652 ordered state pairs ->", sum(mx$grouping == "division"),
    "division cells\n")
cat("within-division moves:", format(within_est, big.mark = ","), "of",
    format(within_est + between, big.mark = ","), "(",
    round(100 * within_est / (within_est + between), 1), "% )\n")
cat("biggest exchange:", big$a, "<->", big$b, format(big$gross, big.mark = ","),
    "gross, net", format(big$net, big.mark = ","), "(",
    big$net_pct_of_gross, "% )\n")
cat("median net as a share of gross:", median(pd$net_pct_of_gross), "%\n\n")
print(gd[, c("group", "out_est", "in_est", "net", "net_per1k", "seats")],
      row.names = FALSE)
cat("\ndone.\n")

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
