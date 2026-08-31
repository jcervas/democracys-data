# ---------------------------------------------------------------------------
# Build the datasets for the "names as data" chapter.
#
# The chapter is about the two tabulations of American names the Census Bureau
# released in April 2026 -- one of first names, one of last names -- and about
# the three jobs a name gets asked to do in a data file: identify a person,
# stand in for their sex, and stand in for their race.
#
# ---------------------------------------------------------------------------
# SOURCES
# ---------------------------------------------------------------------------
#
# U.S. Census Bureau, "Frequently Occurring First Names in the 2020 Census" and
# "Frequently Occurring Last Names in the 2020 Census", April 2026, from
# https://www.census.gov/topics/population/genealogy/data/2020_names.html --
# spreadsheets served over plain HTTPS from
# https://www2.census.gov/topics/genealogy/2020surnames/, no key or account.
#
# Five spreadsheets, all fetched here:
#
#      Names2020_FirstNames_Sex.xlsx                    first name x sex
#      Names2020_FirstNames_RaceHispanic.xlsx           first name x race
#      Names2020_FirstNames_Sex_WithNegatives.xlsx      the same, un-repaired
#      Names2020_FirstNames_RaceHispanic_WithNegatives.xlsx
#      Names2020_LastNames_RaceHispanic.xlsx            last name x race
#
# Two header rows sit above the column names in each, hence skip = 2. Every
# column is read as text and converted here, because the count columns arrive
# as text in some rows and the proportion columns carry seventeen digits.
#
# The last-name spreadsheet is also committed by the surnames chapter, and this
# chapter deliberately does NOT read that copy. It fetches its own, so that the
# chapter stands alone and its provenance record covers every file it uses.
# The cost is 10.8 MB stored twice; the benefit is that neither chapter can be
# broken by a change made in the other.
#
# One further source, and it is not the census:
#
# Social Security Administration, "Beyond the Top 1000 Names", national data,
# from https://www.ssa.gov/oact/babynames/limits.html -- a zip of one text file
# per birth year, 1880 onward. It is read in section 11, for the one question
# the census tables cannot answer: WHEN a name was given. See the long comment
# there, including why this fetch needs a browser user-agent and the others
# do not.
#
# ---------------------------------------------------------------------------
# WHAT IS WRITTEN
# ---------------------------------------------------------------------------
# Twenty-six tables in derived/, all counts and shares. Roughly 2.9 MB, of
# which most is the birth-year table the third browser figure loads. Run this
# script from inside the data/ folder. It downloads about 30 MB and needs no
# API key.
# ---------------------------------------------------------------------------

dir.create("raw",     showWarnings = FALSE)
dir.create("derived", showWarnings = FALSE)
source("../../../_lib/precision.R")    # dd_write_csv(): six significant digits
source("../../../_lib/provenance.R")   # prov_fetch(): notice when a source moves

options(scipen = 999, stringsAsFactors = FALSE)
stopifnot(requireNamespace("readxl", quietly = TRUE))

RACE  <- c("white", "black", "aian", "asian", "twoplus", "hispanic")
PRETTY <- c(white = "white", black = "Black", aian = "Am. Indian",
            asian = "Asian/PI", twoplus = "two or more", hispanic = "Hispanic")

B20  <- "https://www2.census.gov/topics/genealogy/2020surnames/"

grab <- function(f) {
  p <- file.path("raw", f)
  if (!file.exists(p)) prov_fetch(paste0(B20, f), p, label = f) else p
}
rdx <- function(p) as.data.frame(suppressMessages(
  readxl::read_excel(p, skip = 2, col_types = "text")))
num <- function(d) {
  for (v in setdiff(names(d), "name")) d[[v]] <- suppressWarnings(as.numeric(d[[v]]))
  d
}

# --- 1. Read the four first-name tables and the last-name one --------------

fs <- num(setNames(rdx(grab("Names2020_FirstNames_Sex.xlsx")),
                   c("name", "rank", "count", "per100k", "cum", "male", "female")))
fr <- num(setNames(rdx(grab("Names2020_FirstNames_RaceHispanic.xlsx")),
                   c("name", "rank", "count", "per100k", "cum", RACE)))
fsn <- num(setNames(rdx(grab("Names2020_FirstNames_Sex_WithNegatives.xlsx")),
                    c("name", "male", "female")))
frn <- num(setNames(rdx(grab("Names2020_FirstNames_RaceHispanic_WithNegatives.xlsx")),
                    c("name", RACE)))
ln <- num(setNames(rdx(grab("Names2020_LastNames_RaceHispanic.xlsx")),
                   c("name", "rank", "count", "per100k", "cum", RACE)))

# THE LAST ROW OF EVERY ONE OF THESE FILES IS NOT A NAME. Both tabulations
# sweep every name held by fewer than 100 people into a single line called
# ALL OTHER NAMES, and leave its rank cell empty -- so a filter that drops rows
# without a rank silently deletes 18.8 million people from the first-name table
# and 36.3 million from the last-name one. It is separated by its label here,
# never by its rank, and its size is carried into the arithmetic below rather
# than dropped.
is_resid <- function(d) grepl("^ALL OTHER", d$name)
for (v in c("fs", "fr", "fsn", "frn", "ln")) stopifnot(sum(is_resid(get(v))) == 1)

RESID_F <- fs$count[is_resid(fs)]
RESID_L <- ln$count[is_resid(ln)]
TOT_F   <- sum(fs$count)          # everyone who has a first name in the file
TOT_L   <- sum(ln$count)

FS <- fs[!is_resid(fs), ]; FR <- fr[!is_resid(fr), ]; L <- ln[!is_resid(ln), ]
FSN <- fsn[!is_resid(fsn), ]; FRN <- frn[!is_resid(frn), ]
stopifnot(!any(duplicated(FS$name)), !any(duplicated(L$name)),
          nrow(FS) == nrow(FR), setequal(FS$name, FR$name))

cat(sprintf("first names %s (+ residual of %s people); last names %s (+ %s)\n",
            format(nrow(FS), big.mark = ","), format(RESID_F, big.mark = ","),
            format(nrow(L),  big.mark = ","), format(RESID_L, big.mark = ",")))

# 2020 Census resident population, the denominator the Bureau's own briefs use
# when they say a first name was recorded for 91.1 percent of the country.
US2020 <- 331449281

# --- 2. Concentration, and the number of names that behave like all of them -
#
# Two summaries of one distribution, and the chapter turns on the difference.
#
#   Simpson's index, sum p^2, is the probability that two people drawn at
#   random share a name. It is the quantity a record linker needs.
#   The exponential of Shannon entropy, 2^H, is the EFFECTIVE NUMBER of names:
#   how many equally common names would produce the same amount of surprise.
#   It is the quantity that says how much of the raw name count is real.
#
# Both are computed over the FULL population, residual included, not over the
# published rows renormalised to themselves. Renormalising would drop the
# people with rare names, who are exactly the people least likely to collide,
# and would overstate every collision probability below.

conc <- function(cnt, total, resid, label) {
  p  <- cnt / total
  S  <- sum(p^2)
  H  <- -sum(p * log2(p))            # published names only, over the true total
  m  <- resid / total                # the mass the file does not itemise
  # THE TWO SUMMARIES BEHAVE COMPLETELY DIFFERENTLY UNDER THE MISSING TAIL, and
  # that difference is the point of the section that uses them.
  #
  # Simpson's index is nearly pinned down. Every unlisted name is held by fewer
  # than 100 people, so sum q^2 <= max(q) * sum(q) < (100/total) * m -- a bound
  # four orders of magnitude below S.
  #
  # Entropy is not pinned down, only bracketed. The residual's contribution
  # -sum q log2 q is smallest when its names are as LARGE as they can be (all
  # exactly 100, giving m * log2(total/100)) and largest when they are as small
  # as they can be (all exactly 1, giving m * log2(total)). Report the bracket
  # rather than a point, because a point would be a number about the tail this
  # file does not publish.
  bound <- (100 / total) * m
  data.frame(which = label, names_published = length(cnt), residual_people = resid,
             people_total = total,
             p_match = S, one_in = 1 / S,
             residual_bound = bound, residual_bound_pct = 100 * bound / S,
             bits_low  = H + m * log2(total / 100),
             bits_high = H + m * log2(total),
             effective_low  = 2^(H + m * log2(total / 100)),
             effective_high = 2^(H + m * log2(total)),
             stringsAsFactors = FALSE)
}
CF <- conc(FS$count, TOT_F, RESID_F, "first name")
CL <- conc(L$count,  TOT_L, RESID_L, "last name")
concentration <- rbind(CF, CL)
# Written as the audit record of this arithmetic, not as an input to the brief.
# Nothing reads it: the brief takes the effective-name brackets and the residual
# bounds from facts.csv, and the collision probabilities from collision.csv. The
# columns kept here and nowhere else -- names_published, residual_people,
# people_total, p_match, one_in, residual_bound, bits_low, bits_high -- are the
# workings behind those published figures, and exist so the derivation can be
# checked without re-running this script.
dd_write_csv(concentration, "derived/concentration.csv")

# The curve itself: what share of the country the k most common names cover.
# Ranks are laid out log-spaced because the interesting behaviour is all in the
# first thousand and the tail is 150,000 rows long.
curve_of <- function(cnt, total, label) {
  s <- sort(cnt, decreasing = TRUE)
  cs <- cumsum(s)
  ks <- unique(round(exp(seq(0, log(length(s)), length.out = 260))))
  data.frame(which = label, rank = ks, cum_people = cs[ks],
             cum_pct = 100 * cs[ks] / total, stringsAsFactors = FALSE)
}
dd_write_csv(rbind(curve_of(FS$count, TOT_F, "first name"),
                   curve_of(L$count,  TOT_L, "last name")),
             "derived/curve.csv")

half <- function(cnt, total) which(cumsum(sort(cnt, decreasing = TRUE)) >= total / 2)[1]
covers <- function(cnt, total, k) 100 * sum(head(sort(cnt, decreasing = TRUE), k)) / total
cover <- do.call(rbind, lapply(c(10, 100, 1000, 10000), function(k)
  data.frame(top_n = k,
             first_pct = covers(FS$count, TOT_F, k),
             last_pct  = covers(L$count,  TOT_L, k), stringsAsFactors = FALSE)))
dd_write_csv(cover, "derived/cover.csv")

HALF_F <- half(FS$count, TOT_F); HALF_L <- half(L$count, TOT_L)
cat(sprintf("effective names: first %.0f-%.0f of %s | last %.0f-%.0f of %s\n",
            CF$effective_low, CF$effective_high, format(nrow(FS), big.mark = ","),
            CL$effective_low, CL$effective_high, format(nrow(L), big.mark = ",")))
cat(sprintf("names covering half the country: first %d | last %d\n", HALF_F, HALF_L))

# --- 3. The top of each list -----------------------------------------------

top_of <- function(d, total, label, k = 25) {
  d <- d[order(d$rank), ][seq_len(k), ]
  data.frame(which = label, rank = d$rank, name = d$name, count = d$count,
             pct = 100 * d$count / total, stringsAsFactors = FALSE)
}
dd_write_csv(rbind(top_of(FS, TOT_F, "first name"), top_of(L, TOT_L, "last name")),
             "derived/top25.csv")

# --- 4. Sex -----------------------------------------------------------------
#
# The sex table is the only place the Bureau attaches sex to a name, and the
# question the chapter asks of it is the one analysts actually ask: if you
# assign each person the majority sex of their first name, how many people do
# you get wrong? That is a count, not a rate to be argued about.

FS$tot <- FS$male + FS$female
stopifnot(all(FS$tot > 0))
FS$pct_female <- 100 * FS$female / FS$tot
MAJ_RIGHT <- sum(pmax(FS$male, FS$female))
MAJ_WRONG <- sum(FS$tot) - MAJ_RIGHT

bands <- do.call(rbind, lapply(c(1, 5, 10, 25, 40), function(th) {
  s <- FS[FS$pct_female >= th & FS$pct_female <= 100 - th, ]
  data.frame(minority_sex_at_least_pct = th, names = nrow(s), people = sum(s$tot),
             pct_of_people = 100 * sum(s$tot) / sum(FS$tot), stringsAsFactors = FALSE)
}))
dd_write_csv(bands, "derived/sex_bands.csv")

# The exhibit: the most-used names that split most evenly. The 100,000 cut is
# stated in the brief; it is there so that the table is a list of names a
# reader recognises rather than a list of rare spellings sitting at 50%.
uni <- FS[FS$tot >= 100000, ]
uni <- uni[order(abs(uni$pct_female - 50)), ][1:12, ]
dd_write_csv(data.frame(name = uni$name, people = uni$tot,
                        pct_female = uni$pct_female, rank = uni$rank),
             "derived/unisex.csv")

# Every name with 25,000 or more bearers, for the scatter. 25,000 is where the
# figure stops being a cloud; the cut is declared in the caption.
SC_CUT <- 25000
sc <- FS[FS$tot >= SC_CUT, ]
dd_write_csv(data.frame(name = sc$name, people = sc$tot, pct_female = sc$pct_female),
             "derived/sex_scatter.csv")
cat(sprintf("sex: majority rule misassigns %s people (%.2f%% wrong)\n",
            format(MAJ_WRONG, big.mark = ","), 100 * MAJ_WRONG / sum(FS$tot)))

# --- 5. Race, and how much of it a name carries -----------------------------
#
# Measured in bits, so that a first name and a last name can be compared on one
# scale. H0 is how uncertain you are about a person's race before you hear
# their name; H1 is how uncertain you are afterwards, averaged over people.
# The difference is what the name told you.
#
# NOTE, and the Bureau says this too: these files are NOT a source for the
# racial composition of the United States. H0 below is the composition OF THE
# PEOPLE IN THIS FILE, which is the right baseline for this calculation and the
# wrong number to quote as a national statistic.

info <- function(d, label) {
  M <- as.matrix(d[, RACE]); tot <- rowSums(M)
  keep <- tot > 0; M <- M[keep, , drop = FALSE]; tot <- tot[keep]
  P <- M / tot
  w <- tot / sum(tot)
  natl <- colSums(M) / sum(M)
  H0 <- -sum(natl * log2(natl))
  H1 <- sum(w * apply(P, 1, function(p) { p <- p[p > 0]; -sum(p * log2(p)) }))
  mx <- apply(P, 1, max)
  data.frame(which = label, names = nrow(M), people = sum(tot),
             modal_correct_pct = 100 * sum(apply(M, 1, max)) / sum(tot),
             bits_before = H0, bits_after = H1, bits_learned = H0 - H1,
             pct_of_uncertainty_removed = 100 * (H0 - H1) / H0,
             names_over_90 = sum(mx > 0.9),
             people_over_90_pct = 100 * sum(tot[mx > 0.9]) / sum(tot),
             stringsAsFactors = FALSE)
}
race_info <- rbind(info(FR, "first name"), info(L, "last name"))
dd_write_csv(race_info, "derived/race_info.csv")
cat(sprintf("race: a first name carries %.3f bits, a last name %.3f\n",
            race_info$bits_learned[1], race_info$bits_learned[2]))

# The most distinctive name for each group, first and last. The point of the
# table is not the names; it is that the ceiling is 95% for one group and far
# lower for others, because a name cannot outrun the base rate.
extreme <- function(d, label, minc = 50000) {
  d <- d[d$count >= minc, ]
  M <- as.matrix(d[, RACE]) / d$count
  do.call(rbind, lapply(RACE, function(g) {
    i <- which.max(M[, g])
    data.frame(which = label, group = unname(PRETTY[g]), name = d$name[i],
               people = d$count[i], pct_in_group = 100 * M[i, g],
               group_share_of_file = 100 * sum(d[[g]]) / sum(d$count),
               stringsAsFactors = FALSE)
  }))
}
dd_write_csv(rbind(extreme(FR, "first name"), extreme(L, "last name")),
             "derived/race_extremes.csv")

# --- 6. Two people, one name ------------------------------------------------
#
# The independence step is the weak link and it is written out rather than
# buried: P(same first AND same last) is computed as the product of two
# marginals, which is right only if first and last names are unrelated. They
# are not. How wrong that is cannot be measured from these files -- the Bureau
# publishes no joint table -- but a floor on the error can be, by allowing the
# two names to be correlated through the six race categories and no further.

P_F <- CF$p_match; P_L <- CL$p_match
P_BOTH <- P_F * P_L

# The correction is computed as a RATIO on a like-for-like basis and then
# applied, rather than as a rival absolute number. Both models below run over
# the published names only, because p(name | race) can only be formed for names
# the file itemises; dividing one by the other cancels that restriction, which
# an absolute comparison against P_BOTH would not -- it would report the
# residual's dilution as if it were correlation.
cond <- function(d) { M <- as.matrix(d[, RACE]); t(t(M) / colSums(M)) }  # p(name|race)
w    <- (colSums(as.matrix(FR[, RACE])) / sum(as.matrix(FR[, RACE])) +
         colSums(as.matrix(L[, RACE]))  / sum(as.matrix(L[, RACE]))) / 2
Af <- crossprod(cond(FR)); Al <- crossprod(cond(L))   # 6x6: sum_name p(n|r)p(n|s)
pub_f <- sum((FR$count / sum(FR$count))^2)            # marginals, published only
pub_l <- sum((L$count  / sum(L$count))^2)
INFLATE  <- sum(outer(w, w) * Af * Al) / (pub_f * pub_l)
P_BOTH_R <- P_BOTH * INFLATE
stopifnot(INFLATE > 1, INFLATE < 2)

collision <- data.frame(
  quantity = c("two people share a first name",
               "two people share a last name",
               "two people share both, if the two are unrelated",
               "two people share both, allowing correlation through race"),
  probability = c(P_F, P_L, P_BOTH, P_BOTH_R),
  one_in = c(1 / P_F, 1 / P_L, 1 / P_BOTH, 1 / P_BOTH_R),
  stringsAsFactors = FALSE)
dd_write_csv(collision, "derived/collision.csv")
cat(sprintf("collision: both names 1 in %s (1 in %s allowing race correlation)\n",
            format(round(1 / P_BOTH), big.mark = ","),
            format(round(1 / P_BOTH_R), big.mark = ",")))

# --- 7. A name that is both a first name and a last name --------------------
#
# The reason this is here rather than as a curiosity: it is the failure mode of
# every join that assumes the two fields cannot be swapped.

both <- merge(FS[, c("name", "count")], L[, c("name", "count")],
              by = "name", suffixes = c("_first", "_last"))
BOTH_N       <- nrow(both)
BOTH_PEOPLE  <- sum(both$count_first)
BOTH_PCT_1ST <- 100 * BOTH_PEOPLE / TOT_F
BOTH_PCT_LST <- 100 * sum(both$count_last) / TOT_L
both$total   <- both$count_first + both$count_last
both$balance <- pmin(both$count_first, both$count_last) / both$total
bb <- both[both$total >= 200000, ]
bb <- bb[order(-bb$balance), ][1:12, ]
dd_write_csv(data.frame(name = bb$name, as_first_name = bb$count_first,
                        as_last_name = bb$count_last),
             "derived/both_ways.csv")
cat(sprintf("names used both ways: %s (%.1f%% of Americans hold one as a first name)\n",
            format(BOTH_N, big.mark = ","), BOTH_PCT_1ST))

# --- 8. The same name, counted twice, by the same agency --------------------
#
# The Bureau infused noise into the sex tabulation and the race tabulation
# INDEPENDENTLY, then derived each table's total from its own noisy cells. So
# the two files disagree about how many Americans are called MICHAEL. The
# Bureau documents this; almost nobody who downloads one file reads the brief
# for the other. It is measured here.

d2 <- merge(FS[, c("name", "rank", "count")], FR[, c("name", "count")],
            by = "name", suffixes = c("_sex", "_race"))
d2$diff <- d2$count_sex - d2$count_race
DIS_N    <- sum(d2$diff != 0)
DIS_PCT  <- 100 * mean(d2$diff != 0)
DIS_MAX  <- max(abs(d2$diff))
DIS_NAME <- d2$name[which.max(abs(d2$diff))]
RESID_DIFF <- RESID_F - fr$count[is_resid(fr)]

dd_write_csv(data.frame(
  gap = c("0", "1", "2", "3 to 5", "6 to 10", "more than 10"),
  names = as.integer(table(cut(abs(d2$diff), c(-1, 0, 1, 2, 5, 10, Inf))))),
  "derived/disagreement.csv")

top10 <- d2[order(d2$rank), ][1:10, ]
dd_write_csv(data.frame(rank = top10$rank, name = top10$name,
                        in_the_sex_table = top10$count_sex,
                        in_the_race_table = top10$count_race,
                        gap = top10$diff),
             "derived/disagreement_top.csv")

# Names published below the threshold the Bureau states, because the noise
# carried them under it.
BELOW_F <- sum(FS$count < 100); BELOW_L <- sum(L$count < 100)
MIN_F   <- min(FS$count);       MIN_L   <- min(L$count)

# --- 9. What the repair repaired --------------------------------------------
#
# The published tables contain no negative counts. The research copies do,
# and comparing them says how much of the table the repair touched -- and
# confirms that it moved people between cells rather than inventing or
# deleting them.

negsum <- function(pubd, negd, cols, label) {
  m <- merge(pubd[, c("name", cols)], negd[, c("name", cols)],
             by = "name", suffixes = c("_p", "_n"))
  N <- as.matrix(m[, paste0(cols, "_n")])
  tp <- rowSums(m[, paste0(cols, "_p")]); tn <- rowSums(N)
  data.frame(which = label, rows = nrow(m),
             rows_with_a_negative = sum(rowSums(N < 0) > 0),
             pct_of_rows = 100 * mean(rowSums(N < 0) > 0),
             negative_cells = sum(N < 0), cells = length(N),
             worst_cell = min(N),
             rows_whose_total_is_unchanged = sum(abs(tp - tn) < 0.5),
             stringsAsFactors = FALSE)
}
negatives <- rbind(negsum(FS, FSN, c("male", "female"), "first name by sex"),
                   negsum(FR, FRN, RACE,                "first name by race"))
dd_write_csv(negatives, "derived/negatives.csv")

mneg <- merge(FS[, c("name", "count", "male", "female")], FSN, by = "name",
              suffixes = c("", "_raw"))
mneg <- mneg[mneg$male_raw < 0 | mneg$female_raw < 0, ]
mneg <- mneg[order(-mneg$count), ][1:8, ]
dd_write_csv(data.frame(name = mneg$name, people = mneg$count,
                        published_male = mneg$male, published_female = mneg$female,
                        research_male = mneg$male_raw, research_female = mneg$female_raw),
             "derived/negatives_top.csv")

# --- 10. The explorer tables ------------------------------------------------
#
# The browser figure lets a reader look a name up, which needs the data in the
# page. The whole tables are 210,000 rows and will not go in one, so this is a
# DECLARED CUT: the 2,000 most common of each, which is 82% of Americans by
# first name and 57% by last name. Those two coverage figures are printed in
# the caption so nobody mistakes the box for a complete index.

EXPLORE_N <- 2000
ex_first <- FR[order(FR$rank), ][seq_len(EXPLORE_N), ]
ex_first <- merge(ex_first, FS[, c("name", "pct_female", "tot")], by = "name")
ex_first <- ex_first[order(ex_first$rank), ]
for (g in RACE) ex_first[[paste0("pct_", g)]] <- 100 * ex_first[[g]] / ex_first$count
ex_first$also_a_last_name <- ex_first$name %in% L$name
dd_write_csv(data.frame(
  rank = ex_first$rank, name = ex_first$name, people = ex_first$count,
  pct_female = ex_first$pct_female,
  ex_first[, paste0("pct_", RACE)],
  also_a_last_name = ex_first$also_a_last_name),
  "derived/explore_first.csv")

ex_last <- L[order(L$rank), ][seq_len(EXPLORE_N), ]
for (g in RACE) ex_last[[paste0("pct_", g)]] <- 100 * ex_last[[g]] / ex_last$count
ex_last$also_a_first_name <- ex_last$name %in% FS$name
dd_write_csv(data.frame(
  rank = ex_last$rank, name = ex_last$name, people = ex_last$count,
  ex_last[, paste0("pct_", RACE)],
  also_a_first_name = ex_last$also_a_first_name),
  "derived/explore_last.csv")

EX_COV_F <- 100 * sum(ex_first$count) / TOT_F
EX_COV_L <- 100 * sum(ex_last$count)  / TOT_L

# --- 11. The same names, counted at birth instead of counted alive ----------
#
# Everything above is one snapshot of the people alive in 2020. It cannot say
# when a name was given, and when a name was given is most of what there is to
# know about a name. So a second source is read here, and it is the only place
# in this chapter where anything but the census is used.
#
# Social Security Administration, "Beyond the Top 1000 Names", national data,
# from https://www.ssa.gov/oact/babynames/limits.html -- one plain text file
# per birth year, 1880 to 2025, three columns: name, sex, count.
#
# TWO FILES THAT COUNT DIFFERENT THINGS.
#
#   census      people alive on 1 April 2020, by the name they gave the census
#   SSA         applications for a Social Security card, by year of birth and
#               by the name written on the application
#
# One is a stock and one is a flow, and almost every confusing thing about
# putting them side by side comes from that. A stock carries everybody who was
# ever added and has not yet died, so it responds to a change in naming over a
# lifetime. A flow responds within a year.
#
# THE FETCH NEEDS A BROWSER USER-AGENT. ssa.gov sits behind a filter that
# answers 403 to download.file()'s default request and to a bare curl. It wants
# a browser agent string AND a compressed-encoding header; either alone still
# fails. Hence method = "curl" with both, rather than the plain prov_fetch()
# call every other source in this chapter uses. The zip is 7.9 MB and is kept
# in raw/ so that a rebuild does not have to win that argument twice.

SSA_URL <- "https://www.ssa.gov/oact/babynames/names.zip"
SSA_UA  <- paste("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
                 "AppleWebKit/537.36 (KHTML, like Gecko)",
                 "Chrome/126.0 Safari/537.36")
ssa_zip <- "raw/ssa-names.zip"
if (!file.exists(ssa_zip)) {
  download.file(SSA_URL, ssa_zip, mode = "wb", quiet = TRUE, method = "curl",
                extra = c("--compressed", "-A", shQuote(SSA_UA)))
  .prov_record(SSA_URL, ssa_zip, "SSA national baby names")
}

ssa_dir <- file.path(tempdir(), "ssa")
unzip(ssa_zip, exdir = ssa_dir)
yob <- list.files(ssa_dir, "^yob[0-9]{4}\\.txt$", full.names = TRUE)
stopifnot(length(yob) > 100)
ssa <- do.call(rbind, lapply(yob, function(f) {
  d <- read.csv(f, header = FALSE, col.names = c("name", "sex", "n"),
                stringsAsFactors = FALSE)
  d$year <- as.integer(sub(".*yob([0-9]{4})\\.txt$", "\\1", f))
  d
}))
# Upper case to meet the census tables, which publish names in capitals only.
# Aggregating after the fold matters: SSA treats a name and its differently
# cased twin as two rows, and the census would have counted them as one.
ssa$NAME <- toupper(ssa$name)
ssa <- aggregate(n ~ NAME + sex + year, ssa, sum)

SSA_MIN <- min(ssa$year); SSA_MAX <- max(ssa$year)
births <- reshape(aggregate(n ~ year + sex, ssa, sum), direction = "wide",
                  idvar = "year", timevar = "sex")
names(births) <- c("year", "female", "male")
births$total <- births$female + births$male
dd_write_csv(births, "derived/ssa_births.csv")

# THE FLOW IS NOT A BIRTH SERIES AND MUST NOT BE CALLED ONE. It is a count of
# card applications, filed at any age, tabulated by the year of birth the
# applicant reported. Numbers were first issued in late 1936, so a person born
# in 1920 is in this file only if they lived long enough to apply for one --
# which is why the early years are short of the country's actual births. The
# gap is recorded here rather than argued about in prose.
SSA_CARD_YEAR <- 1936
# Live births registered in the United States, from the National Center for
# Health Statistics' historical series. Four years only, as a yardstick for the
# early coverage; nothing else in the chapter depends on them.
NCHS <- data.frame(year = c(1920, 1930, 1940, 1950),
                   registered = c(2950000, 2618000, 2559000, 3632000))
NCHS$in_ssa <- births$total[match(NCHS$year, births$year)]
NCHS$pct    <- 100 * NCHS$in_ssa / NCHS$registered
dd_write_csv(NCHS, "derived/ssa_coverage.csv")

rate <- function(nm, sx) {
  d <- ssa[ssa$NAME == nm & ssa$sex == sx, c("year", "n")]
  d <- merge(data.frame(year = SSA_MIN:SSA_MAX), d, by = "year", all.x = TRUE)
  d$n[is.na(d$n)] <- 0
  d$per100k <- 100000 * d$n / births$total[match(d$year, births$year)]
  d
}

# --- 11a. The name the chapter uses to make the point ----------------------
#
# ALEXA. Amazon announced the Echo in November 2014 and put it on general sale
# in June 2015, and the name is the cleanest natural experiment in the file:
# nothing else about it changed, and the whole country learned a new use for it
# in about eighteen months.

ECHO_YEAR <- 2015
SHOCKS <- data.frame(
  name  = c("ALEXA", "SIRI", "ISIS", "KATRINA"),
  event = c(2015, 2011, 2014, 2005),
  what  = c("Amazon put the Echo on general sale",
            "Apple shipped Siri with the iPhone 4S",
            "the group took Mosul",
            "the hurricane made landfall"),
  stringsAsFactors = FALSE)

shock <- do.call(rbind, lapply(seq_len(nrow(SHOCKS)), function(i) {
  d <- rate(SHOCKS$name[i], "F")
  d <- d[d$year >= 1990, ]
  data.frame(name = SHOCKS$name[i], event = SHOCKS$event[i], d,
             stringsAsFactors = FALSE)
}))
dd_write_csv(shock, "derived/ssa_shock.csv")

# Rank among girls' names, which is the figure people quote and the one that
# makes the Alexa story turn out backwards from the way it is usually told.
rank_of <- function(nm, y) {
  g <- ssa[ssa$sex == "F" & ssa$year == y, ]
  g <- g[order(-g$n), ]
  w <- which(g$NAME == nm)
  if (length(w)) w[1] else NA_integer_
}
AL <- rate("ALEXA", "F")
AL$rank <- vapply(AL$year, function(y) rank_of("ALEXA", y), integer(1))
ALIVE <- AL[AL$n > 0, ]
BEST_RANK  <- min(ALIVE$rank, na.rm = TRUE)
BEST_YEAR  <- ALIVE$year[which.min(ALIVE$rank)]
PEAK_N     <- max(AL$n); PEAK_YEAR <- AL$year[which.max(AL$n)]
LAST_N     <- AL$n[AL$year == SSA_MAX]
LAST_RANK  <- AL$rank[AL$year == SSA_MAX]
DROP_PCT   <- 100 * (1 - LAST_N / AL$n[AL$year == BEST_YEAR])
# The 2015 spike is the part that gets left out of the retelling, so it is
# asserted rather than described: the launch year is the name's best year.
stopifnot(BEST_YEAR == ECHO_YEAR, LAST_RANK > BEST_RANK)

# --- 11b. What the stock would have shown -----------------------------------
#
# The arithmetic the section turns on. Everyone named ALEXA who was born before
# the collapse was still alive in 2020 and still counted, and the babies born
# during it were added on top. So the census pair would have shown the name
# GROWING across exactly the decade its flow fell apart.
#
# COUNTED ON BOTH SEXES, unlike the rank above. The census row for ALEXA holds
# 314 men as well as 115,621 women, so the flow it has to be compared against
# is every baby given the name, not only the girls. The difference is 284
# people and it would not change a sentence -- but a number in this chapter
# that is nearly the same as the one beside it, for a reason nobody stated,
# is the exact defect the chapter spends its last section on.
AL$both <- AL$n + rate("ALEXA", "M")$n
AL$cum <- cumsum(AL$both)
BORN_BY_2010 <- AL$cum[AL$year == 2010]
BORN_BY_2020 <- AL$cum[AL$year == 2020]
BORN_2011_20 <- BORN_BY_2020 - BORN_BY_2010
CENSUS_2020  <- FS$count[FS$name == "ALEXA"]
# Deaths are ignored on purpose and the brief says so. ALEXA is a young name --
# essentially nobody carrying it was born before 1980 -- so cumulative births
# and living people are close enough to compare. For a name given mostly in the
# 1920s they are nothing like each other, which is why DOROTHY is carried into
# the table below as the counter-example rather than left out of it.
SURVIVE_PCT <- 100 * CENSUS_2020 / BORN_BY_2020
STOCK_GROWTH <- 100 * (BORN_BY_2020 / BORN_BY_2010 - 1)
dd_write_csv(data.frame(year = AL$year, girls = AL$n, per100k = AL$per100k,
                        rank = AL$rank, births = AL$both, cumulative_births = AL$cum),
             "derived/ssa_alexa.csv")

vs <- data.frame(name = c("ALEXA", "EMMA", "TAYLOR", "JENNIFER", "DOROTHY"),
                 stringsAsFactors = FALSE)
vs$census_2020 <- FS$count[match(vs$name, FS$name)]
vs$born_through_2020 <- vapply(vs$name, function(nm) {
  d <- rate(nm, "F"); e <- rate(nm, "M")
  sum(d$n[d$year <= 2020]) + sum(e$n[e$year <= 2020])
}, numeric(1))
vs$median_birth_year <- vapply(vs$name, function(nm) {
  d <- rate(nm, "F"); e <- rate(nm, "M")
  tot <- d$n + e$n
  d$year[which(cumsum(tot) >= sum(tot) / 2)[1]]
}, numeric(1))
vs$pct_still_counted <- 100 * vs$census_2020 / vs$born_through_2020
dd_write_csv(vs, "derived/ssa_vs_census.csv")

# --- 11bb. Which way the traffic runs ---------------------------------------
#
# The section so far only shows names leaving. This is the other direction, and
# it turned out to be the more interesting half.
#
# TWO WINDOWS, both three years wide, a decade apart. Three years rather than
# one because a single year of a single name is a noisy thing, and equal widths
# so the two are comparable without any weighting.
NOW <- c(2023, 2025)
THEN <- c(2013, 2015)
stopifnot(NOW[2] == SSA_MAX, NOW[2] - THEN[2] == 10)

byname <- aggregate(n ~ NAME + year, ssa, sum)
byname <- merge(byname, data.frame(year = births$year, births = births$total))
byname$rate <- 1e5 * byname$n / byname$births
mean_in <- function(w) {
  s <- byname[byname$year >= w[1] & byname$year <= w[2], ]
  r <- aggregate(cbind(n, rate) ~ NAME, s, mean)
  setNames(r, c("NAME", "per_year", "rate"))
}
mv <- merge(mean_in(NOW), mean_in(THEN), by = "NAME", all.x = TRUE,
            suffixes = c("_now", "_then"))
mv$per_year_then[is.na(mv$per_year_then)] <- 0
mv$rate_then[is.na(mv$rate_then)] <- 0
mv$rise <- mv$rate_now - mv$rate_then
mv <- mv[order(-mv$rise), ]

# RANKED ON THE RATE, NOT THE COUNT, for the same reason the browser figure
# plots a rate: births per year are not constant, and a name can gain babies
# while losing ground. Ranked on the DIFFERENCE rather than the ratio, because
# a ratio out of near-zero is unbounded and would fill the table with names
# that went from four babies to forty.
RISE_N <- 10
ris <- head(mv, RISE_N)
# When each was last a common name. No special case for the ones that never
# were: the number is simply small, and a reader can see that for themselves.
pre <- byname[byname$year < 1980, ]
ris$old_peak_year <- vapply(ris$NAME, function(nm) {
  d <- pre[pre$NAME == nm, ]
  if (!nrow(d)) return(NA_real_)
  d$year[which.max(d$n)]
}, numeric(1))
ris$old_peak_n <- vapply(ris$NAME, function(nm) {
  d <- pre[pre$NAME == nm, ]
  if (!nrow(d)) return(0)
  max(d$n)
}, numeric(1))
ris$in_census <- ris$NAME %in% FS$name
dd_write_csv(data.frame(name = ris$NAME,
                        given_then = round(ris$per_year_then),
                        given_now  = round(ris$per_year_now),
                        rise_per100k = ris$rise,
                        old_peak_year = ris$old_peak_year,
                        old_peak_n = ris$old_peak_n,
                        in_census = ris$in_census),
             "derived/ssa_risers.csv")

# THE POINT OF THE WHOLE SUBSECTION. A name has to clear about a hundred
# LIVING BEARERS in both 2010 and 2020 to be published, so a name that did not
# exist in 2010 cannot be in the file however many babies now carry it.
#
# The cut is five hundred births over the three recent years, which is about
# five times the number of living people the Bureau itself needs before it will
# print a name. Nothing marginal gets in at that level, and the names between
# five hundred and a thousand are the ones a reader is most likely to recognise
# and go looking for.
NEW_MIN <- 500
recent_births <- aggregate(n ~ NAME, ssa[ssa$year >= NOW[1], ], sum)
by2010 <- aggregate(n ~ NAME, ssa[ssa$year <= 2010, ], sum)
nw <- recent_births[recent_births$n >= NEW_MIN & !recent_births$NAME %in% FS$name, ]
nw$born_by_2010 <- by2010$n[match(nw$NAME, by2010$NAME)]
nw$born_by_2010[is.na(nw$born_by_2010)] <- 0
nw <- nw[order(-nw$n), ]
NEW_N <- nrow(nw)
NEW_PEOPLE <- sum(nw$n)
# The year each first reached SSA's own floor of five babies. Several of these
# are recognisable from film and television, and rather than assert that in the
# prose the table gives the date and lets a reader match it to whatever they
# remember. The section has already shown why a date lining up is not proof.
nw$first_year <- vapply(nw$NAME, function(x) min(ssa$year[ssa$NAME == x]), numeric(1))
dd_write_csv(data.frame(name = head(nw$NAME, 10),
                        first_year = head(nw$first_year, 10),
                        born_by_2010 = head(nw$born_by_2010, 10),
                        births_recent = head(nw$n, 10)),
             "derived/ssa_newcomers.csv")
stopifnot(NEW_N > 0, all(!head(nw$NAME, 10) %in% FS$name))

cat(sprintf("  risers: %s (+%.0f per 100k); %d names over %s recent births are absent from the census\n",
            ris$NAME[1], ris$rise[1], NEW_N, format(NEW_MIN, big.mark = ",")))

# --- 11c. The explorer table ------------------------------------------------
#
# A SECOND DECLARED CUT, and a different one from the lookup above. The birth
# series carries a number for every name in every year, so the whole file will
# not travel inside one page either. What is carried: the 1,000 most common
# first names in the 2020 census, from 1920 on. Census rank rather than SSA
# rank, because every name in the box then has both a stock and a flow, which
# is the comparison the section is about.
BIRTH_N  <- 1000
# THE WHOLE SERIES, 1880 ON, not a window cut to save bytes. Two reasons the
# earlier draft's 1920 start was wrong. The card beside the figure counts every
# birth since 1880, so a chart starting in 1920 disagreed with the number
# printed under it. And a name like MILDRED or CLARENCE has its entire story
# before 1920: cutting there does not shorten those curves, it deletes them.
# The years the file is thin are shaded in the figure instead, which is the
# honest way to carry a source that gets better over time.
BIRTH_Y0 <- min(ssa$year)
bx <- FS$name[order(FS$rank)][seq_len(BIRTH_N)]
# PLUS THE RISERS, whatever they turn out to be. Four of the ten sit between
# census rank 1,000 and 2,400 -- common enough to be in the census file, too
# young to be in the top thousand of a count of living people, which is the
# subsection's whole point. Offering them as things to look up and then
# answering "not carried here" would be the figure contradicting the text
# above it. Union rather than a bigger N: raising the cut to 2,400 to catch
# four names would triple the table.
bx <- union(bx, ris$NAME)
# AND THE NEWCOMERS, which is the one group here with no census row at all.
# They cost nothing in coverage, because the census cannot see them: that is
# the point of carrying them. A reader who looks one up gets a birth curve and
# a card that says the census file has no line for this name, which is the
# subsection above turned into something you can do rather than read.
bx <- union(bx, nw$NAME)
BIRTH_NAMES <- length(bx)
BIRTH_NEW   <- sum(!bx %in% FS$name)
BIRTH_COV <- 100 * sum(FS$count[match(bx, FS$name)], na.rm = TRUE) / TOT_F

# Names in the census file that Social Security has never once recorded. Over
# the whole 53,615 this would mostly be rare spellings sitting under SSA's own
# five-a-year threshold; over the 2,000 most common it is a finding.
NEVER <- setdiff(ex_first$name, unique(ssa$NAME))
NEVER_PEOPLE <- sum(ex_first$count[ex_first$name %in% NEVER])

bx <- intersect(bx, unique(ssa$NAME))

# The card beside that figure needs four numbers per name, and three of them
# cannot be read off the chart. Ever born is counted over the WHOLE file, 1880
# on, while the chart starts at 1920 -- for a name given mostly in the 1910s the
# two are far apart, and the card would otherwise understate its own subject.
tot_by <- aggregate(n ~ NAME, ssa[ssa$NAME %in% bx, ], sum)
by2020 <- aggregate(n ~ NAME, ssa[ssa$NAME %in% bx & ssa$year <= 2020, ], sum)
bsub <- ssa[ssa$NAME %in% bx, ]
med <- vapply(split(bsub, bsub$NAME), function(d) {
  d <- aggregate(n ~ year, d, sum)
  d <- d[order(d$year), ]
  d$year[cumsum(d$n) >= sum(d$n) / 2][1]
}, numeric(1))
med <- data.frame(NAME = names(med), median_birth_year = unname(med),
                  stringsAsFactors = FALSE)
totals <- data.frame(name = bx, stringsAsFactors = FALSE)
totals$born_all         <- tot_by$n[match(totals$name, tot_by$NAME)]
totals$born_through_2020 <- by2020$n[match(totals$name, by2020$NAME)]
totals$median_birth_year <- med$median_birth_year[match(totals$name, med$NAME)]
totals$born_through_2020[is.na(totals$born_through_2020)] <- 0
# THESE THREE ARE ALLOWED TO BE EMPTY, and only for the newcomers. NA here
# means "the census file has no row for this name", which the figure prints as
# a sentence rather than as a blank cell. Every column the birth file supplies
# is still required to be there, so a genuine gap in the SSA side stops the
# build instead of arriving in the page as a missing census row.
totals$census_2020       <- FS$count[match(totals$name, FS$name)]
totals$census_rank       <- FS$rank[match(totals$name, FS$name)]
totals$pct_still_counted <- ifelse(totals$born_through_2020 > 0,
                                   100 * totals$census_2020 / totals$born_through_2020,
                                   NA_real_)
ssa_cols <- c("name", "born_all", "median_birth_year")
stopifnot(!any(is.na(totals[, ssa_cols])), all(totals$born_all > 0))
stopifnot(is.na(totals$census_2020) == !totals$name %in% FS$name)
stopifnot(sum(is.na(totals$census_2020)) == BIRTH_NEW)
dd_write_csv(totals, "derived/ssa_totals.csv")

be <- ssa[ssa$NAME %in% bx & ssa$year >= BIRTH_Y0, ]
be <- reshape(be, direction = "wide", idvar = c("NAME", "year"), timevar = "sex")
names(be) <- c("name", "year", "female", "male")
be$female[is.na(be$female)] <- 0; be$male[is.na(be$male)] <- 0
be <- be[order(be$name, be$year), ]
stopifnot(all(be$female + be$male > 0))     # no all-zero rows travel to the page
dd_write_csv(be, "derived/ssa_explore.csv")

cat(sprintf("SSA %d-%d: ALEXA best rank %d in %d, %d in %d (%.0f%% down)\n",
            SSA_MIN, SSA_MAX, BEST_RANK, BEST_YEAR, LAST_RANK, SSA_MAX, DROP_PCT))
cat(sprintf("  born through 2010 %s, through 2020 %s (+%.0f%%); census counts %s\n",
            format(BORN_BY_2010, big.mark = ","), format(BORN_BY_2020, big.mark = ","),
            STOCK_GROWTH, format(CENSUS_2020, big.mark = ",")))
cat(sprintf("  explorer: %d names, %.1f%% of Americans, %s rows\n",
            length(bx), BIRTH_COV, format(nrow(be), big.mark = ",")))

# --- 12. Facts and checks ---------------------------------------------------

fact <- function(...) {
  a <- list(...)
  data.frame(name = a[[1]], value = as.character(dd_num(a[[2]])), note = a[[3]],
             stringsAsFactors = FALSE)
}
facts <- do.call(rbind, list(
  fact("us_2020", US2020, "2020 Census resident population, the denominator for the coverage shares"),
  fact("first_names", nrow(FS), "first names published, each held by about 100 people or more"),
  fact("last_names", nrow(L), "last names published, same threshold"),
  fact("first_people", TOT_F, "people a first name was recorded for, residual included"),
  fact("last_people", TOT_L, "people a last name was recorded for"),
  fact("first_coverage_pct", 100 * TOT_F / US2020, "share of the enumerated population with a usable first name"),
  fact("last_coverage_pct", 100 * TOT_L / US2020, "share with a usable last name"),
  fact("no_first_name", US2020 - TOT_F, "people enumerated in 2020 with no first name in this file"),
  fact("residual_first", RESID_F, "people swept into ALL OTHER NAMES, first-name table"),
  fact("residual_last", RESID_L, "same, last-name table"),
  fact("effective_first_low", CF$effective_low, "2^H: fewest equally common first names that would be this surprising"),
  fact("effective_first_high", CF$effective_high, "most, the two differing only over the tail the file does not itemise"),
  fact("effective_last_low", CL$effective_low, "same bracket, last names"),
  fact("effective_last_high", CL$effective_high, "same"),
  fact("bound_pct_first", CF$residual_bound_pct, "how much of the first-name collision probability the unlisted tail can account for"),
  fact("bound_pct_last", CL$residual_bound_pct, "same, last names"),
  fact("half_first", HALF_F, "most common first names needed to cover half the country"),
  fact("half_last", HALF_L, "same, last names"),
  fact("maj_wrong", MAJ_WRONG, "people whose sex the majority rule on their first name gets wrong"),
  fact("maj_right_pct", 100 * MAJ_RIGHT / sum(FS$tot), "share the majority rule gets right"),
  fact("sex_scatter_cut", SC_CUT, "smallest name shown in the sex figure"),
  fact("p_first", P_F, "probability two people share a first name"),
  fact("p_last", P_L, "probability two people share a last name"),
  fact("p_both", P_BOTH, "the two multiplied, which assumes they are unrelated"),
  fact("one_in_both", 1 / P_BOTH, "the same number as odds"),
  fact("inflate_race", INFLATE, "how much correlation through six race categories raises it"),
  fact("both_names", BOTH_N, "names that appear in both the first-name and last-name tables"),
  fact("both_pct_first", BOTH_PCT_1ST, "share of Americans whose first name is also somebody's surname"),
  fact("both_pct_last", BOTH_PCT_LST, "share whose surname is also somebody's first name"),
  fact("disagree_n", DIS_N, "first names given different totals by the two tabulations"),
  fact("disagree_pct", DIS_PCT, "share of them"),
  fact("disagree_max", DIS_MAX, "largest gap between the two"),
  fact("disagree_name", DIS_NAME, "the name carrying it"),
  fact("resid_diff", RESID_DIFF, "gap between the two files on the residual line"),
  fact("below_100_first", BELOW_F, "first names published with a total under the stated threshold"),
  fact("below_100_last", BELOW_L, "same, last names"),
  fact("min_first", MIN_F, "smallest published first-name count"),
  fact("min_last", MIN_L, "smallest published last-name count"),
  fact("explore_n", EXPLORE_N, "names of each kind carried into the browser figure"),
  fact("explore_cov_first", EX_COV_F, "share of Americans whose first name is among them"),
  fact("explore_cov_last", EX_COV_L, "same, last names"),
  fact("ssa_min", SSA_MIN, "first birth year in the Social Security file"),
  fact("ssa_max", SSA_MAX, "last birth year in it"),
  fact("ssa_card_year", SSA_CARD_YEAR, "the year Social Security numbers were first issued"),
  fact("ssa_cov_1920", NCHS$pct[NCHS$year == 1920], "share of 1920's registered births that reached the file"),
  fact("ssa_cov_1950", NCHS$pct[NCHS$year == 1950], "same, 1950"),
  fact("echo_year", ECHO_YEAR, "the year Amazon put the Echo on general sale"),
  fact("alexa_best_rank", BEST_RANK, "ALEXA's best rank among girls' names, in any year of the file"),
  fact("alexa_best_year", BEST_YEAR, "the year it held that rank"),
  fact("alexa_best_n", AL$n[AL$year == BEST_YEAR], "girls given the name that year"),
  fact("alexa_peak_n", PEAK_N, "the largest number of girls given it in any year"),
  fact("alexa_peak_year", PEAK_YEAR, "the year of that largest number"),
  fact("alexa_last_n", LAST_N, "girls given it in the last year of the file"),
  fact("alexa_last_rank", LAST_RANK, "its rank that year"),
  fact("alexa_drop_pct", DROP_PCT, "fall from the launch year to the last year"),
  fact("alexa_born_2010", BORN_BY_2010, "people named ALEXA ever born through 2010, both sexes"),
  fact("alexa_born_2020", BORN_BY_2020, "same, through 2020"),
  fact("alexa_born_decade", BORN_2011_20, "born in the decade between the two censuses"),
  fact("alexa_census_2020", CENSUS_2020, "people the 2020 census counts with the name"),
  fact("alexa_stock_growth", STOCK_GROWTH, "how much the stock grew across the decade the flow collapsed"),
  fact("alexa_survive_pct", SURVIVE_PCT, "census count as a share of everyone ever born with the name"),
  fact("birth_n", BIRTH_N, "the cut the birth-year figure starts from: this many census names"),
  fact("birth_names", BIRTH_NAMES, "names it actually carries, risers and newcomers added to that cut"),
  fact("birth_new", BIRTH_NEW, "of those, the ones with no row in the census file"),
  fact("birth_y0", BIRTH_Y0, "first year that figure shows"),
  fact("birth_cov", BIRTH_COV, "share of Americans whose first name is among them"),
  fact("never_n", length(NEVER), "names in the census lookup Social Security has never recorded"),
  fact("never_name", paste(NEVER, collapse = " "), "which ones"),
  fact("never_people", NEVER_PEOPLE, "people the census gives them"),
  fact("rise_now0", NOW[1], "first year of the recent window the risers are measured over"),
  fact("rise_now1", NOW[2], "last year of it"),
  fact("rise_then0", THEN[1], "first year of the window a decade earlier"),
  fact("rise_then1", THEN[2], "last year of it"),
  fact("rise_n", RISE_N, "risers shown"),
  fact("rise_top", ris$NAME[1], "the fastest-rising first name of the decade"),
  fact("rise_top_old_year", ris$old_peak_year[1], "the year it was last at its most common"),
  fact("rise_in_census", sum(ris$in_census), "how many of the risers the 2020 census file carries"),
  fact("new_min", NEW_MIN, "births over the recent window a newcomer needs to be counted here"),
  fact("new_n", NEW_N, "names that clear it and are absent from the census file"),
  fact("new_people", NEW_PEOPLE, "babies given one of those names in the recent window"),
  fact("new_top", nw$NAME[1], "the most given of them")))
dd_write_csv(facts, "derived/facts.csv")

# Checks. Each is a quantity that could come out otherwise if a source moved,
# and each is asserted before the tables above are trusted.
nn <- function(x) format(x, big.mark = ",")
p2 <- function(x) formatC(x, format = "f", digits = 2)

stopifnot(nrow(FS) == nrow(FR))                      # the two first-name files line up
stopifnot(abs(sum(FS$male) + sum(FS$female) - sum(FS$tot)) < 0.5)
stopifnot(all(abs(rowSums(as.matrix(FR[, RACE])) - FR$count) < 0.5))
stopifnot(all(abs(rowSums(as.matrix(L[, RACE]))  - L$count)  < 0.5))
stopifnot(TOT_F < US2020, TOT_L < US2020)            # nobody is counted twice
stopifnot(negatives$rows_whose_total_is_unchanged == negatives$rows)

# The birth file. Each of these could come out otherwise if SSA reissued the
# series, and the third is the one the whole flow-and-stock section rests on.
stopifnot(SSA_MAX >= 2024)                           # the series is still current
stopifnot(all(births$total > 0), nrow(births) == SSA_MAX - SSA_MIN + 1)
stopifnot(BORN_BY_2020 > BORN_BY_2010)               # the stock rose; the flow fell
stopifnot(LAST_N < AL$n[AL$year == BEST_YEAR] / 10)
stopifnot(CENSUS_2020 < BORN_BY_2020)                # fewer alive than ever born

checks <- data.frame(check = c(
  "first names published",
  "last names published",
  "people with a first name in the file",
  "people with a last name in the file",
  "race columns sum to the published total, every last name",
  "sex columns sum to the published total, every first name",
  "first names given two different totals by the two tabulations",
  "rows whose total the negative repair left unchanged",
  "names that are both a first name and a last name",
  "chance two people share both a first and a last name",
  "birth years in the Social Security file",
  "girls named ALEXA, launch year against the last year",
  "ALEXA between the two censuses: born, against counted alive",
  "names given to 1,000+ babies since 2023 that the census file has no row for"),
  value = c(nn(nrow(FS)), nn(nrow(L)), nn(TOT_F), nn(TOT_L),
            paste(nn(nrow(L)), "of", nn(nrow(L))),
            paste(nn(nrow(FS)), "of", nn(nrow(FS))),
            paste0(nn(DIS_N), " of ", nn(nrow(d2)), " (", p2(DIS_PCT), "%)"),
            paste(nn(sum(negatives$rows_whose_total_is_unchanged)), "of",
                  nn(sum(negatives$rows))),
            nn(BOTH_N),
            paste("1 in", nn(round(1 / P_BOTH))),
            paste0(SSA_MIN, "-", SSA_MAX, " (", nn(nrow(births)), " years)"),
            paste0(nn(AL$n[AL$year == BEST_YEAR]), " in ", BEST_YEAR,
                   ", ", nn(LAST_N), " in ", SSA_MAX),
            paste0(nn(BORN_2011_20), " born, stock up ", p2(STOCK_GROWTH), "%"),
            paste0(nn(NEW_N), " names, ", nn(NEW_PEOPLE), " babies")),
  stringsAsFactors = FALSE)
dd_write_csv(checks, "derived/checks.csv")

# --- 13. The exhibit the chapter opens with ---------------------------------
source("make-raw-exhibit.R")
mk_raw_exhibit("raw/Names2020_FirstNames_Sex.xlsx", "raw/first-names-as-it-arrives.txt")

prov_report()
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
