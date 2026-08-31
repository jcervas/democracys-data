# ---------------------------------------------------------------------------
# Nationalization: one claim, three records.
#
# The claim is the textbook's own. Congressional elections have been
# nationalized -- voters decide them on national grounds, so a state or a
# district that backs one party for president now sends that party's candidate
# to Congress as well. It is a claim about a trend, so it needs a series, and a
# series needs a record that reaches back.
#
# THREE RECORDS ANSWER IT, AND THEY ARE NOT THE SAME KIND OF THING:
#
#   1. A ROSTER. Who sat in the Senate for each state, 1879-2025. A state whose
#      two senators belong to different parties is a state that has been
#      answering the same question two different ways. This is a STOCK: it
#      describes a moment, and six years of elections produced that moment.
#
#   2. RETURNS FOR TWO OFFICES ON ONE PAGE. The Clerk of the House prints the
#      presidential vote and the Senate vote for a state in one document, so
#      the subtraction between them cannot pick up a difference between two
#      files. This is a FLOW: one election day at a time. It only reaches back
#      to 2004 here, because that is as far as the machine-readable volumes go.
#
#   3. RETURNS FOR TWO OFFICES, ONE OF WHICH NOBODY OFFICIAL PUBLISHES. The
#      House version of the same measure, 1952-2024, which needs the
#      presidential vote recomputed inside each congressional district. No
#      government produces that number. The house-competition chapter fetched
#      it, and this chapter reads its file.
#
# ---------------------------------------------------------------------------
# WHAT IS WRITTEN
# ---------------------------------------------------------------------------
#
#   derived/delegations.csv     one row per Congress: how many states had two
#                               senators of different parties
#   derived/delegation_states.csv  which states, for the last Congress
#   derived/senate_contests.csv one row per Senate contest 2004-2024: the
#                               state's presidential vote and its Senate vote,
#                               off the same page
#   derived/senate_years.csv    those contests summarised by election
#   derived/house_years.csv     the House measure by presidential election,
#                               1952-2024, with its two ingredients
#   derived/swap.csv            the counterfactual: each year's districts given
#                               another year's candidate gaps
#   derived/facts.csv           single numbers the brief quotes
#   derived/checks.csv          what this script verified before writing
#
# Run this script from inside the data/ folder.
# ---------------------------------------------------------------------------

dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)
options(scipen = 999, stringsAsFactors = FALSE)
source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits

if (file.exists("../../../_lib/provenance.R")) {
  source("../../../_lib/provenance.R")
} else {
  prov_fetch  <- function(url, dest, ...) { download.file(url, dest, mode = "wb", quiet = TRUE); dest }
  prov_report <- function() invisible(FALSE)
}

FETCH_DATE <- "2026-08-29"
say <- function(...) cat(..., "\n", sep = "")
chk <- data.frame(check = character(), value = character())
note <- function(k, v) chk[nrow(chk) + 1, ] <<- c(k, as.character(v))

# --- SOURCES ---------------------------------------------------------------
#
#   https://voteview.com/static/data/out/members/HSall_members.csv
#       Voteview's congressional membership file. One row per member per
#       Congress, 1789 to now, with the state and the party code. Keyless, one
#       CSV, about 6 MB. The retirements chapter reads the same file for a
#       different question; this chapter fetches its own copy because it needs
#       the members that chapter's build drops -- see below.
#
#   ../../house-competition/data/derived/clerk_2004.txt ... clerk_2024.txt
#       The Clerk of the House's Statistics of the Presidential and
#       Congressional Election, six presidential-year volumes, converted from
#       PDF to text by that chapter. The clerk-source chapter is about what
#       makes these documents hard to read.
#
#   ../../house-competition/data/derived/races.csv
#       One row per district per House election, 1946-2024, carrying the
#       district's House vote and its presidential vote. Built there from Gary
#       Jacobson's file, the Clerk's volumes and The Downballot.
#
#   ../../crossover/data/derived/senate.csv
#       The 2024 Senate contests, parsed out of the same Clerk document by a
#       script written independently of this one. Used only as a check.
#
# WHY A SECOND COPY OF THE VOTEVIEW FILE. The dw-nominate chapter keeps the
# major parties only, because a DW-NOMINATE comparison between Democrats and
# Republicans is what that chapter is about. This chapter cannot: a state
# represented by an independent is exactly the case the delegation measure has
# to decide about, and dropping those members would answer the question by
# deleting it.

U_VV  <- "https://voteview.com/static/data/out/members/HSall_members.csv"
HC    <- "../../house-competition/data/derived"
RACES <- file.path(HC, "races.csv")
XOVER <- "../../crossover/data/derived/senate.csv"
PRES_YEARS <- c(2004, 2008, 2012, 2016, 2020, 2024)

stopifnot(file.exists(RACES))
for (y in PRES_YEARS) stopifnot(file.exists(file.path(HC, sprintf("clerk_%d.txt", y))))

# ===========================================================================
# 1. THE ROSTER: SPLIT SENATE DELEGATIONS, 1879-2025
# ===========================================================================
#
# One row of the Voteview file is one member in one Congress. A state has two
# Senate seats, so in most Congresses a state contributes two rows. Where a
# senator died, resigned or was appointed mid-term the state contributes three
# or more, and those rows describe people who were never in the chamber at the
# same time.
#
# That is the flaw in reading a roster as a photograph, and it is measured
# rather than argued about here: the series is computed twice, once over every
# state and once over only the states that sent exactly two senators to that
# Congress. If the two lines agree, the mid-term arrivals are not driving the
# answer.
#
# PARTY IS READ AS THE LINE THE SENATOR RAN ON. Voteview codes 100 Democrat,
# 200 Republican, and everything else something else. An independent is neither,
# and this chapter does not quietly file one with the side they sit with in
# Washington. The crossover chapter shows what that decision is worth.

invisible(prov_fetch(U_VV, "raw/HSall_members.csv", label = "Voteview members"))
vv <- read.csv("raw/HSall_members.csv", colClasses = "character")
VV_ROWS <- nrow(vv)
for (v in c("congress", "party_code")) vv[[v]] <- as.integer(vv[[v]])

# Congress n convenes in 1787 + 2n: the 119th in 2025.
vv$year <- 1787L + 2L * vv$congress

sen <- vv[vv$chamber == "Senate" &
          vv$congress >= 46 &                       # 1879; see the brief
          vv$state_abbrev %in% state.abb, ]
SEN_ROWS <- nrow(sen)

key <- paste(sen$congress, sen$state_abbrev)
agg <- data.frame(
  congress  = as.integer(sub(" .*", "", names(tapply(sen$party_code, key, length)))),
  state     = sub("^[0-9]+ ", "", names(tapply(sen$party_code, key, length))),
  members   = as.vector(tapply(sen$party_code, key, length)),
  parties   = as.vector(tapply(sen$party_code, key, function(x) length(unique(x)))),
  nonmajor  = as.vector(tapply(sen$party_code, key, function(x) any(!x %in% c(100, 200)))))
agg$year  <- 1787L + 2L * agg$congress
agg$split <- agg$parties > 1
agg$two   <- agg$members == 2

del <- do.call(rbind, lapply(split(agg, agg$congress), function(x) data.frame(
  congress     = x$congress[1],
  year         = x$year[1],
  states       = nrow(x),
  split_states = sum(x$split),
  pct_split    = round(100 * mean(x$split), 2),
  states_two   = sum(x$two),
  pct_split_two = round(100 * mean(x$split[x$two]), 2),
  nonmajor_states = sum(x$nonmajor))))
del <- del[order(del$year), ]

# The two readings of the same series, side by side, as one number.
del$reading_gap <- round(abs(del$pct_split - del$pct_split_two), 2)
DEL_MAXGAP <- max(del$reading_gap)
DEL_MAXYR  <- del$year[which.max(del$reading_gap)]
DEL_MEDGAP <- median(del$reading_gap)
DEL_GAP59  <- max(del$reading_gap[del$year >= 1959])
DEL_R      <- cor(del$pct_split, del$pct_split_two)

DEL_PEAK <- del[which.max(del$pct_split), ]
DEL_LAST <- del[nrow(del), ]
DEL_FIRST <- del[1, ]

# Which states they are in the last Congress, and what the other reading of
# party would do to the count.
last <- agg[agg$congress == max(agg$congress), ]
lastsen <- sen[sen$congress == max(sen$congress), ]
lastsen$side <- ifelse(lastsen$party_code == 100, "D",
                ifelse(lastsen$party_code == 200, "R", "I"))
dstates <- do.call(rbind, lapply(split(lastsen, lastsen$state_abbrev), function(x) data.frame(
  state = x$state_abbrev[1],
  senators = paste(sub(",.*$", "", x$bioname), collapse = " and "),
  sides = paste(sort(x$side), collapse = ""),
  split = length(unique(x$party_code)) > 1)))
dstates <- dstates[dstates$split, ]
# Reading an independent as the side they caucus with collapses I+D to one
# side; I+R stays split. Neither reading is wrong and the count is not the same.
dstates$split_caucus <- !dstates$sides %in% c("DI", "ID")
DEL_LAST_BALLOT <- nrow(dstates)
DEL_LAST_CAUCUS <- sum(dstates$split_caucus)

dd_write_csv(del, "derived/delegations.csv")
dd_write_csv(dstates, "derived/delegation_states.csv")
say("delegations: ", nrow(del), " Congresses, ", DEL_FIRST$year, "-", DEL_LAST$year,
    "  peak ", DEL_PEAK$pct_split, "% in ", DEL_PEAK$year,
    "  last ", DEL_LAST$pct_split, "%")

note("Voteview member-Congress rows", format(VV_ROWS, big.mark = ","))
note("Senate rows used, 1879 on", format(SEN_ROWS, big.mark = ","))
note("Congresses in the series", nrow(del))
note("states sending exactly two senators", sum(agg$two))
note("states sending three or more", sum(agg$members > 2))
note("largest gap between the two readings (pts)",
     sprintf("%.2f in %d", DEL_MAXGAP, DEL_MAXYR))
note("median gap between the two readings (pts)", sprintf("%.2f", DEL_MEDGAP))
note("largest gap since 1959 (pts)", sprintf("%.2f", DEL_GAP59))
note("correlation between the two readings", sprintf("%.4f", DEL_R))

# ===========================================================================
# 2. THE CLERK: PRESIDENT AND SENATE OFF ONE PAGE, 2004-2024
# ===========================================================================
#
# HOW THE PARSE WORKS. The document is organised by state. A line holding
# nothing but the state's name in capitals opens the state; a heading opens a
# section within it; every line beneath a heading ends in a vote total.
#
#     ALABAMA
#     FOR PRESIDENTIAL ELECTORS
#     Republican .............................   1,462,616
#     Democratic .............................     772,412
#     FOR UNITED STATES SENATOR
#     Katie Britt, Republican ................   1,435,401
#     Will Boyd, Democrat ....................     521,275
#
# The presidential lines name a PARTY rather than a person, because electors
# are what is on the ballot. The Senate lines name the candidate and print the
# party after a comma.
#
# SIX THINGS THIS PARSE HAS TO GET RIGHT, all of them found by running it:
#
#   THE PAGE BREAK. pdftotext leaves a form feed at the head of the line that
#     carries the state name, so a plain comparison against the state name
#     fails on exactly the states that begin a page.
#   THE FOOTNOTE ON A HEADING. New Jersey's 2004 heading reads "NEW JERSEY 1",
#     with a footnote marker. Left alone, the state never opens.
#   MINNESOTA AND NORTH DAKOTA. Their Democratic lines read
#     "Democratic-Farmer-Labor" and "Democratic-Nonpartisan League". An exact
#     match on "Democratic" silently loses a state's Democratic presidential
#     vote, which is worse than losing the state.
#   TWO CONTESTS IN ONE STATE. A state that lost a senator mid-term votes
#     twice, and the document separates the two with a parenthesis line.
#   THE RECAPITULATION. Each state closes with a summary whose lines also end
#     in numbers. Reading it would double the state, so the section closes when
#     that heading appears.
#   THE RUNOFF, WHICH IS THE ONE THAT BITES. A state that sends its Senate
#     election to a second round prints TWO columns of votes, and the two
#     columns overlap when the page is turned into text. What survives is the
#     first digit of the first-round total and the whole of the second-round
#     total, stranded on a line of its own with no name attached. Read
#     literally, Raphael Warnock received one vote in Georgia in 2020 and a
#     Libertarian won the state. Nothing about that reads as an error: the
#     number parses, the line is well formed, and the file that comes out is a
#     file of statewide Senate results with two states quietly wrong.
#
#     The repair is stated as a rule rather than a list of states. A named
#     candidate whose total is immediately followed by a nameless line holding
#     a number more than a hundred times larger has had their vote split across
#     two columns, and the large number is the one that decided the seat. Those
#     contests keep their winner and are marked, because a runoff was held on
#     another day against another electorate and its share cannot be set beside
#     a presidential share from November.

STATE_UPPER <- toupper(c(state.name, "District of Columbia"))
STATE_ABB   <- c(state.abb, "DC")
names(STATE_ABB) <- STATE_UPPER

AGG <- paste0("^(Blank|Blanks|Blank/Scattering|Blank Votes|Write-in|Scattering|",
              "Over Votes|Under Votes|Void|All Others|Others|",
              "None of These Candidates|Total|Totals)")

# Fusion: one candidate standing on more than one party line. The Clerk labels
# the extra presidential line with the nominee's own party in parentheses,
# which is what makes it addable rather than guessable.
is_dem <- function(l) grepl("^Democrat", l) | grepl("\\(Democratic Party Candidate\\)", l)
is_rep <- function(l) grepl("^Republican", l) | grepl("\\(Republican Party Candidate\\)", l)

parse_clerk <- function(path) {
  txt <- readLines(path, warn = FALSE)
  clean <- trimws(gsub("\f", "", txt))
  # The stranded second column: a line holding a number and nothing else. Its
  # position is recorded here so the candidate line above it can claim it.
  bare_num <- grepl("^[0-9][0-9,]*$", clean)
  next_bare <- function(i) {
    j <- i + 1L
    while (j <= length(clean) && !nzchar(clean[j])) j <- j + 1L
    if (j <= length(clean) && bare_num[j]) as.numeric(gsub(",", "", clean[j])) else NA_real_
  }
  pres <- list(); sen <- list(); runoff <- character()
  cur <- NA; sec <- NA; term <- "full"
  for (i in seq_along(txt)) {
    ln <- txt[i]
    s <- clean[i]
    bare <- trimws(sub("—Continued$", "", s))
    bare <- trimws(sub("-Continued$", "", bare))
    bare <- trimws(sub("[0-9]+$", "", bare))       # the footnote on a heading
    if (bare %in% STATE_UPPER) { cur <- bare; sec <- NA; term <- "full"; next }
    if (is.na(cur) || !nzchar(s)) next
    if (grepl("Recapitulation", s, ignore.case = TRUE)) { sec <- NA; next }
    if (grepl("FOR PRESIDENTIAL ELECTORS", s)) { sec <- "pres"; next }
    if (grepl("FOR UNITED STATES SENATOR", s))  { sec <- "sen"; term <- "full"; next }
    if (grepl("^FOR ", s)) { sec <- NA; next }
    if (is.na(sec)) next
    if (grepl("^\\(For ", s)) {
      term <- if (grepl("unexpired", s)) "unexpired" else "full"
      next
    }
    m <- regexpr("[0-9][0-9,]*$", s)
    if (m == -1) next
    votes <- as.numeric(gsub(",", "", regmatches(s, m)))
    label <- trimws(sub("\\s*\\.{2,}.*$", "", substr(s, 1, m - 1)))
    label <- trimws(sub("[0-9]+$", "", label))     # a footnote after a name
    if (!nzchar(label) || grepl(AGG, label)) next
    if (sec == "pres") {
      v <- pres[[cur]]; if (is.null(v)) v <- c()
      v[label] <- sum(c(v[label], votes), na.rm = TRUE)
      pres[[cur]] <- v
      next
    }
    if (!grepl(",", label)) next                   # a party line with no name
    k <- paste(cur, term)
    stranded <- next_bare(i)
    if (!is.na(stranded) && stranded > 100 * votes) {
      votes <- stranded                            # the runoff, in column two
      runoff <- union(runoff, k)
    }
    row <- data.frame(state = cur,
                      name  = trimws(sub(",[^,]*$", "", label)),
                      party = trimws(sub("^.*,\\s*", "", label)),
                      votes = votes)
    sen[[k]] <- if (is.null(sen[[k]])) row else rbind(sen[[k]], row)
  }
  list(pres = pres, sen = sen, runoff = runoff)
}

side <- function(p) ifelse(grepl("^Democrat", p), "D",
                    ifelse(grepl("^Republican", p), "R", "I"))

contests <- list()
pres_nat <- list()
for (y in PRES_YEARS) {
  z <- parse_clerk(file.path(HC, sprintf("clerk_%d.txt", y)))
  stopifnot(length(z$pres) == 51)

  pr <- do.call(rbind, lapply(names(z$pres), function(st) {
    v <- z$pres[[st]]
    data.frame(upper = st, abbrev = unname(STATE_ABB[st]),
               pres_dem = sum(v[is_dem(names(v))]),
               pres_rep = sum(v[is_rep(names(v))]))
  }))
  stopifnot(all(pr$pres_dem > 0), all(pr$pres_rep > 0))
  pr$pres_r <- round(100 * pr$pres_rep / (pr$pres_rep + pr$pres_dem), 3)
  pr$pres_winner <- ifelse(pr$pres_r > 50, "R", "D")
  pres_nat[[as.character(y)]] <- data.frame(
    year = y,
    nat_r = round(100 * sum(pr$pres_rep) / sum(pr$pres_rep + pr$pres_dem), 2),
    states_r = sum(pr$pres_winner == "R"))

  sc <- do.call(rbind, lapply(names(z$sen), function(k) {
    d <- z$sen[[k]]
    d <- d[order(-d$votes), ]
    dem <- d[side(d$party) == "D", ]
    rep <- d[side(d$party) == "R", ]
    data.frame(year = y, upper = d$state[1],
               term = sub("^.* ", "", k),
               runoff = k %in% z$runoff,
               winner = d$name[1], winner_party = d$party[1],
               winner_side = side(d$party[1]),
               sen_dem = if (nrow(dem)) dem$votes[1] else NA_real_,
               sen_rep = if (nrow(rep)) rep$votes[1] else NA_real_)
  }))
  sc <- merge(sc, pr[, c("upper", "abbrev", "pres_r", "pres_winner")], by = "upper")
  sc$sen_r <- round(100 * sc$sen_rep / (sc$sen_rep + sc$sen_dem), 3)
  sc$gap   <- round(sc$sen_r - sc$pres_r, 3)
  # A runoff was held on another day. Its winner is the state's senator and
  # counts; its share belongs to a different electorate and does not.
  sc$gap[sc$runoff] <- NA_real_
  sc$split <- sc$winner_side != sc$pres_winner
  contests[[as.character(y)]] <- sc[order(sc$abbrev, sc$term), ]
  say("clerk ", y, ": ", nrow(pr), " states, ", nrow(sc), " Senate contests, ",
      sum(sc$split), " split")
}
con <- do.call(rbind, contests)
nat <- do.call(rbind, pres_nat)
rownames(con) <- NULL

# THE CHECK THAT MATTERS. The 2024 contests were parsed out of this same
# document by a script in another chapter, written for another question. If the
# two parses disagree about a single winner, one of them is wrong.
XCHK <- NA_character_
if (file.exists(XOVER)) {
  xo <- read.csv(XOVER)
  a <- con[con$year == 2024, c("abbrev", "term", "winner_side")]
  b <- xo[, c("abbrev", "term", "winner_side")]
  cmp <- merge(a, b, by = c("abbrev", "term"), suffixes = c("_here", "_xover"))
  XCHK <- sprintf("%d of %d contests, %d disagreements",
                  nrow(cmp), nrow(a), sum(cmp$winner_side_here != cmp$winner_side_xover))
  stopifnot(nrow(cmp) == nrow(a), all(cmp$winner_side_here == cmp$winner_side_xover))
}

sy <- do.call(rbind, lapply(split(con, con$year), function(x) data.frame(
  year      = x$year[1],
  contests  = nrow(x),
  split     = sum(x$split),
  pct_split = round(100 * mean(x$split), 2),
  two_party = sum(!is.na(x$gap)),
  wobble    = round(median(abs(x$gap), na.rm = TRUE), 2))))

dd_write_csv(con, "derived/senate_contests.csv")
dd_write_csv(sy,  "derived/senate_years.csv")

RUNOFFS <- con[con$runoff, ]
note("Clerk volumes read", length(PRES_YEARS))
note("contests decided in a runoff, repaired",
     paste(sprintf("%s %d", RUNOFFS$abbrev, RUNOFFS$year), collapse = ", "))
note("Senate contests parsed, 2004-2024", nrow(con))
note("2024 parse against the crossover chapter's", XCHK)
note("national two-party Republican share, 2024",
     sprintf("%.2f%%", nat$nat_r[nat$year == 2024]))
note("national two-party Republican share, 2004",
     sprintf("%.2f%%", nat$nat_r[nat$year == 2004]))

# ===========================================================================
# 3. THE HOUSE: THE SAME MEASURE, AND ITS TWO INGREDIENTS, 1952-2024
# ===========================================================================
#
# One row of races.csv is one district in one House election. `dv` is the
# Democratic share of the two-party House vote and `dpres` the Democratic share
# of the two-party presidential vote in the same district.
#
# ONLY PRESIDENTIAL YEARS ARE USED. In a midterm the presidential number is
# from an election held up to two years earlier, so a midterm comparison is not
# the same measurement. Restricting to presidential years costs half the points
# and buys a series where every point compares two contests decided on one day.
#
# A SPLIT DISTRICT is one where the two shares fall on opposite sides of fifty.
# That is a threshold, and a threshold has two ingredients:
#
#   THE GAP -- how far the House candidate finished from their own party's
#     presidential candidate in the same district. Call it the gap.
#   THE SPREAD -- how far the districts sit from the point where the country
#     divides evenly. Measured around the year's own median, so that a
#     landslide does not read as districts moving apart.
#
# A district splits when the gap is larger than its distance from fifty, and in
# the other direction. So the count can fall because candidates stopped running
# away from their ticket, or because districts moved away from the middle, and
# the count alone cannot say which.

rc <- read.csv(RACES)
rc <- rc[!is.na(rc$dv) & !is.na(rc$dpres) & rc$year %% 4 == 0, ]
rc$gap <- rc$dv - rc$dpres

hy <- do.call(rbind, lapply(split(rc, rc$year), function(x) data.frame(
  year      = x$year[1],
  districts = nrow(x),
  med_pres  = round(median(x$dpres), 2),
  spread    = round(median(abs(x$dpres - median(x$dpres))), 2),
  wobble    = round(median(abs(x$gap)), 2),
  pct_split = round(100 * mean(sign(x$dv - 50) != sign(x$dpres - 50)), 2))))
hy <- hy[order(hy$year), ]

H1 <- hy[1, ]; H2 <- hy[nrow(hy), ]
HPEAK <- hy[which.max(hy$pct_split), ]

dd_write_csv(hy, "derived/house_years.csv")
say("house: ", nrow(hy), " presidential elections, ", H1$year, "-", H2$year,
    "  peak ", HPEAK$pct_split, "% in ", HPEAK$year, "  last ", H2$pct_split, "%")

# --- THE COUNTERFACTUAL ----------------------------------------------------
#
# Take one year's districts, keep their presidential vote exactly as it was,
# and give them another year's gaps. Then count the splits again. Nothing is
# simulated and nothing is random: the districts are ranked by their own gap
# and each is handed the gap at the same rank in the other year, so a district
# that ran furthest ahead of its ticket still does.
#
# Two runs answer two questions. Give the old year the new year's gaps and you
# ask what the collapse would have looked like if only the candidates had
# changed. Give the new year the old year's gaps and you ask what today would
# look like if only the districts had.

swap_gaps <- function(x, y) {
  gx <- x$gap; gy <- sort(y$gap)
  n <- length(gx); m <- length(gy)
  r <- rank(gx, ties.method = "first")
  # Rank i of n maps to rank i of m: the smallest gap to the smallest, the
  # largest to the largest, and no interpolation between them. Where the two
  # years hold the same number of districts this is the identity, which is what
  # the self-swap assertion below tests.
  g2 <- gy[round(1 + (r - 1) * (m - 1) / (n - 1))]
  round(100 * mean(sign(x$dpres + g2 - 50) != sign(x$dpres - 50)), 2)
}
byyr <- split(rc, rc$year)
Y_OLD <- HPEAK$year
Y_NEW <- H2$year
old <- byyr[[as.character(Y_OLD)]]
new <- byyr[[as.character(Y_NEW)]]

swap <- data.frame(
  case = c("actual_old", "actual_new", "old_with_new_gaps", "new_with_old_gaps"),
  districts_from = c(Y_OLD, Y_NEW, Y_OLD, Y_NEW),
  gaps_from      = c(Y_OLD, Y_NEW, Y_NEW, Y_OLD),
  pct_split = c(hy$pct_split[hy$year == Y_OLD], hy$pct_split[hy$year == Y_NEW],
                swap_gaps(old, new), swap_gaps(new, old)))

# A swap that changed nothing would be a broken swap. Handing a year its own
# gaps back must reproduce its own count, and this is the assertion that says
# the rank matching does what the paragraph above claims.
stopifnot(abs(swap_gaps(old, old) - hy$pct_split[hy$year == Y_OLD]) < 0.01,
          abs(swap_gaps(new, new) - hy$pct_split[hy$year == Y_NEW]) < 0.01)

dd_write_csv(swap, "derived/swap.csv")
say("swap: ", Y_OLD, " actual ", swap$pct_split[1], "%  with ", Y_NEW, " gaps ",
    swap$pct_split[3], "%   |   ", Y_NEW, " actual ", swap$pct_split[2],
    "%  with ", Y_OLD, " gaps ", swap$pct_split[4], "%")

note("presidential elections in the House series", nrow(hy))
note("district-elections behind it", format(sum(hy$districts), big.mark = ","))
note("self-swap reproduces its own count", "yes, both years")

# ===========================================================================
# 4. FACTS THE BRIEF QUOTES
# ===========================================================================

fx <- function(...) {
  a <- list(...)
  data.frame(key = names(a), value = unlist(lapply(a, as.character)))
}
facts <- fx(
  vv_rows          = VV_ROWS,
  del_first_year   = DEL_FIRST$year,
  del_last_year    = DEL_LAST$year,
  del_congresses   = nrow(del),
  del_peak_year    = DEL_PEAK$year,
  del_peak_pct     = DEL_PEAK$pct_split,
  del_peak_n       = DEL_PEAK$split_states,
  del_last_pct     = DEL_LAST$pct_split,
  del_last_n       = DEL_LAST$split_states,
  del_first_pct    = DEL_FIRST$pct_split,
  del_maxgap       = round(DEL_MAXGAP, 2),
  del_maxgap_year  = DEL_MAXYR,
  del_medgap       = round(DEL_MEDGAP, 2),
  del_gap59        = round(DEL_GAP59, 2),
  del_r_readings   = round(DEL_R, 4),
  del_three_plus   = sum(agg$members > 2),
  del_state_congresses = nrow(agg),
  del_last_ballot  = DEL_LAST_BALLOT,
  del_last_caucus  = DEL_LAST_CAUCUS,
  del_last_states  = paste(dstates$state, collapse = ", "),
  sen_contests     = nrow(con),
  sen_split_2004   = sy$split[sy$year == 2004],
  sen_split_2024   = sy$split[sy$year == 2024],
  sen_n_2004       = sy$contests[sy$year == 2004],
  sen_n_2024       = sy$contests[sy$year == 2024],
  sen_wobble_2004  = sy$wobble[sy$year == 2004],
  sen_wobble_2024  = sy$wobble[sy$year == 2024],
  sen_no_two_party = sum(is.na(con$gap)),
  sen_runoffs      = nrow(RUNOFFS),
  sen_runoff_where = paste(sprintf("%s %d", RUNOFFS$abbrev, RUNOFFS$year), collapse = ", "),
  nat_r_2024       = nat$nat_r[nat$year == 2024],
  h_first_year     = H1$year,
  h_last_year      = H2$year,
  h_years          = nrow(hy),
  h_districts      = sum(hy$districts),
  h_peak_year      = HPEAK$year,
  h_peak_pct       = HPEAK$pct_split,
  h_last_pct       = H2$pct_split,
  h_wobble_peak    = HPEAK$wobble,
  h_wobble_last    = H2$wobble,
  h_spread_peak    = HPEAK$spread,
  h_spread_last    = H2$spread,
  h_wobble_ratio   = round(HPEAK$wobble / H2$wobble, 1),
  swap_old_new     = swap$pct_split[3],
  swap_new_old     = swap$pct_split[4],
  swap_old_year    = Y_OLD,
  swap_new_year    = Y_NEW)
dd_write_csv(facts, "derived/facts.csv")

note("facts written", nrow(facts))
dd_write_csv(chk, "derived/checks.csv")

# The claims the chapter rests on, asserted rather than hoped for.
stopifnot(
  nrow(del) > 60,
  DEL_LAST$year == 2025,
  DEL_MAXGAP < 12,                      # the two readings of the roster agree
  DEL_GAP59 < 5,
  DEL_R > 0.95,
  nrow(con) > 190,                      # six elections of Senate contests
  nrow(RUNOFFS) == 3,                   # GA twice in 2020, LA once in 2016
  all(con$sen_dem[!is.na(con$sen_dem)] > 1000),   # no vote total lost a column
  all(con$sen_rep[!is.na(con$sen_rep)] > 1000),
  all(nat$nat_r > 44 & nat$nat_r < 56), # no year's presidential parse is absurd
  nrow(hy) == 19,
  H2$year == 2024,
  HPEAK$wobble > 5 * H2$wobble,         # the gap really did collapse
  swap$pct_split[3] < swap$pct_split[1],
  swap$pct_split[4] > swap$pct_split[2])

cat("\nbuilt ", FETCH_DATE, "\n\n", sep = "")
print(chk, row.names = FALSE)
cat("\nfiles written:\n")
for (f in list.files("derived", full.names = TRUE))
  cat(sprintf("  %-38s %5s rows %7.0f KB\n", f,
              format(nrow(read.csv(f)), big.mark = ","), file.size(f) / 1024))
prov_report()

if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_stamp()
}
