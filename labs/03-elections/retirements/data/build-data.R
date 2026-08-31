# ---------------------------------------------------------------------------
# Build the data for TWO chapters: `retirements` and `primary-defeats`.
#
# ONE SCRIPT, TWO FOLDERS. The two chapters ask different questions of exactly
# the same derivation: take every sitting House member, ask whether they are
# there again after the next election, and if not, ask by what route they left.
# Splitting that derivation across two build scripts would mean maintaining two
# copies of the same 300 lines and letting them drift. So this file is the
# build for both, and `../../primary-defeats/data/build-data.R` is a four-line
# wrapper that runs it. Outputs are written into both folders, and every file
# says at the top which chapter reads it.
#
# FETCHED 2026-08-11.  Sources, with what each returned on that date:
#
#   1. Voteview congressional member file
#      https://voteview.com/static/data/out/members/HSall_members.csv
#      HTTP 200 - 6,201,247 bytes - 51,062 rows - Congresses 1 to 119
#      One row per member per Congress since 1789. The primary compilation of
#      congressional membership. Carries `occupancy`, which says whether a
#      member was the first, second or third person to hold that seat in that
#      Congress -- the only structural evidence in the file that somebody left
#      in the middle of a term.
#
#   2. Federal Election Commission, "Federal Elections" biennial compilation,
#      2004 through 2022, ten Excel workbooks
#      https://www.fec.gov/documents/<id>/federalelections<year>.xls[x]
#      HTTP 200 - 602,712 to 6,251,110 bytes each
#      Candidate-level PRIMARY, RUNOFF and GENERAL vote totals for every U.S.
#      House candidate, with an incumbent indicator, compiled by a federal
#      agency from the official canvasses of the state election officials.
#      **This is the primary source for primary elections, and it is the only
#      one.** See the long note under "WHY THE FEC AND NOT THE CLERK" below.
#
#   3. Brookings Institution, "Vital Statistics on Congress", Chapter 2,
#      edition of April 2026
#      https://www.brookings.edu/wp-content/uploads/2026/04/Chpt-2.xlsx
#      HTTP 200 - 95,968 bytes - 20 sheets
#      Table 2-7, "House Incumbents Retired, Defeated, or Reelected,
#      1946-2024": 40 election years, with retirements, primary defeats and
#      general-election defeats already counted. A COMPILATION, not a primary
#      source -- it is assembled from the Biographical Directory, CQ Almanac,
#      CQ Weekly Report and National Journal. It is here because it is the
#      standard series, because it runs three times as long as anything this
#      script can build from primary documents, and because it gives the FEC
#      derivation below something independent to be checked against.
#      Table 2-9 gives House retirements by party back to 1930.
#
#   4. Greg Giroux, "Members of Congress Defeated For Renomination, 1966-2026"
#      https://docs.google.com/spreadsheets/d/1X97S6hUby6PPHAMoEwvAxkAUo8EUwLk8xMosx4hVd8c/export?format=csv&gid=638845180
#      HTTP 200 - text/csv - 23,511 bytes - 287 rows
#      A hand-kept tracker: one line per member denied renomination, naming who
#      beat them, the date, and the year they were first elected. Not a primary
#      source and not machine-readable; it is a journalist's list. It is here
#      because it is the only series that covers the CURRENT cycle, because it
#      records things no vote-total file does (that a defeat was two members
#      paired by redistricting; that the winner went on to lose in November),
#      and because it and Brookings were compiled independently and agree.
#
# WHY THE FEC AND NOT THE CLERK OF THE HOUSE
#
# The obvious place to look for congressional election returns is the Clerk of
# the House's "Statistics of the Congressional Election", which the
# house-competition chapter in this corpus already parses. It does not contain
# primary results and never has. Its own title page says what it is: "SHOWING
# THE VOTE CAST FOR EACH NOMINEE". Every edition from 1946 to 2024 was checked;
# the word "primary" appears in a handful of them and every occurrence is a
# footnote about Louisiana's nonpartisan blanket primary, which is that state's
# general election. The pre-2000 editions are image scans with no text layer,
# and were checked by eye. There is no primary appendix in any of them.
#
# The FEC compilation is a different publication with a different remit: it
# reports primary, runoff and general votes for every federal candidate. It has
# carried congressional primaries since the 1994 edition. The machine-readable
# workbooks begin in 2002, and the 2002 edition uses a merged-cell layout with
# no usable header row, so this script starts at 2004.
#
# A URL THAT RETURNS 200 AND IS NOT WHAT IT SAYS
#
# The FEC's document server addresses files by a numeric id and ignores the
# filename entirely. Asking for
#   https://www.fec.gov/documents/5675/federalelections2024.xlsx
# returns HTTP 200, content-type application/pdf, and 8,047,121 bytes of the
# 2022 report. There is no 2024 edition; the FEC has not published one. This is
# exactly the failure `../../../_lib/provenance.R` exists to catch, and it is why
# every id below is pinned and every download is size-checked.
#
# WHAT THIS SCRIPT CAN AND CANNOT SEPARATE
#
# A member who serves in Congress N and not in Congress N+1 has left. Voteview
# alone can count that, back to 1789, and it cannot say why. Adding the FEC
# file adds the one fact that does the separating: whether the member appeared
# on a ballot at all.
#
#   not on any House ballot ....... did not seek re-election
#   on the primary ballot, lost ... denied renomination
#   nominated, lost in November ... defeated in the general election
#   nominated, won ................ returned
#
# The first of those four is a residual, and it is a mixture: a member who
# retired, a member who died, a member who resigned, and a member who ran for
# something else all land in it together. Deaths and Senate runs are pulled out
# here where the files support it. What is left is still not "retirement" in
# the sense of a decision to stop; it is "did not appear on a ballot", and no
# file records the difference between choosing to leave and leaving before
# being pushed. The chapters say so.
#
# REQUIRES `readxl`.  install.packages("readxl")
#
# OUTPUTS
#
#   retirements/data/
#     derived/departures.csv     one row per Congress, House, 1789-2025: members,
#                        how many did not return, how many left mid-Congress,
#                        how many died, whether the seat stayed in the party
#     derived/exits.csv          one row per sitting member per cycle, 2004-2022, with
#                        the four-way outcome above
#     derived/exits_by_year.csv  the same, collapsed to one row per cycle
#     derived/vsoc.csv           Brookings Table 2-7, 1946-2024, tidied
#     derived/vsoc_party.csv     Brookings Table 2-9, House retirements by party
#     derived/checks.csv         the validation results printed at the end
#
#   primary-defeats/data/
#     incumbents.csv     one row per incumbent per cycle, 2004-2022: did they
#                        face a primary, what share did they take, did they win
#     by_year.csv        one row per cycle
#     denied.csv         the roster of members denied renomination
#     derived/vsoc.csv           Brookings Table 2-7 again (same file, both chapters)
#     giroux.csv         the tracker's per-year counts, 1966-2026
#     derived/checks.csv
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
source("../../../_lib/precision.R")   # dd_write_csv(): six significant digits
dir.create("derived", showWarnings = FALSE)
dir.create("raw", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)
suppressMessages(library(readxl))

FETCH_DATE <- "2026-08-11"
OUT_R <- "."            # retirements/data  -- this directory
OUT_P <- "../../primary-defeats/data"
dir.create("raw", showWarnings = FALSE)
dir.create(OUT_P, showWarnings = FALSE, recursive = TRUE)

if (file.exists("../../../_lib/provenance.R")) {
  source("../../../_lib/provenance.R")
} else {
  prov_fetch  <- function(url, dest, ...) { download.file(url, dest, mode = "wb", quiet = TRUE); dest }
  prov_report <- function() invisible(FALSE)
}

# The FEC and Brookings both refuse a bare libcurl user agent on some paths.
options(HTTPUserAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)")

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)

# ===========================================================================
# 1. VOTEVIEW: every House member of every Congress, 1789 to now
# ===========================================================================

U_VV <- "https://voteview.com/static/data/out/members/HSall_members.csv"
invisible(prov_fetch(U_VV, "raw/HSall_members.csv", label = "Voteview members"))
raw <- read.csv("raw/HSall_members.csv", colClasses = "character")
VV_ROWS <- nrow(raw)

for (v in c("congress", "icpsr", "born", "died", "district_code", "occupancy", "party_code"))
  raw[[v]] <- suppressWarnings(as.integer(raw[[v]]))
raw$year <- 1787L + 2L * raw$congress          # Congress n convenes in 1787+2n

# Delegates from DC, the territories and Puerto Rico are in this file only from
# the 116th Congress on. Including them would make the House appear to grow by
# six seats in 2019 and would put a set of members into the 2020 and 2022 exit
# tables who are simply absent from the 2018 one. Everything below is the
# voting House of the fifty states.
DELEG <- c("DC", "GU", "AS", "VI", "MP", "PR")

# ICPSR NUMBERS ARE NOT PERSON IDENTIFIERS IN THIS FILE, AND THAT MATTERS.
# A member who changes party in the middle of a Congress gets a SECOND row for
# that Congress with a DIFFERENT icpsr: Ralph Hall of Texas is 14828 through
# the 108th and 94828 from the 108th on, Parker Griffith of Alabama is 20901
# and 90901 in the 111th, Jeff Van Drew of New Jersey is 21980 and 91980 in the
# 116th. Keyed on icpsr, each of those men appears to leave the House at the
# moment he switched parties and a stranger appears in his seat -- and Parker
# Griffith, who switched to the Republicans in December 2009 and was then
# refused the Republican nomination in June 2010, is one of the events these
# chapters exist to count. `bioguide_id` is stable across the switch and is
# used instead. Eighteen House rows in the whole file are affected; three very
# old rows carry no bioguide id at all and fall back to icpsr.
raw$pid <- ifelse(is.na(raw$bioguide_id) | raw$bioguide_id == "",
                  paste0("icpsr:", raw$icpsr), raw$bioguide_id)

h <- raw[raw$chamber == "House" & !raw$state_abbrev %in% DELEG, ]
h <- h[order(h$congress, h$state_abbrev, h$district_code), ]
DUPE_PID <- sum(duplicated(paste(h$congress, h$pid)))
h <- h[!duplicated(paste(h$congress, h$pid)), ]   # keep the party first elected under
MAXC <- max(h$congress)

# THE DENOMINATOR IS EVERYONE WHO HELD A SEAT, AND THAT IS NOT A FREE CHOICE.
#
# The natural denominator for a chapter about leaving would be the class the
# last general election seated: 435 people, one per seat. Voteview carries a
# field that would give it -- `occupancy`, which says whether a member was the
# first, second or third person to hold that seat in that Congress -- and the
# field STOPS. It is populated for the 1st Congress through the 114th and is
# empty for every row of the 115th, 116th, 117th, 118th and 119th, along with
# `last_means`, which recorded whether a member arrived by general election or
# by special election. Nothing in the file announces this. A build that used
# `occupancy` would run clean and silently treat the 2017 House as though
# nobody had ever been replaced in the middle of a term.
#
# So the denominator here is every person who held a House seat during a
# Congress, which the file can always supply: 445 people in the 118th, not 435.
# A member who resigned in March and the member who replaced her in July are
# both in it, and both get an exit outcome of their own. Brookings counts
# differently, and that is one reason the two series in these chapters are
# close without being identical.
h$seat <- paste(h$state_abbrev, h$district_code)

# Did this person serve in the next Congress? In EITHER chamber -- a
# representative who moves to the Senate has not left Congress, and counting
# them as a departure from the institution would be wrong.
inany <- paste(raw$congress, raw$pid)
h$returns_house <- paste(h$congress + 1L, h$pid) %in% paste(h$congress, h$pid)
h$returns_cong  <- paste(h$congress + 1L, h$pid) %in% inany

# Did the seat stay with the same party? Free from this file, and available for
# all 119 Congresses, which nothing else in either chapter is. Where a seat had
# more than one occupant in the following Congress the first listed is taken,
# so this is the party that WON the seat, not the party that ended up holding
# it after a subsequent resignation.
nxt <- h[, c("congress", "seat", "party_code")]
nxt <- nxt[!duplicated(paste(nxt$congress, nxt$seat)), ]
h$next_party <- nxt$party_code[match(paste(h$congress + 1L, h$seat),
                                     paste(nxt$congress, nxt$seat))]

# `died` is a YEAR, not a date, so "died in office" can only ever be
# approximate here. A Congress convening in year Y sits until January of Y+2.
# A member who does not return and whose recorded death year is Y or Y+1 was
# almost certainly still in office; Y+2 is genuinely ambiguous and is NOT
# counted, which makes this an undercount rather than an overcount.
h$died_in_office <- !is.na(h$died) & h$died >= h$year & h$died <= h$year + 1L &
                    !h$returns_cong

dep <- do.call(rbind, lapply(split(h, h$congress), function(z) {
  data.frame(
    congress      = z$congress[1],
    year          = z$year[1],
    members       = nrow(z),
    seats         = length(unique(z$seat)),
    # Members minus seats is the number of mid-term replacements, exactly, in
    # any era of single-member districts. Before 1843 several states elected a
    # whole delegation at large on one district number, so for those Congresses
    # this is a floor rather than a count.
    mid_congress  = nrow(z) - length(unique(z$seat)),
    left          = sum(!z$returns_cong),
    left_house    = sum(!z$returns_house),
    died          = sum(z$died_in_office),
    to_senate     = sum(!z$returns_house & z$returns_cong),
    seat_held     = sum(!z$returns_cong & !is.na(z$next_party) &
                          z$next_party == z$party_code),
    seat_flipped  = sum(!z$returns_cong & !is.na(z$next_party) &
                          z$next_party != z$party_code),
    dem           = sum(z$party_code == 100, na.rm = TRUE),
    rep           = sum(z$party_code == 200, na.rm = TRUE),
    dem_left      = sum(!z$returns_cong & z$party_code == 100),
    rep_left      = sum(!z$returns_cong & z$party_code == 200))
}))
dep <- dep[dep$congress < MAXC, ]      # the sitting Congress has no successor
dep$pct_left <- 100 * dep$left / dep$members
dd_write_csv(dep, file.path(OUT_R, "derived/departures.csv"))

# ===========================================================================
# 2. THE FEC WORKBOOKS
# ===========================================================================
#
# Ten cycles, ten layouts. The columns are the same quantities throughout but
# the headers are not: PRIMARY becomes PRIMARY VOTES in 2012, INCUMBENT
# INDICATOR becomes (I) in 2012 and (I) INCUMBENT INDICATOR in 2020, DISTRICT
# becomes D in 2012 and back again in 2018, and the sheet holding the House is
# called five different things. In 2004 through 2012 the House and the Senate
# share one sheet. So columns are located by NORMALIZED HEADER TEXT rather than
# by position, and House rows are separated from Senate rows by the FEC ID,
# which begins H or S. A positional read would have silently shifted.

FEC <- list(
  list(y=2004, id="1633/federalelections2004.xls",  bytes=1377792,
       h="2004 US HOUSE & SENATE RESULTS", s=NA),
  list(y=2006, id="1642/federalelections2006.xls",  bytes=1058816,
       h="2006 US House & Senate Results", s=NA),
  list(y=2008, id="1666/federalelections2008.xls",  bytes=1581568,
       h="2008 House and Senate Results",  s=NA),
  list(y=2010, id="1675/federalelections2010.xls",  bytes=1377792,
       h="2010 US House & Senate Results", s=NA),
  list(y=2012, id="1691/federalelections2012.xls",  bytes=1827840,
       h="2012 US House & Senate Resuts",  s=NA),   # sic: the FEC's own typo
  list(y=2014, id="1700/federalelections2014.xls",  bytes=1280000,
       h="2014 US House Results by State", s="2014 US Senate Results by State"),
  list(y=2016, id="1890/federalelections2016.xlsx", bytes=1549194,
       h="2016 US House Results by State", s="2016 US Senate Results by State"),
  list(y=2018, id="2706/federalelections2018.xlsx", bytes=602712,
       h="2018 US House Results by State", s="2018 US Senate Results by State"),
  list(y=2020, id="4228/federalelections2020.xlsx", bytes=6251110,
       h="13. US House Results by State",  s="12. US Senate Results by State"),
  list(y=2022, id="5676/federalelections2022.xlsx", bytes=3179722,
       h="8. US House Results by State",   s="7. US Senate Results by State"))

nm  <- function(x) { x <- toupper(gsub("[^A-Za-z0-9 ]", " ", x)); gsub("\\s+", " ", trimws(x)) }
num <- function(x) suppressWarnings(as.numeric(gsub(",", "", trimws(x))))

read_fec_sheet <- function(f, sheet, yr) {
  d <- suppressMessages(read_excel(f, sheet = sheet, col_names = FALSE,
                                   .name_repair = "minimal", col_types = "text"))
  hd <- nm(as.character(unlist(d[1, ])))
  d  <- as.data.frame(d[-1, ], stringsAsFactors = FALSE)
  # First match wins. In 2004-2010 the vote column and the percentage column
  # carry the SAME header ("PRIMARY" twice), and the votes come first.
  g <- function(...) { i <- which(hd %in% c(...))[1]
                       if (is.na(i)) rep(NA_character_, nrow(d)) else d[[i]] }
  data.frame(
    year  = yr,
    stab  = g("STATE ABBREVIATION"),
    dist  = g("DISTRICT", "D"),
    fecid = g("FEC ID", "FEC ID"),
    inc   = g("INCUMBENT INDICATOR", "INCUMBENT INDICATOR I", "I",
              "I INCUMBENT INDICATOR"),
    last  = g("CANDIDATE NAME LAST", "LAST NAME"),
    first = g("CANDIDATE NAME FIRST", "FIRST NAME"),
    party = g("PARTY"),
    prim  = g("PRIMARY VOTES", "PRIMARY"),
    runo  = g("RUNOFF VOTES", "RUNOFF"),
    gen   = g("GENERAL VOTES", "GENERAL"),
    pewin = g("PE WINNER INDICATOR"),
    gewin = g("GE WINNER INDICATOR"),
    stringsAsFactors = FALSE)
}

FBASE <- "https://www.fec.gov/documents/"
fec <- list(); sen <- list(); fec_sizes <- integer(0)
for (q in FEC) {
  # Twenty-one megabytes of workbooks that change roughly never, so a second
  # run reads the cached copy. It still has to be RECORDED, though, or the
  # drift check in ../../../_lib/provenance.R quietly stops watching the largest
  # sources in the build: hash a cached file rather than skipping it.
  dest <- file.path("raw", basename(q$id))
  if (!file.exists(dest)) {
    invisible(prov_fetch(paste0(FBASE, q$id), dest, label = paste("FEC", q$y)))
  } else if (exists(".prov_record", mode = "function")) {
    invisible(.prov_record(paste0(FBASE, q$id), dest, paste("FEC", q$y)))
  }
  fec_sizes[as.character(q$y)] <- file.size(dest)
  if (file.size(dest) != q$bytes)
    cat(sprintf("  *** FEC %d moved: expected %d bytes, got %.0f ***\n",
                q$y, q$bytes, file.size(dest)))
  a <- read_fec_sheet(dest, q$h, q$y)
  fec[[as.character(q$y)]] <- a[!is.na(a$fecid) & grepl("^H", a$fecid), ]
  s <- if (is.na(q$s)) a else read_fec_sheet(dest, q$s, q$y)
  sen[[as.character(q$y)]] <- s[!is.na(s$fecid) & grepl("^S", s$fecid), ]
}
fe <- do.call(rbind, fec)
se <- do.call(rbind, sen)
FEC_ROWS <- nrow(fe)

# --- cleaning the four fields the whole derivation rests on ----------------

# DISTRICT. Mostly "01". Sometimes "01 - FULL TERM" / "01 - UNEXPIRED TERM",
# because a special election to fill the rest of a term and the regular
# election for the next one are held the same November in the same district and
# both appear here. The unexpired-term rows are a different election and are
# dropped from the renomination question.
fe$dnum  <- sub("^([0-9]{1,2}).*$", "\\1", trimws(fe$dist))
fe$dnum  <- ifelse(grepl("^[0-9]{1,2}$", fe$dnum),
                   sprintf("%02d", suppressWarnings(as.integer(fe$dnum))), NA)
fe$special <- grepl("UNEXPIRED|SPECIAL", toupper(paste(fe$dist, fe$inc)))

# INCUMBENT. The marker is "(I)" or "(I)*". It is NOT enough to test for the
# letter I: three rows in the 2010 file carry "SPECIAL PRIMARY" in this column,
# and `grepl("I", ...)` matches the I in PRIMARY. That bug put three Ohio
# challengers into the incumbent table.
fe$is_inc <- !is.na(fe$inc) & fe$inc %in% c("(I)", "(I)*")
se$is_inc <- !is.na(se$inc) & se$inc %in% c("(I)", "(I)*")

# PARTY. "D/WF Combined Parties" is a fusion line and is still the Democrat.
# "W(R)" is a WRITE-IN campaign in the Republican primary and is NOT a bid for
# the Republican nomination in any sense this chapter cares about: in 2016 both
# New Hampshire Democrats picked up a few dozen write-in votes in the
# Republican primary, and folding those rows into the Republican contest
# recorded both of them as having been denied renomination in a year when both
# were renominated and re-elected. Write-ins get their own party label so they
# can never win or lose somebody else's nomination.
# And one character that is not a space. Several party cells end in U+00A0, a
# NON-BREAKING space, which `trimws` does not remove: in the 2016 Kansas file
# Roger Marshall's party is "R\u00a0" and Tim Huelskamp's is "R/W", so after
# ordinary trimming they sit in two different Republican primaries in the same
# district, each unopposed. Huelskamp lost that primary by fourteen thousand
# votes and this document would have recorded him as renominated.
p <- toupper(trimws(gsub("\u00a0", " ", fe$party)))
fe$writein <- grepl("^W\\(", p)
p <- sub(" COMBINED PARTIES$", "", p)
p <- sub("/.*$", "", p)
p[p %in% c("REP")] <- "R"; p[p %in% c("DEM", "DFL")] <- "D"
fe$pty <- ifelse(fe$writein, paste0("WI-", p), p)

# VOTES. "Unopposed" and "Winner" are strings in a numeric column and mean the
# candidate was nominated without a contest. Treating them as NA -- which is
# what as.numeric does, silently -- removes 2,048 candidacies from the
# denominator, nearly all of them incumbents who had no primary challenger.
fe$prim_n <- num(fe$prim); fe$run_n <- num(fe$runo); fe$gen_n <- num(fe$gen)
fe$prim_unopposed <- !is.na(fe$prim) & grepl("unopposed|winner", fe$prim, ignore.case = TRUE)
fe$ran_prim <- !is.na(fe$prim_n) | fe$prim_unopposed
fe$on_gen   <- (!is.na(fe$gen_n) & fe$gen_n > 0) |
               (!is.na(fe$gen) & grepl("unopposed", fe$gen, ignore.case = TRUE)) |
               (!is.na(fe$gewin) & grepl("W", fe$gewin))
fe$ge_win   <- !is.na(fe$gewin) & grepl("W", fe$gewin)
fe$pe_win   <- !is.na(fe$pewin) & grepl("W", fe$pewin)

# Louisiana runs no party primary at all: every candidate appears on the
# November ballot and a December runoff follows if nobody clears 50%. California
# (from 2012), Washington (from 2008) and Alaska (2022) run a nonpartisan
# primary in which the top two -- top four in Alaska -- advance regardless of
# party. In those states "denied renomination" is not the same event, and it is
# flagged rather than quietly folded in.
# Louisiana is the hard case and it moves. Through 2006 the state ran a
# nonpartisan blanket primary in November with a December runoff, so its
# "primary" WAS its general election and the FEC file carries no primary votes
# for it at all. In 2008 and 2010 Louisiana held ordinary closed party
# primaries, and the file carries them. From 2012 it went back to the blanket
# system. Flagging Louisiana as nonpartisan in every year pools the 2008
# Democratic and Republican primaries in the 2nd district into a single contest
# and records William Jefferson as denied renomination in a year when he won
# his runoff by fifty-seven points and then lost in November.
fe$nonpartisan <-
  (fe$stab == "LA" & !fe$year %in% c(2008, 2010)) |
  (fe$stab == "WA" & fe$year >= 2008) |
  (fe$stab == "CA" & fe$year >= 2012) |
  (fe$stab == "AK" & fe$year >= 2022)

# --- who won the nomination ------------------------------------------------
#
# From 2020 the FEC prints a PE WINNER INDICATOR and the question is answered
# in the file. Before that it is not, and the nominee has to be derived: within
# one state, district and party, the nominee is whoever was unopposed, or won
# the runoff if there was one, or led the primary if there was not. In the
# nonpartisan states the top two advance rather than the top one.
#
# The derivation is then CHECKED against the FEC's own flag in the two cycles
# that have both. That check is reported in checks.csv and quoted in the
# chapters; it is the reason the pre-2020 numbers can be believed.

grp <- paste(fe$year, fe$stab, fe$dnum, fe$special,
             ifelse(fe$nonpartisan, "ALL", fe$pty))
fe$derived_win <- FALSE; fe$grp_has_pe <- FALSE; fe$grp_size <- 0L
for (ix in split(seq_len(nrow(fe)), grp)) {
  z <- fe[ix, ]; w <- rep(FALSE, length(ix))
  nadv <- if (z$nonpartisan[1] && z$stab[1] != "LA") 2L else 1L
  if (any(z$prim_unopposed)) {
    w[z$prim_unopposed] <- TRUE
  } else if (any(!is.na(z$run_n))) {
    w[order(-replace(z$run_n, is.na(z$run_n), -1))[seq_len(min(nadv, length(ix)))]] <- TRUE
  } else if (any(!is.na(z$prim_n))) {
    w[order(-replace(z$prim_n, is.na(z$prim_n), -1))[seq_len(min(nadv, length(ix)))]] <- TRUE
  }
  fe$derived_win[ix] <- w & z$ran_prim
  fe$grp_has_pe[ix]  <- any(z$pe_win)
  # How many people actually stood in this contest. An incumbent whose primary
  # cell reads "Unopposed" had no challenger; so, in practice, did an incumbent
  # who is the only candidate in the file with a vote total, which is how most
  # states report a primary nobody contested. Both are one candidate here.
  fe$grp_size[ix]    <- max(sum(!is.na(z$prim_n)), sum(z$ran_prim > 0))
}
fe$won_nom <- fe$derived_win | fe$pe_win

# validation: derivation vs the FEC's own flag, 2020 and 2022
vv <- fe[fe$year >= 2020 & fe$ran_prim & fe$grp_has_pe, ]
V_ALL <- nrow(vv); V_DIS <- sum(vv$derived_win != vv$pe_win)
vi <- vv[vv$is_inc, ]; VI_ALL <- nrow(vi); VI_DIS <- sum(vi$derived_win != vi$pe_win)

# --- one row per incumbent per cycle ---------------------------------------
#
# A candidate can occupy several rows: one per ballot line in a fusion state,
# plus a write-in line. The renomination question is asked of the rows for the
# candidate's own major party where those exist, because that is the nomination
# at stake. Joe Crowley lost the 2018 Democratic primary in New York's 14th and
# was still on the Working Families line in November; asking the question of
# all his rows at once records him as renominated, which is how he goes missing
# from a count that should include him.

collapse_cand <- function(z) {
  zz <- if (any(z$pty %in% c("D", "R"))) z[z$pty %in% c("D", "R"), ] else z
  pv <- zz$prim_n[!is.na(zz$prim_n)]
  data.frame(
    year = z$year[1], fecid = z$fecid[1], stab = z$stab[1], dnum = z$dnum[1],
    last = z$last[1], first = z$first[1], party = zz$pty[1],
    nonpartisan = any(z$nonpartisan),
    ran_prim    = any(zz$ran_prim),
    unopposed   = any(zz$prim_unopposed) && !any(!is.na(zz$prim_n)),
    prim_votes  = if (length(pv)) max(pv) else NA_real_,
    won_nom     = !any(zz$ran_prim & !zz$won_nom),
    rivals      = max(c(0L, zz$grp_size[zz$ran_prim] - 1L)),
    on_gen      = any(z$on_gen),
    ge_win      = any(z$ge_win),
    fec_flag    = any(z$is_inc),
    stringsAsFactors = FALSE)
}

# ===========================================================================
# 3. MATCHING THE FEC TO VOTEVIEW
# ===========================================================================
#
# The two files share no identifier. The FEC has its own candidate ids
# (H0AL01055); Voteview has ICPSR numbers. So they are joined on state and
# name, and names do not cooperate:
#
#   compound surnames split differently  FEC "Schultz" / Voteview
#                                        "WASSERMAN SCHULTZ"; also Jackson Lee,
#                                        McMorris Rodgers, Herrera Beutler
#   suffixes                             "Conyers, Jr." / "CONYERS"
#   accents                              Voteview "LUJAN" carries an acute
#   plain typographical errors in the    "Synder" for Snyder, "Gallegy" for
#     official federal file              Gallegly, "Alderholt" for Aderholt,
#                                        "Norcoss" for Norcross, "Sherril" for
#                                        Sherrill, "Buchson" for Bucshon
#   two members with the same surname    Linda and Loretta Sanchez, California
#     in the same state, same initial
#
# The rule is a ladder: exact surname within the state; then a surname that is
# the head or tail of the other; then first initial plus five characters; then
# an edit distance of two or less with a matching initial. Ties are broken on
# first initial and then on district number. Everything that survives is
# counted in checks.csv, and the residue is listed there by name rather than
# dropped quietly.

fold <- function(x) {
  z <- toupper(gsub("[^A-Za-z]", "", iconv(x, "UTF-8", "ASCII//TRANSLIT")))
  ifelse(is.na(z), "", z)
}
desuf <- function(x) gsub("\\b(JR|SR|II|III|IV)\\b", "", toupper(x))
h$sur <- fold(desuf(sub(",.*$", "", h$bioname)))
h$fi  <- substr(fold(sub("^[^,]*,\\s*", "", h$bioname)), 1, 1)

link <- function(f, hv) {           # f: FEC rows (stab, sur, fi, dn) -> hv rows
  out <- rep(NA_integer_, nrow(f)); how <- rep(NA_character_, nrow(f))
  narrow <- function(j, i) {
    if (length(j) <= 1) return(j)
    # Voteview gives a member who changes party in the middle of a Congress two
    # rows in that Congress -- Ralph Hall in 2004, Parker Griffith in 2010,
    # Jeff Van Drew in 2020. Two rows, one person, and treating it as an
    # ambiguous name drops exactly the members whose careers were eventful.
    if (length(unique(hv$pid[j])) == 1) return(j[1])
    j2 <- j[hv$fi[j] == f$fi[i]]; if (length(j2) == 1) return(j2)
    if (length(j2)) j <- j2
    j3 <- j[hv$district_code[j] == f$dn[i]]; if (length(j3) == 1) return(j3)
    j
  }
  for (i in seq_len(nrow(f))) {
    if (is.na(f$stab[i]) || is.na(f$sur[i]) || !nzchar(f$sur[i])) next
    cand <- which(hv$state_abbrev == f$stab[i]); if (!length(cand)) next
    j <- narrow(cand[hv$sur[cand] == f$sur[i]], i)
    if (length(j) == 1) { out[i] <- j; how[i] <- "exact"; next }
    j <- narrow(cand[endsWith(hv$sur[cand], f$sur[i]) |
                     startsWith(hv$sur[cand], f$sur[i])], i)
    if (length(j) == 1) { out[i] <- j; how[i] <- "compound surname"; next }
    j <- narrow(cand[hv$fi[cand] == f$fi[i] &
                     substr(hv$sur[cand], 1, 5) == substr(f$sur[i], 1, 5)], i)
    if (length(j) == 1) { out[i] <- j; how[i] <- "initial + 5 letters"; next }
    k <- cand[hv$fi[cand] == f$fi[i]]
    if (length(k)) {
      d <- as.vector(adist(f$sur[i], hv$sur[k]))
      if (min(d) <= 2 && sum(d == min(d)) == 1) {
        out[i] <- k[which.min(d)]; how[i] <- "edit distance <= 2"; next }
    }
  }
  list(idx = out, how = how)
}

# --- 3a. every cycle in turn ------------------------------------------------

fe$sur <- fold(desuf(fe$last)); fe$fi <- substr(fold(fe$first), 1, 1)
fe$dn  <- suppressWarnings(as.integer(fe$dnum))
se$sur <- fold(desuf(se$last)); se$fi <- substr(fold(se$first), 1, 1)

# --- 3a. every cycle in turn, oldest first ---------------------------------
#
# Order matters. The FEC gives each candidate a permanent id -- H2AL01077 is
# Jo Bonner in 2004 and in 2010 and in every file between -- so once a name has
# been matched in one cycle the id can carry the match into every later one,
# and the ambiguous names never have to be resolved twice. That is what rescues
# the cases a name can never settle: California sent two Sanchezes and two
# Millers to the 112th Congress, and in 2012 both pairs ran in renumbered
# districts, so state, surname, first initial and district number all fail at
# once. Their ids had already been matched in 2010, when the district numbers
# still agreed.

idmap <- character(0)                     # FEC candidate id -> bioguide id

# An id is only good while it points at the right person, and in the 2022 file
# it sometimes does not. That file gives Sean Patrick Maloney of New York the
# id H2NY14037, which had belonged to Carolyn B. Maloney since the 1990s, and
# gives Carolyn Maloney a new one; it also hands Ruben Ramirez, a candidate in
# the 2016 Texas primary, the id belonging to Ruben Hinojosa, the member he was
# running to replace. Trusting the id blindly merges the two Maloneys into one
# person and then deletes whichever arrives second -- which happened to be the
# one who was denied renomination. So every id lookup is required to agree with
# the name on the row before it is believed.
id_agrees <- function(pid, sur, fi, hv) {
  k <- match(pid, hv$pid)
  ok <- !is.na(k) & hv$fi[k] == fi &
        (hv$sur[k] == sur | endsWith(hv$sur[k], sur) | startsWith(hv$sur[k], sur))
  ok & !is.na(ok)
}
MATCH_N <- 0L; MATCH_ID <- 0L; MATCH_NAME <- 0L; MATCH_HOW <- integer(0)
UNMATCHED <- NULL; DROPPED <- NULL; RECOVERED <- NULL
exits <- list(); incs <- list()
for (q in FEC) {
  Y  <- q$y
  C  <- (Y - 1788L) / 2L                  # the Congress sitting on election day
  # `hva` is everyone who sat in Congress C, and it is what the FEC candidate
  # file is matched against.
  # `hv` is the same set, and is the denominator of every exit rate in the
  # retirements chapter. See the note on `occupancy` at the top: the class the
  # previous general election seated cannot be separated from the members who
  # arrived mid-term after 2015, because Voteview stopped recording which was
  # which. Both are here, and a member who won a special election in March is
  # correctly an incumbent in November.
  hva <- h[h$congress == C, ]
  hv  <- hva

  cy <- fe[fe$year == Y & !is.na(fe$dnum) & !fe$special & !fe$stab %in% DELEG, ]

  # --- pass A: start from the FEC's own incumbent marker -------------------
  ia <- cy[cy$is_inc, ]
  fy <- do.call(rbind, lapply(split(seq_len(nrow(ia)), ia$fecid),
                              function(ix) collapse_cand(ia[ix, ])))
  fy$sur <- fold(desuf(fy$last)); fy$fi <- substr(fold(fy$first), 1, 1)
  fy$dn  <- suppressWarnings(as.integer(fy$dnum))
  fy$pid <- unname(idmap[fy$fecid])
  # An id learned in an earlier cycle that points at somebody who is not in
  # this Congress is not usable here: former members do run again. Those fall
  # back to the name ladder rather than being trusted.
  fy$pid[!is.na(fy$pid) &
         !id_agrees(fy$pid, fy$sur, fy$fi, hva)] <- NA_character_
  fy$how   <- ifelse(is.na(fy$pid), NA_character_, "FEC candidate id")
  MATCH_N  <- MATCH_N + nrow(fy)
  MATCH_ID <- MATCH_ID + sum(!is.na(fy$pid))
  todo <- which(is.na(fy$pid))
  if (length(todo)) {
    m <- link(fy[todo, ], hva)
    fy$pid[todo] <- ifelse(is.na(m$idx), NA_character_, hva$pid[m$idx])
    fy$how[todo]   <- m$how
    MATCH_NAME <- MATCH_NAME + sum(!is.na(m$idx))
    t <- table(m$how); for (k in names(t))
      MATCH_HOW[k] <- (if (is.na(MATCH_HOW[k])) 0L else MATCH_HOW[k]) + t[[k]]
    if (any(is.na(m$idx)))
      UNMATCHED <- rbind(UNMATCHED, data.frame(
        year = Y, state = fy$stab[todo][is.na(m$idx)],
        district = fy$dnum[todo][is.na(m$idx)],
        name = trimws(paste(fy$first[todo][is.na(m$idx)],
                            fy$last[todo][is.na(m$idx)])),
        denied = with(fy[todo, ][is.na(m$idx), ], ran_prim & !won_nom)))
  }
  learn <- !is.na(fy$pid) & !(fy$fecid %in% names(idmap))
  if (any(learn)) { v <- fy$pid[learn]; names(v) <- fy$fecid[learn]
                    idmap <- c(idmap, v) }

  # An incumbent marker the membership record will not confirm is not an
  # incumbent. In 2022 the FEC file marks Tim Reichert, a Republican candidate
  # in Colorado's 7th, with (I); he had never held the seat, and the 7th was
  # open that year because Ed Perlmutter retired. The same test catches former
  # members trying to come back, whom the file also marks: Carol Shea-Porter
  # and Joe Garcia both lost in 2014 and both ran again in 2016 carrying an (I)
  # they were not entitled to. Rows like that go to dropped.csv.
  bad <- is.na(fy$pid) | !(fy$pid %in% hva$pid)
  if (any(bad)) DROPPED <- rbind(DROPPED, data.frame(
    year = Y, state = fy$stab[bad], district = fy$dnum[bad],
    name = trimws(paste(fy$first[bad], fy$last[bad])),
    reason = ifelse(is.na(fy$pid[bad]), "no match in Voteview at all",
                    "matched a person who did not sit in this Congress")))
  fy <- fy[!bad, ]

  # --- pass B: sitting members the FEC forgot to mark ----------------------
  # The marker is also sometimes simply missing. Every row of Michigan's 7th
  # district in 2006 has an empty incumbent column, including Joe Schwarz, who
  # was the sitting member and lost the Republican primary to Tim Walberg. So
  # the unmarked candidates are searched too -- but only by FEC id, and only
  # for ids already matched to a sitting member in an earlier cycle. Searching
  # by name instead recovers Joe Schwarz and also, from the 2016 file, "Joe
  # McDermott" of Washington's 7th, who is not Jim McDermott of Washington's
  # 7th, and who is thereby recorded as a sitting member denied renomination.
  rest <- cy[!cy$is_inc & cy$fecid %in% names(idmap), ]
  if (nrow(rest)) {
    ric <- unname(idmap[rest$fecid])
    keep <- !is.na(ric) & !ric %in% fy$pid &
            id_agrees(ric, fold(desuf(rest$last)),
                      substr(fold(rest$first), 1, 1), hva)
    for (id in unique(rest$fecid[keep])) {
      z <- rest[rest$fecid == id, ]
      r <- collapse_cand(z)
      r$sur <- fold(desuf(r$last)); r$fi <- substr(fold(r$first), 1, 1)
      r$dn  <- suppressWarnings(as.integer(r$dnum))
      r$pid <- unname(idmap[id]); r$how <- "FEC candidate id, marker missing"
      if (r$pid %in% fy$pid) next
      fy <- rbind(fy, r)
      RECOVERED <- rbind(RECOVERED, data.frame(
        year = Y, state = r$stab, district = r$dnum,
        name = trimws(paste(r$first, r$last)),
        denied = r$ran_prim & !r$won_nom))
    }
  }
  fy <- fy[!duplicated(fy$pid), ]
  incs[[as.character(Y)]] <- fy

  # Senate candidacies that cycle, matched the same way
  sy <- se[se$year == Y, ]; sy$dn <- NA_integer_
  ms <- link(sy, hva)
  sen_pid <- unique(hva$pid[ms$idx[!is.na(ms$idx)]])

  # Now go the other way: every member sitting in Congress C, and what the
  # FEC file says about them.
  z <- hv[, c("congress", "year", "pid", "icpsr", "bioname", "state_abbrev",
              "district_code", "party_code", "born", "died",
              "returns_cong", "returns_house", "died_in_office")]
  z$election_year <- Y
  j <- match(z$pid, fy$pid)
  z$on_house_ballot <- !is.na(j)
  z$ran_primary <- ifelse(is.na(j), FALSE, fy$ran_prim[j])
  z$denied      <- ifelse(is.na(j), FALSE, fy$ran_prim[j] & !fy$won_nom[j])
  z$ge_win      <- ifelse(is.na(j), FALSE, fy$ge_win[j])
  z$on_general  <- ifelse(is.na(j), FALSE, fy$on_gen[j])
  z$ran_senate  <- z$pid %in% sen_pid

  z$outcome <- with(z, ifelse(
    !on_house_ballot,
      ifelse(died_in_office, "died in office",
      ifelse(ran_senate, "ran for the Senate", "did not seek re-election")),
    ifelse(denied, "denied renomination",
    ifelse(returns_cong, "re-elected", "defeated in the general election"))))
  exits[[as.character(Y)]] <- z
}
MATCH_OK <- MATCH_ID + MATCH_NAME
inc <- do.call(rbind, incs)
inc <- inc[order(inc$year, inc$stab, inc$dnum), ]
inc$denied <- inc$ran_prim & !inc$won_nom
inc$contested <- inc$ran_prim & inc$rivals > 0

# The incumbent's share of the primary vote in their own party's contest.
# Needed for the distribution figure: the question is not only how often an
# incumbent loses, but how close anybody gets.
tot  <- tapply(fe$prim_n[!is.na(fe$prim_n)], grp[!is.na(fe$prim_n)], sum)
ikey <- paste(inc$year, inc$stab, inc$dnum, FALSE,
              ifelse(inc$nonpartisan, "ALL", inc$party))
inc$prim_total <- as.numeric(tot[ikey])
inc$prim_share <- 100 * inc$prim_votes / inc$prim_total

ex <- do.call(rbind, exits)

# The GE winner indicator only exists from 2012, so "re-elected" is taken from
# Voteview -- from whether the member is actually in the next Congress -- and
# not from the FEC file at all. That makes the two sources genuinely
# independent on the one quantity where they can be compared, and the
# comparison is in checks.csv.

exyr <- do.call(rbind, lapply(split(ex, ex$election_year), function(z)
  data.frame(
    year        = z$election_year[1],
    congress    = z$congress[1],
    members     = nrow(z),
    reelected   = sum(z$outcome == "re-elected"),
    lost_general= sum(z$outcome == "defeated in the general election"),
    denied      = sum(z$outcome == "denied renomination"),
    senate_run  = sum(z$outcome == "ran for the Senate"),
    died        = sum(z$outcome == "died in office"),
    not_running = sum(z$outcome == "did not seek re-election"),
    left        = sum(z$outcome != "re-elected"))))
exyr$pct_not_running <- 100 * exyr$not_running / exyr$members
exyr$pct_denied      <- 100 * exyr$denied / exyr$members

dd_write_csv(ex, file.path(OUT_R, "derived/exits.csv"))
dd_write_csv(exyr, file.path(OUT_R, "derived/exits_by_year.csv"))

inc$sur <- NULL; inc$fi <- NULL; inc$dn <- NULL
dd_write_csv(inc, file.path(OUT_P, "derived/incumbents.csv"))
if (!is.null(DROPPED))   dd_write_csv(DROPPED, file.path(OUT_R, "derived/dropped.csv"))
if (!is.null(RECOVERED)) dd_write_csv(RECOVERED, file.path(OUT_R, "derived/recovered.csv"))

byyr <- do.call(rbind, lapply(split(inc, inc$year), function(z) data.frame(
  year          = z$year[1],
  incumbents    = nrow(z),
  ran_primary   = sum(z$ran_prim),
  unopposed     = sum(z$ran_prim & !z$contested),
  contested     = sum(z$contested),
  denied        = sum(z$denied),
  denied_np     = sum(z$denied & z$nonpartisan),
  pct_denied    = 100 * sum(z$denied) / sum(z$ran_prim),
  pct_unopposed = 100 * sum(z$ran_prim & !z$contested) / sum(z$ran_prim),
  pct_contested = 100 * sum(z$contested) / sum(z$ran_prim),
  median_share  = median(z$prim_share[z$contested], na.rm = TRUE),
  min_share     = min(z$prim_share[z$contested], na.rm = TRUE),
  under60       = sum(z$contested & z$prim_share < 60, na.rm = TRUE),
  under55       = sum(z$contested & z$prim_share < 55, na.rm = TRUE))))
dd_write_csv(byyr, file.path(OUT_P, "derived/by_year.csv"))

den <- inc[inc$denied, c("year", "stab", "dnum", "last", "first", "party",
                         "nonpartisan", "prim_votes", "prim_share")]
dd_write_csv(den, file.path(OUT_P, "derived/denied.csv"))

# ===========================================================================
# 4. BROOKINGS, VITAL STATISTICS ON CONGRESS, CHAPTER 2
# ===========================================================================

U_VS <- "https://www.brookings.edu/wp-content/uploads/2026/04/Chpt-2.xlsx"
invisible(prov_fetch(U_VS, "raw/vsoc_ch2.xlsx", label = "Brookings VSoC ch.2"))

t7 <- suppressMessages(read_excel("raw/vsoc_ch2.xlsx", sheet = "2-7",
                                  col_names = FALSE, .name_repair = "minimal",
                                  col_types = "text"))
t7 <- as.data.frame(t7)
# Row 3 is the header; data begin at row 4 and stop at the first blank year.
# Several cells carry footnote letters glued to the number ("387e"), so every
# value is stripped to its digits before conversion.
t7 <- t7[4:nrow(t7), 1:6]
names(t7) <- c("year", "retired", "seeking", "lost_primary", "lost_general",
               "reelected")
strip <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.]", "", x)))
for (v in names(t7)) t7[[v]] <- strip(t7[[v]])
t7 <- t7[!is.na(t7$year) & t7$year >= 1946, ]
t7$pct_reelected   <- 100 * t7$reelected / t7$seeking
t7$pct_lost_primary<- 100 * t7$lost_primary / t7$seeking
t7$pct_retired     <- 100 * t7$retired / (t7$retired + t7$seeking)
dd_write_csv(t7, file.path(OUT_R, "derived/vsoc.csv"))
dd_write_csv(t7, file.path(OUT_P, "derived/vsoc.csv"))

t9 <- suppressMessages(read_excel("raw/vsoc_ch2.xlsx", sheet = "2-9",
                                  col_names = FALSE, .name_repair = "minimal",
                                  col_types = "text"))
t9 <- as.data.frame(t9)
# Two panels side by side: 1930-1976 in columns 1-6, 1978-2024 in 8-13.
p1 <- t9[5:nrow(t9), c(1, 2, 3)]; p2 <- t9[5:nrow(t9), c(8, 9, 10)]
names(p1) <- names(p2) <- c("year", "dem", "rep")
vp <- rbind(p1, p2)
for (v in names(vp)) vp[[v]] <- strip(vp[[v]])
vp <- vp[!is.na(vp$year), ]; vp <- vp[order(vp$year), ]
vp$total <- vp$dem + vp$rep
dd_write_csv(vp, file.path(OUT_R, "derived/vsoc_party.csv"))

# ===========================================================================
# 5. THE GIROUX TRACKER
# ===========================================================================
#
# Not a table. A stack of 32 year-blocks, each opening with a summary line like
#   "2022: 14 House (6 D, 8 R)^"
# and then one free-text line per member. The per-year House counts are parsed
# out of the summary lines; the member lines are kept as text because that is
# what they are.

U_GG <- paste0("https://docs.google.com/spreadsheets/d/",
               "1X97S6hUby6PPHAMoEwvAxkAUo8EUwLk8xMosx4hVd8c/",
               "export?format=csv&gid=638845180")
invisible(prov_fetch(U_GG, "raw/giroux.csv", label = "Giroux renomination tracker"))
gg <- read.csv("raw/giroux.csv", header = FALSE, colClasses = "character")
GG_ROWS <- nrow(gg)
GG_TITLE <- gg[[1]][1]
lines <- gg[[1]]
hdr <- grep("^(19|20)[0-9]{2}:", lines)
gy <- do.call(rbind, lapply(hdr, function(i) {
  s <- lines[i]
  yr <- as.integer(substr(s, 1, 4))
  mH <- regmatches(s, regexec("([0-9]+)\\^?\\s*House", s))[[1]]
  mS <- regmatches(s, regexec("([0-9]+)\\^?\\s*Senate", s))[[1]]
  mD <- regmatches(s, regexec("House \\(([0-9]+) D", s))[[1]]
  mR <- regmatches(s, regexec("D, ([0-9]+) R\\)", s))[[1]]
  data.frame(year = yr,
             house  = if (length(mH)) as.integer(mH[2]) else 0L,
             senate = if (length(mS)) as.integer(mS[2]) else 0L,
             house_dem = if (length(mD)) as.integer(mD[2]) else NA_integer_,
             house_rep = if (length(mR)) as.integer(mR[2]) else NA_integer_,
             n_lines = (if (i == max(hdr)) length(lines) else hdr[which(hdr == i) + 1] - 1) - i,
             summary_line = s, stringsAsFactors = FALSE)
}))
gy <- gy[order(gy$year), ]
dd_write_csv(gy, file.path(OUT_P, "derived/giroux.csv"))

# ===========================================================================
# 6. VALIDATION
# ===========================================================================
#
# Three independent things are compared here, and the point of each is that it
# COULD fail.
#
#   * the nominee derivation against the FEC's own PE winner flag, in the two
#     cycles that carry both;
#   * this script's primary-defeat count against Brookings and against Giroux,
#     which were compiled by different people from different sources;
#   * the FEC's incumbent marker against Voteview's membership record.

cmp <- merge(byyr[, c("year", "denied")],
             t7[, c("year", "lost_primary", "retired", "seeking")],
             by = "year")
cmp <- merge(cmp, gy[, c("year", "house")], by = "year", all.x = TRUE)
names(cmp) <- c("year", "fec_derived", "brookings", "retired_brookings",
                "seeking_brookings", "giroux")
cmp$gap_brookings <- cmp$fec_derived - cmp$brookings
cmp$gap_giroux    <- cmp$fec_derived - cmp$giroux
dd_write_csv(cmp, file.path(OUT_P, "derived/compare.csv"))
dd_write_csv(cmp, file.path(OUT_R, "derived/compare.csv"))

# Voteview's own count of who came back, against the FEC-based classification.
vv_cmp <- merge(exyr[, c("year", "members", "reelected", "left")],
                t7[, c("year", "seeking", "reelected")], by = "year",
                suffixes = c("_here", "_brookings"))

chk <- data.frame(
  check = c(
    "Voteview rows",
    "House member-Congress rows, 50 states",
    "Congresses covered",
    "FEC House candidate rows, 2004-2022",
    "FEC cycles read",
    "incumbent-cycles in the FEC files",
    "nominee derivation vs the FEC's own flag, 2020-2022: candidacies compared",
    "of those, candidacies where the two disagree",
    "same test restricted to incumbents: candidacies compared",
    "of those, incumbent candidacies where the two disagree",
    "incumbent-cycles matched to a Voteview member",
    "of those, matched by the permanent FEC candidate id",
    "matched on the exact surname within the state",
    "matched by a looser name rule",
    "not matched at all, and dropped",
    "incumbents the FEC did not mark, recovered by id",
    "party switchers with two Voteview rows in one Congress",
    "primary defeats, this script, 2004-2022",
    "primary defeats, Brookings Table 2-7, same years",
    "primary defeats, Giroux tracker, same years",
    "largest single-year gap to Brookings",
    "largest single-year gap to Giroux",
    "House members re-elected, this script, 2004-2022",
    "House members re-elected, Brookings, same years",
    "Giroux tracker rows",
    "Giroux year-blocks parsed"),
  value = c(
    format(VV_ROWS, big.mark = ","),
    format(nrow(h), big.mark = ","),
    paste(min(h$congress), "to", max(h$congress)),
    format(FEC_ROWS, big.mark = ","),
    as.character(length(FEC)),
    format(nrow(inc), big.mark = ","),
    format(V_ALL, big.mark = ","),
    sprintf("%d (%s%%)", V_DIS, pc(100 * V_DIS / V_ALL, 2)),
    as.character(VI_ALL),
    sprintf("%d (%s%%)", VI_DIS, pc(100 * VI_DIS / VI_ALL, 2)),
    sprintf("%d of %d (%s%%)", MATCH_OK, MATCH_N, pc(100 * MATCH_OK / MATCH_N, 2)),
    format(MATCH_ID, big.mark = ","),
    format(unname(MATCH_HOW["exact"]), big.mark = ","),
    as.character(sum(MATCH_HOW) - unname(MATCH_HOW["exact"])),
    as.character(if (is.null(DROPPED)) 0L else nrow(DROPPED)),
    as.character(if (is.null(RECOVERED)) 0L else nrow(RECOVERED)),
    as.character(DUPE_PID),
    as.character(sum(cmp$fec_derived)),
    as.character(sum(cmp$brookings)),
    as.character(sum(cmp$giroux)),
    sprintf("%d (%d)", max(abs(cmp$gap_brookings)),
            cmp$year[which.max(abs(cmp$gap_brookings))]),
    sprintf("%d (%d)", max(abs(cmp$gap_giroux)),
            cmp$year[which.max(abs(cmp$gap_giroux))]),
    format(sum(vv_cmp$reelected_here), big.mark = ","),
    format(sum(vv_cmp$reelected_brookings), big.mark = ","),
    as.character(GG_ROWS),
    as.character(nrow(gy))),
  stringsAsFactors = FALSE)
dd_write_csv(chk, file.path(OUT_R, "derived/checks.csv"))
dd_write_csv(chk, file.path(OUT_P, "derived/checks.csv"))

if (!is.null(UNMATCHED))
  dd_write_csv(UNMATCHED, file.path(OUT_R, "derived/unmatched.csv"))

# Hard stops. Each of these would have to move a long way before a number in
# either chapter changed, and each has a reason to be where it is.
stopifnot(
  VV_ROWS > 50000,
  unique(h$year[h$congress == 1]) == 1789,
  nrow(dep) > 110,
  FEC_ROWS > 20000,
  nrow(inc) > 3800,
  VI_DIS / VI_ALL < 0.02,               # the derivation reproduces the flag
  MATCH_OK / MATCH_N > 0.99,            # the name join actually joins
  nrow(t7) == 40, min(t7$year) == 1946, max(t7$year) == 2024,
  nrow(gy) > 25,
  max(abs(cmp$gap_brookings)) <= 3)     # three independent counts, same answer

cat("\nbuilt", FETCH_DATE, "\n\n")
print(chk, row.names = FALSE)
cat("\ncomparison of three counts of the same event:\n")
print(cmp[, c("year", "fec_derived", "brookings", "giroux")], row.names = FALSE)
cat("\nfiles written:\n")
for (f in c(file.path(OUT_R, c("derived/departures.csv", "derived/exits.csv", "derived/exits_by_year.csv",
                               "derived/vsoc.csv", "derived/vsoc_party.csv", "derived/compare.csv",
                               "derived/checks.csv", "derived/unmatched.csv")),
            file.path(OUT_P, c("incumbents.csv", "by_year.csv", "denied.csv",
                               "derived/vsoc.csv", "giroux.csv", "derived/compare.csv",
                               "derived/checks.csv"))))
  if (file.exists(f))
    cat(sprintf("  %-44s %6s rows %7.0f KB\n", f,
                format(nrow(read.csv(f)), big.mark = ","), file.size(f) / 1024))
prov_report()

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
