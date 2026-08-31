# ---------------------------------------------------------------------------
# Build the 2026 Senate prediction datasets.
#
# The chapter asks a reader to rate all 35 Senate races before the election and
# to keep the sheet. To do that it needs three things about each race, and all
# three exist today:
#
#   derived/races.csv          the 35 seats, who holds each one, whether that
#                              person is on the ballot, who is, and how the
#                              seat voted the last time it was contested.
#   derived/ratings_long.csv   twelve published forecasters' ratings of each of
#                              the 35 races, one row per forecaster per race.
#   derived/seat_math.csv      the arithmetic that turns 35 called races into a
#                              majority: what is up, what is not, and where 51
#                              comes from.
#
# Nothing here is a prediction. The forecasters' ratings are other people's
# predictions, recorded with the date they were published, which is a fact
# about what was said and not a claim about what will happen.
#
# derived/class_ratings.csv is the one file this script does not always write.
# It holds the class's own ratings, averaged, and it can only exist after the
# class has submitted any. See PART 5.
# ---------------------------------------------------------------------------

options(scipen = 999, stringsAsFactors = FALSE)

dir.create("raw",     showWarnings = FALSE)
dir.create("derived", showWarnings = FALSE)

if (file.exists("../../../_lib/provenance.R")) source("../../../_lib/provenance.R")
if (file.exists("../../../_lib/precision.R"))  source("../../../_lib/precision.R")

say <- function(...) cat(..., "\n", sep = "")

# The 35 states with a Senate race in 2026, in the order the rating code uses.
# Alphabetical by postal abbreviation, and the order is frozen here because a
# reader's 35-character code means nothing without it.
CODE_ORDER <- c("AK","AL","AR","CO","DE","FL","GA","IA","ID","IL","KS","KY",
                "LA","MA","ME","MI","MN","MS","MT","NC","NE","NH","NJ","NM",
                "OH","OK","OR","RI","SC","SD","TN","TX","VA","WV","WY")
stopifnot(length(CODE_ORDER) == 35, !anyDuplicated(CODE_ORDER))

# The seven categories a reader may choose, and the number each one stands for.
# Positive is Democratic. The scale is symmetric on purpose: a reader who calls
# every race the way the other side would gets exactly the negated total.
SCALE <- c("Safe D" = 3, "Likely D" = 2, "Lean D" = 1, "Toss-up" = 0,
           "Lean R" = -1, "Likely R" = -2, "Safe R" = -3)

# ===========================================================================
# PART 1 -- THE SOURCE
# ===========================================================================
#
# Wikipedia's "2026 United States Senate elections" article carries two tables
# this chapter needs, and both are maintained close to daily during a campaign:
#
#   Predictions      one row per race, with the Cook PVI, the incumbent, the
#                    result of the seat's last election, and twelve
#                    forecasters' current ratings, each dated.
#   Race summary     one row per race, with the candidates who qualified.
#
# The article is fetched as wikitext rather than as a rendered page, because
# the wikitext is what carries the rating templates as data. A rendered page
# turns {{USRaceRating|Lean|D}} into a coloured cell and the category becomes a
# background colour, which is exactly the sort of thing that cannot be read
# back out.
#
# The copy is kept, not thrown away. An article that changes daily is not a
# stable source, and the only way this build stays reproducible is that the
# version it read is on disk beside it.

WIKI_PAGE <- "2026_United_States_Senate_elections"
raw_wt <- "raw/wikipedia-senate-2026.wikitext"

if (!file.exists(raw_wt)) {
  api <- paste0("https://en.wikipedia.org/w/api.php?action=parse&page=",
                WIKI_PAGE, "&prop=wikitext&format=json&formatversion=2")
  raw_json <- "raw/wikipedia-senate-2026.json"
  if (exists("prov_fetch")) {
    prov_fetch(api, raw_json, quiet = TRUE, method = "curl",
               extra = '-L -A "Mozilla/5.0 (84-355 Democracys Data, course material)"')
  } else {
    download.file(api, raw_json, quiet = TRUE, method = "curl",
                  extra = '-L -A "Mozilla/5.0 (84-355 Democracys Data, course material)"')
  }
  j <- paste(readLines(raw_json, warn = FALSE), collapse = "\n")
  w <- sub('.*"wikitext"\\s*:\\s*"', "", j)
  w <- sub('"\\s*\\}\\s*\\}\\s*$', "", w)
  w <- gsub('\\\\n', "\n", w); w <- gsub('\\\\"', '"', w)
  w <- gsub('\\\\\\\\', "\\\\", w)
  writeLines(w, raw_wt)
}
WT <- paste(readLines(raw_wt, warn = FALSE), collapse = "\n")
say("read ", raw_wt, ": ", format(nchar(WT), big.mark = ","), " characters")

# Cut one == section == out of the article by name.
section <- function(txt, heading) {
  i <- regexpr(paste0("\n==\\s*", heading, "\\s*==\\s*\n"), txt)
  stopifnot(i > 0)
  rest <- substring(txt, i + 1)
  e <- regexpr("\n==[^=]", substring(rest, 2))
  stopifnot(e > 0)
  substring(rest, 1, e)
}

# ===========================================================================
# PART 2 -- THE 35 RACES
# ===========================================================================

pred <- section(WT, "Predictions")

# Rows are separated by a line containing only |-. The first piece is the
# table header and the prose above it, so it is dropped by the state test
# rather than by position.
blocks <- strsplit(pred, "\n\\|-\n")[[1]]

# {{sortname|Tommy|Tuberville}} -> "Tommy Tuberville"; [[Jon Husted]] -> the
# same; a piped link keeps the display text. The third argument of sortname is
# a disambiguated article title, never a name, so it is dropped.
person <- function(cell) {
  m <- regmatches(cell, regexec("\\{\\{sortname\\|([^|}]+)\\|([^|}]+)", cell))[[1]]
  if (length(m) == 3) return(trimws(paste(m[2], m[3])))
  m <- regmatches(cell, regexec("\\[\\[([^]|]+)(\\|([^]]+))?\\]\\]", cell))[[1]]
  if (length(m) >= 2) return(trimws(if (nzchar(m[4])) m[4] else m[2]))
  NA_character_
}

rows <- list()
for (b in blocks) {
  m <- regmatches(b, regexec("^!\\s*\\[\\[2026 United States Senate[^]]*\\|([^]]+)\\]\\]", b))[[1]]
  if (length(m) < 2) next
  state_name <- trimws(m[2])
  lines <- strsplit(b, "\n")[[1]]

  # A Senate special election is held in a state whose regular seat is not up,
  # so "(special)" in the header is what separates the two Class 3 seats from
  # the 33 Class 2 ones.
  special <- grepl("\\(special\\)", lines[1])

  pvi_cell <- lines[2]
  pm <- regmatches(pvi_cell, regexec("\\{\\{Shading PVI\\|([A-Z]+)\\|([0-9]+)\\}\\}", pvi_cell))[[1]]
  stopifnot(length(pm) == 3)
  pvi_party <- pm[2]; pvi_num <- as.integer(pm[3])

  sen_cell <- lines[3]
  senator <- person(sen_cell)
  # Minnesota's senators are shaded DFL, the state party's own name. It is the
  # Democratic Party's Minnesota affiliate and its senators are Democrats.
  sen_party <- if (grepl("Party shading/(Democratic|DFL)", sen_cell)) "D"
               else if (grepl("Party shading/Republican", sen_cell)) "R"
               else NA_character_

  status <- if (grepl("\\(retiring\\)", sen_cell, ignore.case = TRUE)) "retiring"
            else if (grepl("lost renomination", sen_cell, ignore.case = TRUE)) "lost renomination"
            else "running"

  last_cell <- lines[4]
  if (grepl("Appointed", last_cell)) {
    # An appointed senator has no last election of their own. The footnote
    # gives the seat's history instead, and the last contest named in it is
    # the one the seat was actually last decided by.
    status <- "appointed"
    pcts <- regmatches(last_cell,
      gregexpr("([0-9.]+)% of the vote in ([0-9]{4})", last_cell))[[1]]
    stopifnot(length(pcts) >= 1)
    lastm <- regmatches(pcts[length(pcts)],
      regexec("([0-9.]+)% of the vote in ([0-9]{4})", pcts[length(pcts)]))[[1]]
    last_share <- as.numeric(lastm[2]); last_year <- as.integer(lastm[3])
    pm2 <- regmatches(last_cell, regexec("\\{\\{efn\\|(name=[^|]*\\|)?(Republican|Democratic)", last_cell))[[1]]
    stopifnot(length(pm2) == 3)
    last_party <- substring(pm2[3], 1, 1)
    # Oklahoma's appointee is barred by state law from running for the seat, so
    # "appointed" and "retiring" are both true there. The chapter says so in
    # prose rather than inventing a fifth status.
  } else {
    lm <- regmatches(last_cell, regexec("\\|\\s*([0-9.]+)%\\s*([RD])", last_cell))[[1]]
    stopifnot(length(lm) == 3)
    last_share <- as.numeric(lm[2]); last_party <- lm[3]
    # Class 2's last regular election was 2020, unless the cell names another
    # contest -- Nebraska's seat was last decided by a 2024 special.
    ym <- regmatches(last_cell, regexec("\\(([0-9]{4})\\s*\\{\\{abbr\\|sp\\.", last_cell))[[1]]
    last_year <- if (length(ym) == 2) as.integer(ym[2]) else 2020L
  }

  ratings <- regmatches(b, gregexpr("\\{\\{USRaceRating\\|([^}]*)\\}\\}", b))[[1]]
  ratings <- sub("^\\{\\{USRaceRating\\|", "", sub("\\}\\}$", "", ratings))
  stopifnot(length(ratings) == 12)

  rows[[length(rows) + 1]] <- list(
    state_name = state_name, special = special,
    pvi_party = pvi_party, pvi = pvi_num,
    senator = senator, senator_party = sen_party, status = status,
    last_year = last_year, last_share = last_share, last_party = last_party,
    ratings = ratings)
}

say("parsed ", length(rows), " races from the Predictions table")
stopifnot(length(rows) == 35)

abb <- setNames(state.abb, state.name)
races <- data.frame(
  state      = unname(abb[vapply(rows, `[[`, "", "state_name")]),
  state_name = vapply(rows, `[[`, "", "state_name"),
  special    = vapply(rows, `[[`, TRUE, "special"),
  pvi_party  = vapply(rows, `[[`, "", "pvi_party"),
  pvi        = vapply(rows, `[[`, 1L, "pvi"),
  senator    = vapply(rows, `[[`, "", "senator"),
  seat_party = vapply(rows, `[[`, "", "senator_party"),
  status     = vapply(rows, `[[`, "", "status"),
  last_year  = vapply(rows, `[[`, 1L, "last_year"),
  last_share = vapply(rows, `[[`, 1.0, "last_share"),
  last_party = vapply(rows, `[[`, "", "last_party"))

stopifnot(!anyNA(races$state), setequal(races$state, CODE_ORDER))
races <- races[match(CODE_ORDER, races$state), ]
row.names(races) <- NULL

say("  special elections: ", paste(races$state[races$special], collapse = " "))
say("  seats held: D ", sum(races$seat_party == "D"),
    "  R ", sum(races$seat_party == "R"))
print(table(races$status))

# ---------------------------------------------------------------------------
# THE CHECK THAT MATTERS: is this really the Class 2 field?
#
# The 33 non-special races should be exactly the states with a Class 2 senator,
# and that is knowable from a source that has nothing to do with Wikipedia.
# @unitedstates/congress-legislators is maintained from the Senate's own
# records. If the two disagree, the Wikipedia parse is wrong and everything
# downstream is wrong with it.
leg_url <- "https://unitedstates.github.io/congress-legislators/legislators-current.csv"
leg_raw <- "raw/legislators-current.csv"
if (!file.exists(leg_raw)) {
  if (exists("prov_fetch")) prov_fetch(leg_url, leg_raw, quiet = TRUE)
  else download.file(leg_url, leg_raw, quiet = TRUE)
}
leg <- read.csv(leg_raw)
class2 <- sort(leg$state[leg$type == "sen" & leg$senate_class == 2])
regular <- sort(races$state[!races$special])
say("Class 2 seats per congress-legislators: ", length(class2))
CLASS2_AGREE <- setequal(class2, regular)
stopifnot(CLASS2_AGREE, length(class2) == 33)
say("  and they are the same 33 states the article lists. good.")

# The two specials must be seats whose regular class is NOT 2, or they would be
# double-counted.
spec_states <- races$state[races$special]
spec_class <- leg$senate_class[leg$type == "sen" & leg$state %in% spec_states]
stopifnot(all(spec_class != 2))
say("  the ", length(spec_states), " special seats are class ",
    paste(sort(unique(spec_class)), collapse = "/"), ", not class 2.")

# ===========================================================================
# PART 3 -- THE CANDIDATES
# ===========================================================================
#
# The incumbent is not the ballot. Ten of these senators are retiring, two lost
# their own primary, and four hold seats they were appointed to. The race
# summary tables carry who actually qualified.
#
# Not every race has a Democrat and a Republican in it. Three do not, and in
# two of those the serious challenger is an independent. A seven-point scale
# running from Safe D to Safe R has no square for that, which is a limitation
# of the scale and not a rounding error -- the chapter says so where it uses
# the scale. Every candidate is kept, party and all, so the gap is visible.

# One bullet of a candidate list: strip the party-stripe template, drop the
# citation, and take the display text of a wiki link if there is one. The
# party in parentheses at the end is what the list is keyed on, so it is read
# before the name is trimmed -- doing it the other way round decapitates
# "[[Mike Collins (politician)|Mike Collins]]" at the first bracket.
one_cand <- function(item) {
  s <- sub("^\\*\\s*\\{\\{Party stripe\\|[^}]*\\}\\}", "", item)
  s <- gsub("<ref[^>]*/>", "", s)
  s <- sub("<ref.*$", "", s)
  pm <- regmatches(s, regexec("\\(([^()]+)\\)\\s*$", trimws(s)))[[1]]
  party <- if (length(pm) == 2) pm[2] else NA_character_
  s <- sub("\\s*\\([^()]*\\)\\s*$", "", trimws(s))
  m2 <- regmatches(s, regexec("\\[\\[([^]|]+)(\\|([^]]+))?\\]\\]", s))[[1]]
  nm <- if (length(m2) >= 2) {
    if (nzchar(m2[4])) m2[4] else sub("\\s*\\([^()]*\\)$", "", m2[2])
  } else s
  c(name = trimws(nm), party = party)
}

cand_of <- function(sec) {
  out <- list()
  for (b in strsplit(sec, "\n\\|-\n")[[1]]) {
    m <- regmatches(b, regexec("^!\\s*\\[\\[2026 United States Senate[^]]*\\|([^]]+)\\]\\]", b))[[1]]
    if (length(m) < 2) next
    st <- trimws(m[2])
    if (!st %in% state.name) next
    items <- regmatches(b, gregexpr("\\*\\s*\\{\\{Party stripe\\|[^}]*\\}\\}[^\n]*", b))[[1]]
    if (!length(items)) next
    parsed <- lapply(items, one_cand)
    out[[st]] <- data.frame(
      name  = vapply(parsed, `[[`, "", "name"),
      party = vapply(parsed, `[[`, "", "party"))
  }
  out
}

summ <- section(WT, "Race summary")
cands <- cand_of(summ)
say("candidate lists found for ", length(cands), " races")
stopifnot(length(cands) == 35)

# Minnesota's Democrats run as the Democratic-Farmer-Labor Party, the state
# affiliate's own name, and the candidate list uses it.
DEM_LABELS <- c("Democratic", "DFL")
pick_from <- function(st, labels) {
  d <- cands[[st]]
  if (is.null(d)) return(NA_character_)
  hit <- d$name[d$party %in% labels]
  if (!length(hit)) NA_character_ else hit[1]
}
races$dem_cand <- vapply(races$state_name, pick_from, "", labels = DEM_LABELS)
races$rep_cand <- vapply(races$state_name, pick_from, "", labels = "Republican")
races$candidates <- vapply(races$state_name, function(s) {
  d <- cands[[s]]
  if (is.null(d)) return(NA_character_)
  paste(sprintf("%s (%s)", d$name, d$party), collapse = "; ")
}, "")
races$n_cands <- vapply(races$state_name,
  function(s) if (is.null(cands[[s]])) NA_integer_ else nrow(cands[[s]]), 1L)

NO_DEM <- races$state[is.na(races$dem_cand)]
NO_REP <- races$state[is.na(races$rep_cand)]
say("  races with both major-party candidates named: ",
    sum(!is.na(races$dem_cand) & !is.na(races$rep_cand)), " of 35")
if (length(NO_DEM)) say("  no Democrat on the ballot: ", paste(NO_DEM, collapse = " "))
if (length(NO_REP)) say("  no Republican on the ballot: ", paste(NO_REP, collapse = " "))

# ===========================================================================
# PART 4 -- THE RATINGS, AND THE NUMBER LINE THEY GO ON
# ===========================================================================
#
# Twelve forecasters, five words between them. To average them at all, the
# words have to become numbers, and that step is a choice this chapter makes
# in the open rather than borrowing from anyone:
#
#     Safe / Solid  3      the forecaster is not entertaining the other result
#     Likely        2
#     Lean          1
#     Tilt          0.5    used by two of the twelve; weaker than lean
#     Tossup        0
#
# signed positive for the Democrat. Nothing makes the gaps equal. Calling
# "Likely" exactly twice "Lean" is an assumption, and the chapter says so.

FORECASTERS <- c("Cook", "Inside Elections", "Sabato", "Race to the WH",
                 "The Economist", "RealClearPolitics", "Decision Desk HQ",
                 "Fox News", "VoteHub", "FiftyPlusOne", "Split Ticket",
                 "Silver Bulletin")
# The order of the twelve rating columns is a property of the table, not
# something to assume. Each header cell links the forecaster's article, so the
# link targets in order ARE the column order, and this asserts they still are.
# A column inserted or reordered upstream would otherwise attach every rating
# to the wrong forecaster, silently, and every average would still look fine.
HEADER <- strsplit(pred, "\n\\|-\n")[[1]][1]
ANCHORS <- c("Amy Walter|Cook", "Inside Elections|IE", "Sabato's Crystal Ball",
             "Race to the WH|WH", "The Economist|Econ",
             "RealClearPolitics|RCP", "Decision Desk HQ|DDHQ",
             "Fox News|Fox", "VoteHub<br", "FiftyPlusOne|FPO",
             "Split Ticket (website)|ST", "Silver Bulletin|Silver")
apos <- vapply(ANCHORS, function(a) as.integer(regexpr(a, HEADER, fixed = TRUE)), 1L)
COLS_IN_ORDER <- all(apos > 0) && !is.unsorted(apos, strictly = TRUE)
stopifnot(length(ANCHORS) == length(FORECASTERS), COLS_IN_ORDER)
say("the ", length(FORECASTERS), " rating columns are in the expected order")

# Each column also carries the date that forecaster's ratings were last
# changed, as {{small|Aug. 20,<br />2026}}. Read them rather than typing them:
# a rating set that moves is republished with a new date, and a date typed into
# a script goes stale without anything noticing.
dm <- regmatches(HEADER, gregexpr(
  "\\{\\{[Ss]mall\\|([A-Z][a-z]+\\.?)\\s*([0-9]{1,2}),\\s*<br\\s*/?>\\s*([0-9]{4})\\}\\}",
  HEADER))[[1]]
stopifnot(length(dm) == length(FORECASTERS))
MON <- c(Jan = 1, Feb = 2, Mar = 3, Apr = 4, May = 5, Jun = 6,
         Jul = 7, Aug = 8, Sep = 9, Oct = 10, Nov = 11, Dec = 12)
FORECAST_DATES <- vapply(dm, function(x) {
  p <- regmatches(x, regexec("([A-Z][a-z]{2})[a-z]*\\.?\\s*([0-9]{1,2}),\\s*<br\\s*/?>\\s*([0-9]{4})", x))[[1]]
  stopifnot(length(p) == 4, p[2] %in% names(MON))
  sprintf("%s-%02d-%02d", p[4], MON[[p[2]]], as.integer(p[3]))
}, "", USE.NAMES = FALSE)
say("  ratings published between ", min(FORECAST_DATES), " and ", max(FORECAST_DATES))

STRENGTH <- c("Safe" = 3, "Solid" = 3, "Likely" = 2, "Lean" = 1,
              "Tilt" = 0.5, "Tossup" = 0)

rate_parse <- function(x) {
  parts <- strsplit(x, "\\|")[[1]]
  parts <- parts[nzchar(parts)]
  word <- parts[1]
  stopifnot(word %in% names(STRENGTH))
  mag <- unname(STRENGTH[word])
  if (word == "Tossup") return(list(word = "Toss-up", score = 0))
  side <- parts[2]
  stopifnot(side %in% c("D", "R"))
  list(word = paste(word, side), score = if (side == "D") mag else -mag)
}

long <- do.call(rbind, lapply(seq_len(nrow(races)), function(i) {
  st <- races$state[i]
  r <- rows[[which(vapply(rows, `[[`, "", "state_name") == races$state_name[i])]]$ratings
  p <- lapply(r, rate_parse)
  data.frame(state = st,
             forecaster = FORECASTERS,
             published = FORECAST_DATES,
             rating = vapply(p, `[[`, "", "word"),
             score = vapply(p, `[[`, 1.0, "score"))
}))
stopifnot(nrow(long) == 35 * 12)
say("ratings: ", nrow(long), " rows, ", length(unique(long$forecaster)), " forecasters")

# The twelve, with the date each one's ratings carry and how far its own
# ratings sit from the average of the other eleven. The last column is the only
# way to see which forecaster is pulling an average around.
fdev <- vapply(FORECASTERS, function(f) {
  a <- long$score[long$forecaster == f]
  b <- vapply(long$state[long$forecaster == f], function(s)
    mean(long$score[long$state == s & long$forecaster != f]), 1.0)
  round(mean(abs(a - b)), 3)
}, 1.0, USE.NAMES = FALSE)
forecasters <- data.frame(
  forecaster = FORECASTERS, published = FORECAST_DATES,
  n_races = as.integer(table(long$forecaster)[FORECASTERS]),
  mean_gap = fdev)
forecasters <- forecasters[order(-forecasters$mean_gap), ]
if (exists("dd_write_csv")) dd_write_csv(forecasters, "derived/forecasters.csv") else
  write.csv(forecasters, "derived/forecasters.csv", row.names = FALSE)
print(forecasters, row.names = FALSE)

agg <- aggregate(score ~ state, long, function(v)
  c(mean = mean(v), sd = sd(v), min = min(v), max = max(v)))
fc <- data.frame(state = agg$state,
                 fc_mean = round(agg$score[, "mean"], 3),
                 fc_sd   = round(agg$score[, "sd"], 3),
                 fc_min  = agg$score[, "min"],
                 fc_max  = agg$score[, "max"])
races <- merge(races, fc, by = "state")
races <- races[match(CODE_ORDER, races$state), ]
row.names(races) <- NULL

# ---------------------------------------------------------------------------
# THE BASELINE MAP: last time this seat was on the ballot, put on the same
# seven-point scale so the three maps can be laid beside each other.
#
# The rule is arbitrary and visible, which is the point of showing it:
#
#     winner's share >= 60      Safe
#     55 to 60                  Likely
#     52 to 55                  Lean
#     under 52                  Toss-up
#
# The share is the winner's share of ALL votes cast, third parties included,
# because that is what the article's table publishes. In a race with a strong
# third candidate that makes the winner look weaker than the two-party number
# would.
BASE_CUTS <- c(52, 55, 60)
base_word <- function(share) {
  ifelse(share >= BASE_CUTS[3], "Safe",
  ifelse(share >= BASE_CUTS[2], "Likely",
  ifelse(share >= BASE_CUTS[1], "Lean", "Toss-up")))
}
races$base_word  <- base_word(races$last_share)
races$base_score <- ifelse(races$base_word == "Toss-up", 0,
                    ifelse(races$base_word == "Lean", 1,
                    ifelse(races$base_word == "Likely", 2, 3))) *
                    ifelse(races$last_party == "D", 1, -1)
races$base_rating <- ifelse(races$base_word == "Toss-up", "Toss-up",
                            paste(races$base_word, races$last_party))

say("baseline categories:")
print(table(races$base_rating))

races <- races[, c("state", "state_name", "special", "seat_party", "senator",
                   "status", "dem_cand", "rep_cand", "n_cands", "candidates",
                   "pvi_party", "pvi",
                   "last_year", "last_share", "last_party",
                   "base_word", "base_rating", "base_score",
                   "fc_mean", "fc_sd", "fc_min", "fc_max")]

if (exists("dd_write_csv")) {
  dd_write_csv(races, "derived/races.csv")
  dd_write_csv(long,  "derived/ratings_long.csv")
} else {
  write.csv(races, "derived/races.csv", row.names = FALSE)
  write.csv(long,  "derived/ratings_long.csv", row.names = FALSE)
}

# ---------------------------------------------------------------------------
# THE SEAT ARITHMETIC.
#
# 100 seats, 35 of them on the ballot. Everything a reader's 35 answers can
# change is in those 35; the other 65 are already decided and are what a
# majority is counted from.
SENATE_SIZE <- 100L
# The Senate that will be counting: 53 Republicans, 47 Democrats (the two
# independents caucus with the Democrats and are counted with them, which is
# how every seat count in the coverage is stated).
D_NOW <- 47L; R_NOW <- 53L
stopifnot(D_NOW + R_NOW == SENATE_SIZE)
d_up <- sum(races$seat_party == "D"); r_up <- sum(races$seat_party == "R")
d_hold <- D_NOW - d_up; r_hold <- R_NOW - r_up
stopifnot(d_up + r_up == 35, d_hold + r_hold == SENATE_SIZE - 35)

# The Vice President breaks ties and is a Republican, so 50-50 keeps the
# chamber Republican. A Democratic majority takes 51.
D_MAJORITY <- 51L
D_SEATS_NEEDED <- D_MAJORITY - d_hold          # of the 35
D_NET_GAIN     <- D_SEATS_NEEDED - d_up        # relative to the seats they hold

seat_math <- data.frame(
  quantity = c("seats in the Senate", "Democratic seats now", "Republican seats now",
               "seats on the ballot in 2026",
               "of those, Democratic-held", "of those, Republican-held",
               "Democratic seats not on the ballot", "Republican seats not on the ballot",
               "seats for a Democratic majority",
               "of the 35, Democrats must win", "net Democratic gain required"),
  value = c(SENATE_SIZE, D_NOW, R_NOW, 35L, d_up, r_up, d_hold, r_hold,
            D_MAJORITY, D_SEATS_NEEDED, D_NET_GAIN))
if (exists("dd_write_csv")) dd_write_csv(seat_math, "derived/seat_math.csv") else
  write.csv(seat_math, "derived/seat_math.csv", row.names = FALSE)
say("Democrats hold ", d_hold, " seats that are not up; ", D_SEATS_NEEDED,
    " of the 35 gets them to ", D_MAJORITY, " -- a net gain of ", D_NET_GAIN, ".")

# What the twelve forecasters' average implies, if a positive average is read
# as a Democratic win. This is not a forecast; it is what the ratings say when
# you insist on reading them as calls.
fc_d <- sum(races$fc_mean > 0); fc_r <- sum(races$fc_mean < 0)
fc_t <- sum(races$fc_mean == 0)
say("forecaster average reads as: D ", fc_d, ", R ", fc_r, ", exactly even ", fc_t)

# ===========================================================================
# PART 5 -- THE CLASS'S OWN RATINGS
# ===========================================================================
#
# Readers submit a 35-character code through a Google Form. The form's
# responses live in a Google Sheet; publishing that sheet to the web as CSV
# gives an address this build can read. Set it here, or leave it empty and the
# chapter falls back to the baseline map until there are answers.
#
# The code is one digit per race, in CODE_ORDER, 1 = Safe D through 7 = Safe R.
CLASS_CSV <- Sys.getenv("DD_SENATE_CLASS_CSV", "")
if (!nzchar(CLASS_CSV) && file.exists("raw/class-responses.csv"))
  CLASS_CSV <- "raw/class-responses.csv"

decode <- function(code) {
  code <- gsub("[^1-7]", "", code)
  if (nchar(code) != 35) return(NULL)
  4 - as.integer(strsplit(code, "")[[1]])   # 1 -> +3 (Safe D), 7 -> -3 (Safe R)
}

class_n <- 0L
if (nzchar(CLASS_CSV)) {
  resp <- read.csv(CLASS_CSV, check.names = FALSE)
  # Whichever column holds something that decodes is the code column; a form
  # may also collect a name, a timestamp and an email.
  cells <- unlist(lapply(resp, as.character), use.names = FALSE)
  decoded <- Filter(Negate(is.null), lapply(cells, decode))
  class_n <- length(decoded)
  say("class responses: ", class_n, " usable codes out of ",
      nrow(resp), " submissions")
}

if (class_n > 0) {
  M <- do.call(rbind, decoded)
  colnames(M) <- CODE_ORDER
  class_ratings <- data.frame(
    state    = CODE_ORDER,
    n        = class_n,
    cl_mean  = round(colMeans(M), 3),
    cl_sd    = round(apply(M, 2, sd), 3),
    cl_d     = colSums(M > 0),
    cl_toss  = colSums(M == 0),
    cl_r     = colSums(M < 0))
  if (exists("dd_write_csv")) dd_write_csv(class_ratings, "derived/class_ratings.csv") else
    write.csv(class_ratings, "derived/class_ratings.csv", row.names = FALSE)
  say("wrote class_ratings.csv for ", class_n, " respondents")
} else {
  # An empty file with the right columns, so the chapter never has to test
  # whether a file exists -- only whether it has rows in it.
  class_ratings <- data.frame(state = character(), n = integer(),
                              cl_mean = numeric(), cl_sd = numeric(),
                              cl_d = integer(), cl_toss = integer(),
                              cl_r = integer())
  write.csv(class_ratings, "derived/class_ratings.csv", row.names = FALSE)
  say("no class responses yet; wrote an empty class_ratings.csv")
}

# ===========================================================================
# PART 6 -- FACTS AND CHECKS
# ===========================================================================

fact <- function(k, v) data.frame(key = k, value = as.character(v))
disagree <- races[order(-races$fc_sd), ]
closest  <- races[order(abs(races$fc_mean)), ]

facts <- do.call(rbind, list(
  fact("n_races", nrow(races)),
  fact("n_special", sum(races$special)),
  fact("n_forecasters", length(FORECASTERS)),
  fact("n_ratings", nrow(long)),
  fact("d_up", d_up), fact("r_up", r_up),
  fact("d_hold", d_hold), fact("r_hold", r_hold),
  fact("d_needed", D_SEATS_NEEDED), fact("d_net_gain", D_NET_GAIN),
  fact("n_retiring", sum(races$status == "retiring")),
  fact("n_appointed", sum(races$status == "appointed")),
  fact("n_lost_primary", sum(races$status == "lost renomination")),
  fact("n_no_dem", length(NO_DEM)),
  fact("no_dem_states", paste(NO_DEM, collapse = ", ")),
  fact("n_no_rep", length(NO_REP)),
  fact("n_incumbent_running", sum(races$status == "running")),
  fact("n_tossup_any", sum(vapply(split(long$rating, long$state),
                                  function(v) any(v == "Toss-up"), TRUE))),
  fact("n_unanimous", sum(races$fc_sd == 0)),
  fact("most_disagreed", disagree$state_name[1]),
  fact("most_disagreed_sd", sprintf("%.2f", disagree$fc_sd[1])),
  fact("most_disagreed_min", disagree$fc_min[1]),
  fact("most_disagreed_max", disagree$fc_max[1]),
  fact("closest_state", closest$state_name[1]),
  fact("closest_mean", sprintf("%.2f", closest$fc_mean[1])),
  fact("fc_d", fc_d), fact("fc_r", fc_r),
  fact("fc_d_total", fc_d + d_hold),
  fact("class_n", class_n),
  fact("wiki_chars", nchar(WT)),
  fact("ratings_first_date", min(FORECAST_DATES)),
  fact("ratings_last_date", max(FORECAST_DATES)),
  fact("outlier_forecaster", forecasters$forecaster[1]),
  fact("outlier_gap", sprintf("%.2f", forecasters$mean_gap[1])),
  fact("closest_forecaster", forecasters$forecaster[nrow(forecasters)]),
  fact("base_cut_lean", BASE_CUTS[1]),
  fact("base_cut_likely", BASE_CUTS[2]),
  fact("base_cut_safe", BASE_CUTS[3])))
if (exists("dd_write_csv")) dd_write_csv(facts, "derived/facts.csv") else
  write.csv(facts, "derived/facts.csv", row.names = FALSE)

checks <- rbind(
  data.frame(check = "every race carries all twelve ratings",
             expected = "35 races x 12 = 420",
             got = paste(nrow(long), "rating rows"),
             ok = nrow(long) == 420),
  data.frame(check = "the 33 regular races are exactly the Class 2 seats",
             expected = "identical to congress-legislators",
             got = if (CLASS2_AGREE) "identical" else "DIFFERENT",
             ok = CLASS2_AGREE),
  data.frame(check = "the two special seats are not Class 2",
             expected = "no class-2 seat counted twice",
             got = paste("classes", paste(sort(unique(spec_class)), collapse = "/")),
             ok = all(spec_class != 2)),
  data.frame(check = "seats up plus seats not up is the whole Senate",
             expected = "100",
             got = as.character(d_up + r_up + d_hold + r_hold),
             ok = d_up + r_up + d_hold + r_hold == SENATE_SIZE),
  data.frame(check = "every seat is held by one of the two parties",
             expected = "35",
             got = as.character(sum(races$seat_party %in% c("D", "R"))),
             ok = sum(races$seat_party %in% c("D", "R")) == 35),
  data.frame(check = "the twelve rating columns are in the published order",
             expected = "each forecaster's header link before the next",
             got = if (COLS_IN_ORDER) "in order" else "OUT OF ORDER",
             ok = COLS_IN_ORDER),
  data.frame(check = "every forecaster rated every race",
             expected = "35 each",
             got = paste(range(forecasters$n_races), collapse = " to "),
             ok = all(forecasters$n_races == 35)),
  data.frame(check = "the rating scale is symmetric",
             expected = "scores sum to zero across the seven categories",
             got = as.character(sum(SCALE)),
             ok = sum(SCALE) == 0),
  data.frame(check = "no forecaster average sits outside the scale",
             expected = "-3 to 3",
             got = sprintf("%.2f to %.2f", min(races$fc_mean), max(races$fc_mean)),
             ok = min(races$fc_mean) >= -3 && max(races$fc_mean) <= 3),
  data.frame(check = "the baseline rule assigns every race a category",
             expected = "35",
             got = as.character(sum(!is.na(races$base_rating))),
             ok = sum(!is.na(races$base_rating)) == 35),
  data.frame(check = "a code decodes to the scale it was encoded from",
             expected = "1234567 round-trips to 3 2 1 0 -1 -2 -3",
             got = paste(decode(paste0("1234567", strrep("4", 28)))[1:7], collapse = " "),
             ok = identical(decode(paste0("1234567", strrep("4", 28)))[1:7],
                            c(3, 2, 1, 0, -1, -2, -3))),
  data.frame(check = "a code of the wrong length is refused",
             expected = "NULL",
             got = if (is.null(decode(strrep("4", 34)))) "NULL" else "accepted",
             ok = is.null(decode(strrep("4", 34)))))
checks$ok <- ifelse(checks$ok, "yes", "NO")
write.csv(checks, "derived/checks.csv", row.names = FALSE)
print(checks)
stopifnot(all(checks$ok == "yes"))

say("\ndone.")

# ---------------------------------------------------------------------------
if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_stamp()
}
