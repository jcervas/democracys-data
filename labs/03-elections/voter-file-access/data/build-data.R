# ---------------------------------------------------------------------------
# Build the voter-file-access dataset: what happens when you go and ask.
#
# SOURCE. Two things, both frozen in raw/, both dated 2026-08-13:
#
#   1. ncsl-access-table.html -- the National Conference of State Legislatures'
#      50-state survey "Access To and Use Of Voter Registration Lists". It
#      carries five narrative columns per jurisdiction and, for each, a link
#      labelled "<State> Access/Purchase List" pointing at that state's own
#      page. Those 51 links are the addresses the scan uses.
#   2. probe-2026-08-13.psv and probe-signals-2026-08-13.csv -- the scan, made
#      by data/probe.sh, which requested every one of those addresses as a
#      browser would and recorded what came back.
#
# THE DIVISION OF LABOUR MATTERS. The LEGAL facts in this chapter are NCSL's and
# are cited to NCSL: who may request a list, what it contains, what is
# confidential, what it may be used for. The RETRIEVABILITY facts are ours and
# are measured: status codes, content types, byte counts, and whether the page
# links a data file. Nothing here restates a statute on our own authority.
#
# THE THREE CLAIMED DOWNLOADS WERE OPENED, and only one of them is the voter
# file. That check is the difference between counting hyperlinks and reporting a
# finding, and it is hard-coded below rather than inferred, because "the page
# links a .txt" is a fact about HTML and "the state publishes its voter file" is
# a fact about the world.
#
# Run from this directory:  Rscript build-data.R      (offline; reads raw/)
# ---------------------------------------------------------------------------

source("../../../_lib/precision.R")

dir.create("derived", showWarnings = FALSE)
options(scipen = 999, stringsAsFactors = FALSE, warn = 1)

SCAN_DATE <- "2026-08-13"
n   <- function(x) format(as.numeric(x), big.mark = ",")
say <- function(...) cat(sprintf(...), "\n", sep = "")

FACTS <- list()
fact  <- function(key, value, note) {
  FACTS[[key]] <<- list(value = dd_num(value), note = note); invisible(value)
}
CHECKS <- list()
check <- function(label, value) {
  CHECKS[[length(CHECKS) + 1L]] <<- list(check = label, value = value)
  say("  check: %-58s %s", label, value); invisible(value)
}

s <- read.csv(sprintf("raw/probe-signals-%s.csv", SCAN_DATE), colClasses = "character")
s$bytes      <- as.numeric(s$bytes)
for (v in c("is_pdf", "login", "application", "fee", "data_links"))
  s[[v]] <- as.integer(s[[v]])

fact("scan_date", SCAN_DATE, "the day every address in this chapter was requested")
fact("n_states", nrow(s), "jurisdictions with an address in the survey")
check("the survey covers every state plus the District", nrow(s) == 51)

# --- the funnel -------------------------------------------------------------

ok      <- s$http == "200"
blocked <- s$http == "403"
dead    <- s$http == "404"
noconn  <- s$http == "000"
fact("n_ok",      sum(ok),      "addresses that answered")
fact("n_blocked", sum(blocked), "that refused a script with 403")
fact("n_dead",    sum(dead),    "whose link in the survey is dead")
fact("n_noconn",  sum(noconn),  "that did not complete a connection at all")
check("every address falls into exactly one outcome",
      sum(ok) + sum(blocked) + sum(dead) + sum(noconn) == nrow(s))

funnel <- data.frame(
  stage = c("Jurisdictions in the survey",
            "Address answers a script",
            "...and links a data file of any kind",
            "...and that file is the voter list"),
  states = c(nrow(s), sum(ok), sum(ok & s$data_links > 0), 1L))
dd_write_csv(funnel, "derived/funnel.csv")

# --- what the pages that answered are ---------------------------------------

fact("n_pdf",   sum(ok & s$is_pdf == 1),      "of those that answered are a PDF to print")
fact("n_login", sum(ok & s$login == 1),       "that ask you to log in")
fact("n_app",   sum(ok & s$application == 1), "that present an application or request form")
fact("n_fee",   sum(ok & s$fee == 1),         "that mention a fee, a cost or a purchase")
shape <- data.frame(
  reading = c("presents an application or request form",
              "mentions a fee, a cost or a purchase",
              "asks you to log in",
              "is a PDF form to print and post"),
  states = c(sum(ok & s$application == 1), sum(ok & s$fee == 1),
             sum(ok & s$login == 1), sum(ok & s$is_pdf == 1)),
  of = sum(ok))
dd_write_csv(shape, "derived/page_shape.csv")
check("asking is the norm: most answering pages present a form",
      sum(ok & s$application == 1) > sum(ok) / 2)

# --- the three that link a data file, opened one by one ---------------------
#
# Recorded as findings about what the file IS, because a hyperlink ending .txt
# is not evidence of anything. Each was requested and its first bytes read;
# North Carolina's was confirmed by content-length on the zip itself.

linked <- s$state[ok & s$data_links > 0]
fact("n_linked", length(linked), "states whose page links a data file at all")
claims <- data.frame(
  state = c("North Carolina", "Maine", "Virginia"),
  what_the_link_is = c(
    "ncvoter_Statewide.zip -- the statewide registration file",
    "an absentee-ballot file: one row per absentee request, with party and status",
    "a candidate and committee directory"),
  is_the_voter_list = c("yes", "no", "no"),
  bytes = c(519773993, 9485160, NA))
dd_write_csv(claims, "derived/claimed_downloads.csv")
stopifnot(setequal(linked, claims$state))
check("the linked-file count and the opened-file list agree",
      setequal(linked, claims$state))
check("only one of the three linked files is the registration list",
      sum(claims$is_the_voter_list == "yes") == 1)

fact("nc_bytes", 519773993, "bytes of North Carolina's statewide file, served without a login")
fact("nc_mb", round(519773993 / 1024^2), "the same, in megabytes")
fact("me_bytes", 9485160, "bytes of Maine's openly posted absentee file")
fact("n_true_download", sum(claims$is_the_voter_list == "yes"),
     "jurisdictions of 51 that publish the list for download")

# --- the survey's own link rot ----------------------------------------------

rot <- s[dead | noconn, c("state", "http")]
rot$outcome <- ifelse(rot$http == "404", "dead link", "no connection")
dd_write_csv(rot[order(rot$state), c("state", "outcome")], "derived/link_rot.csv")
fact("n_rot", nrow(rot), "of the survey's own links that did not resolve")
fact("rot_pct", round(100 * nrow(rot) / nrow(s), 1), "the same, as a share")

dd_write_csv(s[order(s$state), c("state", "http", "content_type", "bytes",
                                 "is_pdf", "login", "application", "fee",
                                 "data_links")],
             "derived/scan.csv")


# ===========================================================================
# WHAT IT COSTS
# ===========================================================================
#
# THE GAP THIS FILLS. Everything above measures whether an address answers.
# It never says what any state charges, and the chapter used to name that as
# a limit: the amounts sit in statutes, fee schedules and request forms
# rather than in anything the scan touched. Two compilations do publish
# them, and both are frozen in raw/ rather than fetched here, exactly like
# the NCSL survey.
#
#   1. eac-price-table.pdf -- U.S. Election Assistance Commission,
#      "Availability of State Voter File and Confidential Information."
#      https://www.eac.gov/sites/default/files/voters/Available_Voter_File_Information.pdf
#      Captured 2026-08-29, 286,329 bytes, 11 pages. Page 2 is a 51-row table
#      of state, availability and price estimate; pages 3 to 11 repeat it with
#      two narrative columns.
#
#      THE DOCUMENT CARRIES NO DATE ON ITS FACE. Nothing printed on any of
#      its eleven pages says when it was written. Its PDF metadata does:
#      created 30 October 2020. It sits at a stable federal URL, looks
#      current, is cited as current, and its headline sentence -- "The price
#      of the statewide voter file ranges from $0 to $37,000" -- rests on an
#      Alabama figure that the 2026 compilation below puts at $1,000.
#      EAC_MADE below is that metadata date, read out of the file and
#      asserted, so the chapter can print it rather than assume it.
#
#      It is also a compilation of compilations. Its own source line reads:
#      "National Conference of State Legislatures, United States Elections
#      Project, State Election Officials." No federal agency collects these
#      prices directly.
#
#   2. ballotpedia-price-table.html -- Ballotpedia, "Availability of state
#      voter files." https://ballotpedia.org/Availability_of_state_voter_files
#      Captured 2026-08-29, 238,931 bytes. The page says "As of July 2026"
#      and carries the same 51 jurisdictions with a Price column.
#
# WHY BOTH, AND WHY NEITHER IS THE ANSWER. Put side by side they disagree
# about half the country, sometimes by a factor of thirty-seven. Neither is
# a primary record: the primary records are 51 separate fee schedules, which
# is the reason both of these exist and the reason they differ. Printing one
# of them alone would turn a genuinely uncertain number into a fact.
#
# THE SECOND FINDING IS THAT A PRICE IS OFTEN NOT A NUMBER. Ballotpedia
# records what the states actually charge, and several do not charge an
# amount: Arizona and Texas quote a base plus a fraction of a cent per
# record, Michigan charges for the time taken, Tennessee prices digital and
# paper differently, Montana and West Virginia sell a subscription. The EAC
# table has a single dollar figure for every one of them, which is what a
# column of one number per state costs you.
#
# REQUIRES `pdftotext` (poppler), as the census-coverage build does.

EAC_PDF <- "raw/eac-price-table.pdf"
BP_HTML <- "raw/ballotpedia-price-table.html"
PRICE_CAPTURED <- "2026-08-29"

# --- the date the EAC document will not print -------------------------------
# Read from the PDF's own CreationDate, so the chapter quotes the file rather
# than something somebody remembered.
eac_info <- system2("pdfinfo", shQuote(EAC_PDF), stdout = TRUE)
eac_made <- grep("^CreationDate:", eac_info, value = TRUE)
stopifnot(length(eac_made) == 1L)
EAC_MADE <- as.Date(sub("^CreationDate: +", "", eac_made), "%a %b %d %T %Y")
stopifnot(!is.na(EAC_MADE), EAC_MADE < as.Date(PRICE_CAPTURED))

# --- the EAC's page 2 -------------------------------------------------------
# `pdftotext -layout` keeps the columns lined up in spaces. Page 2 prints two
# jurisdictions per printed line, side by side, so each line yields up to two
# records and the pattern is applied twice rather than anchored to the line.
eac_txt <- system2("pdftotext", c("-layout", "-f", "2", "-l", "2",
                                  shQuote(EAC_PDF), "-"), stdout = TRUE)
EAC_ROW <- "([A-Z][A-Za-z. ]+?) +(Open|Mixed|Restricted) +\\$([0-9,]+)"
eac_hits <- regmatches(eac_txt, gregexpr(EAC_ROW, eac_txt))
eac_hits <- unlist(eac_hits)
eac <- data.frame(
  state = trimws(sub(paste0("^", EAC_ROW, "$"), "\\1", eac_hits)),
  eac_availability = sub(paste0("^", EAC_ROW, "$"), "\\2", eac_hits),
  eac_price_usd = as.numeric(gsub(",", "", sub(paste0("^", EAC_ROW, "$"), "\\3", eac_hits))))
stopifnot(nrow(eac) == 51L, !any(duplicated(eac$state)), !any(is.na(eac$eac_price_usd)))

# --- Ballotpedia's table ----------------------------------------------------
# The price column is prose, not a number, and is kept as prose. Footnote
# markers are stripped; nothing else is.
bp_raw <- paste(readLines(BP_HTML, warn = FALSE), collapse = "\n")
# Ballotpedia writes its footnote brackets as numeric character references
# (&#91; and &#93;), so a strip that only handles named entities leaves them in
# the cell and every footnoted price -- "$20&#91;8&#93;" -- fails to read as a
# number. Decode numeric references before anything else looks at the text.
unent <- function(x) {
  m <- gregexpr("&#[0-9]+;", x)
  regmatches(x, m) <- lapply(regmatches(x, m), function(h)
    intToUtf8(as.integer(gsub("[^0-9]", "", h)), multiple = TRUE))
  x
}
untag  <- function(x) {
  x <- gsub("<[^>]*>", "", x)
  x <- unent(x)
  x <- gsub("&nbsp;", " ", x)
  x <- gsub("&amp;", "&", x); x <- gsub("&rsquo;", "'", x)
  x <- gsub("&ndash;", "-", x)
  trimws(gsub("[[:space:]]+", " ", x))
}
tables <- regmatches(bp_raw, gregexpr("(?s)<table.*?</table>", bp_raw, perl = TRUE))[[1]]
prices <- tables[grepl("Price", tables) & grepl("Availability", tables)]
stopifnot(length(prices) == 1L)
trs <- regmatches(prices, gregexpr("(?s)<tr.*?</tr>", prices, perl = TRUE))[[1]]
bp <- do.call(rbind, lapply(trs[-1], function(tr) {
  cells <- untag(regmatches(tr, gregexpr("(?s)<t[dh].*?</t[dh]>", tr, perl = TRUE))[[1]])
  if (length(cells) < 4) return(NULL)
  data.frame(state = cells[1], bp_availability = cells[3], bp_price = cells[4])
}))
bp$state <- sub("^D\\.C\\.$", "District of Columbia", bp$state)
bp$bp_price <- trimws(gsub("\\[[0-9]+\\]", "", bp$bp_price))
bp$bp_availability <- trimws(gsub("\\[[0-9]+\\]", "", bp$bp_availability))
stopifnot(nrow(bp) == 51L, !any(duplicated(bp$state)))

# A price is "one number" only when the whole entry is a single dollar amount.
# "$1,000" is; "$328.13 plus $0.0000625 per record" is not, and neither is
# "Cost varies based on time taken to produce records".
ONE <- "^\\$[0-9][0-9,]*(\\.[0-9]+)?$"
bp$bp_price_is_one_number <- ifelse(grepl(ONE, bp$bp_price), "yes", "no")
# Not ifelse(): it evaluates both arms, and as.numeric() over the prose prices
# then warns about coercion the script has already decided not to do.
bp$bp_price_usd <- NA_real_
plain <- bp$bp_price_is_one_number == "yes"
bp$bp_price_usd[plain] <- as.numeric(gsub("[$,]", "", bp$bp_price[plain]))
stopifnot(!any(is.na(bp$bp_price_usd[plain])))

price <- merge(eac, bp, by = "state", all = TRUE)
stopifnot(nrow(price) == 51L, !any(is.na(price$eac_price_usd)),
          !any(is.na(price$bp_price)))
price <- price[order(price$state), ]
price$both_are_numbers <- ifelse(price$bp_price_is_one_number == "yes", "yes", "no")
price$same_price <- ifelse(price$both_are_numbers == "no", "",
                           ifelse(price$eac_price_usd == price$bp_price_usd, "yes", "no"))

dd_write_csv(price[, c("state", "eac_availability", "eac_price_usd",
                       "bp_availability", "bp_price", "bp_price_usd",
                       "bp_price_is_one_number", "same_price")],
             "derived/price.csv")

CMP <- price[price$bp_price_is_one_number == "yes", ]
fact("eac_made", format(EAC_MADE, "%Y-%m-%d"),
     "the EAC price table's own creation date, from its PDF metadata; nothing on the page says it")
fact("price_captured", PRICE_CAPTURED, "the day both price tables were captured")
fact("eac_max", max(eac$eac_price_usd), "the highest price in the EAC table")
fact("eac_max_state", eac$state[which.max(eac$eac_price_usd)], "and the state it belongs to")
fact("eac_free", sum(eac$eac_price_usd == 0), "jurisdictions the EAC prices at nothing")
fact("bp_not_a_number", sum(price$bp_price_is_one_number == "no"),
     "jurisdictions whose 2026 price is not a single dollar amount")
fact("n_comparable", nrow(CMP), "jurisdictions where both tables give one plain number")
fact("n_same", sum(CMP$same_price == "yes"), "... and the two agree")
fact("n_differ", sum(CMP$same_price == "no"), "... and they do not")
fact("ga_eac", eac$eac_price_usd[eac$state == "Georgia"], "the EAC's price for Georgia")
fact("ga_bp", price$bp_price[price$state == "Georgia"], "Ballotpedia's price for Georgia")

check("both price tables cover every state plus the District", nrow(price) == 51)
check("the EAC document prints no date and its file records one",
      !is.na(EAC_MADE) && EAC_MADE == as.Date("2020-10-30"))
check("the two tables give the same plain price for fewer than half the country",
      sum(CMP$same_price == "yes") < nrow(price) / 2)

say("\nprice: EAC %s to %s, made %s; Ballotpedia has %d prices that are not one number",
    paste0("$", format(min(eac$eac_price_usd), big.mark = ",")),
    paste0("$", format(max(eac$eac_price_usd), big.mark = ",")),
    format(EAC_MADE, "%Y-%m-%d"), sum(price$bp_price_is_one_number == "no"))
say("       of %d comparable jurisdictions the two agree on %d and differ on %d",
    nrow(CMP), sum(CMP$same_price == "yes"), sum(CMP$same_price == "no"))

# ---------------------------------------------------------------------------

facts <- data.frame(key = names(FACTS),
                    value = vapply(FACTS, function(x) as.character(x$value), ""),
                    note  = vapply(FACTS, function(x) x$note, ""))
dd_write_csv(facts, "derived/facts.csv")
dd_write_csv(do.call(rbind, lapply(CHECKS, as.data.frame)), "derived/checks.csv")
say("\ndone: %d facts, %d checks", nrow(facts), length(CHECKS))

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
