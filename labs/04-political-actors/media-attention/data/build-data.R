# ---------------------------------------------------------------------------
# Build the media-attention dataset.
#
# Three files end up in this folder:
#
#   derived/wiki_attention_2024.csv   daily English Wikipedia pageviews for 12 articles
#                             across all of 2024, in long format
#                             (date, article, views)
#   derived/campaign_events_2024.csv  a hand-checked list of dated 2024 campaign events,
#                             used to label the spikes
#   derived/wiki_titles_2024.csv      the same pageviews carrying the article's resolved
#                             title alongside the requested one, so a redirect
#                             cannot silently change what was counted
#
# Run this script from inside the data/ folder. It needs a network connection;
# the whole point of committing the outputs is that the lab does not.
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

options(scipen = 999, stringsAsFactors = FALSE)

# --- Source -----------------------------------------------------------------
#
# Wikimedia Foundation, Pageviews API:
#   https://wikimedia.org/api/rest_v1/#/Pageviews%20data
#
# The "user" agent filter excludes known bots and spiders, which matters
# enormously here -- raw "all-agents" counts are heavily contaminated.
# Wikimedia publishes pageview data under CC0, so it can be redistributed
# freely, which is why this lab can ship its own copy.
#
# A pageview is a request for a page. It is a measure of ATTENTION, not of
# exposure, belief, or persuasion. The lab is careful about this and so
# should you be.

# --- Articles, and the titles each one has had -------------------------------
#
# THE API REPORTS BY TITLE, AND AN ARTICLE IS NOT ITS TITLE. Wikipedia articles
# get renamed, and when one does, the Pageviews API keeps reporting the old
# title and the new title as two separate series. It does not merge them and it
# does not tell you that a move happened. Ask for the title the article has
# today and you get, for every day before the move, only the people who arrived
# through the leftover redirect.
#
# That is not hypothetical here. The article on JD Vance sat at "J. D. Vance"
# until it was moved at the start of August 2024. Under the current title the
# series records a median of 1 to 6 views a day from January to June -- for a
# sitting United States senator -- and misses the single largest spike in this
# whole dataset: 5,117,480 views on 15 July, the day he was named to the
# ticket, all of it filed under the old title.
#
# So each entry below is an article and EVERY title it is known to have held in
# 2024, and the daily counts are summed across them. Summing is the right
# operation and does not double-count: at any given moment one title is the
# article and the others are redirects to it, and a redirect hit is a real
# person arriving at the page.
#
# Adding a title here is a claim that the two titles are the same article. It
# should be checked -- the two series should look like one handing off to the
# other, not like two independent things.

articles <- list(
  "Kamala_Harris"    = "Kamala_Harris",
  "Donald_Trump"     = "Donald_Trump",
  "Joe_Biden"        = "Joe_Biden",
  "JD_Vance"         = c("JD_Vance", "J._D._Vance"),
  "Tim_Walz"         = "Tim_Walz",
  "2024_United_States_presidential_election" =
    "2024_United_States_presidential_election",
  "Project_2025"     = "Project_2025",
  "Springfield,_Ohio" = "Springfield,_Ohio",
  "Haitian_Americans" = "Haitian_Americans",
  "Electoral_College_(United_States)" = "Electoral_College_(United_States)",
  "Swing_state"      = "Swing_state",
  "Opinion_poll"     = "Opinion_poll"
)

ua <- "cmu-84355-teaching-lab/1.0 (cervas@cmu.edu)"

fetch_one <- function(title) {
  url <- paste0(
    "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/",
    "en.wikipedia/all-access/user/",
    utils::URLencode(title, reserved = TRUE),
    "/daily/20240101/20241231")

  con <- url(url, headers = c("User-Agent" = ua))
  on.exit(try(close(con), silent = TRUE))
  txt <- paste(readLines(con, warn = FALSE), collapse = "")

  # Minimal JSON pull -- the response is a flat list of {timestamp, views}.
  ts <- regmatches(txt, gregexpr('"timestamp":"[0-9]{10}"', txt))[[1]]
  vw <- regmatches(txt, gregexpr('"views":[0-9]+', txt))[[1]]
  stopifnot(length(ts) == length(vw), length(ts) > 300)

  data.frame(
    date  = as.Date(substr(ts, 14, 21), format = "%Y%m%d"),
    views = as.numeric(sub('"views":', "", vw)))
}

BY_TITLE <- list()          # kept for the articles that have more than one

fetch_article <- function(article, titles) {
  parts <- lapply(titles, fetch_one)
  for (i in seq_along(titles)) {
    cat(sprintf("  %-44s %3d days, median %8.0f, peak %9.0f\n",
                titles[i], nrow(parts[[i]]), median(parts[[i]]$views),
                max(parts[[i]]$views)))
  }
  if (length(titles) > 1) {
    BY_TITLE[[article]] <<- do.call(rbind, lapply(seq_along(titles),
      function(i) data.frame(date = parts[[i]]$date, article = article,
                             title = titles[i], views = parts[[i]]$views)))
  }
  d <- do.call(rbind, parts)
  d <- aggregate(views ~ date, d, sum)
  data.frame(date = d$date, article = article, views = d$views)
}

out <- do.call(rbind, lapply(names(articles), function(a) {
  cat(a, "\n")
  d <- fetch_article(a, articles[[a]])
  if (length(articles[[a]]) > 1) {
    cat(sprintf("  %-44s %3d days, median %8.0f, peak %9.0f  <- summed\n",
                "", nrow(d), median(d$views), max(d$views)))
  }
  d
}))

out <- out[order(out$article, out$date), ]
stopifnot(!any(is.na(out$date)), !any(is.na(out$views)))

# --- The tripwire that would have caught it ---------------------------------
#
# A renamed title does not announce itself; it shows up as a series whose early
# months are three orders of magnitude below its own typical day. Every article
# in this set was chosen because it is about national politics in an election
# year, so none of them should have a quiet month that is a thousandth of its
# median month. This is deliberately loose -- Springfield, Ohio legitimately
# ranges 22x and Tim Walz 204x -- so it fires on a title problem and not on a
# news cycle.

ratios <- vapply(unique(out$article), function(a) {
  v <- out$views[out$article == a]
  mo <- tapply(v, format(out$date[out$article == a], "%m"), median)
  max(mo) / max(1, min(mo))
}, numeric(1))
bad <- names(ratios)[ratios > 1000]
if (length(bad)) {
  cat("\nmonthly median range looks like a page move, not a news cycle:\n")
  print(round(ratios[bad]))
}
stopifnot(length(bad) == 0)

cat("\ntotal rows:", nrow(out), "across", length(unique(out$article)),
    "articles, from", length(unlist(articles)), "titles\n")
write.csv(out, "derived/wiki_attention_2024.csv", row.names = FALSE)

# --- The titles, kept separately ---------------------------------------------
#
# The summed series is what the chapter analyses, but the two halves of it are
# the evidence that the sum is the right thing to do -- one title hands off to
# the other on a single day -- so they are committed too rather than thrown
# away inside this script.

bt <- do.call(rbind, BY_TITLE)
if (!is.null(bt)) {
  bt <- bt[order(bt$article, bt$title, bt$date), ]
  write.csv(bt, "derived/wiki_titles_2024.csv", row.names = FALSE)
  cat("by-title rows:", nrow(bt), "for",
      length(unique(bt$article)), "renamed article(s)\n")
}

# --- Campaign events --------------------------------------------------------
#
# Dates of the 2024 campaign's set-piece moments. These are not derived from
# the pageview data -- they are the independent record against which the
# pageview spikes are checked. Each was verified against contemporaneous
# reporting before being written here.

events <- data.frame(
  date = as.Date(c(
    "2024-06-27", "2024-07-13", "2024-07-15", "2024-07-21",
    "2024-08-06", "2024-08-19", "2024-09-10", "2024-10-01",
    "2024-11-05")),
  event = c(
    "First presidential debate (Biden-Trump)",
    "Assassination attempt on Trump, Butler PA",
    "Vance named Republican VP nominee",
    "Biden withdraws, endorses Harris",
    "Walz named Democratic VP nominee",
    "Democratic National Convention opens",
    "Harris-Trump debate; false Springfield claim aired",
    "Vance-Walz vice-presidential debate",
    "Election Day"))

write.csv(events, "derived/campaign_events_2024.csv", row.names = FALSE)
cat("wrote", nrow(events), "labelled events\n\ndone.\n")

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
