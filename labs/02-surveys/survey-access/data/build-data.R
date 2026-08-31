# ---------------------------------------------------------------------------
# Build the survey-access dataset: what a script gets when it asks an archive.
#
# SOURCE. raw/probe-2026-08-13.csv, frozen, made by probe.py. Six addresses --
# the CPS voting supplement, the GSS cumulative file, the ANES cumulative file's
# download page, two Harvard Dataverse datasets, and an ICPSR study page -- each
# requested twice with BYTE-IDENTICAL headers from two different HTTP clients.
#
# THE TWO CLIENTS ARE THE DESIGN. A wall that read the request would answer both
# the same way. One of the six does not, which is the chapter's finding: what is
# being judged is the client, not the request, and no amount of setting a
# User-Agent changes it.
#
# WHAT THIS SCRIPT DOES NOT CLAIM. It does not say any archive is refusing
# access as a matter of policy. Every one of these surveys is free, and two of
# the four walled ones are openly licensed. A bot wall is infrastructure, and
# the distance between "this data is public" and "a program can fetch it" is
# exactly what is being measured.
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
  say("  check: %-56s %s", label, value); invisible(value)
}

p <- read.csv(sprintf("raw/probe-%s.csv", SCAN_DATE), colClasses = "character")
p$bytes <- as.numeric(p$bytes)
p$wall  <- ifelse(nzchar(p$cf_mitigated), "Cloudflare",
           ifelse(nzchar(p$waf_action),  "AWS WAF", ""))

fact("scan_date", SCAN_DATE, "the day every address was requested")
fact("n_addresses", length(unique(p$archive)), "archives probed")
fact("n_probes", nrow(p), "requests made, two clients per address")
check("every address was tried with both clients",
      all(table(p$archive) == 2))

# --- open or walled ---------------------------------------------------------
#
# An address is OPEN when both clients got the thing; WALLED when either was
# answered by a bot wall instead of a server.

by_arch <- split(p, p$archive)
status <- do.call(rbind, lapply(by_arch, function(d) {
  d <- d[order(d$client), ]
  data.frame(archive = d$archive[1],
             what = d$what_the_address_is[1],
             curl = paste0(d$http[d$client == "curl"]),
             urllib = paste0(d$http[d$client == "python-urllib"]),
             wall = paste(unique(d$wall[nzchar(d$wall)]), collapse = "/"),
             max_bytes = max(d$bytes),
             open = all(d$http == "200") && !any(nzchar(d$wall)))
}))
row.names(status) <- NULL
status <- status[order(!status$open, status$archive), ]
dd_write_csv(status, "derived/status.csv")

fact("n_open",   sum(status$open),  "archives that answered both clients with the data")
fact("n_walled", sum(!status$open), "that put a bot wall in front of at least one")
check("the split is not trivial -- some open, some walled",
      sum(status$open) > 0 && sum(!status$open) > 0)

# --- the two walls, and how legible each refusal is -------------------------

walls <- unique(p$wall[nzchar(p$wall)])
fact("n_wall_kinds", length(walls), "distinct wall technologies among them")
fact("wall_names", paste(sort(walls), collapse = " and "), "which ones")

refusal <- data.frame(
  http = c("403", "202"),
  wall = c("Cloudflare", "AWS WAF"),
  archives = c(paste(sort(unique(p$archive[p$http == "403"])), collapse = "; "),
               paste(sort(unique(p$archive[p$http == "202"])), collapse = "; ")),
  what_a_program_sees = c(
    "an error. The request failed and the code says so.",
    "a success. 202 is Accepted, and the body is a challenge page."))
dd_write_csv(refusal, "derived/refusal.csv")

n202 <- length(unique(p$archive[p$http == "202"]))
fact("n_403", length(unique(p$archive[p$http == "403"])), "archives that refuse with an error code")
fact("n_202", n202, "that refuse with a success code")
fact("bytes_202", unique(p$bytes[p$http == "202"]),
     "bytes of challenge page served under that success code")
check("a success-shaped refusal is present in this corpus again",
      n202 > 0)

# --- the address whose answer depends on the client -------------------------

disagree <- status[status$curl != status$urllib, ]
fact("n_disagree", nrow(disagree), "addresses that answered the two clients differently")
if (nrow(disagree)) {
  fact("disagree_archive", disagree$archive[1], "the archive that did so")
  fact("disagree_curl",    disagree$curl[1],    "what curl was told")
  fact("disagree_urllib",  disagree$urllib[1],  "what Python was told, with identical headers")
  fact("disagree_bytes",   disagree$max_bytes[1], "bytes the client that got through received")
}
dd_write_csv(disagree, "derived/client_dependent.csv")
check("at least one wall judges the client rather than the request",
      nrow(disagree) >= 1)

# --- is the Dataverse wall a property of one dataset or the platform? -------

dv <- p[grepl("Dataverse", p$archive), ]
fact("dv_datasets", length(unique(dv$archive)), "Dataverse datasets probed")
fact("dv_walled",   length(unique(dv$archive[nzchar(dv$wall)])), "of them behind the wall")
check("the Dataverse wall is the platform, not one dataset's setting",
      length(unique(dv$archive[nzchar(dv$wall)])) == length(unique(dv$archive)))

fact("gss_bytes", max(p$bytes[grepl("GSS", p$archive)]),
     "bytes the GSS hands any client with no gate at all")

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
