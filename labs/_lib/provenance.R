# ---------------------------------------------------------------------------
# provenance.R — make a build script notice when its source moves.
#
# WHY THIS EXISTS
#
# The campaign-visits lab pinned an Associated Press dataset at version /50/
# and captured 323 events. By the time anyone looked again the AP was serving
# /54/ with 376 events — the committed file stopped three days before the
# election and was missing the entire closing push. Nothing failed. Nothing
# warned. The build script ran clean and produced stale data.
#
# That is the failure mode this file exists to catch: a URL that still returns
# 200, still parses, and no longer means what it meant.
#
# WHAT IT DOES
#
# Wrap a download and it records, in `PROVENANCE.tsv` next to the data:
#
#     url, first_seen, last_seen, bytes, sha256, rows
#
# On every later run it compares. If the bytes, hash or row count changed it
# prints a loud banner saying WHAT changed and BY HOW MUCH — then carries on,
# because a source legitimately updating is normal and should not break a
# build. The point is that a human sees it.
#
# USE
#
#     source("../../../_lib/provenance.R")       # from a lab's data/ folder
#     prov_fetch(URL, "raw.csv")                 # instead of download.file()
#     d <- prov_read_csv(URL)                    # instead of read.csv(URL)
#     prov_report()                              # at the end of the script
#     prov_stamp()                               # at the end of the script
#
# `prov_fetch` returns the destination path, so it drops into existing code:
#     download.file(u, f)      ->  prov_fetch(u, f)
#     read.csv(u)              ->  prov_read_csv(u)
# ---------------------------------------------------------------------------

PROV_FILE <- "PROVENANCE.tsv"
.prov_changes <- new.env(parent = emptyenv())
.prov_changes$rows <- list()

# When the R process started -- NOT when this file was sourced. prov_stamp()
# uses it to tell what this run wrote from what was already lying in the
# directory, and a script that sources the helper on its last line rather than
# its first would otherwise appear to have produced nothing at all.
# proc.time()["elapsed"] is wall-clock seconds since the session began.
PROV_START <- Sys.time() - as.numeric(proc.time()[["elapsed"]])

.prov_load <- function() {
  if (!file.exists(PROV_FILE)) {
    return(data.frame(url = character(), first_seen = character(),
                      last_seen = character(), bytes = numeric(),
                      sha256 = character(), rows = character(),
                      stringsAsFactors = FALSE))
  }
  read.delim(PROV_FILE, stringsAsFactors = FALSE, colClasses = "character")
}

.prov_sha <- function(path) {
  # tools::md5sum is base; prefer sha256 via openssl if available, else md5.
  #
  # THE CONNECTION MUST BE OPENED "rb". file(path) opens in TEXT mode, and on a
  # binary source -- an .RData, a .dta, a .zip -- openssl then digests something
  # no other tool reproduces. A hash nobody else can compute is not a hash: it
  # cannot be checked against the publisher, and `stamp-builds.py`, which reads
  # raw bytes, reports every such file as "content changed" forever after.
  # Caught on media-ideology/raw/PolShares.RData, where text mode gave
  # 1d734c32... and the true digest is fa320555...  CSV captures were unaffected,
  # which is why this survived: every earlier raw capture in the corpus is text.
  # THE CONNECTION MUST ALSO BE CLOSED. openssl reads it to the end but leaves
  # it open, so a bare sha256(file(path, "rb")) leaks one connection per file
  # and the garbage collector later prints "closing unused connection N" for
  # every one of them -- a build that stamps 11 files ends in 11 warnings that
  # look like they belong to the build. Streamed rather than read whole,
  # because raw captures are kept at whatever size they arrived.
  if (requireNamespace("openssl", quietly = TRUE)) {
    con <- file(path, "rb")
    on.exit(close(con), add = TRUE)
    return(paste0("sha256:", as.character(openssl::sha256(con))))
  }
  paste0("md5:", unname(tools::md5sum(path)))
}

.prov_rows <- function(path) {
  # Cheap row estimate. Not meaningful for binary formats; returns NA there.
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("csv", "tsv", "tab", "txt", "json", "xml")) return(NA_character_)
  n <- tryCatch(length(readLines(path, warn = FALSE)), error = function(e) NA_integer_)
  if (is.na(n)) return(NA_character_)
  as.character(if (ext %in% c("csv", "tsv", "tab")) max(n - 1L, 0L) else n)
}

# Strip credentials before anything is written to disk. PROVENANCE.tsv is a
# committed file, so a key in a query string would be published with the lab.
# This happened once: an ACS build wrote a live Census key into 22 rows.
.prov_scrub <- function(url) {
  gsub("([?&](key|api_key|apikey|token|access_token|signature|secret)=)[^&#]*",
       "\\1REDACTED", url, ignore.case = TRUE)
}

.prov_record <- function(url, path, label = NULL) {
  url <- .prov_scrub(url)
  p   <- .prov_load()
  now <- format(Sys.Date())
  new <- list(url = url, bytes = as.character(file.size(path)),
              sha256 = .prov_sha(path), rows = .prov_rows(path))
  hit <- which(p$url == url)

  if (length(hit) == 0) {
    p <- rbind(p, data.frame(url = url, first_seen = now, last_seen = now,
                             bytes = new$bytes, sha256 = new$sha256,
                             rows = new$rows, stringsAsFactors = FALSE))
    cat(sprintf("  [prov] first capture: %s  (%s bytes%s)\n",
                basename(path), new$bytes,
                if (is.na(new$rows)) "" else paste0(", ", new$rows, " rows")))
  } else {
    old <- p[hit[1], ]
    moved <- c(
      if (!identical(old$bytes,  new$bytes))  sprintf("bytes %s -> %s", old$bytes,  new$bytes),
      if (!identical(old$sha256, new$sha256)) "content hash changed",
      if (!identical(old$rows,   new$rows) && !is.na(new$rows))
        sprintf("rows %s -> %s (%+d)", old$rows, new$rows,
                suppressWarnings(as.integer(new$rows) - as.integer(old$rows))))
    if (length(moved)) {
      .prov_changes$rows[[length(.prov_changes$rows) + 1]] <-
        list(url = url, file = basename(path), first = old$first_seen,
             moved = moved, label = label)
    }
    p[hit[1], c("last_seen", "bytes", "sha256", "rows")] <-
      list(now, new$bytes, new$sha256, new$rows)
  }
  write.table(p, PROV_FILE, sep = "\t", row.names = FALSE, quote = FALSE)
  invisible(path)
}

#' Download a URL and record its provenance.
prov_fetch <- function(url, dest, label = NULL, mode = "wb", quiet = TRUE, ...) {
  utils::download.file(url, dest, mode = mode, quiet = quiet, ...)
  .prov_record(url, dest, label)
  # Invisible, like download.file(). A bare prov_fetch() at the top of a script
  # would otherwise auto-print its return value, and every build log would
  # carry a line of [1] "raw/whatever.zip" for each file it collected.
  invisible(dest)
}

#' read.csv() from a URL, with provenance recorded.
prov_read_csv <- function(url, label = NULL, ...) {
  tmp <- tempfile(fileext = paste0(".", tools::file_ext(sub("\\?.*$", "", url))))
  utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
  .prov_record(url, tmp, label)
  utils::read.csv(tmp, ...)
}

# ---------------------------------------------------------------------------
# THE BUILD STAMP
#
# PROVENANCE.tsv, above, answers "where did this file come from, and has the
# source moved since". It can only answer that for a file that was downloaded,
# which leaves out about half the corpus: a chapter whose data is simulated,
# derived from another chapter, or typed out of a published table fetches
# nothing, so it records nothing, and the book cannot say anything about it.
#
# BUILD-STAMP.tsv answers the smaller question every chapter can answer --
# which script produced what is sitting in this directory, when it last ran,
# and what the files looked like when it finished:
#
#     script  stamped_on  stamp_source  file  bytes  sha256  rows  file_mtime
#
# `stamp_source` is the honest column. "build" means a running script wrote
# the row and `stamped_on` is a real run date. "disk" means the row was taken
# by reading the directory afterwards -- the sizes and hashes are real, the
# build date is not known, and file_mtime is the best evidence there is. A
# backfilled chapter promotes itself to "build" the next time its script runs.
#
# ONLY WHAT THIS RUN WROTE. Files whose mtime predates PROV_START are left
# with whatever row they already had, rather than being restamped with today's
# date. A build that deliberately reuses a cached download should not be able
# to claim it fetched it this morning, and that is the whole point of the file.
# ---------------------------------------------------------------------------

STAMP_FILE <- "BUILD-STAMP.tsv"

STAMP_COLS <- c("script", "stamped_on", "stamp_source", "file",
                "bytes", "sha256", "rows", "file_mtime")

.stamp_empty <- function() {
  d <- as.data.frame(setNames(rep(list(character()), length(STAMP_COLS)), STAMP_COLS),
                     stringsAsFactors = FALSE)
  d
}

.stamp_load <- function() {
  if (!file.exists(STAMP_FILE)) return(.stamp_empty())
  read.delim(STAMP_FILE, stringsAsFactors = FALSE, colClasses = "character")
}

# Which script is running. Rscript puts it on the command line; source() leaves
# it on the frame stack. R encodes spaces in --file= as "~+~", which matters
# here because these trees live under "My Drive".
.stamp_script <- function() {
  a <- grep("^--file=", commandArgs(), value = TRUE)
  if (length(a)) {
    return(basename(gsub("~+~", " ", sub("^--file=", "", a[1]), fixed = TRUE)))
  }
  for (i in rev(seq_len(sys.nframe()))) {
    of <- sys.frame(i)$ofile
    if (!is.null(of)) {
      b <- basename(as.character(of)[1])
      if (b != "provenance.R") return(b)
    }
  }
  NA_character_
}

# Everything a build is allowed to have produced. Data lives in derived/ and
# raw/ by convention (check-layout.py enforces it), so a file at the top of
# data/ is metadata or a script and is not a build output.
.stamp_outputs <- function() {
  fs <- character()
  for (d in c("derived", "raw")) {
    if (dir.exists(d)) fs <- c(fs, list.files(d, recursive = TRUE, full.names = TRUE))
  }
  fs[!dir.exists(fs)]
}

#' Record which script built what is in this directory, and when.
#'
#' @param outputs  files to stamp. The default, NULL, is everything under
#'                 derived/ and raw/ that THIS RUN wrote. "all" is everything
#'                 there whenever it was written -- for a backfill, or for a
#'                 script whose outputs cannot be told apart by their mtime.
#' @param script   name to record; default is the running script
#' @param source_kind  "build" from a running script, "disk" when backfilling
prov_stamp <- function(outputs = NULL, script = NULL, source_kind = "build") {
  script <- if (is.null(script)) .stamp_script() else script
  all_out <- .stamp_outputs()
  fs <- if (identical(outputs, "all")) all_out
        else if (!is.null(outputs)) outputs
        else {
          info <- file.info(all_out)
          all_out[!is.na(info$mtime) & info$mtime >= PROV_START]
        }

  p <- .stamp_load()
  # Drop rows for files that are no longer there. A stamp that still lists a
  # deleted file is worse than no stamp: it reads as a promise.
  if (nrow(p)) p <- p[p$file %in% all_out, , drop = FALSE]

  if (!length(fs)) {
    write.table(p, STAMP_FILE, sep = "\t", row.names = FALSE, quote = FALSE)
    cat("  [stamp] no new outputs this run;", nrow(p), "row(s) kept\n")
    return(invisible(p))
  }

  now  <- format(Sys.Date())
  info <- file.info(fs)
  new  <- data.frame(
    script       = script,
    stamped_on   = now,
    stamp_source = source_kind,
    file         = fs,
    bytes        = as.character(info$size),
    sha256       = vapply(fs, .prov_sha, character(1), USE.NAMES = FALSE),
    rows         = vapply(fs, .prov_rows, character(1), USE.NAMES = FALSE),
    # format(), not as.Date(): as.Date() on a POSIXct converts in UTC unless
    # told otherwise, which stamps a file written this evening with tomorrow's
    # date and makes file_mtime disagree with stamped_on for no visible reason.
    file_mtime   = format(info$mtime, "%Y-%m-%d"),
    stringsAsFactors = FALSE)

  p <- rbind(p[!p$file %in% new$file, , drop = FALSE], new)
  p <- p[order(p$file), , drop = FALSE]
  write.table(p, STAMP_FILE, sep = "\t", row.names = FALSE, quote = FALSE)
  cat(sprintf("  [stamp] %s: %d file(s) %s %s\n", script, nrow(new),
              if (identical(source_kind, "build")) "built on" else "read off disk on",
              now))
  invisible(p)
}

#' Print the drift banner. Call once at the end of a build script.
prov_report <- function() {
  ch <- .prov_changes$rows
  if (!length(ch)) {
    cat("  [prov] all sources unchanged since last build\n")
    return(invisible(FALSE))
  }
  bar <- strrep("=", 72)
  cat("\n", bar, "\n", sep = "")
  cat("  *** SOURCE DATA CHANGED SINCE THE LAST BUILD ***\n\n")
  for (c_ in ch) {
    cat(sprintf("  %s\n    %s\n    first captured %s\n",
                c_$file, c_$url, c_$first))
    for (m in c_$moved) cat("      - ", m, "\n", sep = "")
    cat("\n")
  }
  cat("  Every figure derived from these files may have moved. Re-check any\n")
  cat("  number quoted in the lab, the key or the brief before trusting it.\n")
  cat(bar, "\n\n", sep = "")
  invisible(TRUE)
}
