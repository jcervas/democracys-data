# Shared brief renderer (HTML + PDF, rendered in place).
#
# BUILD EVERY BRIEF THROUGH THIS FILE. A bare rmarkdown::render() or the RStudio
# Knit button produces an HTML file whose embedded images do not display: pandoc
# 2.19.1 writes the data URIs in the URL-safe base64 alphabet, Chrome refuses to
# decode them, and the console fills with ERR_INVALID_URL. Nothing about the
# failure is loud — the render reports success and the broken image is only
# visible if someone opens the page and scrolls to it. render_brief() runs the
# repair (fix_base64_uris(), below) immediately after the render, which is the
# only thing that makes the output correct. Knitting by any other route silently
# reintroduces the breakage.
#
# Companion to render.R. That file's render_syllabus() is built for a syllabus:
# it forces an out_dir and emits md_document for GitHub. A lab brief wants
# neither — it is knitted next to its own data/ directory and read from there,
# and it has no markdown twin. But it needs the same post-render repair, so
# this file borrows fix_base64_uris() from render.R rather than restating it.
#
# Usage, from anywhere:
#
#   source("/path/to/_syllabus-template/render-brief.R")
#   render_brief("F26/labs/eavs/eavs-brief.Rmd")
#
# or straight from a shell, one or more briefs at a time:
#
#   Rscript /path/to/_syllabus-template/render-brief.R F26/labs/*/*-brief.Rmd
#
# Rendering is done with the working directory set to the brief's own folder,
# because every brief reads its inputs by relative path ("data/facts.csv").
#
# If the corpus ships a `_lib/check-all.sh` above the brief, it is run once
# before the first render and a failure stops the render. Pass every brief to
# one invocation rather than looping in the shell, so the checks run once
# rather than once per process. DD_SKIP_CHECKS=1 turns them off. Corpora with
# no such file are unaffected -- see "corpus checks" below.

# --- locate this file, so render.R can be found beside it --------------------
.brief_this_file <- function() {
  # source(): the frame stack carries ofile. Ask this first, and walk the stack
  # from the innermost frame outwards, so the source() of *this* file wins over
  # both an outer script that sourced it and the --file= of whatever Rscript
  # invoked that script. The briefs' knit: fields source this file, so this is
  # the path that has to be right.
  for (i in rev(seq_len(sys.nframe()))) {
    of <- sys.frame(i)$ofile
    if (!is.null(of)) return(normalizePath(as.character(of)[1], mustWork = FALSE))
  }
  # Rscript render-brief.R: --file=... on the command line. R encodes spaces in
  # that argument as "~+~", which matters here — these trees live under "My Drive".
  a <- grep("^--file=", commandArgs(), value = TRUE)
  if (length(a)) {
    f <- gsub("~+~", " ", sub("^--file=", "", a[1]), fixed = TRUE)
    return(normalizePath(f, mustWork = FALSE))
  }
  NULL
}

local({
  here <- .brief_this_file()
  shared <- if (is.null(here)) "render.R" else file.path(dirname(here), "render.R")
  if (!file.exists(shared)) {
    stop("render-brief.R cannot find render.R beside it (looked at: ", shared, ")")
  }
  # render.R only defines functions; sourcing it has no other effect.
  source(shared, local = FALSE)
})

if (!exists("fix_base64_uris", mode = "function")) {
  stop("render.R did not supply fix_base64_uris()")
}



# --- default output formats, so a brief need not restate them ---------------
#
# Every brief in a corpus wants the same html_document and pdf_document
# options, and repeating them puts a 28-line `output:` block above the title of
# every chapter -- the first thing anyone opening the file has to scroll past.
# When a brief gives no `output:` of its own, the formats below are supplied
# instead, built with ABSOLUTE paths so they do not depend on how deep the
# chapter sits (a brief one directory up used to need its own ../ count).
#
# A brief that DOES declare `output:` is passed through untouched, so this
# changes nothing for any corpus that has not opted in.
#
# Two things stay in the brief because they genuinely vary: `toc_depth:` and
# `md_extensions:`, read here as plain top-level keys.

# Resolved once, HERE, while this file is being sourced -- .brief_this_file()
# reads the frame stack and the command line, and by the time a render is
# under way neither still names this file. Calling it lazily worked under
# `Rscript render-brief.R` and failed under `source()` + render_brief(),
# which is exactly the call an interactive session makes.
.BRIEF_TPL <- local({
  here <- .brief_this_file()
  if (is.null(here)) NULL else dirname(here)
})

brief_default_format <- function(fmt, src, toc_depth = 2L, md_extensions = NULL) {
  tpl <- .BRIEF_TPL
  if (is.null(tpl)) {
    stop("render-brief.R cannot locate its own directory, so the default ",
         "output formats cannot be built; give the brief an explicit `output:`")
  }

  # course-meta.tex belongs to the course, not the template, and sits at an
  # unpredictable height above the brief. Walk up and take the first one.
  meta <- NULL
  d <- dirname(src)
  for (i in 1:8) {
    cand <- file.path(d, "course-meta.tex")
    if (file.exists(cand)) { meta <- normalizePath(cand); break }
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }

  if (identical(fmt, "html_document")) {
    rmarkdown::html_document(
      theme = NULL,
      css = file.path(tpl, "brief.css"),
      toc = TRUE, toc_depth = toc_depth,
      self_contained = TRUE, fig_caption = TRUE, mathjax = NULL,
      pandoc_args = "--mathml",
      md_extensions = md_extensions,
      includes = rmarkdown::includes(
        in_header = file.path(tpl, "brief-head.html")))
  } else if (identical(fmt, "pdf_document")) {
    rmarkdown::pdf_document(
      latex_engine = "xelatex", fig_caption = TRUE, number_sections = FALSE,
      md_extensions = md_extensions,
      includes = rmarkdown::includes(
        in_header = c(meta, file.path(tpl, "brief-preamble.tex"))))
  } else {
    fmt
  }
}

# --- corpus checks, before anything is rendered ------------------------------
#
# A corpus may ship its own checks. If a `_lib/check-all.sh` sits above the
# brief, it runs once before the first render and the render stops if it fails.
# Rendering is the expensive step and the published artifact; a check that runs
# afterwards is a check nobody reads.
#
# NOTHING HERE IS COURSE-SPECIFIC. The hook looks for the file and does nothing
# if there isn't one, so this template stays shared: a course opts in by adding
# `_lib/check-all.sh` to its corpus, and every other course is unaffected.
#
# ONCE PER SESSION, not once per brief. The checks take about 13 seconds, and
# a 65-brief batch would otherwise spend a quarter of an hour re-answering the
# same question. render_briefs() therefore checks once and the rest inherit it,
# and a shell loop that calls Rscript per brief pays it per process -- which is
# a reason to hand render-brief.R all the briefs at once rather than to loop.
#
#   DD_SKIP_CHECKS=1   skip them, for when the brief you are fixing is the
#                      thing the checks are complaining about
.checks_done <- new.env(parent = emptyenv())

find_checks <- function(from) {
  d <- normalizePath(from, mustWork = FALSE)
  for (i in 1:6) {
    p <- file.path(d, "_lib", "check-all.sh")
    if (file.exists(p)) return(p)
    up <- dirname(d)
    if (identical(up, d)) break
    d <- up
  }
  NULL
}

run_corpus_checks <- function(src) {
  if (nzchar(Sys.getenv("DD_SKIP_CHECKS"))) return(invisible(TRUE))
  sh <- find_checks(dirname(normalizePath(src, mustWork = FALSE)))
  if (is.null(sh)) return(invisible(TRUE))
  if (!is.null(.checks_done[[sh]])) return(invisible(.checks_done[[sh]]))

  message("Checking the corpus (", basename(dirname(dirname(sh))), ") ...")
  out <- suppressWarnings(system2("sh", shQuote(sh), stdout = TRUE, stderr = TRUE))
  ok  <- is.null(attr(out, "status")) || identical(attr(out, "status"), 0L)
  .checks_done[[sh]] <- ok
  if (!ok) {
    message(paste(out, collapse = "\n"))
    stop("corpus checks failed; nothing was rendered.\n",
         "  Fix the above, or set DD_SKIP_CHECKS=1 to render anyway.",
         call. = FALSE)
  }
  message("  checks clean")
  invisible(TRUE)
}


#' Render one brief to HTML and PDF, in place, and repair the HTML.
#'
#' @param src      path to a *-brief.Rmd
#' @param formats  which output formats to build
#' @param quiet    passed to rmarkdown::render
#' @return invisibly, a named list of the files written
render_brief <- function(src,
                         formats = c("html_document", "pdf_document"),
                         quiet = TRUE) {
  if (length(src) != 1L || !is.character(src) || is.na(src)) {
    stop("render_brief() takes one file path; use render_briefs() for several")
  }
  if (!file.exists(src)) stop("no such brief: ", src)
  src <- normalizePath(src, mustWork = TRUE)
  run_corpus_checks(src)               # before the expensive part, not after
  dir <- dirname(src)
  base <- basename(src)

  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)

  # Does this brief declare its own output formats? If so it is rendered
  # exactly as before; if not, the shared defaults above stand in.
  ymlf <- tryCatch(rmarkdown::yaml_front_matter(base), error = function(e) list())
  own  <- names(ymlf$output)
  if (is.null(own) && is.character(ymlf$output)) own <- ymlf$output
  tocd <- if (is.null(ymlf$toc_depth)) 2L else as.integer(ymlf$toc_depth)
  mdx  <- ymlf$md_extensions

  out <- list()
  for (fmt in formats) {
    message("  [", fmt, "] ", base)
    target <- if (fmt %in% own) fmt else
      brief_default_format(fmt, src, toc_depth = tocd, md_extensions = mdx)
    # Give the brief its own environment. rmarkdown::render() otherwise
    # evaluates the chunks in parent.frame() — this function's frame — where a
    # brief that happens to name a variable `base`, `src` or `fmt` quietly
    # overwrites the loop's own state mid-render.
    out[[fmt]] <- rmarkdown::render(base, output_format = target, quiet = quiet,
                                    envir = new.env(parent = globalenv()))
  }

  # The one repair. pandoc 2.19.1 writes --embed-resources data URIs in the
  # URL-safe base64 alphabet, which browsers refuse to decode; every embedded
  # figure comes out broken and the console fills with ERR_INVALID_URL.
  if ("html_document" %in% formats) {
    html <- file.path(dir, sub("\\.Rmd$", ".html", base))
    if (!isTRUE(fix_base64_uris(html))) {
      # Only reachable if the HTML render was skipped or wrote somewhere else.
      # Say so rather than returning quietly: an unrepaired brief looks fine
      # until someone opens it.
      warning("no HTML found to repair at ", html,
              " - if that brief has images, they will not display.",
              call. = FALSE)
    }
  }

  invisible(out)
}


#' Render several briefs, carrying on past any that fail.
#'
#' @return a data frame of one row per brief: ok, and the error if not.
render_briefs <- function(srcs, ...) {
  res <- lapply(srcs, function(s) {
    message("Rendering ", s, " ...")
    e <- tryCatch({ render_brief(s, ...); NULL },
                  error = function(e) conditionMessage(e))
    data.frame(brief = s, ok = is.null(e),
               error = if (is.null(e)) "" else e,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, res)
  bad <- out[!out$ok, ]
  message("\n", sum(out$ok), "/", nrow(out), " briefs rendered.")
  if (nrow(bad)) {
    message("Failed:")
    for (i in seq_len(nrow(bad))) message("  ", bad$brief[i], ": ", bad$error[i])
  }
  invisible(out)
}


# Run directly: Rscript render-brief.R <brief.Rmd> [<brief.Rmd> ...]
if (!interactive() && sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args)) render_briefs(args)
}
