# Inline helpers for the shared syllabus theme.
#
# Source these from the setup chunk of a course syllabus/syllabus.Rmd:
#
#   source("../../_syllabus-template/syllabus-helpers.R")
#
# Each helper emits whatever the format being rendered understands, so the
# same Rmd still produces a clean PDF and a readable GitHub markdown file.
#
# Numbered sidenotes need no helper at all — write an ordinary markdown
# footnote, ^[like this], and the theme puts it in the margin.

# Which format is knitr rendering to right now?
.syllabus_fmt <- function() {
  to <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (is.null(to)) return("markdown")
  if (grepl("^html", to)) return("html")
  if (identical(to, "latex") || identical(to, "beamer")) return("latex")
  "markdown"
}

#' An unnumbered note in the margin (Tufte's margin note).
#'
#'   Turnout fell again in 2022.`r mnote("Voting-eligible population, not
#'   registered voters — the two diverge by about eight points.")`
#'
#' HTML: a margin note with a ⊕ toggle on narrow screens.
#' PDF:  \ddmarginnote, set in the margin column.
#' gfm:  a trailing parenthetical, since GitHub has no margin.
mnote <- function(text) {
  text <- paste(text, collapse = " ")
  switch(.syllabus_fmt(),
    html     = sprintf('<span class="marginnote">%s</span>', text),
    latex    = sprintf("\\ddmarginnote{%s}", text),
    sprintf(" (%s)", text)
  )
}

#' A small-caps opener for the first words of a section.
#'
#'   `r newthought("The lab write-up")` is the heart of the course.
newthought <- function(text) {
  text <- paste(text, collapse = " ")
  switch(.syllabus_fmt(),
    html     = sprintf('<span class="newthought">%s</span>', text),
    latex    = sprintf("\\ddnewthought{%s}", text),
    text
  )
}

#' Open a block that spans the text column *and* the margin column —
#' for a wide table or figure. Close it with end_fullwidth().
#'
#'   `r fullwidth()`
#'
#'   | a wide table |
#'
#'   `r end_fullwidth()`
#'
#' PDF and gfm ignore it; only the HTML has a margin to reclaim.
fullwidth <- function() {
  if (identical(.syllabus_fmt(), "html")) '<div class="fullwidth">' else ""
}

end_fullwidth <- function() {
  if (identical(.syllabus_fmt(), "html")) "</div>" else ""
}


# ---- table alignment -------------------------------------------------------
#
# The house rule for every table in a brief: the first column carries the
# label and reads left; every other column carries a value and reads right,
# with its header right along with it. Digits only line up place over place
# when the column is flushed right, and a header parked over the far end of
# its own column is the reason a reader has to look twice to see which
# number belongs to which heading.
#
# This has to be decided here, in R, rather than in brief.css, because the
# stylesheet only reaches the HTML. The PDF takes its column spec from the
# markdown table pandoc is handed, and that spec comes from kable(align=) —
# so one vector, passed once, is what keeps the two formats saying the same
# thing:
#
#   knitr::kable(x, col.names = n, row.names = FALSE, align = table_align(x))
#
# THE ONE EXCEPTION IS PROSE. Some of these tables carry a column of
# sentences — "Meaning", "Why it matters", "What the column actually is" —
# and a sentence flushed right is a ragged left edge on every line of it,
# which is unreadable in a way a right-flushed number never is. So a column
# whose widest cell runs past `prose` characters is left where a reader can
# find the start of each line. The test is deliberately crude and
# deliberately about width, not type: a number stored as text ("0097498",
# "2025 to 2025") is nowhere near 25 characters and stays right, and a
# genuinely numeric column stays right however it is formatted.
#
# TO SPECIFY SOMETHING ELSE for one table, set an `align` attribute on the
# data frame before it prints. That wins outright, on screen and on paper:
#
#   attr(d, "align") <- "llr"          # or c("l", "l", "r")
#
# One letter per column, l / r / c, and the count has to match ncol(d) —
# a mismatch is an error here rather than a silently misaligned table.
table_align <- function(x, prose = 25) {
  spec <- attr(x, "align")
  if (!is.null(spec)) {
    spec <- unlist(strsplit(as.character(spec), ""))
    bad <- setdiff(spec, c("l", "r", "c"))
    if (length(bad)) {
      stop("align attribute has ", paste(sQuote(bad), collapse = ", "),
           "; use l, r or c")
    }
    if (length(spec) != ncol(x)) {
      stop("align attribute gives ", length(spec), " column(s) for a table of ",
           ncol(x))
    }
    return(spec)
  }
  if (ncol(x) == 0L) return(character(0))

  a <- vapply(x, function(v) {
    if (is.numeric(v) || is.logical(v) || inherits(v, c("Date", "POSIXt"))) return("r")
    s <- as.character(v)
    s <- s[!is.na(s) & nzchar(s)]
    if (length(s) && max(nchar(s)) > prose) "l" else "r"
  }, character(1))

  a[1] <- "l"                      # the first column is the label, always
  unname(a)
}


# ---- editorial components --------------------------------------------------
#
# Small pieces of furniture borrowed from the course-notes design: a tinted
# callout, a muted standfirst, a grid of figures, a status badge. Each degrades
# to something readable in the PDF and in the GitHub markdown.

.esc_tex <- function(x) {
  # only the characters these snippets realistically contain
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  gsub("~", "\\\\textasciitilde{}", x)
}

#' A tinted callout — the one thing on the page that must not be missed.
#'
#'   `r note("**Bring a laptop on Thursdays.** If you do not have one, tell me.")`
#'
#' Put it on its own line, with a blank line either side.
note <- function(text) {
  text <- paste(text, collapse = " ")
  switch(.syllabus_fmt(),
    html  = sprintf('<div class="note">%s</div>', text),
    latex = sprintf("\\begin{ddnote}%s\\end{ddnote}", text),
    sprintf("> %s", text)
  )
}

.esc_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

#' A verbatim prompt the reader can paste into an AI assistant to rebuild the
#' chapter's data from its original source. Text is authored in the chapter's
#' data/ai-prompt.txt and passed through readLines(); the call sits in its own
#' results="asis" chunk at the end of ## Sources.
#'
#'   cat(ai_prompt(readLines("data/ai-prompt.txt")))
#'
#' `tone = "rebuild"` (default) is the box for a source a stranger can fetch
#' today; `tone = "frozen"` is the amber variant for a source that cannot be
#' fetched again, whose text explains why and what to do instead. The title is
#' fixed by the tone so every box in the book reads the same.
ai_prompt <- function(text, tone = c("rebuild", "frozen")) {
  tone  <- match.arg(tone)
  text  <- paste(text, collapse = "\n")
  title <- if (tone == "rebuild") "Rebuild this data yourself, with an AI assistant"
           else                   "Why this data cannot be fetched again"
  switch(.syllabus_fmt(),
    html  = sprintf(paste0(
      '<div class="ai-prompt%s">',
      '<div class="ai-prompt-title">%s</div>',
      '<pre class="ai-prompt-text">%s</pre>',
      '</div>'),
      if (tone == "frozen") " frozen" else "", title, .esc_html(text)),
    latex = sprintf("\\begin{ddaiprompt}{%s}{%s}\n%s\n\\end{ddaiprompt}",
                    tone, title, .esc_tex(text)),
    sprintf("> **%s**\n\n```\n%s\n```", title, text)
  )
}

#' A muted standfirst, for the sentence that sets up a section.
lede <- function(text) {
  text <- paste(text, collapse = " ")
  switch(.syllabus_fmt(),
    html  = sprintf('<p class="lede">%s</p>', text),
    latex = sprintf("\\begin{ddlede}%s\\end{ddlede}", text),
    text
  )
}

#' A row of figures with micro-labels beneath them, for the numbers a reader
#' wants at a glance — grade weights, counts, dates.
#'
#'   `r statgrid(c("15%" = "Participation", "40%" = "Labs", "20%" = "Final"))`
#'
#' Names are the big figures; values are the labels under them.
statgrid <- function(x) {
  figures <- names(x)
  labels  <- unname(x)
  if (is.null(figures)) stop("statgrid() needs a named vector: c(\"15%\" = \"Participation\")")

  switch(.syllabus_fmt(),
    html = paste0(
      '<div class="stats">',
      paste0(sprintf('<div class="stat"><b>%s</b><span>%s</span></div>', figures, labels),
             collapse = ""),
      "</div>"
    ),
    latex = sprintf("\\begin{ddstats}{%d}%s\\\\\\end{ddstats}",
                    length(x),
                    paste0(sprintf("\\ddstat{%s}{%s}",
                                   .esc_tex(figures), .esc_tex(labels)), collapse = " & ")),
    paste0("**", figures, "** ", labels, collapse = " · ")
  )
}

#' A small status badge. `tone` is one of "accent" (CMU red, the default),
#' "built" (green), "gap" (amber), "free" (blue).
pill <- function(text, tone = c("accent", "built", "gap", "free")) {
  tone <- match.arg(tone)
  text <- paste(text, collapse = " ")
  switch(.syllabus_fmt(),
    html  = sprintf('<span class="pill %s">%s</span>',
                    if (tone == "accent") "" else tone, text),
    latex = sprintf("\\ddpill{%s}{%s}", tone, .esc_tex(text)),
    sprintf("`%s`", text)
  )
}


# ---- fig.margin ------------------------------------------------------------
#
# `fig.margin = TRUE` is a tufte-package chunk option, and we are not using the
# tufte output formats — so wire it up by hand against the margin apparatus the
# theme already has.
#
#   ```{r echo=FALSE, fig.margin=TRUE, fig.width=3.2, fig.height=2.6, fig.cap="..."}
#   plot_absence_curve()
#   ```
#
# The margin column is about 1.8in wide, so set fig.width to roughly 3.2 and let
# the graphic scale down; a figure drawn at full text width and squeezed in here
# arrives with unreadable axis labels.
#
# PDF  routes through \ddmarginnote, so a margin figure inherits the same
#      parking and footnote-fallback behaviour as a margin note.
# HTML emits .marginfigure, styled off .marginnote in syllabus.css.
# gfm  falls through to the ordinary inline figure — GitHub has no margin.
local({
  .default_plot <- knitr::knit_hooks$get("plot")

  knitr::knit_hooks$set(plot = function(x, options) {
    if (!isTRUE(options$fig.margin)) return(.default_plot(x, options))

    cap <- options$fig.cap
    if (is.null(cap)) cap <- ""

    switch(.syllabus_fmt(),
      latex = sprintf("\\ddmarginnote{\\includegraphics[width=\\marginparwidth]{%s}%s}",
                      x,
                      if (nzchar(cap)) sprintf("\\par\\vspace{0.35em}%s", cap) else ""),
      html  = sprintf('<span class="marginfigure"><img src="%s" alt="%s" />%s</span>',
                      x, cap,
                      if (nzchar(cap))
                        sprintf('<span class="marginfigure-caption">%s</span>', cap) else ""),
      .default_plot(x, options)
    )
  })
})


# ---------------------------------------------------------------------------
# STATIC FALLBACKS FOR THE JAVASCRIPT FIGURES
#
# Most figures in this book draw themselves with d3, into a <div> that is empty
# until the page loads. That is fine in a browser and useless everywhere else.
# Mail and messaging clients strip <script> before they render anything, so a
# brief sent as an attachment arrives with holes where the figures were. The
# file is not broken and nothing is missing from it; the pictures simply were
# never in it, only the instructions for drawing them.
#
# Every such figure already has a static twin, drawn in base R for the PDF and
# labelled `<something>-static`. These two hooks emit that twin into the HTML
# as well, wrapped in a marker div, and the sweep in brief-head.html removes it
# again in any browser where the interactive figure actually drew. A reader
# with JavaScript sees exactly what they saw before. A reader without it now
# sees the figure instead of a gap.
#
# WHY THE LABEL. `grepl("static", label)` is the rule because it is exactly the
# set of chunks the book already disables for HTML: every one of the 319 such
# chunks carries "static" in its label, and no chunk carrying "static" is
# enabled for HTML. The guard on isFALSE(options$eval) means this can only ever
# switch ON a chunk the brief had switched off, never the reverse.
#
# The PDF path is untouched. Both hooks return early unless output is HTML.
# ---------------------------------------------------------------------------
local({
  is_html <- function() identical(.syllabus_fmt(), "html")

  knitr::opts_hooks$set(eval = function(options) {
    if (!is_html()) return(options)
    if (!isFALSE(options$eval)) return(options)
    if (!grepl("static", options$label)) return(options)
    options$eval        <- TRUE
    options$echo        <- FALSE
    # results="hide" so a fallback chunk contributes its PLOT and nothing else.
    # A few of these chunks print an object on the way past, which in the PDF
    # is wanted and in the HTML would duplicate a table already on the page.
    options$results     <- "hide"
    options$message     <- FALSE
    options$warning     <- FALSE
    options$dd_fallback <- TRUE
    options
  })

  # Composed with whatever plot hook is already installed above, not replacing
  # it, so a margin figure that is also a fallback keeps its margin treatment.
  .prev_plot <- knitr::knit_hooks$get("plot")

  knitr::knit_hooks$set(plot = function(x, options) {
    out <- .prev_plot(x, options)
    if (!isTRUE(options$dd_fallback)) return(out)
    # A pandoc fenced div, not raw <div> tags: markdown inside a raw HTML block
    # is passed through untouched, which would leave the image unrendered.
    paste0("\n\n::: dd-fallback\n", out, "\n:::\n\n")
  })
})

#' A row of small month grids showing which days the class meets.
#'
#' Drawn with base graphics, so the same figure lands in the HTML, the PDF and
#' the GitHub markdown — no JavaScript, nothing to fall back to. Call it from a
#' chunk, not inline:
#'
#'   ```{r meetings, echo=FALSE, fig.width=7.2, fig.height=1.6}
#'   class_calendar("2026-08-25", "2026-12-03",
#'                  cancelled = c("2026-09-15", "2026-11-03"))
#'   ```
#'
#' `days` are ISO weekday names; `cancelled` are days the class does not meet
#' (breaks, holidays, anything you have called off). A cancelled day is drawn
#' as an empty outline, so the gaps in the term read as deliberate rather than
#' as missing squares. List every day of a closure, not just the ones that fall
#' on a meeting day: a fall break given as Mon-Fri draws as the whole week,
#' which is what a reader looking for the break expects to see.
#' Colours. One PNG is shown on the light page (#EFF1F2) and the dark one
#' (#101418), so every ink here has to clear 3:1 against BOTH — which caps any
#' single value at 4.04:1 and rules out most of the palette. Carnegie Red
#' manages 5.33:1 and 3.06:1, so it carries the meeting squares, with white
#' numerals on it at 6.04:1. The greys are CMU Iron Gray and a tint of it;
#' the tint replaced a lighter grey that failed the light page at 2.64:1.
#' Re-check both surfaces before changing any of these.
class_calendar <- function(start, end, days = c("Tue", "Thu"),
                           cancelled = character(0), remote = character(0),
                           per_row = NULL, legend = TRUE,
                           accent = "#C41230",
                           ink = "#6D6E71", faint = "#838488") {
  start <- as.Date(start); end <- as.Date(end)
  cancelled <- if (length(cancelled)) as.Date(cancelled) else as.Date(character(0))
  remote    <- if (length(remote))    as.Date(remote)    else as.Date(character(0))
  iso <- c(Mon = 1L, Tue = 2L, Wed = 3L, Thu = 4L, Fri = 5L, Sat = 6L, Sun = 7L)
  want <- unname(iso[days])
  if (anyNA(want)) stop("class_calendar(): days must be Mon..Sun")

  # %u is 1=Monday..7=Sunday and is locale-independent, unlike weekdays()
  dow    <- function(d) as.integer(format(d, "%u"))
  col_of <- function(d) (dow(d) %% 7L) + 1L            # Sunday-first grid
  mstart <- function(d) as.Date(format(d, "%Y-%m-01"))
  row_of <- function(d) {
    fcol <- col_of(mstart(d))
    ceiling((as.integer(format(d, "%d")) + fcol - 1L) / 7)
  }

  months <- seq(mstart(start), mstart(end), by = "month")
  nm <- length(months)
  # A row of five months squeezes the day numbers to nothing once the figure is
  # scaled into a text column, so wrap after three unless told otherwise.
  if (is.null(per_row)) per_row <- if (nm > 3) 3L else nm
  CW <- 7 + 1.6                                        # cells across, plus gutter
  RH <- 8.6                                            # month title + header + 6 weeks
  nr <- ceiling(nm / per_row)

  op <- graphics::par(mar = c(0, 0, 0, 0)); on.exit(graphics::par(op), add = TRUE)
  # Without the legend the panel can stop just under the last week row; with
  # it, leave the gutter the swatches sit in.
  ybot <- if (legend) (nr - 1) * RH + 8.6 else (nr - 1) * RH + 6.8
  plot(NA, xlim = c(-0.3, per_row * CW - 1.3),
       ylim = c(ybot, -1.9), asp = 1,
       axes = FALSE, ann = FALSE, xaxs = "i", yaxs = "i")

  for (mi in seq_along(months)) {
    m0 <- months[mi]
    ox <- ((mi - 1) %% per_row) * CW
    oy <- ((mi - 1) %/% per_row) * RH
    ndays <- as.integer(format(seq(m0, by = "month", length.out = 2)[2] - 1, "%d"))
    text(ox, oy - 1.15, format(m0, "%B"), adj = c(0, 0.5), cex = 0.78, font = 2,
         col = ink, xpd = NA)
    for (i in 1:7)
      text(ox + i - 0.5, oy, c("S","M","T","W","T","F","S")[i],
           cex = 0.56, col = faint, xpd = NA)
    for (dd in 1:ndays) {
      d <- m0 + (dd - 1)
      interm <- d >= start && d <= end
      # A cancelled day is drawn whatever weekday it falls on, so a whole
      # break week reads as a block rather than as two missing meetings.
      off   <- interm && (d %in% cancelled)
      meets <- interm && dow(d) %in% want && !off
      away  <- meets && (d %in% remote)
      x <- ox + col_of(d) - 0.5; y <- oy + row_of(d)
      if (away) {
        # ruled, and grey throughout: the accent means "be in the room", so a
        # remote day does not get it. It is boxed and hatched in `faint`, the
        # same grey the closed days are drawn in, which puts both kinds of
        # not-in-the-room on one side of the accent and leaves the eye a single
        # colour to hunt for.
        rect(x - 0.44, y - 0.42, x + 0.44, y + 0.42, col = faint,
             density = 18, angle = 45, lwd = 0.7, border = faint)
        text(x, y, dd, cex = 0.54, col = faint)
      } else if (meets) {
        # solid: the ordinary case, and the one the eye should find first
        rect(x - 0.44, y - 0.42, x + 0.44, y + 0.42, col = accent, border = NA)
        text(x, y, dd, cex = 0.54, col = "white")
      } else if (off) {
        # an empty box: a closure is an absence, so nothing fills it
        rect(x - 0.44, y - 0.42, x + 0.44, y + 0.42, col = NA, border = faint,
             lwd = 0.7)
        text(x, y, dd, cex = 0.54, col = faint)
      } else {
        text(x, y, dd, cex = 0.54, col = faint)
      }
    }
  }
  # key, on one line under the months
  if (!legend) return(invisible(NULL))
  ky <- (nr - 1) * RH + 7.9
  # each swatch drawn exactly as the day squares are, so the key cannot drift
  # from the grid it explains
  sw <- function(x, style) {
    xl <- x; xr <- x + 0.7; yb <- ky - 0.34; yt <- ky + 0.34
    if (style == "solid")   rect(xl, yb, xr, yt, col = accent, border = NA)
    if (style == "hatch")   rect(xl, yb, xr, yt, col = faint, density = 18,
                                 angle = 45, lwd = 0.7, border = faint)
    if (style == "empty")   rect(xl, yb, xr, yt, col = NA, border = faint, lwd = 0.7)
  }
  lb <- function(x, s) text(x, ky, s, adj = c(0, 0.5), cex = 0.56, col = ink, xpd = NA)
  sw(0.06, "solid");  lb(1.0,  "in person")
  sw(6.2,  "hatch");  lb(7.15, "remote")
  sw(12.4, "empty");  lb(13.3, "no class")
  invisible(NULL)
}

# ---- title_block -----------------------------------------------------------
# The markdown build targets gfm-yaml_metadata_block, which strips the YAML
# title block, so the author and updated date that head the HTML and PDF never
# reach the .md — and so never reach Canvas, where they used to be retyped by
# hand. Emit them as headings for the markdown build only; HTML and PDF already
# render them from the YAML, and emitting there would duplicate.
#
#   ```{r title-block, echo=FALSE, results="asis"}
#   title_block()                                    # or a prefix, e.g.
#   title_block(if (params$full) "DRAFT " else "")
#   ```
#
# is_html_output() is TRUE for gfm, so the test has to be on the pandoc target.
# metadata$date is the RAW yaml string, inline R and all, so the date is built
# here instead of reused.
title_block <- function(prefix = "") {
  to <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (is.null(to) || !grepl("^gfm", to)) return(invisible(NULL))
  cat(sprintf("#### %s\n\n#### %sUpdated: %s\n\n",
              rmarkdown::metadata$author, prefix,
              format(Sys.time(), "%B %d, %Y")))
  invisible(TRUE)
}

# ---- figure_device ---------------------------------------------------------
# The calendar is line art set full-width in the PDF and at 100% of the text
# column in HTML, so a 150dpi raster shows its pixels at both sizes — and on a
# 2x display the HTML is being asked for roughly double what it has.
#
# Use a vector device wherever the format takes one:
#   latex  -> pdf, sharp at any size in print, and a smaller file than a raster
#   html   -> svg, sharp on any display; self_contained base64-embeds it
#   gfm    -> png, the one target that cannot take a vector, so raise the dpi
#             instead. This is also the file Canvas links to.
#
# is_html_output() answers TRUE for gfm, so gfm has to be ruled out first, on
# the pandoc target itself.
figure_device <- function(dpi = 300) {
  to <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  gfm <- length(to) == 1 && grepl("^gfm", to)
  if (!gfm && knitr::is_latex_output()) {
    knitr::opts_chunk$set(dev = "pdf", dev.args = list(bg = "transparent"))
  } else if (!gfm && knitr::is_html_output()) {
    knitr::opts_chunk$set(dev = "svg", dev.args = list(bg = "transparent"))
  } else {
    knitr::opts_chunk$set(dev = "png", dpi = dpi,
                          dev.args = list(bg = "transparent"))
  }
  invisible(knitr::opts_chunk$get("dev"))
}
