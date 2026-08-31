# Chapters now live at labs/NN-part/<slug>/; a chapter with no part still sits
# at the labs root. dd_chapter_dirs() returns full paths for both depths, so
# every walk below is unchanged apart from calling it.
dd_chapter_dirs <- function(root) {
  # Filter on the BASENAME, never on the full path. The corpus lives under
  # .../_teaching/_Democracy's Data/, so an absolute path always contains "/_"
  # and a `grepl("/_", d)` test drops every chapter -- silently, leaving a
  # checker that examines nothing and reports clean.
  keep <- function(p) p[!grepl("^_", basename(p))]
  d <- list.dirs(root, recursive = FALSE)
  d <- keep(d[d != root])
  out <- character(0)
  for (x in d) {
    if (grepl("/[0-9]{2}-[^/]+$", x)) {
      k <- list.dirs(x, recursive = FALSE)
      out <- c(out, keep(k[k != x]))
    } else out <- c(out, x)
  }
  out
}

#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# check-vacuous.R — find checks that cannot fail.
#
#     Rscript _lib/check-vacuous.R              # the whole corpus
#     Rscript _lib/check-vacuous.R --self-test  # prove the checker still works
#     Rscript _lib/check-vacuous.R some.R       # one file
#
# WHY THIS EXISTS
#
# The independent-expenditures chapter kept the rows above $100m in one file and
# then, in another, counted the rows between $50m and $100m. The answer was 0,
# the prose leaned on the 0, and it would have been 0 whatever the data said.
# A check like that is worse than no check: it looks like verification, it
# passes forever, and it goes on passing after the thing it was meant to catch
# has happened.
#
# WHAT IT LOOKS FOR, in three passes
#
#   1. WITHIN A SCRIPT   a filter and a stopifnot() in the same file, where the
#                        filter already settles the assertion.
#   2. ACROSS FILES      a table narrowed on its way to disk, and a condition on
#                        the same column wherever that CSV is read -- including
#                        from another chapter. This is the shape the real bug
#                        had, and neither file is wrong on its own.
#   3. CONSTANT          an assertion with no data in it at all.
#
# HOW IT DECIDES. Each comparison on a column is an interval, and constants are
# folded first, so `> CUT` and `> 1e8` are the same test and `> 1e8` is
# recognised as already implying `> 5e7`. If the filter's interval sits inside
# the assertion's, the assertion is ALWAYS TRUE; if they do not meet, it is
# ALWAYS FALSE; if they merely overlap, it is a real test and is left alone.
#
# NOTHING IS EXECUTED. Folding evaluates only arithmetic on values already
# proven constant -- an earlier version put baseenv() in scope and actually ran
# a build script's pdftotext call. --self-test includes a fixture that would
# write a file if the sandbox ever leaked.
#
# WHAT IT CANNOT SEE. Only tables narrowed by a row filter carry a predicate:
# an aggregate or a merge carries none, so a vacuous check downstream of one has
# nothing to be compared against. Interval logic applies to numeric comparisons;
# character ones fall back to exact text.
# ---------------------------------------------------------------------------

.self <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
.self <- gsub("~+~", " ", .self, fixed = TRUE)
LABS  <- normalizePath(file.path(dirname(.self), ".."), mustWork = FALSE)

# --- constant folding ------------------------------------------------------
#
# A filter written `amount > CUT` and an assertion written `amount > 1e8` are
# the same test, and comparing the two right-hand sides as TEXT says they are
# not. So the script's constants are collected first and each right-hand side is
# evaluated against them: both sides become the number 1e+08 and match.
#
# Evaluation happens in an environment holding only those constants, with
# baseenv() as its parent, so anything touching the data -- `nrow(d)`, a column,
# a variable assigned from a file -- fails to resolve and falls back to text.
# That is the safe direction: an unfoldable right-hand side is compared exactly
# as before, so folding can only ever add matches, never invent them.
# NOTHING IS EVALUATED UNLESS IT IS ARITHMETIC ON CONSTANTS.
#
# The first version of this evaluated each right-hand side in an environment
# whose parent was baseenv(), which meant `system()`, `download.file()` and
# every other base function were in scope -- so analysing the corpus RAN pieces
# of the build scripts, and one of them shelled out to pdftotext. A reader is
# not a runner. Only the operators below are allowed, every symbol has to
# already be a known constant, and anything else is left as text.
SAFE_FNS <- c("+", "-", "*", "/", "^", "(", "c", "%%", "%/%",
              "as.numeric", "as.integer", "as.character",
              "round", "signif", "abs", "sqrt", "min", "max", "log", "exp")

pure_const <- function(e, known) {
  if (is.numeric(e) || is.character(e) || is.logical(e)) return(TRUE)
  if (is.name(e)) return(as.character(e) %in% known)
  if (!is.call(e)) return(FALSE)
  fn <- as.character(e[[1]])[1]
  if (!fn %in% SAFE_FNS) return(FALSE)
  for (i in seq_along(e)[-1]) if (!is.null(e[[i]]) && !pure_const(e[[i]], known)) return(FALSE)
  TRUE
}

collect_constants <- function(ex) {
  # parent is baseenv() so that `+` and friends resolve; the guarantee that
  # nothing dangerous runs comes from pure_const(), which admits only the
  # operators in SAFE_FNS and symbols already proven constant.
  env <- new.env(parent = baseenv())
  for (e in ex) {
    if (!is.call(e) || !as.character(e[[1]])[1] %in% c("<-", "=") || length(e) != 3) next
    if (!is.name(e[[2]])) next
    nm <- as.character(e[[2]])
    if (!pure_const(e[[3]], ls(env))) next
    val <- try(eval(e[[3]], env), silent = TRUE)
    if (inherits(val, "try-error") || !is.atomic(val) || length(val) != 1 || is.na(val)) next
    assign(nm, val, envir = env)
  }
  env
}

fold <- function(txt, env) {
  e <- try(parse(text = txt)[[1]], silent = TRUE)
  if (inherits(e, "try-error") || !pure_const(e, ls(env))) return(txt)
  v <- try(eval(e, env), silent = TRUE)
  if (inherits(v, "try-error") || !is.atomic(v) || length(v) != 1 || is.na(v))
    return(txt)                                        # not a constant: keep text
  if (is.numeric(v)) sprintf("%.15g", v) else as.character(v)
}

# --- does the filter settle the assertion? ---------------------------------
#
# With both right-hand sides folded to numbers, each comparison on a column is a
# half-line, and the question stops being "is this the same test" and becomes
# "does the filter's interval already decide it". Equality was only the easy
# case: filtering `amount > 1e8` and asserting `amount > 5e7` is written
# differently, compares a different number, and is every bit as vacuous.
#
#   ALWAYS TRUE   the filter's interval lies inside the assertion's
#   ALWAYS FALSE  the two intervals do not meet
#
# Anything else -- overlapping but not contained -- is a real test and is left
# alone. Non-numeric right-hand sides fall back to exact text equality.
settles <- function(fop, fa, aop, aa) {
  if (!is.finite(fa) || !is.finite(aa)) return(NA_character_)
  # the filter as an interval on the column
  lo <- -Inf; hi <- Inf; loq <- FALSE; hiq <- FALSE   # q = closed end
  switch(fop,
    ">"  = { lo <- fa },              ">=" = { lo <- fa; loq <- TRUE },
    "<"  = { hi <- fa },              "<=" = { hi <- fa; hiq <- TRUE },
    "==" = { lo <- hi <- fa; loq <- hiq <- TRUE },
    "!=" = return(if (aop == "!=" && aa == fa) "ALWAYS TRUE" else NA_character_),
    return(NA_character_))
  # does every point of [lo,hi] satisfy the assertion?
  all_sat <- switch(aop,
    ">"  = lo > aa || (loq && lo > aa) || (lo >= aa && !loq && lo >= aa),
    ">=" = (loq && lo >= aa) || (!loq && lo >= aa),
    "<"  = hi < aa || (hiq && hi < aa) || (hi <= aa && !hiq && hi <= aa),
    "<=" = (hiq && hi <= aa) || (!hiq && hi <= aa),
    "==" = loq && hiq && lo == hi && lo == aa,
    "!=" = (lo > aa) || (hi < aa) || (!loq && lo == aa && hi == aa),
    FALSE)
  if (isTRUE(all_sat)) return("ALWAYS TRUE")
  # does no point of [lo,hi] satisfy it?
  none_sat <- switch(aop,
    ">"  = (hiq && hi <= aa) || (!hiq && hi <= aa),
    ">=" = hi < aa || (!hiq && hi <= aa),
    "<"  = (loq && lo >= aa) || (!loq && lo >= aa),
    "<=" = lo > aa || (!loq && lo >= aa),
    "==" = aa < lo || aa > hi || (!loq && aa == lo) || (!hiq && aa == hi),
    "!=" = loq && hiq && lo == hi && lo == aa,
    FALSE)
  if (isTRUE(none_sat)) return("ALWAYS FALSE")
  NA_character_
}

# every comparison in an expression, as (column, operator, right-hand side)
OPS <- c(">", "<", ">=", "<=", "==", "!=")
FLIP <- c(">" = "<=", "<=" = ">", "<" = ">=", ">=" = "<", "==" = "!=", "!=" = "==")
comparisons <- function(x, env = NULL) {
  out <- character()
  walk <- function(e) {
    if (is.call(e)) {
      fn <- as.character(e[[1]])[1]
      if (fn %in% OPS && length(e) == 3) {
        # THE LEFT SIDE HAS TO BE A BARE COLUMN, `d$col`, and not merely an
        # expression containing one. `sum(x$minVotes) == 1` is not a statement
        # about the column minVotes, and `abs(d$diff) > 1` is a statement about
        # its magnitude -- treating either as `minVotes ==` or `diff >` invents
        # a predicate the code never made, which is how a checker starts
        # reporting things that are not true.
        lhs <- e[[2]]
        if (is.call(lhs) && identical(as.character(lhs[[1]])[1], "$") && length(lhs) == 3) {
          r <- paste(deparse(e[[3]]), collapse = "")
          out <<- c(out, paste(as.character(lhs[[3]]), fn,
                               if (is.null(env)) r else fold(r, env)))
        }
      }
      for (i in seq_along(e)) if (!is.null(e[[i]])) try(walk(e[[i]]), silent = TRUE)
    }
  }
  try(walk(x), silent = TRUE)
  unique(out)
}

cols_in <- function(x) {                      # every `$col` in an expression
  out <- character()
  walk <- function(e) {
    if (is.call(e)) {
      if (identical(as.character(e[[1]])[1], "$") && length(e) == 3)
        out <<- c(out, paste0(deparse(e[[2]]), "$", as.character(e[[3]])))
      for (i in seq_along(e)) if (!is.null(e[[i]])) try(walk(e[[i]]), silent = TRUE)
    }
  }
  try(walk(x), silent = TRUE)
  unique(out)
}

vars_in <- function(x) {
  out <- character()
  walk <- function(e) {
    if (is.name(e)) out <<- c(out, as.character(e))
    else if (is.call(e)) for (i in seq_along(e)) if (!is.null(e[[i]])) try(walk(e[[i]]), silent = TRUE)
  }
  try(walk(x), silent = TRUE)
  unique(out)
}

# --- collect, for one script: filters applied, and assertions made -----------
analyse <- function(path) {
  ex <- try(parse(path), silent = TRUE)
  if (inherits(ex, "try-error")) return(NULL)
  KENV <- collect_constants(ex)

  filt  <- list()     # object name -> columns it was filtered on
  preds <- list()     # object name -> the comparisons that narrowed it
  flag <- list()      # logical variable -> columns it tests
  flagc <- list()     # logical variable -> the comparisons it stands for
  asserts <- list()   # assertion expressions

  logical_index <- function(pred) {
    # `d[order(d$x), ]` reorders; it does not filter. Only a logical index
    # narrows the rows, and only a narrowing can make an assertion vacuous.
    txt <- paste(deparse(pred), collapse = " ")
    if (grepl("^\\s*(order|sort|rev|sample|match|seq_len|seq_along|head|tail)\\(", txt)) return(FALSE)
    if (grepl("[<>]|==|!=|%in%|is\\.na|grepl|duplicated|!", txt)) return(TRUE)
    for (v in vars_in(pred)) if (!is.null(flag[[v]])) return(TRUE)
    FALSE
  }

  note_filter <- function(target, pred) {
    if (!logical_index(pred)) return(invisible())
    # `ie[outlier, ]` narrows on whatever `outlier` compared -- the IE bug took
    # exactly this form, so a filter recorded only from inline comparisons would
    # miss the one case this whole script exists to find.
    cmps <- comparisons(pred, KENV)
    for (v in vars_in(pred)) if (!is.null(flagc[[v]])) cmps <- c(cmps, flagc[[v]])
    preds[[target]] <<- unique(c(preds[[target]], cmps))
    cs <- cols_in(pred)
    # a bare logical flag used as the index, e.g. ie[outlier, ]
    for (v in vars_in(pred)) if (!is.null(flag[[v]])) cs <- c(cs, flag[[v]])
    cs <- unique(sub("^[^$]*\\$", "", cs))
    if (length(cs)) filt[[target]] <<- unique(c(filt[[target]], cs))
  }

  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    fn <- as.character(e[[1]])[1]
    if (fn %in% c("<-", "=") && length(e) == 3) {
      lhs <- deparse(e[[2]]); rhs <- e[[3]]
      if (is.call(rhs)) {
        rfn <- as.character(rhs[[1]])[1]
        if (rfn == "[" && length(rhs) >= 3) note_filter(lhs, rhs[[3]])
        if (rfn == "subset" && length(rhs) >= 3) note_filter(lhs, rhs[[3]])
        # a logical flag defined from a comparison
        if (rfn %in% c(">", "<", ">=", "<=", "==", "!=", "%in%", "&", "|", "!")) {
          cs <- sub("^[^$]*\\$", "", cols_in(rhs))
          if (length(cs)) flag[[lhs]] <<- unique(cs)
          fc <- comparisons(rhs, KENV)
          if (length(fc)) flagc[[lhs]] <<- unique(fc)
        }
      }
    }
    if (fn == "stopifnot") asserts[[length(asserts) + 1]] <<- e
    if (fn == "stop") asserts[[length(asserts) + 1]] <<- e
    for (i in seq_along(e)) if (!is.null(e[[i]])) try(walk(e[[i]]), silent = TRUE)
  }
  for (e in ex) try(walk(e), silent = TRUE)

  hits <- list()
  for (a in asserts) {
    txt <- paste(deparse(a), collapse = " ")
    acmp <- comparisons(a, KENV)
    for (obj in names(preds)) {
      if (!grepl(paste0("\\b", obj, "\\$"), txt)) next
      for (ac in acmp) {
        pa <- strsplit(ac, " ", fixed = TRUE)[[1]]
        if (length(pa) < 3) next
        col <- pa[1]; op <- pa[2]; rhs <- paste(pa[-(1:2)], collapse = " ")
        for (fc in preds[[obj]]) {
          pf <- strsplit(fc, " ", fixed = TRUE)[[1]]
          if (length(pf) < 3 || pf[1] != col) next
          frhs <- paste(pf[-(1:2)], collapse = " ")
          fn_ <- suppressWarnings(as.numeric(frhs)); an_ <- suppressWarnings(as.numeric(rhs))
          verdict <- if (!is.na(fn_) && !is.na(an_)) settles(pf[2], fn_, op, an_)
                     else if (frhs == rhs) {
                       if (identical(pf[2], op)) "ALWAYS TRUE"
                       else if (identical(unname(FLIP[pf[2]]), op)) "ALWAYS FALSE"
                       else NA_character_
                     } else NA_character_
          if (is.na(verdict)) next
          hits[[length(hits) + 1]] <- data.frame(
            object = obj, column = col, filtered_on = fc, tested = ac,
            verdict = verdict,
            assertion = substr(gsub("\\s+", " ", txt), 1, 78),
            stringsAsFactors = FALSE)
        }
      }
    }
  }
  if (!length(hits)) return(NULL)
  h <- do.call(rbind, hits)
  h[!duplicated(paste(h$object, h$tested, h$assertion)), ]
}


# =========================================================================
# pass 2: the cross-file seam
# =========================================================================
cross_file <- function(root) {
  # --- pass 1: what each derived table was filtered on, on its way to disk -----
  WRITERS <- c("dd_write_csv", "write.csv", "fwrite", "write_csv")

  producers <- list()                  # "chapter/file.csv" -> character() of preds

  for (lab in dd_chapter_dirs(root)) {
    d <- file.path(lab, "data")
    if (!dir.exists(d)) next
    for (f in list.files(d, pattern = "[.][Rr]$", full.names = TRUE)) {
      ex <- try(parse(f), silent = TRUE); if (inherits(ex, "try-error")) next
      KENV <- collect_constants(ex)
      preds <- list(); flagc <- list()

      walk <- function(e) {
        if (!is.call(e)) return(invisible())
        fn <- as.character(e[[1]])[1]
        if (fn %in% c("<-", "=") && length(e) == 3 && is.name(e[[2]])) {
          lhs <- as.character(e[[2]]); rhs <- e[[3]]
          if (is.call(rhs)) {
            rfn <- as.character(rhs[[1]])[1]
            if (rfn %in% c("[", "subset") && length(rhs) >= 3) {
              pred <- rhs[[3]]
              txt <- paste(deparse(pred), collapse = " ")
              if (!grepl("^\\s*(order|sort|rev|sample|match|seq_len|seq_along|head|tail)\\(", txt)) {
                cm <- comparisons(pred, KENV)
                for (v in all.vars(pred)) if (!is.null(flagc[[v]])) cm <- c(cm, flagc[[v]])
                # a subset inherits whatever already narrowed its parent
                base <- if (is.name(rhs[[2]])) as.character(rhs[[2]]) else ""
                if (nzchar(base) && !is.null(preds[[base]])) cm <- c(cm, preds[[base]])
                if (length(cm)) preds[[lhs]] <<- unique(c(preds[[lhs]], cm))
              }
            }
            if (rfn %in% c(">", "<", ">=", "<=", "==", "!=", "&", "|", "!")) {
              cm <- comparisons(rhs, KENV)
              if (length(cm)) flagc[[lhs]] <<- unique(cm)
            }
          }
        }
        if (fn %in% WRITERS && length(e) >= 3 && is.name(e[[2]])) {
          var <- as.character(e[[2]])
          path <- paste(deparse(e[[3]]), collapse = "")
          m <- regmatches(path, regexpr("derived/[^\"']+\\.csv", path))
          if (length(m) && !is.null(preds[[var]]))
            producers[[paste0(basename(lab), "/", basename(m))]] <<-
              unique(c(producers[[paste0(basename(lab), "/", basename(m))]], preds[[var]]))
        }
        for (i in seq_along(e)) if (!is.null(e[[i]])) try(walk(e[[i]]), silent = TRUE)
      }
      for (e in ex) try(walk(e), silent = TRUE)
    }
  }

  # --- pass 2: every consumer of those tables ---------------------------------
  chunks_of <- function(rmd) {
    L <- readLines(rmd, warn = FALSE)
    o <- grep("^```\\{r", L); c2 <- grep("^```\\s*$", L)
    out <- character()
    for (s in o) { e <- c2[c2 > s][1]; if (!is.na(e) && e > s + 1) out <- c(out, L[(s + 1):(e - 1)]) }
    out
  }

  hits <- list(); nconsumers <- 0; nchecked <- 0

  for (lab in dd_chapter_dirs(root)) {
    files <- c(list.files(lab, pattern = "[.](Rmd|R)$", full.names = TRUE),
               list.files(file.path(lab, "data"), pattern = "[.][Rr]$", full.names = TRUE))
    for (f in files) {
      code <- if (grepl("[.]Rmd$", f)) chunks_of(f) else readLines(f, warn = FALSE)
      ex <- try(parse(text = code), silent = TRUE); if (inherits(ex, "try-error")) next
      KENV <- collect_constants(ex)
      bind <- list()                   # local frame -> "chapter/file.csv"

      walk_read <- function(e) {
        if (!is.call(e)) return(invisible())
        if (as.character(e[[1]])[1] %in% c("<-", "=") && length(e) == 3 && is.name(e[[2]])) {
          txt <- paste(deparse(e[[3]]), collapse = " ")
          m <- regmatches(txt, regexpr("(\\.\\./)*([A-Za-z0-9_-]+/)?data/derived/[^\"']+\\.csv", txt))
          if (length(m)) {
            parts <- strsplit(m, "/")[[1]]
            i <- which(parts == "data")[1]
            chap <- if (!is.na(i) && i > 1) parts[i - 1] else basename(lab)
            bind[[as.character(e[[2]])]] <<- paste0(chap, "/", parts[length(parts)])
          }
        }
        for (i in seq_along(e)) if (!is.null(e[[i]])) try(walk_read(e[[i]]), silent = TRUE)
      }
      for (e in ex) try(walk_read(e), silent = TRUE)
      if (!length(bind)) next
      nconsumers <- nconsumers + 1

      # every comparison in this file, attributed to the frame it is about
      walk_cmp <- function(e, ctx) {
        if (!is.call(e)) return(invisible())
        fn <- as.character(e[[1]])[1]
        here <- if (fn %in% c("sum", "mean", "any", "all", "which", "nrow", "length",
                              "subset", "[")) fn else ctx
        if (fn %in% OPS && length(e) == 3) {
          l <- paste(deparse(e[[2]]), collapse = "")
          if (grepl("$", l, fixed = TRUE)) {
            obj <- sub("\\$.*$", "", sub("^.*?([A-Za-z0-9_.]+)\\$.*$", "\\1", l))
            col <- sub("^.*\\$", "", l)
            key <- bind[[obj]]
            if (!is.null(key) && !is.null(producers[[key]])) {
              rhs <- fold(paste(deparse(e[[3]]), collapse = ""), KENV)
              an <- suppressWarnings(as.numeric(rhs))
              nchecked <<- nchecked + 1
              for (fc in producers[[key]]) {
                pf <- strsplit(fc, " ", fixed = TRUE)[[1]]
                if (length(pf) < 3 || pf[1] != col) next
                fn_ <- suppressWarnings(as.numeric(paste(pf[-(1:2)], collapse = " ")))
                v <- if (!is.na(fn_) && !is.na(an)) settles(pf[2], fn_, fn, an) else NA_character_
                if (!is.na(v))
                  hits[[length(hits) + 1]] <<- data.frame(
                    consumer = sub(paste0("^", root, "/"), "", f),
                    frame = obj, source = key, filter = fc,
                    test = paste(col, fn, rhs), used_in = here, verdict = v,
                    stringsAsFactors = FALSE)
              }
            }
          }
        }
        for (i in seq_along(e)) if (!is.null(e[[i]])) try(walk_cmp(e[[i]], here), silent = TRUE)
      }
      for (e in ex) try(walk_cmp(e, ""), silent = TRUE)
    }
  }

  list(producers = producers, hits = hits, nconsumers = nconsumers, nchecked = nchecked)
}

# =========================================================================
# pass 3: assertions that are constant on their face
# =========================================================================
constant_assertions <- function(root) {
  hits <- list(); n <- 0
  for (lab in dd_chapter_dirs(root)) {
    d <- file.path(lab, "data"); if (!dir.exists(d)) next
    for (f in list.files(d, pattern = "[.][Rr]$", full.names = TRUE)) {
      ex <- try(parse(f), silent = TRUE); if (inherits(ex, "try-error")) next
      walk <- function(e) {
        if (!is.call(e)) return(invisible())
        if (identical(as.character(e[[1]])[1], "stopifnot"))
          for (i in seq_along(e)[-1]) {
            a <- e[[i]]; n <<- n + 1
            txt <- gsub("\\s+", " ", paste(deparse(a), collapse = " "))
            why <- NULL
            # a precondition about the machine (file.exists, requireNamespace)
            # references no data either, and is not what this is looking for
            if (!length(all.vars(a)) && !grepl("exists|require|file[.]|nzchar|Sys[.]", txt))
              why <- "no data referenced"
            if (grepl("(nrow|length|sum|ncol)\\([^)]*\\)\\s*>=\\s*0\\b", txt))
              why <- "a count is never negative"
            if (is.call(a) && as.character(a[[1]])[1] %in% c("==", "identical") &&
                length(a) == 3 && identical(deparse(a[[2]]), deparse(a[[3]])))
              why <- "compares a value to itself"
            if (!is.null(why)) hits[[length(hits) + 1]] <<- data.frame(
              script = file.path(basename(lab), basename(f)), why = why,
              expr = substr(txt, 1, 66), stringsAsFactors = FALSE)
          }
        for (i in seq_along(e)) if (!is.null(e[[i]])) try(walk(e[[i]]), silent = TRUE)
      }
      for (e in ex) try(walk(e), silent = TRUE)
    }
  }
  list(hits = hits, n = n)
}

# =========================================================================
# self-test — a checker that has never been shown to fire proves nothing
# =========================================================================
self_test <- function() {
  root <- file.path(tempdir(), "vacuous-fixture")
  unlink(root, recursive = TRUE)
  dir.create(file.path(root, "faketown", "data"), recursive = TRUE)
  dir.create(file.path(root, "goodtown", "data"), recursive = TRUE)
  breach <- file.path(tempdir(), "sandbox-breach.txt"); unlink(breach)

  writeLines(c(
    'BAD <- system(paste0("touch ", shQuote("', breach, '")), intern = TRUE)',
    'CUT <- 1e8', 'HALF <- CUT / 2',
    'ie <- read.csv("raw/x.csv")',
    'outlier <- ie$amount > CUT',
    'o <- ie[outlier, ]',                       # filter via a flag variable
    'dd_write_csv(o, "derived/outliers.csv")',
    'k <- ie[ie$amount > HALF, ]',
    'stopifnot(all(k$amount > 5e7))',           # ALWAYS TRUE, folded via HALF
    'stopifnot(sum(k$amount <= 5e7) == 0)',     # ALWAYS FALSE
    'stopifnot(all(o$amount > 5e7))',           # ALWAYS TRUE, implied bound
    'd <- ie[order(ie$amount), ]',
    'stopifnot(!any(is.na(d$amount)))',         # legitimate: order() is no filter
    'g <- ie[ie$votes == 0, ]',
    'stopifnot(all(g$total >= g$votes))',       # legitimate: a different claim
    'h <- ie[ie$amount > 5e7, ]',
    'stopifnot(all(h$amount > 1e8))'            # legitimate: stronger, can fail
  ), file.path(root, "faketown", "data", "build-data.R"))

  writeLines(c('```{r setup}',
    'o <- read.csv("data/derived/outliers.csv")',
    'BAND <- sum(o$amount > 5e7 & o$amount <= 1e8)',   # the real bug, cross-file
    '```'), file.path(root, "faketown", "faketown-brief.Rmd"))

  writeLines(c('ie <- read.csv("raw/x.csv")', 'kp <- ie[ie$votes > 0, ]',
               'dd_write_csv(kp, "derived/kept.csv")'),
             file.path(root, "goodtown", "data", "build-data.R"))
  writeLines(c('```{r setup}', 'kp <- read.csv("data/derived/kept.csv")',
    'BIG <- sum(kp$votes > 1000)',   # legitimate: overlaps, decides nothing
    'LOW <- sum(kp$total < 50)',     # legitimate: nobody filtered this column
    '```'), file.path(root, "goodtown", "goodtown-brief.Rmd"))

  within <- analyse(file.path(root, "faketown", "data", "build-data.R"))
  cx     <- cross_file(root)
  nw <- if (is.null(within)) 0 else nrow(within)
  nc <- length(cx$hits)
  leaked <- file.exists(breach)

  cat("self-test\n")
  cat(sprintf("  within a script : %d vacuous found, expected 3   %s\n", nw,
              if (nw == 3) "ok" else "FAIL"))
  cat(sprintf("  across files    : %d vacuous found, expected 2   %s\n", nc,
              if (nc == 2) "ok" else "FAIL"))
  cat(sprintf("  sandbox held    : %s\n", if (!leaked) "ok" else "FAIL - it ran system()"))
  ok <- nw == 3 && nc == 2 && !leaked
  cat(if (ok) "  the checker works\n" else "  THE CHECKER IS BROKEN\n")
  invisible(ok)
}

# =========================================================================
ARGV <- commandArgs(trailingOnly = TRUE)

if (length(ARGV) && ARGV[1] == "--self-test") {
  quit(status = if (isTRUE(self_test())) 0 else 1)
}

if (length(ARGV)) {                       # one file
  r <- analyse(ARGV[1])
  if (is.null(r)) cat("no vacuous assertion in", ARGV[1], "\n")
  else print(r[, c("object", "filtered_on", "tested", "verdict")])
  quit(status = if (is.null(r)) 0 else 1)
}

problems <- 0

cat("1. within a script\n")
w <- list(); nscripts <- 0; nassert <- 0
for (lab in dd_chapter_dirs(LABS)) {
  d <- file.path(lab, "data"); if (!dir.exists(d)) next
  for (f in list.files(d, pattern = "[.][Rr]$", full.names = TRUE)) {
    nscripts <- nscripts + 1
    nassert <- nassert + length(grep("stopifnot", readLines(f, warn = FALSE)))
    r <- analyse(f)
    if (!is.null(r)) { r$script <- file.path(basename(lab), basename(f)); w[[length(w) + 1]] <- r }
  }
}
cat(sprintf("   %d build scripts, %d stopifnot lines\n", nscripts, nassert))
if (length(w)) {
  o <- do.call(rbind, w); problems <- problems + nrow(o)
  for (i in seq_len(nrow(o)))
    cat(sprintf("   %-38s %-11s filter %-22s test %s\n   %s\n", o$script[i],
                o$verdict[i], o$filtered_on[i], o$tested[i], o$assertion[i]))
} else cat("   clean\n")

cat("\n2. across files\n")
cx <- cross_file(LABS)
cat(sprintf("   %d filtered tables, %d consuming files, %d conditions tested\n",
            length(cx$producers), cx$nconsumers, cx$nchecked))
if (length(cx$hits)) {
  h <- do.call(rbind, cx$hits); h <- h[!duplicated(paste(h$consumer, h$test)), ]
  problems <- problems + nrow(h)
  for (i in seq_len(nrow(h)))
    cat(sprintf("   %-46s %s\n      %s reads %s (filtered %s), tests %s in %s()\n",
                h$consumer[i], h$verdict[i], h$frame[i], h$source[i],
                h$filter[i], h$test[i], h$used_in[i]))
} else cat("   clean\n")

cat("\n3. constant assertions\n")
ca <- constant_assertions(LABS)
cat(sprintf("   %d stopifnot conditions examined\n", ca$n))
if (length(ca$hits)) {
  cc <- do.call(rbind, ca$hits); problems <- problems + nrow(cc)
  for (i in seq_len(nrow(cc)))
    cat(sprintf("   %-38s %s\n      %s\n", cc$script[i], cc$why[i], cc$expr[i]))
} else cat("   clean\n")

cat(sprintf("\n%s\n", if (problems == 0) "no check in the corpus is unable to fail"
                      else paste(problems, "problem(s)")))
quit(status = if (problems == 0) 0 else 1)
