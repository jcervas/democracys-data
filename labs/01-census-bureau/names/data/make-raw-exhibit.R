# Write the "what arrives" exhibit: the first-name-by-sex spreadsheet as a
# reader opening it would see it, with the two title rows above the column
# names and the unlabelled residual line at the bottom kept in place. Called by
# build-data.R; the output is a raw file only in the sense that nothing has been
# computed from it.
mk_raw_exhibit <- function(path, out) {
  full <- as.data.frame(suppressMessages(
    readxl::read_excel(path, col_types = "text", col_names = FALSE)))
  keep <- full[c(1:3, 4:8, nrow(full)), ]
  # The proportion columns arrive with seventeen digits. Shown at four, so the
  # line fits a page; the brief says so where the exhibit appears.
  cell <- function(x) {
    x <- ifelse(is.na(x), "", x)
    ifelse(grepl("^[0-9]+\\.[0-9]{6,}$", x),
           formatC(suppressWarnings(as.numeric(x)), format = "f", digits = 4), x)
  }
  keep[] <- lapply(keep, cell)
  # Column widths from the header and the data, so the Bureau's own column
  # names sit over their own numbers. The title row is excluded from the
  # measurement -- it is one long string in column 1, and letting it set that
  # column's width would push every number off to the right.
  w <- vapply(keep[-(1:2), ], function(col) max(nchar(col)), integer(1))
  fmt <- function(r) sub(" +$", "",
                         paste(mapply(function(x, k) formatC(x, width = k, flag = "-"),
                                      r, w), collapse = " "))
  body <- c(keep[1, 1], "", apply(keep[-(1:2), ], 1, fmt))
  writeLines(c(body[1:3], "",
               body[4:8],
               sprintf("   ... %s rows omitted ...",
                       format(nrow(full) - 9L, big.mark = ",")),
               body[9]), out)
  invisible(out)
}
