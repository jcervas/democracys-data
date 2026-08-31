# ---------------------------------------------------------------------------
# Fetch ONE filed LD-2 and turn it into the figure at Step 4 of the brief.
#
# Everything else in this folder is the database's *output*: rows in a CSV,
# one line per filing. This script fetches the thing those rows were typed
# into, because three of the brief's findings are properties of the form
# itself and are invisible in any extract of it.
#
#   Source   United States Senate / Clerk of the House, Lobbying Disclosure Act
#            database, public filing print view.
#            https://lda.gov/filings/public/filing/<uuid>/print/
#   Filing   302fd116-100b-475c-9d3b-37c3c78dcdc5
#            Registrant  Barker Leavitt, PLLC     Client  Millcreek, Utah
#            Q2 2024, income $20,000, issue codes BUD and DIS
#            This filing IS one of the rows in filings.csv, so the figure and
#            the table are the same object seen twice.
#   Fetched  2026-08-10          Rows fetched  1 filing (1 HTML document)
#   Output   ../img/ld2-millcreek-2024q2.png   (annotated, redacted crop)
#
# REDACTION. The print view carries the filer's direct telephone number and
# e-mail address in line 4. Those are public record and they are also a living
# person's contact details, so they are replaced with "[redacted]" before the
# screenshot is taken. Nothing else is altered: every box the figure points at
# is exactly as filed.
#
# Run from inside data/. Needs a network connection and Google Chrome; the
# committed PNG means the brief does not.
# ---------------------------------------------------------------------------

library(magick)

UUID   <- "302fd116-100b-475c-9d3b-37c3c78dcdc5"
URL    <- paste0("https://lda.gov/filings/public/filing/", UUID, "/print/")
CHROME <- "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
OUT    <- "../img/ld2-millcreek-2024q2.png"
dir.create("../img", showWarnings = FALSE)

tmp_raw <- tempfile(fileext = ".html")
tmp_red <- tempfile(fileext = ".html")
tmp_png <- tempfile(fileext = ".png")

# --- 1. fetch ---------------------------------------------------------------
ok <- system2("curl", c("-sL", "--max-time", "60", "-A", shQuote("84-355 course build"),
                        shQuote(URL), "-o", shQuote(tmp_raw)))
stopifnot(ok == 0, file.size(tmp_raw) > 5000)
raw <- readLines(tmp_raw, warn = FALSE, encoding = "UTF-8")
cat("fetched", length(raw), "lines from", URL, "\n")

# The three fields the figure is about must be present, or the layout changed
# and the crop box below is wrong.
flat <- paste(raw, collapse = " ")
stopifnot(grepl("20,000.00", flat, fixed = TRUE),          # line 12, the one box
          grepl("General issue area code", flat, fixed = TRUE),
          grepl("Covered Official Position", flat, fixed = TRUE))

# --- 2. redact the filer's phone and e-mail ---------------------------------
red <- gsub("2022983722", "[redacted]", raw, fixed = TRUE)
red <- gsub("ryanleavitt@barkerleavitt.com", "[redacted]", red, fixed = TRUE)
stopifnot(!any(grepl("2022983722|ryanleavitt@", red)))
writeLines(red, tmp_red, useBytes = TRUE)

# --- 3. render ---------------------------------------------------------------
system2(CHROME, c("--headless", "--disable-gpu", "--hide-scrollbars",
                  "--force-device-scale-factor=2", "--window-size=1100,2400",
                  paste0("--screenshot=", tmp_png),
                  shQuote(paste0("file://", tmp_red))),
        stdout = FALSE, stderr = FALSE)
img <- image_read(tmp_png)
stopifnot(image_info(img)$width == 2200)

# --- 4. crop to lines 1-19 and annotate --------------------------------------
# Everything below line 19 of the second issue block is empty boilerplate
# (information update, affiliated organisations, foreign entities).
img <- image_crop(img, "2200x3128+0+0")

# One colour, one meaning, matching the rest of the brief:
#   green  = money            blue = issue codes
#   red    = parts of government contacted      purple = the revolving-door field
# A numbered badge rather than a caption: text set beside these boxes collides
# with the form's own rules. The key lives in the brief, in both formats.
mark <- function(im, x, y, w, h, col, num) {
  im <- image_draw(im)
  rect(x, y, x + w, y + h, border = col, lwd = 7)
  symbols(x, y, circles = 26, inches = FALSE, add = TRUE, bg = col, fg = col)
  text(x, y, num, col = "white", cex = 2.2, font = 2)
  dev.off()
  im
}
img <- mark(img,  400, 1092, 470,  68, "#1b7837", "1")   # line 12, income
img <- mark(img,   14, 1552, 400,  54, "#2c7fb8", "2")   # line 15, first code
img <- mark(img,   14, 2400, 400,  54, "#2c7fb8", "2")   # line 15, second code
img <- mark(img,   14, 1836, 2160, 58, "#C41230", "3")   # line 17, entities
img <- mark(img, 1150, 1934, 1010, 250, "#8856a7", "4")  # line 18, covered position

img <- image_resize(img, "1240x")
img <- image_convert(image_quantize(img, max = 64, dither = FALSE), colorspace = "sRGB")
image_write(img, OUT, format = "png", depth = 8, compression = "Zip")
cat("wrote", OUT, paste(image_info(image_read(OUT))[, c("width", "height")],
                        collapse = "x"),
    round(file.size(OUT) / 1024), "KB\n")
