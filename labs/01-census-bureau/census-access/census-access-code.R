# census-access-code.R -- chunk bodies for census-access-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.2,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

rd    <- function(f) read.csv(file.path("data/derived", f), stringsAsFactors = FALSE)
facts <- rd("facts.csv")
FV    <- function(k) facts$value[facts$key == k]
FN    <- function(k) as.numeric(FV(k))

catalog <- rd("catalog.csv")
samedt  <- rd("pep_same_date.csv")
revs    <- rd("pep_revisions.csv")
overlap <- rd("route_overlap.csv")
routes  <- rd("routes.csv")
apiv    <- rd("api_vintages.csv")

# Every table this chapter writes is handed over in the brief's data-itself
# list; dd_derived() stops the build if one appears in derived/ without a link.
dd_derived(c("api_vintages.csv", "catalog.csv", "checks.csv", "decennial_pa.csv", "facts.csv", "national_spread.csv", "pep_revision_national.csv", "pep_revisions.csv", "pep_same_date.csv", "route_overlap.csv", "routes.csv"))

n  <- function(x) format(as.numeric(x), big.mark = ",")
pc <- function(x, k = 1) formatC(as.numeric(x), format = "f", digits = k)

COUNT <- "#1C4C5C"    # the count
SURVE <- "#3F7C57"    # the survey
MODEL <- "#8A3B2C"    # the model
GRID  <- "#D8D2C8"

# ---- render every data.frame as a TABLE, not code output -------------------
# These are front-facing documents. A data.frame printed the ordinary way comes
# out as a "##"-prefixed code block, which reads as machinery rather than as a
# result.
knit_print.data.frame <- function(x, ...) {
  nm <- names(x)
  nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- catalog
cg <- catalog[, c("program", "product", "counts", "refers_to", "population")]
cg$population <- n(cg$population)
names(cg) <- c("Program", "Product", "Counts", "Refers to", "Population")
cg

## ---- catfig
d <- catalog[catalog$counts == "everybody" & catalog$refers_mid >= 2020, ]
sv <- d[grepl("^Amer", d$program), ]
ot <- d[!grepl("^Amer", d$program), ]
ot$col <- ifelse(grepl("^Dec", ot$program), COUNT, MODEL)
op <- par(mar = c(4.2, 6.2, 0.8, 1.0), cex = 0.86)
plot(d$refers_mid, d$population, type = "n", bty = "n", xaxt = "n", yaxt = "n",
     xlab = "the date, or middle of the period, that the figure describes",
     ylab = "", xlim = c(2019.9, 2026.1))
yt <- pretty(d$population)
abline(h = yt, col = GRID, lwd = 0.8)
axis(1, at = 2020:2026, labels = 2020:2026, tick = FALSE)
axis(2, at = yt, labels = format(yt, big.mark = ","), las = 1, tick = FALSE)
mtext("published population", side = 2, line = 4.6, cex = 0.86)
abline(lm(population ~ refers_mid, data = d), col = "#9A9384", lwd = 1.4, lty = 2)
points(sv$refers_mid, sv$population, pch = 21, col = SURVE, bg = "white",
       cex = 1.9, lwd = 2)
points(ot$refers_mid, ot$population, pch = 19, col = ot$col, cex = 1.05)
legend("bottomleft", bty = "n", cex = 0.92, pt.cex = c(1.05, 1.5, 1.05),
       pch = c(19, 21, 19), col = c(COUNT, SURVE, MODEL),
       pt.bg = "white", pt.lwd = 2,
       legend = c("census count", "survey estimate", "calculated estimate"))
par(op)

## ---- samedate
sd <- samedt
sd$lowest  <- n(sd$lowest)
sd$highest <- n(sd$highest)
sd$spread  <- n(sd$spread)
names(sd) <- c("Refers to (1 July)", "Times published", "Lowest", "Highest",
               "Spread")
sd

## ---- revfig
op <- par(mar = c(4.2, 6.4, 0.8, 6.4), cex = 0.84)
yr <- sort(unique(revs$refers_to))
plot(range(revs$vintage), range(revs$population), type = "n", bty = "n",
     xlab = "vintage: the year the Bureau did the arithmetic",
     ylab = "", xaxt = "n", yaxt = "n")
axis(1, at = yr, labels = yr, tick = FALSE)
axis(2, at = pretty(revs$population),
     labels = format(pretty(revs$population), big.mark = ","), las = 1,
     tick = FALSE)
mtext("published population", side = 2, line = 4.9, cex = 0.84)
abline(h = pretty(revs$population), col = GRID, lwd = 0.8)
for (y in yr) {
  s <- revs[revs$refers_to == y, ]
  s <- s[order(s$vintage), ]
  lines(s$vintage, s$population, col = MODEL, lwd = 1.6)
  points(s$vintage, s$population, pch = 19, col = MODEL, cex = 0.9)
  text(max(s$vintage) + 0.12, s$population[which.max(s$vintage)],
       paste0(" 1 July ", y), adj = 0, cex = 0.82, xpd = NA)
}
par(op)

## ---- overlap
ov <- overlap
ov$counties <- n(ov$counties)
ov$exact_matches <- n(ov$exact_matches)
names(ov) <- c("Survey window", "Compared with", "Counties", "Identical")
ov

## ---- routes
rt <- routes
names(rt) <- c("Address", "Hands you", "You need", "Carries", "Best for")
rt

## ---- apiv
av <- apiv
names(av) <- c("Product", "Path", "Vintages", "Earliest", "Latest")
av

## ---- rounded
ev       <- catalog[catalog$counts == "everybody", ]
ev$mill  <- round(ev$population / 1e6, 2)
common   <- as.numeric(names(sort(table(ev$mill), decreasing = TRUE))[1])
hit      <- ev[ev$mill == common, ]
n_common <- nrow(hit)
n_period <- length(unique(hit$refers_to))
n_value  <- length(unique(hit$population))
n_twice  <- sum(table(hit$population) > 1)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
