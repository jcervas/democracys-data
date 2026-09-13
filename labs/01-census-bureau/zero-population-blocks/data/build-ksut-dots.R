# build-ksut-dots.R -- where the people of Kansas and Utah actually are.
#
# A dot map of population for the two states drawn side by side elsewhere in
# this chapter, so that "where people live" and "where blocks have nobody" can
# be set against each other in the same frame at the same scale.
#
# Source: the TIGER/Line block files, which carry POP20 and the Bureau's own
# interior point for every block (INTPTLAT20 / INTPTLON20).
#
# Two details decide whether the picture is honest.
#
# Dots are allocated PROBABILISTICALLY. Kansas's median inhabited block holds
# eleven people, so at any sensible dot value most blocks round to zero, and rounding every such block to zero would
# empty out exactly the thinly-settled countryside the figure exists to show.
# Each block instead gets floor(pop/PER) dots plus one more with probability
# equal to the remainder, which preserves the state total in expectation and
# leaves the countryside populated.
#
# Dots are SCATTERED inside their block rather than stacked on its centre. A
# block's interior point is one coordinate; a rural block can be forty square
# miles. Each dot is placed uniformly in a disc of the block's own radius,
# sqrt(ALAND20/pi), so a big block spreads its people over the ground it
# actually covers.

suppressPackageStartupMessages({ library(foreign); library(sf) })

TIG <- Sys.getenv("TIGER_DIR", "raw/tiger")
D   <- Sys.getenv("DERIVED", "derived")
PER <- 25L                                    # people per dot
MINPOP <- 2L                                  # a lone resident hosts no dot
set.seed(20200401)                            # census day; any fixed seed does
KU  <- c("20", "49")

ms <- function(...) if (system(paste("mapshaper", paste(..., collapse = " ")),
                             ignore.stderr = TRUE) != 0L) stop("mapshaper failed")

work <- tempfile(); dir.create(work)
out <- do.call(rbind, lapply(KU, function(f) {
  unlink(list.files(work, full.names = TRUE))
  zip <- file.path(TIG, sprintf("tl_2020_%s_tabblock20.zip", f))
  unzip(zip, exdir = work,
        files = grep("[.]dbf$", unzip(zip, list = TRUE)$Name, value = TRUE))
  d <- read.dbf(list.files(work, pattern = "[.]dbf$", full.names = TRUE)[1],
                as.is = TRUE)
  # A dot stands for PER people drawn from the neighbourhood, so it should not
  # be planted on a block holding one person. Those blocks are 4.2% of the
  # inhabited ones and 0.105% of the population, so barring them from the
  # lottery costs a rounding error and stops the map asserting a settlement
  # where there is a single household.
  pop <- as.numeric(d$POP20); keep <- pop >= MINPOP; d <- d[keep, ]; pop <- pop[keep]
  lat <- as.numeric(d$INTPTLAT20); lon <- as.numeric(d$INTPTLON20)
  rad <- sqrt(as.numeric(d$ALAND20) / pi)      # metres

  q <- pop / PER
  n <- floor(q) + (runif(length(q)) < (q - floor(q)))
  k <- rep(seq_along(n), n)                    # one row per dot
  if (!length(k)) return(NULL)

  # uniform in a disc: sqrt(u) keeps it uniform by AREA, not by radius
  th <- runif(length(k), 0, 2 * pi); rr <- rad[k] * sqrt(runif(length(k)))
  dlat <- (rr * sin(th)) / 111320
  dlon <- (rr * cos(th)) / (111320 * cos(lat[k] * pi / 180))
  data.frame(st = f, lon = lon[k] + dlon, lat = lat[k] + dlat)
}))

cat(sprintf("  dots: %s (%s Kansas, %s Utah) at 1 per %d people\n",
            format(nrow(out), big.mark = ","),
            format(sum(out$st == "20"), big.mark = ","),
            format(sum(out$st == "49"), big.mark = ","), PER))

# Projected by mapshaper, not by sf, so the dots land in exactly the same
# Albers USA the block geometry went through.
gj <- tempfile(fileext = ".json"); pj <- tempfile(fileext = ".json")
st_write(st_as_sf(out, coords = c("lon", "lat"), crs = 4326), gj,
         driver = "GeoJSON", quiet = TRUE)
ms("-i", shQuote(gj), "-proj albersusa -o", shQuote(pj), "format=geojson")
p  <- st_read(pj, quiet = TRUE)
xy <- st_coordinates(p)

# Into the same figure grid the block geometry uses. The frame is one linear
# map from projected metres to figure units, so it can be recovered by
# comparing any state's outline in both systems -- and checked by doing it
# twice and seeing that the two agree.
STG <- Sys.getenv("STATES_GEOJSON", "raw/states_500k.geojson")
sp  <- tempfile(fileext = ".json")
ms("-i", shQuote(STG), "-proj albersusa -o", shQuote(sp), "format=geojson precision=1")
og  <- st_read(sp, quiet = TRUE)
og  <- og[og$GEOID %in% KU, ]
MSF <- read.csv(file.path(D, "map_states.csv"), colClasses = c(st = "character"))

fit <- t(vapply(KU, function(f) {
  mb <- st_bbox(og[og$GEOID == f, ])
  fb <- MSF[MSF$st == f, ]
  mb <- unname(as.numeric(mb))               # st_bbox names would mangle below
  sx <- (max(fb$x) - min(fb$x)) / (mb[3] - mb[1])
  sy <- (max(fb$y) - min(fb$y)) / (mb[4] - mb[2])
  c(sx = sx, sy = sy,
    bx = min(fb$x) - mb[1] * sx,               # fig_x = m_x*sx + bx
    by = max(fb$y) + mb[2] * sy)               # fig_y = by - m_y*sy
}, numeric(4)))
stopifnot(max(abs(fit[, "sx"] - fit[, "sy"])) / mean(fit[, "sx"]) < 0.01)
if (max(abs(diff(fit[, "sx"]))) / mean(fit[, "sx"]) > 0.01)
  stop("the two states disagree about the frame's scale")
SC <- mean(fit[, "sx"]); BX <- mean(fit[, "bx"]); BY <- mean(fit[, "by"])
cat(sprintf("  frame: %.6f figure units per metre (agreed by both states)\n", SC))

dots <- data.frame(st = as.character(p$st),
                   x = as.integer(round(xy[, 1] * SC + BX)),
                   y = as.integer(round(BY - xy[, 2] * SC)))
write.csv(dots, file.path(D, "ksut_dots.csv"), row.names = FALSE)

chk <- vapply(KU, function(f) {
  d <- dots[dots$st == f, ]; fb <- MSF[MSF$st == f, ]
  mean(d$x >= min(fb$x) & d$x <= max(fb$x) & d$y >= min(fb$y) & d$y <= max(fb$y))
}, 0)
cat(sprintf("  dots landing inside their own state's outline box: %s\n",
            paste(sprintf("%s %.1f%%", c("KS", "UT"), 100 * chk), collapse = ", ")))
