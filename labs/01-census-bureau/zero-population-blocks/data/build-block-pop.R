# build-block-pop.R -- the population half of the zero-population-blocks brief.
#
# The 2020 count for every census block, read straight out of the TIGER/Line
# block files. The Bureau prepends the decennial count to the geography: every
# tabblock20 record carries POP20, the number of people counted in that block
# on 1 April 2020, beside the block's boundary and its GEOID20. So the map's
# geometry and its population arrive in the same file, and no second source
# has to be fetched and joined.
#
# Source: https://www2.census.gov/geo/tiger/TIGER2020/TABBLOCK20/
#   tl_2020_<FIPS>_tabblock20.zip, one per state. These are large -- about
#   10 GB for the country -- so they live outside this chapter. Point
#   TIGER_DIR at a local copy, or let this script download them.
#
# Only the .dbf is needed here: it is the shapefile's attribute table, and
# the boundaries themselves are not read.

suppressPackageStartupMessages(library(foreign))

TIG <- Sys.getenv("TIGER_DIR", "raw/tiger")
D   <- Sys.getenv("DERIVED", "derived")
BASE <- "https://www2.census.gov/geo/tiger/TIGER2020/TABBLOCK20"
dir.create(TIG, recursive = TRUE, showWarnings = FALSE)
dir.create(D,   recursive = TRUE, showWarnings = FALSE)

# 50 states + DC + Puerto Rico, the universe the PL 94-171 file covers.
FIPS <- c("01","02","04","05","06","08","09","10","11","12","13","15","16","17",
          "18","19","20","21","22","23","24","25","26","27","28","29","30","31",
          "32","33","34","35","36","37","38","39","40","41","42","44","45","46",
          "47","48","49","50","51","53","54","55","56","72")
NAMES <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado",
  "Connecticut","Delaware","District of Columbia","Florida","Georgia","Hawaii",
  "Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine",
  "Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri",
  "Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico",
  "New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon",
  "Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee",
  "Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin",
  "Wyoming","Puerto Rico")
# Postal codes, in the same order, so a scatter can label a point in the room
# a point has.
USPS <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL",
  "IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV",
  "NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX",
  "UT","VT","VA","WA","WV","WI","WY","PR")
stopifnot(length(FIPS) == length(NAMES), length(FIPS) == length(USPS))

CAP   <- 1000L                     # top bucket is "CAP and over"
tally <- integer(CAP + 1L)
rows  <- vector("list", length(FIPS))
land  <- numeric(0)                # every block's land area, for the median
bigA  <- list(a = -1); bigP <- list(p = -1)   # the two extremes
M2FT  <- 10.7639104; M2MI <- 2589988.110336; M2AC <- 4046.8564224
work  <- tempfile(); dir.create(work)
on.exit(unlink(work, recursive = TRUE), add = TRUE)

for (i in seq_along(FIPS)) {
  f <- FIPS[i]
  zip <- file.path(TIG, sprintf("tl_2020_%s_tabblock20.zip", f))
  if (!file.exists(zip))
    download.file(sprintf("%s/tl_2020_%s_tabblock20.zip", BASE, f), zip,
                  quiet = TRUE, mode = "wb")
  unlink(list.files(work, full.names = TRUE))
  unzip(zip, exdir = work, files = grep("[.]dbf$", unzip(zip, list = TRUE)$Name,
                                        value = TRUE))
  dbf <- list.files(work, pattern = "[.]dbf$", full.names = TRUE)[1]
  d   <- read.dbf(dbf, as.is = TRUE)
  pop <- as.integer(d$POP20)
  ar  <- as.numeric(d$ALAND20)

  tally <- tally + tabulate(pmin(pop, CAP) + 1L, nbins = CAP + 1L)
  land  <- c(land, ar)
  ia <- which.max(ar); if (ar[ia] > bigA$a)
    bigA <- list(a = ar[ia], g = d$GEOID20[ia], p = pop[ia])
  ip <- which.max(pop); if (pop[ip] > bigP$p)
    bigP <- list(p = pop[ip], g = d$GEOID20[ip], a = ar[ip])
  rows[[i]] <- data.frame(STATEFP = f, state = NAMES[i], usps = USPS[i],
                          blocks = length(pop),
                          zero_blocks = sum(pop == 0), pop = sum(pop),
                          land_sqmi = round(sum(ar) / M2MI, 1),
                          stringsAsFactors = FALSE)
  cat(sprintf("%s %-22s %8d blocks %8d zero\n", f, NAMES[i], length(pop),
              sum(pop == 0)))
}

sb <- do.call(rbind, rows)
sb$pct_zero <- round(100 * sb$zero_blocks / sb$blocks, 2)
write.csv(sb[order(-sb$pct_zero), ], file.path(D, "state_blocks.csv"), row.names = FALSE)
write.csv(data.frame(pop = 0:CAP, blocks = tally),
          file.path(D, "block_pop_hist.csv"), row.names = FALSE)

# --- how big is a block, and what are the extremes ---------------------------
# County names for the two extreme blocks come from the Bureau's generalized
# county outlines, a small file, so the facts can name a place rather than
# quote a fifteen-character identifier at the reader.
cb <- file.path(dirname(TIG), "cb_2020_us_county_20m.zip")
if (!file.exists(cb))
  download.file("https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_county_20m.zip",
                cb, quiet = TRUE, mode = "wb")
cw <- tempfile(); dir.create(cw); unzip(cb, exdir = cw)
cn <- read.dbf(list.files(cw, pattern = "[.]dbf$", full.names = TRUE)[1], as.is = TRUE)
where <- function(geoid) {
  k <- match(substr(geoid, 1, 5), cn$GEOID)
  st <- NAMES[match(substr(geoid, 1, 2), FIPS)]
  if (is.na(k)) st else paste0(cn$NAMELSAD[k], ", ", st)
}
lp <- land[land > 0]
fx <- function(k, v, note) data.frame(name = k, value = v, note = note,
                                      stringsAsFactors = FALSE)
write.csv(rbind(
  fx("median_block_sqft",  round(median(lp) * M2FT),  "median land area of a block with land (sq ft)"),
  fx("median_block_acres", round(median(lp) / M2AC, 1), "the same, in acres"),
  fx("biggest_area_sqmi",  round(bigA$a / M2MI),      "largest block by land area (sq mi)"),
  fx("biggest_area_where", where(bigA$g),             "where that block is"),
  fx("biggest_area_pop",   bigA$p,                    "its population"),
  fx("biggest_pop",        bigP$p,                    "largest block by population"),
  fx("biggest_pop_where",  where(bigP$g),             "where that block is"),
  fx("biggest_pop_acres",  round(bigP$a / M2AC),      "its land area, acres")
), file.path(D, "block_size.csv"), row.names = FALSE)
cat("wrote state_blocks.csv, block_pop_hist.csv and block_size.csv\n")
