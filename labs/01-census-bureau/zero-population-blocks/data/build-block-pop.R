# build-block-pop.R -- the population half of the zero-population-blocks brief.
#
# Pulls the 2020 count for every census block in the country from the Census
# API, one county at a time, and writes two small tables: the distribution of
# block populations, and a per-state summary. Nothing here is estimated; the
# PL 94-171 file is a complete count.
#
# Source: https://api.census.gov/data/2020/dec/pl  (variable P1_001N)
# Needs CENSUS_API_KEY. Takes about 12 minutes at WORKERS=4.

suppressPackageStartupMessages({ library(jsonlite); library(parallel) })

key <- Sys.getenv("CENSUS_API_KEY")
if (!nzchar(key)) stop("CENSUS_API_KEY is not set")
D <- file.path(dirname(sys.function(0) |> environment() |> attr("srcfile") |>
                       (\(s) if (is.null(s)) "data" else s$filename)()), "derived")
D <- Sys.getenv("DERIVED", "derived")
dir.create(D, recursive = TRUE, showWarnings = FALSE)

BASE <- "https://api.census.gov/data/2020/dec/pl"
api <- function(q, tries = 6) {
  url <- paste0(BASE, "?", q, "&key=", key)
  for (i in seq_len(tries)) {
    out <- tryCatch(fromJSON(url), error = function(e) e)
    if (!inherits(out, "error") && is.matrix(out) && nrow(out) > 1) return(out)
    Sys.sleep(min(2^i, 30))
  }
  stop("API failed: ", sub(key, "<KEY>", url, fixed = TRUE))
}
as_df <- function(m) { d <- as.data.frame(m[-1, , drop = FALSE], stringsAsFactors = FALSE)
                       names(d) <- m[1, ]; d }

states <- as_df(api("get=NAME&for=state:*")); states <- states[order(states$state), ]

# The distribution is kept as a tally, not as 8.2 million rows: counts of
# blocks at each exact population 0..999, plus one bucket for 1000 and over.
CAP <- 1000L
tally <- integer(CAP + 1L)
rows  <- vector("list", nrow(states))

for (i in seq_len(nrow(states))) {
  fips <- states$state[i]
  cty  <- as_df(api(sprintf("get=NAME&for=county:*&in=state:%s", fips)))$county
  pops <- unlist(mclapply(cty, function(cc)
    as.integer(as_df(api(sprintf("get=P1_001N&for=block:*&in=state:%s%%20county:%s",
                                 fips, cc)))$P1_001N),
    mc.cores = as.integer(Sys.getenv("WORKERS", "4")), mc.preschedule = FALSE))
  b <- pmin(pops, CAP)
  tally <- tally + tabulate(b + 1L, nbins = CAP + 1L)
  rows[[i]] <- data.frame(STATEFP = fips, state = states$NAME[i],
                          blocks = length(pops), zero_blocks = sum(pops == 0),
                          pop = sum(pops), stringsAsFactors = FALSE)
  cat(sprintf("%s %-22s %8d blocks %8d zero\n", fips, states$NAME[i],
              length(pops), sum(pops == 0)))
}

sb <- do.call(rbind, rows)
sb$pct_zero <- round(100 * sb$zero_blocks / sb$blocks, 2)
write.csv(sb[order(-sb$pct_zero), ], file.path(D, "state_blocks.csv"), row.names = FALSE)

hist <- data.frame(pop = 0:CAP, blocks = tally)
write.csv(hist, file.path(D, "block_pop_hist.csv"), row.names = FALSE)
cat("wrote state_blocks.csv and block_pop_hist.csv\n")
