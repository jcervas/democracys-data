# ---------------------------------------------------------------------------
# Build the census-api materials.
#
# THE POINT OF THIS SCRIPT is that the Census API is split in two, and only
# half of it is open:
#
#   METADATA  (what exists) -- open, no key, works right now.
#   DATA      (the numbers) -- REQUIRES AN API KEY as of 2026. A keyless
#                             request returns an HTML page titled "Missing
#                             Key", not an error code and not JSON.
#
# So the lab teaches discovery live and ships the numbers from elsewhere.
# Students who want to run a live data query get a free key in about a minute;
# nobody is blocked if they do not.
#
# Two files are produced here from the cached metadata, so the lab runs with
# no network and no key:
#
#   derived/api_variables.csv   the 338 variables in the 2020 PL 94-171 API, with
#                       their labels and concepts
#   derived/api_geography.csv   the 96 geography levels the same API will serve
#
# The raw JSON is kept alongside them (api_variables_2020pl.json,
# raw/api_geography_2020pl.json) so students can see what actually came back.
#
# Refresh the cache with:
#   curl -sL -o api_variables_2020pl.json \
#     'https://api.census.gov/data/2020/dec/pl/variables.json'
#   curl -sL -o api_geography_2020pl.json \
#     'https://api.census.gov/data/2020/dec/pl/geography.json'
#
# Base R only -- no JSON package. These files have a flat, predictable shape,
# so the fields come out with regmatches(). That is not a general JSON parser
# and is not meant to be.
#
# Run from this directory:  Rscript build-api-data.R
# ---------------------------------------------------------------------------

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

slurp <- function(f) paste(readLines(f, warn = FALSE), collapse = "")

# pull the value of "key":"value" out of a JSON object fragment
field <- function(txt, key) {
  m <- regexpr(paste0('"', key, '"[[:space:]]*:[[:space:]]*"'), txt)
  if (m == -1) return(NA_character_)
  rest <- substring(txt, m + attr(m, "match.length"))
  sub('".*$', "", rest)
}

# --- variables --------------------------------------------------------------
v <- slurp("raw/api_variables_2020pl.json")

# each variable is  "NAME":{ ...no nested braces... }
pat    <- '"[A-Za-z0-9_]+"[[:space:]]*:[[:space:]]*\\{[^{}]*\\}'
chunks <- regmatches(v, gregexpr(pat, v))[[1]]

name  <- sub('^"([A-Za-z0-9_]+)".*$', "\\1", chunks)
label <- vapply(chunks, field, "", key = "label", USE.NAMES = FALSE)
conc  <- vapply(chunks, field, "", key = "concept", USE.NAMES = FALSE)

vars <- data.frame(variable = name, label = label, concept = conc,
                   stringsAsFactors = FALSE)
vars <- vars[!is.na(vars$label), ]
vars <- vars[order(vars$variable), ]
write.csv(vars, "derived/api_variables.csv", row.names = FALSE)

# --- geography levels -------------------------------------------------------
g <- slurp("raw/api_geography_2020pl.json")
gch <- regmatches(g, gregexpr('\\{[^{}]*"geoLevelDisplay"[^{}]*\\}', g))[[1]]
geo <- data.frame(
  level = vapply(gch, field, "", key = "geoLevelDisplay", USE.NAMES = FALSE),
  name  = vapply(gch, field, "", key = "name",            USE.NAMES = FALSE),
  stringsAsFactors = FALSE)
geo <- geo[!is.na(geo$name), ]
write.csv(geo, "derived/api_geography.csv", row.names = FALSE)

cat(sprintf("variables: %d\n", nrow(vars)))
cat(sprintf("geography levels: %d\n", nrow(geo)))
cat(sprintf("P1 (race) variables: %d\n", sum(grepl("^P1_", vars$variable))))
cat("\nsmallest and largest geography the API will serve:\n")
print(utils::head(geo[order(nchar(geo$level), geo$level), "name"], 3))

# ---------------------------------------------------------------------------
# Build stamp. Records which script produced what is now in this directory --
# every file under derived/ and raw/ with its size, hash and row count, and the
# date this ran -- into BUILD-STAMP.tsv beside the data. See
# ../../../_lib/provenance.R. Guarded, because a missing helper must not fail a
# build that was otherwise fine.
if (file.exists("../../../_lib/provenance.R")) {
  if (!exists("prov_stamp")) source("../../../_lib/provenance.R")
  prov_stamp()
}
