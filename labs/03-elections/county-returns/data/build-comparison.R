# ---------------------------------------------------------------------------
# Build the county-returns comparison tables.
#
# SOURCE. Two files that answer the same question and were assembled in
# opposite ways:
#
#   1. The states' own certified publications, 51 jurisdictions x 2 elections,
#      already assembled in derived/pres2024_counties_official.csv by
#      build_states.py, with one provenance row per jurisdiction-year in
#      provenance.csv recording the address, the format and the date.
#   2. tonmcg/US_County_Level_Election_Results_08-24, the GitHub compilation
#      three chapters of this book already read, held at
#      ../../data-sources/data/derived/pres2024_counties.csv
#
# THE POINT OF THIS SCRIPT is that the two agree about who won almost
# everywhere and disagree about the denominator in half the country. Which of
# them is right is not the question -- they are counting different things, and
# neither file says which.
#
# NOTHING HERE FETCHES. Both inputs are already on disk, one built by this
# folder and one by a sibling chapter, so this runs offline and in a second.
# The acquisition itself is build_states.py, which is the subject rather than
# the machinery: 102 documents in 11 formats, and the record of what each one
# was is provenance.csv.
#
# Run from this directory:  Rscript build-comparison.R
# ---------------------------------------------------------------------------

source("../../../_lib/precision.R")

dir.create("derived", showWarnings = FALSE)
options(scipen = 999, stringsAsFactors = FALSE, warn = 1)

n   <- function(x) format(as.numeric(x), big.mark = ",")
say <- function(...) cat(sprintf(...), "\n", sep = "")

FACTS <- list()
fact  <- function(key, value, note) {
  FACTS[[key]] <<- list(value = dd_num(value), note = note)
  invisible(value)
}
CHECKS <- list()
check <- function(label, value) {
  CHECKS[[length(CHECKS) + 1L]] <<- list(check = label, value = value)
  say("  check: %-56s %s", label, value)
  invisible(value)
}

num <- function(d) {
  for (c in c("votes_dem", "votes_gop", "total_votes")) d[[c]] <- as.numeric(d[[c]])
  d
}
off <- num(read.csv("derived/pres2024_counties_official.csv", colClasses = "character"))
com <- num(read.csv("../../data-sources/data/derived/pres2024_counties.csv",
                    colClasses = "character"))

# --- what the assembly cost ------------------------------------------------

prov <- read.csv("provenance.csv", colClasses = "character")
fact("prov_rows",   nrow(prov),                  "documents the assembly reads")
fact("prov_states", length(unique(prov$state)),  "jurisdictions they come from")
fact("prov_formats", length(unique(prov$format)),"distinct file formats among them")
fact("prov_pdf",    sum(grepl("^pdf", prov$format)), "of the documents that are PDFs")
fact("prov_scan",   sum(prov$format == "pdf (scanned)"),
     "that are scans, with no text layer at all")
fact("prov_uncertified", sum(prov$certified != "yes"),
     "whose page does not say the returns are certified")
fmt <- as.data.frame(sort(table(prov$format), decreasing = TRUE))
names(fmt) <- c("format", "documents")
dd_write_csv(fmt, "derived/formats.csv")
check("every jurisdiction is present for both elections", nrow(prov) == 102)
check("the assembly spans more than one file format", length(unique(prov$format)) > 1)

fact("off_rows", nrow(off), "rows in the certified assembly for 2024")
fact("off_total", sum(off$total_votes), "votes for president it accounts for")

# --- rows that are not counties --------------------------------------------
#
# A row with no county FIPS is not a failure of the build. It is a jurisdiction
# that does not publish by county, recorded as what it is rather than forced
# into a code that would be wrong.

nocode <- off[off$county_fips == "" | is.na(off$county_fips), ]
fact("nocode_rows", nrow(nocode), "rows in the certified file that are not counties")
nc <- as.data.frame(table(nocode$state_name))
names(nc) <- c("jurisdiction", "rows")
nc$unit <- c("ward", "town or unorganized territory",
             "city reporting separately from its county",
             "town")[match(nc$jurisdiction,
             c("District of Columbia", "Maine", "Missouri", "Rhode Island"))]
dd_write_csv(nc, "derived/not_counties.csv")
check("some published units have no county code, and keep none",
      nrow(nocode) > 0)

# --- the same jurisdictions, as the compilation reports them ---------------

shape <- do.call(rbind, lapply(c("Rhode Island", "District of Columbia",
                                 "Connecticut"), function(s)
  data.frame(jurisdiction = s,
             certified  = nrow(off[off$state_name == s, ]),
             compilation = nrow(com[com$state_name == s, ]),
             certified_unit  = off$county_name[off$state_name == s][1],
             compilation_unit = com$county_name[com$state_name == s][1])))
dd_write_csv(shape, "derived/unit_shapes.csv")
fact("ct_certified",   shape$certified[shape$jurisdiction == "Connecticut"],
     "Connecticut units in the certified file")
fact("ct_compilation", shape$compilation[shape$jurisdiction == "Connecticut"],
     "Connecticut units in the compilation, and they are not counties")
fact("ri_certified",   shape$certified[shape$jurisdiction == "Rhode Island"],
     "Rhode Island towns in the certified file")
fact("ri_compilation", shape$compilation[shape$jurisdiction == "Rhode Island"],
     "Rhode Island counties in the compilation")
check("Connecticut is not the same geography in the two files",
      shape$certified[shape$jurisdiction == "Connecticut"] !=
      shape$compilation[shape$jurisdiction == "Connecticut"])

# --- where they can be compared at all -------------------------------------

m <- merge(off, com, by = "county_fips", suffixes = c("_o", "_c"))
m <- m[m$county_fips != "", ]
fact("matched", nrow(m), "counties carrying a code in both files")

for (v in c("dem", "gop")) {
  d <- m[[paste0("votes_", v, "_o")]] - m[[paste0("votes_", v, "_c")]]
  fact(paste0("diff_", v), sum(d != 0),
       sprintf("counties where the two disagree on the %s vote",
               ifelse(v == "dem", "Democratic", "Republican")))
}
m$dt <- m$total_votes_o - m$total_votes_c
fact("diff_total",  sum(m$dt != 0),   "counties where they disagree on votes cast")
fact("diff_total_pct", round(100 * mean(m$dt != 0), 1),
     "the same, as a share of the counties that can be compared")
fact("dt_median", median(abs(m$dt[m$dt != 0])), "the median size of that disagreement, in votes")
fact("dt_over_100",  sum(abs(m$dt) > 100),  "counties where it exceeds a hundred votes")
fact("dt_over_1000", sum(abs(m$dt) > 1000), "counties where it exceeds a thousand")
fact("dt_max", max(abs(m$dt)), "the largest single disagreement")
fact("nat_gap", sum(off$total_votes) - sum(com$total_votes),
     "votes separating the two national totals")
check("they agree about the major-party vote far more often than the total",
      sum(m$votes_dem_o != m$votes_dem_c) < sum(m$dt != 0))
check("the typical disagreement is small, so this is not a winner dispute",
      median(abs(m$dt[m$dt != 0])) < 100)

dd_write_csv(m[order(-abs(m$dt)), c("county_fips", "county_name_o", "state_name_o",
             "total_votes_o", "total_votes_c", "dt")][1:15, ],
             "derived/largest_gaps.csv")

# --- the third-party explanation, and its limits ---------------------------
#
# Where a compilation's total equals the two major parties exactly, no third
# party is in the denominator. That is checkable, and it explains part of the
# gap and nowhere near all of it.

m$third_o <- m$total_votes_o - m$votes_dem_o - m$votes_gop_o
m$third_c <- m$total_votes_c - m$votes_dem_c - m$votes_gop_c
zero_c <- m$third_c == 0
fact("third_zero", sum(zero_c), "counties whose compilation total holds no third party")
fact("third_zero_real", sum(zero_c & m$third_o > 0),
     "of them where the certified return does record third-party votes")
tz <- as.data.frame(sort(table(m$state_name_o[zero_c & m$third_o > 0]), decreasing = TRUE))
names(tz) <- c("state", "counties")
dd_write_csv(tz, "derived/third_party_zero.csv")
fact("third_explains", round(100 * sum(zero_c & m$third_o > 0) / sum(m$dt != 0), 1),
     "the share of disagreeing counties this explains")
check("dropped third parties explain a minority of the disagreement",
      sum(zero_c & m$third_o > 0) < sum(m$dt != 0) / 2)

# --- Kansas City, the largest single row --------------------------------
kc <- m[which.max(abs(m$dt)), ]
fact("kc_county", paste(kc$county_name_o, kc$state_name_o), "the largest disagreement's county")
fact("kc_off", kc$total_votes_o, "votes the certified return gives it")
fact("kc_com", kc$total_votes_c, "votes the compilation gives it")
fact("kc_dem_gap", abs(kc$votes_dem_o - kc$votes_dem_c),
     "Democratic votes that move with it")

# ---------------------------------------------------------------------------

facts <- data.frame(key = names(FACTS),
                    value = vapply(FACTS, function(x) as.character(x$value), ""),
                    note  = vapply(FACTS, function(x) x$note, ""))
dd_write_csv(facts, "derived/facts.csv")
dd_write_csv(do.call(rbind, lapply(CHECKS, as.data.frame)), "derived/checks.csv")
say("\ndone: %d facts, %d checks", nrow(facts), length(CHECKS))

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
