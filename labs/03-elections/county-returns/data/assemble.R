# Assemble the 102 official state-year files into two national tables.
#
# Column order and names match what the three consuming chapters already read,
# so the swap is a path change rather than a rewrite:
#   state_name, county_fips, county_name, votes_dem, votes_gop, total_votes
#
# NOTE ON ROWS THAT ARE NOT COUNTIES. Some states do not report by county and
# this file does not pretend otherwise -- Alaska reports by State House
# District, Rhode Island by city and town, DC by ward in 2024. Those rows carry
# the state's own unit names, and where no Census FIPS exists county_fips is
# empty rather than invented. A consumer that needs strict county geography
# must filter on nchar(county_fips) == 5 and know what it is dropping.

# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

options(stringsAsFactors = FALSE)
for (yr in c(2020, 2024)) {
  fs <- list.files("derived/states", pattern = paste0("_", yr, ".csv$"), full.names = TRUE)
  d <- do.call(rbind, lapply(fs, function(f)
    read.csv(f, colClasses = c(county_fips = "character"))))
  d <- d[order(d$state_name, d$county_fips, d$county_name), ]
  keep <- c("state_name","county_fips","county_name","votes_dem","votes_gop","total_votes")
  d <- d[, keep]
  out <- sprintf("pres%d_counties_official.csv", yr)
  write.csv(d, out, row.names = FALSE)
  cat(sprintf("%s : %s rows, %d states, %s with a 5-char FIPS\n", out,
      format(nrow(d), big.mark=","), length(unique(d$state_name)),
      format(sum(nchar(d$county_fips)==5), big.mark=",")))
}
