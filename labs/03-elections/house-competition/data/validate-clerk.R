# Compare the Clerk parse against Jacobson on the years both cover (2004-2014).
# raw/ holds the sources as they arrive; derived/ is what this script writes.
dir.create("derived", showWarnings = FALSE)

j <- read.csv("derived/races.csv", stringsAsFactors = FALSE)
k <- read.csv("derived/clerk_house.csv", stringsAsFactors = FALSE)
ov <- sort(intersect(unique(j$year), unique(k$year)))
cat("overlap years:", paste(ov, collapse=", "), "\n\n")
out <- do.call(rbind, lapply(ov, function(y) {
  a <- j[j$year==y, c("stcd","dv","uncontested")]
  b <- k[k$year==y, c("stcd","dv","uncontested","top_two")]
  m <- merge(a, b, by="stcd", suffixes=c("_jac","_clerk"))
  d <- m$dv_clerk - m$dv_jac
  data.frame(year=y, jac=nrow(a), clerk=nrow(b), matched=nrow(m),
             dv_both=sum(!is.na(d)),
             median_abs_diff=round(median(abs(d), na.rm=TRUE), 3),
             pct_within_0.5=round(100*mean(abs(d) < 0.5, na.rm=TRUE), 1),
             worst=round(max(abs(d), na.rm=TRUE), 1),
             unc_jac=sum(a$uncontested), unc_clerk=sum(b$uncontested),
             top_two=sum(b$top_two))
}))
print(out, row.names=FALSE)
cat("\noverall: median |dv difference| =",
    round(median(abs(do.call(c, lapply(ov, function(y){
      m <- merge(j[j$year==y,c("stcd","dv")], k[k$year==y,c("stcd","dv")],
                 by="stcd", suffixes=c("_j","_k")); m$dv_k - m$dv_j}))), na.rm=TRUE), 3), "points\n")
