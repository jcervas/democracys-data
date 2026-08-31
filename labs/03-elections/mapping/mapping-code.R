# mapping-code.R -- chunk bodies for mapping-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.6,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

# FIPS is text, always. A county identifier is five digits and every county in
# Alabama, Alaska, Arizona, Arkansas, California, Colorado and Connecticut
# begins with a zero. Read as a number, 01001 becomes 1001, which is a county
# in Georgia. A sibling lab in this course lost the leading zero on 1,549 of
# 4,489 tract identifiers exactly this way.
d  <- read.csv("data/derived/counties.csv",      stringsAsFactors = FALSE,
               colClasses = c(fips = "character"))
cr <- read.csv("data/derived/county_rings.csv",  stringsAsFactors = FALSE,
               colClasses = c(id = "character"))
sr <- read.csv("data/derived/state_rings.csv",   stringsAsFactors = FALSE,
               colClasses = c(id = "character"))
dl <- read.csv("data/derived/dorling.csv",       stringsAsFactors = FALSE,
               colClasses = c(fips = "character"))
lz <- read.csv("data/derived/lorenz.csv",        stringsAsFactors = FALSE)
ex <- read.csv("data/derived/excluded.csv",      stringsAsFactors = FALSE,
               colClasses = c(fips = "character"))
pa <- read.csv("data/derived/parity.csv",        stringsAsFactors = FALSE)
ff <- read.csv("data/derived/facts.csv",         stringsAsFactors = FALSE)

# Every scalar about the map comes out of facts.csv, which data/build-data.R
# wrote from the sources it fetched. The section that checks the returns
# against the fifty-one state canvasses computes its numbers on the page, from
# the official CSVs read below. Either way, nothing here is typed in.
gs <- function(k) ff$value[ff$key == k]
g  <- function(k) as.numeric(gs(k))
pc <- function(x, k = 1) formatC(x, format = "f", digits = k, big.mark = ",")
nn <- function(x) formatC(round(as.numeric(x)), format = "d", big.mark = ",")

# ---------------------------------------------------------------------------
# THE RULER. Two files that did not exist when Figure 1 was drawn: the same
# election as the fifty-one chief election offices published it, assembled one
# jurisdiction at a time in ../county-returns/ with the URL, the format and the
# state's own word on certification recorded for each. The rule that folder
# followed is the one that matters here: a unit with no Census FIPS gets an
# EMPTY county_fips, never a guessed one.
#
# Nothing in this chapter is redrawn from these files, and the section "The
# file the states published" says why. They are read here so that every claim
# the chapter makes about the returns can be measured rather than asserted.
OFFD <- file.path("..", "county-returns", "data")
of <- read.csv(file.path(OFFD, "derived/pres2024_counties_official.csv"),
               stringsAsFactors = FALSE,
               colClasses = c(county_fips = "character"))
o20 <- read.csv(file.path(OFFD, "derived/pres2020_counties_official.csv"),
                stringsAsFactors = FALSE,
                colClasses = c(county_fips = "character"))
ap <- read.csv(file.path(OFFD, "derived/crosscheck_ap_2024.csv"),
               stringsAsFactors = FALSE)
# the compilation itself, as committed -- the same copy data/build-data.R read
cp <- read.csv(file.path("..", "data-sources", "data", "derived", "pres2024_counties.csv"),
               stringsAsFactors = FALSE,
               colClasses = c(county_fips = "character"))

# the mapped frame, unit by unit, against the states' own returns
mo   <- match(d$fips, of$county_fips)
mhit <- !is.na(mo)
o_d  <- of$votes_dem[mo]; o_g <- of$votes_gop[mo]
n_chk  <- sum(mhit)
n_miss <- sum(!mhit)
miss_by <- sort(table(d$state[!mhit]), decreasing = TRUE)
miss_txt <- paste0(nn(miss_by), " ", names(miss_by), collapse = ", ")
n_diff <- sum(o_d[mhit] != d$votes_dem[mhit] | o_g[mhit] != d$votes_gop[mhit])
n_flip <- sum(ifelse(o_g[mhit] >= o_d[mhit], "R", "D") != d$winner[mhit])
flip_nm <- d$name[mhit][ifelse(o_g[mhit] >= o_d[mhit], "R", "D") !=
                        d$winner[mhit]]
flip_st <- d$state[mhit][ifelse(o_g[mhit] >= o_d[mhit], "R", "D") !=
                         d$winner[mhit]]

# Kansas City is its own returning jurisdiction in Missouri's canvass, with no
# Census FIPS. Adding it back to Jackson County reproduces the compilation's
# row exactly, which is the check that says the two files disagree about the
# UNIT rather than about the count.
kc <- of[of$county_name == "Kansas City", ]
jk <- of[of$county_fips == "29095", ]
cj <- d[d$fips == "29095", ]
kc_exact <- (jk$votes_dem + kc$votes_dem == cj$votes_dem) &&
            (jk$votes_gop + kc$votes_gop == cj$votes_gop)
n_diff_kc <- sum((o_d[mhit] != d$votes_dem[mhit] |
                  o_g[mhit] != d$votes_gop[mhit]) & d$fips[mhit] != "29095")

# the chapter's headline ratio, computed both ways on the units both files
# cover, so that the comparison is of two files and not of two frames
sh   <- d[mhit, ]; sd_ <- o_d[mhit]; sg_ <- o_g[mhit]
inkr <- function(win, two) (sum(sh$aland_km2[win]) / sum(two[win])) /
                           (sum(sh$aland_km2[!win]) / sum(two[!win]))
ink_cmp <- inkr(sh$winner == "R", sh$two)
ink_off <- inkr(sg_ >= sd_, sd_ + sg_)

# the District, ward by ward, in the two files
dcc <- cp[substr(cp$county_fips, 1, 2) == "11", ]
dco <- of[of$state_name == "District of Columbia", ]
dck <- match(dco$county_name, dcc$county_name)         # by ward name, not row
dc_same <- sum(dcc$votes_dem[dck] == dco$votes_dem &
               dcc$votes_gop[dck] == dco$votes_gop)
dc_all  <- if (dc_same == nrow(dco)) paste("All", nn(nrow(dco))) else
             paste(nn(dc_same), "of the", nn(nrow(dco)))
# the same jurisdictions in the 2020 file, so that "in both years" is a check
dc20     <- o20[o20$state_name == "District of Columbia", ]
dc_blank <- sum(dco$county_fips == "") + sum(dc20$county_fips == "")
ct_rows  <- c(sum(of$state_name == "Connecticut"),
              sum(o20$state_name == "Connecticut"))

# every unit in the official file that the states publish without a FIPS
nofips  <- of[of$county_fips == "", ]
nof_by  <- sort(table(nofips$state_name), decreasing = TRUE)
nof_txt <- paste0(nn(nof_by), " in ", names(nof_by), collapse = ", ")
ri_rows <- sum(of$state_name == "Rhode Island")

# the national totals, three ways. The AP's certified state totals are the
# referee: neither file was built from them.
ap_dem <- sum(ap$votes_dem); cp_dem <- sum(cp$votes_dem)
of_dem <- sum(of$votes_dem)
gap_cp <- ap_dem - cp_dem; gap_of <- ap_dem - of_dem
gap_rt <- gap_cp / gap_of
PXW <- 7.2 * 96          # the printed width of a figure in pixels, taken from
                         # the chunk options set above

RED <- gs("col_gop"); BLU <- gs("col_dem")
W   <- g("canvas_w"); H <- g("canvas_h"); CELL <- g("grid_cell_units")
X0  <- g("canvas_x0"); Y0 <- g("canvas_y0")   # the CONUS crop of the shared
                                              # frame the canvas lives on
KMU <- g("km_per_unit")

# the two counties this chapter opens on
big <- d[d$aland_km2 == max(d$aland_km2), ]
sml <- d[d$density   == max(d$density),   ]
# how many mapped states are smaller than the largest county, and which state
# packs in the most counties: counted here rather than remembered
starea  <- tapply(d$aland_km2, d$state, sum)
n_small <- sum(starea < big$aland_km2)
ncty    <- sort(table(d$state), decreasing = TRUE)

# draw order: largest land area first, so that a county drawn inside another
# county's hole -- every Virginia independent city -- lands on top of it.
ord <- order(-d$aland_km2)
D   <- d[ord, ]
DCOL <- ifelse(D$winner == "R", RED, BLU)

# One path per county, all of its rings in one string, rendered even-odd so a
# ring inside a ring is a hole rather than a blot -- which is what a Virginia
# county with an independent city cut out of it actually is. Segments after the
# first are written as steps from the previous vertex rather than as absolute
# positions: the vertices are integers, so the running total is exact, and the
# browser figure loses about a third of its weight.
prt   <- split(seq_len(nrow(cr)), cr$part)
pid   <- vapply(prt, function(i) cr$id[i[1]], character(1))
byc   <- split(names(pid), pid)
step  <- function(x, y) paste0("M", x[1], ",", y[1],
  if (length(x) > 1) paste0("l", paste0(diff(x), ",", diff(y), collapse = "l")) else "",
  "Z")
pstr  <- vapply(prt, function(i) step(cr$x[i], cr$y[i]), character(1))
cpath <- tapply(pstr, pid, paste, collapse = "")
D$path <- cpath[D$fips]
ST     <- sort(unique(D$state))                    # every jurisdiction drawn;
NST    <- sum(ST != "District of Columbia")        # the District is not a state
D$sidx <- match(D$state, ST) - 1L

# render every data.frame in this document as a table rather than as code output
knit_print.data.frame <- function(x, ...) {
  nm <- gsub("_", " ", names(x))
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

# the concentration curve, thinned once, here, so that both renderers draw the
# same polyline from the same vertices
kx <- sort(unique(c(1, seq(1, nrow(lz), by = 8), nrow(lz))))
LZ <- lz[kx, ]
RUG <- paste(lz$winner, collapse = "")

## ---- fig1-d3
# ---------------------------------------------------------------------------
# THE OBJECT THIS CHAPTER IS ABOUT: the standard county choropleth, drawn from
# the Census Bureau's own boundaries and a national compilation of county
# returns. Coordinates were projected (Albers equal-area), simplified and
# turned into integers in data/build-data.R; this chunk does no arithmetic on
# them beyond pasting them into a path string, and neither does the print
# version below. That is why the two figures cannot disagree.
#
# This chunk carries the ONE d3 <script src> for the document, and publishes
# the county metadata as a shared array that Figures 2 and 4 reuse, so that all
# three maps are guaranteed to be describing the same 3,109 counties.
# ---------------------------------------------------------------------------
# The hover strings live only in the browser figure -- the print figures carry
# no hover text at all -- so the vote and area counts travel as bare integers
# and are given their thousands separators by the browser. No number that both
# renderers display is formatted anywhere but in R.
meta <- paste(paste0('["', gsub('"', "", D$name), '",', D$sidx,
  ',"', D$winner, '",', round(D$two), ',', round(D$aland_km2), ']'),
  collapse = ",")
paths <- paste(paste0('"', D$path, '"'), collapse = ",")
cat(paste0('
<div id="fig1" style="margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
window.CTY=[', meta, '];
window.STA=["', paste(gsub('"', "", ST), collapse = '","'), '"];
window.PAL={R:"', RED, '",D:"', BLU, '"};
window.MAPW=', W, ';window.MAPH=', H, ';window.MAPX=', X0, ';window.MAPY=', Y0, ';
window.CNAME=function(i){return "<b>"+window.CTY[i][0]+"</b>, "+
  window.STA[window.CTY[i][1]];};
window.CNUM=function(v){return v.toLocaleString("en-US");};
(function(){
const P=[', paths, '],C=window.CTY,PAL=window.PAL;
const svg=d3.select("#fig1").append("svg")
  .attr("viewBox",window.MAPX+" "+window.MAPY+" "+window.MAPW+" "+window.MAPH)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const cap=d3.select("#fig1").append("p").attr("style",
  "font-size:0.86em;color:#444;min-height:2.6em;margin:0.3em 0 0 0");
const DEF="Each county filled with the color of the candidate who carried it. "+
  "<i>Hover a county for its votes and its land area.</i>";
cap.html(DEF);
const g=svg.append("g");
g.selectAll("path").data(P.map((d,i)=>i)).join("path")
  .attr("d",i=>P[i]).attr("fill",i=>PAL[C[i][2]])
  .attr("fill-rule","evenodd")
  .attr("stroke","#ffffff").attr("stroke-width",0.7)
  .style("cursor","pointer")
  .on("mouseenter",function(e,i){
    d3.select(this).attr("stroke","#111").attr("stroke-width",3).raise();
    cap.html(window.CNAME(i)+" &mdash; "+window.CNUM(C[i][3])+" votes on "+
      window.CNUM(C[i][4])+" km&sup2; of land.");
  })
  .on("mouseleave",function(){
    d3.select(this).attr("stroke","#ffffff").attr("stroke-width",0.7);
    cap.html(DEF);});
const S=[', paste(vapply(split(seq_len(nrow(sr)), sr$part), function(i)
    paste0('"', step(sr$x[i], sr$y[i]), '"'), character(1)), collapse = ","), '];
svg.append("g").selectAll("path").data(S).join("path").attr("d",d=>d)
  .attr("fill","none").attr("stroke","#ffffff").attr("stroke-width",2.4)
  .attr("pointer-events","none");
const lg=svg.append("g").attr("transform","translate(', X0 + 24, ','
, round(Y0 + H - 96), ')");
[["R","Trump", "', nn(g("n_R")), ' counties"],
 ["D","Harris","', nn(g("n_D")), ' counties"]].forEach((r,i)=>{
  lg.append("rect").attr("y",i*30).attr("width",22).attr("height",22)
    .attr("fill",PAL[r[0]]).attr("stroke","#666").attr("stroke-width",0.6);
  lg.append("text").attr("x",30).attr("y",i*30+16).attr("font-size","17px")
    .text(r[1]+", "+r[2]);
});
})();
</script>'))

## ---- fig1-static
# The same integers, the same even-odd rule, the same draw order. polypath()
# rather than polygon() because a Virginia county with an independent city cut
# out of it is a ring inside a ring, and polygon() would fill the hole.
par(mar = rep(0.1, 4))
plot(NA, xlim = c(X0, X0 + W), ylim = c(Y0 + H, Y0), asp = 1, axes = FALSE, ann = FALSE)
for (i in seq_len(nrow(D))) {
  p  <- prt[byc[[D$fips[i]]]]
  xs <- head(unlist(lapply(p, function(k) c(cr$x[k], NA)), use.names = FALSE), -1)
  ys <- head(unlist(lapply(p, function(k) c(cr$y[k], NA)), use.names = FALSE), -1)
  polypath(xs, ys, col = DCOL[i], border = "#ffffff", lwd = 0.1,
           rule = "evenodd")
}
for (k in split(seq_len(nrow(sr)), sr$part))
  polypath(sr$x[k], sr$y[k], col = NA, border = "#ffffff", lwd = 0.7)
legend(X0 + 24, Y0 + H - 70, c(paste0("Trump, ", nn(g("n_R")), " counties"),
                     paste0("Harris, ", nn(g("n_D")), " counties")),
       fill = c(RED, BLU), border = "#666666", bty = "n", cex = 0.72)

## ---- units
# Alaska's two counts -- districts in the returns, boroughs and census areas in
# the geography -- are read out of data/excluded.csv, which is where the build
# script recorded them, so this row and the exclusion table below cannot drift
# apart. Nothing here is typed.
akr <- unique(ex$reason[ex$state == "Alaska"])
akn <- as.numeric(regmatches(akr, gregexpr("[0-9]+", akr))[[1]])
data.frame(
  # The first header is long on purpose: it is what tells the PDF's table
  # layout to leave column one wide enough for "Connecticut" on one line.
  `where the sources disagree` = c("Alaska", "District of Columbia",
                                   "Connecticut"),
  `what goes wrong` = c(
    paste0("The returns report ", nn(akn[1]),
           " State House Districts under invented FIPS codes; the geography file has ",
           nn(akn[2]), " boroughs and census areas"),
    paste0("The returns report ", nn(g("dc_wards")), " wards, and 11001 means ",
           gs("dc_naive_name"), ", not the District"),
    paste0("The counties were abolished in 2022 and replaced by ",
           nn(g("ct_units")), " planning regions")),
  `what was done about it` = c(
    paste0("Dropped whole (", nn(g("ak_votes")),
           " votes), along with Hawaii, which is outside the frame"),
    paste0("The ", nn(g("dc_wards")), " wards summed back to one District (",
           nn(g("dc_true_votes")), " votes)"),
    paste0("Nothing: the 2024 boundary file already reports the ",
           nn(g("ct_units")), " regions, so the vintages agree")),
  check.names = FALSE)

## ---- excluded
# read straight off data/excluded.csv, which names every unit individually
exs <- aggregate(cbind(units = rep(1, nrow(ex)), votes = ex$votes),
                 by = list(reason = ex$reason), FUN = sum)
data.frame(`why it is not on the map` = exs$reason,
           units = nn(exs$units), `votes involved` = nn(exs$votes),
           check.names = FALSE)

## ---- clean-mapping
o <- d[d$fips == "11001", c("fips", "name", "votes_dem", "votes_gop",
                            "votes_tot", "winner", "aland_km2")]
o$votes_dem <- nn(o$votes_dem); o$votes_gop <- nn(o$votes_gop)
o$votes_tot <- nn(o$votes_tot); o$aland_km2 <- pc(o$aland_km2, 1)
names(o) <- c("fips", "name", "dem", "rep", "total", "winner", "land km2")
o

## ---- clean-rings
o <- head(cr[cr$id == "11001", ], 4)
names(o) <- c("id", "part", "x", "y")
o

## ---- official-dc
# Read out of the file, not typed. The compilation's block earlier in this
# chapter is a verbatim capture; this one is generated, which is the only way
# to guarantee that the thing being contrasted with it is current.
o <- dco[1:4, ]
# Same columns and same order as the compilation's table above, so the empty
# identifier column can be found by looking at the same place twice.
rbind(
  data.frame(state_name  = o$state_name,
             county_fips = ifelse(nzchar(o$county_fips), o$county_fips,
                                  "(empty)"),
             county_name = o$county_name,
             votes_dem   = nn(o$votes_dem),
             votes_gop   = nn(o$votes_gop),
             total_votes = nn(o$total_votes),
             stringsAsFactors = FALSE),
  data.frame(state_name = "… four more wards", county_fips = "",
             county_name = "", votes_dem = "", votes_gop = "",
             total_votes = "", stringsAsFactors = FALSE))

## ---- official-frame
data.frame(
  `the mapped frame against the states' own returns` = c(
    "Mapped units with a row in the official file",
    "Mapped units with no official counterpart",
    "Units where a major-party count differs",
    "Units where the winner differs",
    "Units still differing once Kansas City is put back"),
  count = c(paste0(nn(n_chk), " of ", nn(nrow(d))),
            paste0(nn(n_miss), "  (", miss_txt, ")"),
            nn(n_diff),
            paste0(nn(n_flip), "  (", flip_nm, " County, ", flip_st, ")"),
            nn(n_diff_kc)),
  check.names = FALSE)

## ---- ledger
data.frame(
  quantity = c("Counties", "Land area", "Population, 2020",
               "Votes cast for the two candidates"),
  `carried by Trump` = c(paste0(nn(g("n_R")), "  (", pc(g("pct_counties_R"), 1), "%)"),
                         paste0(nn(g("land_R_km2")), " km²  (", pc(g("pct_land_R"), 1), "%)"),
                         paste0(nn(g("pop_R")), "  (", pc(g("pct_pop_R"), 1), "%)"),
                         paste0(nn(g("votes_R_counties")), "  (",
                                pc(g("pct_votes_R_counties"), 1), "%)")),
  `carried by Harris` = c(paste0(nn(g("n_D")), "  (", pc(100 - g("pct_counties_R"), 1), "%)"),
                          paste0(nn(g("land_total_km2") - g("land_R_km2")), " km²  (",
                                 pc(g("pct_land_D"), 1), "%)"),
                          paste0(nn(g("pop_D")), "  (", pc(100 - g("pct_pop_R"), 1), "%)"),
                          paste0(nn(g("votes_D_counties")), "  (",
                                 pc(g("pct_votes_D_counties"), 1), "%)")),
  check.names = FALSE)

## ---- fig2-d3
# ---------------------------------------------------------------------------
# ONE IDENTICAL SQUARE PER COUNTY. The layout was solved once in
# data/build-data.R; the cell coordinates are integers on the same canvas as
# Figure 1, so the two maps are in register and the reader can compare shapes.
# Metadata comes from window.CTY, published by Figure 1 -- the same array, the
# same order, so this figure cannot drift out of step with that one.
# ---------------------------------------------------------------------------
cells <- paste(paste0("[", D$gcol, ",", D$grow, "]"), collapse = ",")
cat(paste0('
<div id="fig2" style="margin:1em 0"></div>
<script>
(function(){
const G=[', cells, '],C=window.CTY,PAL=window.PAL,S=', CELL,
',OX=', min(D$gx) - min(D$gcol) * CELL, ',OY=', min(D$gy) - min(D$grow) * CELL, ';
const svg=d3.select("#fig2").append("svg")
  .attr("viewBox",window.MAPX+" "+window.MAPY+" "+window.MAPW+" "+window.MAPH)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const cap=d3.select("#fig2").append("p").attr("style",
  "font-size:0.86em;color:#444;min-height:2.6em;margin:0.3em 0 0 0");
const DEF="One square per county, every square the same size, positioned as "+
  "near as the grid allows to the county\'s real location. "+
  "<i>Hover a square.</i>";
cap.html(DEF);
svg.append("g").selectAll("rect").data(G.map((d,i)=>i)).join("rect")
  .attr("x",i=>OX+G[i][0]*S).attr("y",i=>OY+G[i][1]*S)
  .attr("width",S).attr("height",S)
  .attr("fill",i=>PAL[C[i][2]]).style("cursor","pointer")
  .on("mouseenter",function(e,i){
    d3.select(this).attr("stroke","#111").attr("stroke-width",2).raise();
    cap.html(window.CNAME(i)+" &mdash; "+window.CNUM(C[i][3])+" votes. On this "+
      "map it gets the same square as every other county; on Figure 1 it got "+
      window.CNUM(C[i][4])+" km&sup2;.");
  })
  .on("mouseleave",function(){
    d3.select(this).attr("stroke",null);cap.html(DEF);});
const lg=svg.append("g").attr("transform","translate(', X0 + 24, ',', round(Y0 + H - 96), ')");
[["R","Trump"],["D","Harris"]].forEach((r,i)=>{
  lg.append("rect").attr("y",i*30).attr("width",22).attr("height",22)
    .attr("fill",PAL[r[0]]);
  lg.append("text").attr("x",30).attr("y",i*30+16).attr("font-size","17px")
    .text(r[1]);
});
lg.append("text").attr("y",76).attr("font-size","15px").attr("fill","#555")
  .text("one square = one county = ', pc(g("grid_cell_km"), 1), ' km across");
})();
</script>'))

## ---- fig2-static
par(mar = rep(0.1, 4))
plot(NA, xlim = c(X0, X0 + W), ylim = c(Y0 + H, Y0), asp = 1, axes = FALSE, ann = FALSE)
rect(D$gx, D$gy + CELL, D$gx + CELL, D$gy, col = DCOL, border = NA)
legend(X0 + 24, Y0 + H - 70, c("Trump", "Harris"), fill = c(RED, BLU), border = NA,
       bty = "n", cex = 0.72,
       title = paste0("one square = one county"), title.adj = 0)

## ---- fig3-d3
# ---------------------------------------------------------------------------
# The two cumulative curves, plus a strip under the axis recording who carried
# each county in the same order, so that the party pattern and the population
# pattern can be read off one picture. The polyline was thinned once in the
# setup chunk and both renderers draw the identical vertices; the strip is the
# full 3,109-character winner sequence, which costs almost nothing.
# ---------------------------------------------------------------------------
cat(paste0('
<div id="fig3" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const X=[', paste(sprintf("%.3f", LZ$cum_counties), collapse = ","), '],
      L=[', paste(sprintf("%.3f", LZ$cum_land),     collapse = ","), '],
      V=[', paste(sprintf("%.3f", LZ$cum_votes),    collapse = ","), '],
      RG="', RUG, '",PAL=window.PAL;
const W=760,H=430,M={t:44,r:118,b:46,l:52};
const svg=d3.select("#fig3").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,100]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,120]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickValues([0,25,50,75,100]).tickFormat(d=>d+"%"));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickValues([0,25,50,75,100]).tickFormat(d=>d+"%"));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-8)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("counties, ordered from the emptiest land to the fullest");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",14).attr("text-anchor","middle").attr("font-size","12px")
  .attr("fill","#444").text("cumulative share of the national total");
const ln=d3.line().x((d,i)=>x(X[i])).y(d=>y(d));
svg.append("path").datum(L).attr("d",ln).attr("fill","none")
  .attr("stroke","#8a6d3b").attr("stroke-width",2.6);
svg.append("path").datum(V).attr("d",ln).attr("fill","none")
  .attr("stroke","#111111").attr("stroke-width",2.6);
svg.append("text").attr("x",W-M.r+7).attr("y",y(100)+4).attr("font-size","12.5px")
  .attr("font-weight","600").attr("fill","#8a6d3b").text("land");
svg.append("text").attr("x",W-M.r+7).attr("y",y(88)+4).attr("font-size","12.5px")
  .attr("font-weight","600").attr("fill","#111111").text("votes");
[[50,', sprintf("%.1f", g("land_at_half_counties")), ',"#8a6d3b"],
 [50,', sprintf("%.1f", g("votes_at_half_counties")), ',"#111111"]].forEach(p=>{
  svg.append("line").attr("x1",x(p[0])).attr("x2",x(p[0])).attr("y1",y(0))
    .attr("y2",y(p[1])).attr("stroke","#bbb").attr("stroke-dasharray","3,3");
  svg.append("circle").attr("cx",x(p[0])).attr("cy",y(p[1])).attr("r",4)
    .attr("fill",p[2]);
  svg.append("text").attr("x",x(p[0])+7).attr("y",y(p[1])+4)
    .attr("font-size","12px").attr("fill",p[2]).text(p[1]+"%");
});
svg.append("line").attr("x1",M.l).attr("x2",x(', sprintf("%.2f", g("counties_for_half_votes")), '))
  .attr("y1",y(50)).attr("y2",y(50)).attr("stroke","#111")
  .attr("stroke-dasharray","4,3").attr("opacity",0.55);
svg.append("text").attr("x",x(', sprintf("%.2f", g("counties_for_half_votes")), ')-6)
  .attr("y",y(50)-7).attr("text-anchor","end").attr("font-size","11.5px")
  .text("half the votes are in by the ', pc(g("counties_for_half_votes"), 1), 'th percentile of counties");
const sw=(x(100)-x(0))/RG.length;
const gg=svg.append("g");
for(let i=0;i<RG.length;i++){
  gg.append("rect").attr("x",x(0)+i*sw).attr("y",y(110))
    .attr("width",Math.max(sw,0.35)).attr("height",y(104)-y(110))
    .attr("fill",PAL[RG[i]]);
}
svg.append("text").attr("x",x(0)).attr("y",y(116)).attr("font-size","11px")
  .attr("fill","#555").text("who carried each county, in the same order");
})();
</script>'))

## ---- fig3-static
par(mar = c(3.6, 4.2, 2.4, 5.4), mgp = c(2.4, 0.7, 0))
plot(NA, xlim = c(0, 100), ylim = c(0, 120), axes = FALSE,
     xlab = "counties, ordered from the emptiest land to the fullest",
     ylab = "cumulative share of the national total")
axis(1, at = c(0, 25, 50, 75, 100), labels = paste0(c(0, 25, 50, 75, 100), "%"),
     cex.axis = 0.8, tcl = -0.25)
axis(2, at = c(0, 25, 50, 75, 100), labels = paste0(c(0, 25, 50, 75, 100), "%"),
     las = 1, cex.axis = 0.8, tcl = -0.25)
lines(LZ$cum_counties, LZ$cum_land,  col = "#8a6d3b", lwd = 2.4)
lines(LZ$cum_counties, LZ$cum_votes, col = "#111111", lwd = 2.4)
text(101, 100, "land",  adj = 0, cex = 0.78, font = 2, col = "#8a6d3b", xpd = NA)
text(101, 88,  "votes", adj = 0, cex = 0.78, font = 2, col = "#111111", xpd = NA)
for (p in list(c(50, g("land_at_half_counties"), 1), c(50, g("votes_at_half_counties"), 2))) {
  cl <- c("#8a6d3b", "#111111")[p[3]]
  segments(p[1], 0, p[1], p[2], col = "#bbbbbb", lty = 3)
  points(p[1], p[2], pch = 19, col = cl, cex = 0.8)
  text(p[1] + 1.5, p[2], paste0(pc(p[2], 1), "%"), adj = 0, cex = 0.72, col = cl)
}
segments(0, 50, g("counties_for_half_votes"), 50, col = "#111111", lty = 2)
text(g("counties_for_half_votes") - 1, 54,
     paste0("half the votes are in by the ", pc(g("counties_for_half_votes"), 1),
            "th percentile"), adj = 1, cex = 0.66)
sq <- seq(0, 100, length.out = nrow(lz))
rect(sq, 104, sq + 100 / nrow(lz), 110,
     col = ifelse(lz$winner == "R", RED, BLU), border = NA)
text(0, 116, "who carried each county, in the same order", adj = c(0, 0.5),
     cex = 0.66, col = "#555555")

## ---- conc
data.frame(
  quantity = c("Counties casting half the two-party vote (largest first)",
               "Land those counties cover",
               "Counties casting the other half",
               "Land those counties cover",
               "Median county, two-party votes",
               "Median county, land area"),
  value = c(nn(g("counties_for_half")),
            paste0(pc(g("land_for_half"), 1), "% of the total"),
            nn(nrow(d) - g("counties_for_half")),
            paste0(pc(100 - g("land_for_half"), 1), "% of the total"),
            nn(g("median_votes")),
            paste0(nn(g("median_km2")), " km²")))

## ---- fig4-d3
# ---------------------------------------------------------------------------
# AREA = VOTES. Radius is a single constant times the square root of the county
# two-party vote, one constant for the whole map, stated in the legend, so any
# two circles on this page are directly comparable and convertible back into
# votes. The relaxed positions were solved in data/build-data.R and are read
# here as integers, as everywhere else; the print figure reads the same file.
# Circles are drawn largest first so that a small one is never buried.
# ---------------------------------------------------------------------------
dd <- dl[match(D$fips, dl$fips), ]
circ <- paste(paste0("[", dd$x, ",", dd$y, ",", dd$r, "]"), collapse = ",")
LEGV <- c(50000, 250000, 1000000)
LEGR <- g("dorling_S") * sqrt(LEGV)
cat(paste0('
<div id="fig4" style="margin:1em 0"></div>
<script>
(function(){
const K=[', circ, '],C=window.CTY,PAL=window.PAL;
const svg=d3.select("#fig4").append("svg")
  .attr("viewBox",window.MAPX+" "+window.MAPY+" "+window.MAPW+" "+window.MAPH)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const cap=d3.select("#fig4").append("p").attr("style",
  "font-size:0.86em;color:#444;min-height:2.6em;margin:0.3em 0 0 0");
const DEF="One circle per county, its AREA proportional to the two-party votes "+
  "cast there. <i>Hover a circle.</i>";
cap.html(DEF);
const Z=K.map((d,i)=>i).sort((a,b)=>K[b][2]-K[a][2]);
svg.append("g").selectAll("circle").data(Z).join("circle")
  .attr("cx",i=>K[i][0]).attr("cy",i=>K[i][1]).attr("r",i=>K[i][2])
  .attr("fill",i=>PAL[C[i][2]]).attr("stroke","#fff").attr("stroke-width",0.5)
  .style("cursor","pointer")
  .on("mouseenter",function(e,i){
    d3.select(this).attr("stroke","#111").attr("stroke-width",2).raise();
    cap.html(window.CNAME(i)+" &mdash; "+window.CNUM(C[i][3])+" votes. Its "+
      "circle is this size because of that vote count and nothing else; on "+
      "Figure 1 its size was "+window.CNUM(C[i][4])+" km&sup2; of land.");
  })
  .on("mouseleave",function(){
    d3.select(this).attr("stroke","#fff").attr("stroke-width",0.5);
    cap.html(DEF);});
const lg=svg.append("g").attr("transform","translate(', X0 + 30, ',', round(Y0 + H - 150), ')");
[["R","Trump"],["D","Harris"]].forEach((r,i)=>{
  lg.append("rect").attr("y",i*30).attr("width",22).attr("height",22)
    .attr("fill",PAL[r[0]]);
  lg.append("text").attr("x",30).attr("y",i*30+16).attr("font-size","17px")
    .text(r[1]);
});
// Nested circles on a common baseline, labeled to the right, so that three
// very different radii can share a small corner of the map without colliding.
const sz=[', paste(sprintf("[%.2f,\"%s\"]", LEGR, nn(LEGV)), collapse = ","), '];
const RX=190,BY=76,RMAX=', sprintf("%.2f", max(LEGR)), ';
sz.forEach(s=>{
  lg.append("circle").attr("cx",RX+RMAX).attr("cy",BY-s[0]).attr("r",s[0])
    .attr("fill","none").attr("stroke","#555").attr("stroke-width",1);
  lg.append("line").attr("x1",RX+RMAX).attr("x2",RX+2*RMAX+8)
    .attr("y1",BY-2*s[0]).attr("y2",BY-2*s[0])
    .attr("stroke","#999").attr("stroke-width",0.8);
  lg.append("text").attr("x",RX+2*RMAX+12).attr("y",BY-2*s[0]+5)
    .attr("font-size","14px").attr("fill","#444").text(s[1]);
});
lg.append("text").attr("x",RX).attr("y",BY+20).attr("font-size","14px")
  .attr("fill","#444").text("votes cast, area to scale");
})();
</script>'))

## ---- fig4-static
par(mar = rep(0.1, 4))
dd <- dl[match(D$fips, dl$fips), ]
o  <- order(-dd$r)
plot(NA, xlim = c(X0, X0 + W), ylim = c(Y0 + H, Y0), asp = 1, axes = FALSE, ann = FALSE)
symbols(dd$x[o], dd$y[o], circles = dd$r[o], inches = FALSE, add = TRUE,
        bg = DCOL[o], fg = "#ffffff", lwd = 0.2)
legend(X0 + 30, Y0 + H - 120, c("Trump", "Harris"), fill = c(RED, BLU), border = NA,
       bty = "n", cex = 0.72)
# The same nested-circle key as the browser figure: common baseline, labels to
# the right on leader lines, so nothing collides at any of the three radii.
LEGV <- c(50000, 250000, 1000000)
LEGR <- g("dorling_S") * sqrt(LEGV)
RX <- X0 + 190; BY <- Y0 + H - 74; RM <- max(LEGR)
for (i in seq_along(LEGR)) {
  symbols(RX + RM, BY - LEGR[i], circles = LEGR[i], inches = FALSE, add = TRUE,
          fg = "#555555", lwd = 0.7)
  segments(RX + RM, BY - 2 * LEGR[i], RX + 2 * RM + 8, BY - 2 * LEGR[i],
           col = "#999999", lwd = 0.6)
  text(RX + 2 * RM + 12, BY - 2 * LEGR[i], nn(LEGV[i]), cex = 0.56, adj = 0,
       col = "#444444")
}
text(RX, BY + 18, "votes cast, area to scale", cex = 0.6, adj = 0,
     col = "#444444")

## ---- encodings
data.frame(
  figure = c("1. Choropleth", "2. Grid cartogram", "3. Cumulative curves",
             "4. Dorling cartogram"),
  `area means` = c("land", "one county", "nothing; position means share",
                               "votes cast"),
  `good for` = c("Which candidate carried the ground you are standing on",
                            "How many counties, and roughly where",
                            "How concentrated the electorate is",
                            "How many votes, and roughly where"),
  `destroys` = c("Any sense of how many people live there",
                         "The size of a county, and of its electorate",
                         "Geography entirely",
                         "Shape, adjacency, and finding your own county"),
  check.names = FALSE)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
