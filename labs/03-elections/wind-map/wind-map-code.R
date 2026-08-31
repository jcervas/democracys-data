# wind-map-code.R -- chunk bodies for wind-map-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.6, dpi = 96,
                      fig.retina = 1)
options(scipen = 999)

# The D3 figures follow the page theme through CSS. The base-R ones are
# rasterised and cannot, so at least stop them carrying a paper colour of
# their own: a transparent PNG sits on whatever paper the reader has, and the
# ink goes to a mid grey that holds up against both. In print, where the paper
# is known to be white, the ink goes back to near-black.
HTMLOUT <- knitr::is_html_output()
if (HTMLOUT) knitr::opts_chunk$set(dev.args = list(bg = "transparent"))
D <- "data"

us  <- read.csv(file.path(D, "derived/wind_us.csv"), colClasses = c(county_fips = "character"))
ga  <- read.csv(file.path(D, "derived/wind_ga.csv"), colClasses = c(fips = "character"))
uo  <- read.csv(file.path(D, "derived/us_outline.csv"))
go  <- read.csv(file.path(D, "derived/ga_outline.csv"))
aud <- read.csv(file.path(D, "derived/provenance_audit.csv"), colClasses = c(fips = "character"))
lad <- read.csv(file.path(D, "derived/precinct_join_ladder.csv"))
res <- read.csv(file.path(D, "derived/precinct_join_residual.csv"))
og  <- read.csv(file.path(D, "derived/office_gap.csv"))
g22 <- read.csv(file.path(D, "derived/wind_ga_counties.csv"), colClasses = c(fips = "character"))
u16 <- read.csv(file.path(D, "derived/wind_us_1620.csv"), colClasses = c(county_fips = "character"))
crr <- read.csv(file.path(D, "derived/correction_2016.csv"), colClasses = c(county_fips = "character"))
u24 <- read.csv(file.path(D, "derived/wind_us_1624.csv"), colClasses = c(county_fips = "character"))
ff  <- read.csv(file.path(D, "derived/facts.csv"))
# Every county's state, keyed by FIPS. The 2016 file carries no state column
# and the 2024 file has dropped Connecticut, so neither can name every unit on
# its own; the centroid file covers all 3,221 and is already here.
cc  <- read.csv(file.path(D, "derived/county_centroids.csv"), colClasses = c(fips = "character"))
st_of <- function(fips) cc$state[match(fips, cc$fips)]

# ---------------------------------------------------------------------------
# THE RULER. Everything above is the compilation and the files built from it.
# What follows is the same two elections as the fifty-one chief election
# offices published them, assembled one jurisdiction at a time in
# ../county-returns/ with the URL, the format and the state's own word on
# certification recorded for each, under a rule that decides most of what the
# section "The same election, as the states published it" makes of them: a
# unit with no Census FIPS gets an EMPTY identifier, never a guessed one.
#
# NOT ONE ARROW IN THIS CHAPTER IS DRAWN FROM THESE FILES. The compilation is
# still the cautionary map and the audit is still the point of it. These are
# read so that the audit can say WHERE each break happened -- in the state or
# in the compiler -- which is the one thing it could not say before.
OFFD <- file.path("..", "county-returns", "data")
of20 <- read.csv(file.path(OFFD, "derived/pres2020_counties_official.csv"),
                 colClasses = c(county_fips = "character"))
of24 <- read.csv(file.path(OFFD, "derived/pres2024_counties_official.csv"),
                 colClasses = c(county_fips = "character"))
apx  <- read.csv(file.path(OFFD, "derived/crosscheck_ap_2024.csv"))
# the compilation itself, as committed: the same two files the builds read
cm20 <- read.csv(file.path("..", "data-sources", "data", "derived",
                           "pres2020_counties.csv"),
                 colClasses = c(county_fips = "character"))
cm24 <- read.csv(file.path("..", "data-sources", "data", "derived",
                           "pres2024_counties.csv"),
                 colClasses = c(county_fips = "character"))

mgn <- function(dm, gp) 100 * (gp - dm) / (gp + dm)
nst <- function(x, s) sum(x$state_name == s)
row_of <- function(x, f) x[x$county_fips == f, ]
swg <- function(a, b) mgn(b$votes_dem, b$votes_gop) -
                      mgn(a$votes_dem, a$votes_gop)
plus <- function(a, b) data.frame(votes_dem = a$votes_dem + b$votes_dem,
                                  votes_gop = a$votes_gop + b$votes_gop)

# how far apart the two files are, county by county, in each year
diverge <- function(o, cp) {
  o <- o[nchar(o$county_fips) == 5, ]
  ii <- match(cp$county_fips, o$county_fips); kk <- !is.na(ii)
  list(checked = sum(kk), unmatched = sum(!kk),
       differ = sum(cp$votes_dem[kk] != o$votes_dem[ii[kk]] |
                    cp$votes_gop[kk] != o$votes_gop[ii[kk]]))
}
DV20 <- diverge(of20, cm20); DV24 <- diverge(of24, cm24)

# and state by state, on the two-party margin the arrows actually draw
stagg <- function(x) aggregate(cbind(dem = x$votes_dem, gop = x$votes_gop),
                               by = list(state = x$state_name), sum)
serr <- function(o, cp) {
  A <- stagg(o); B <- stagg(cp); ii <- match(A$state, B$state)
  e <- mgn(B$dem[ii], B$gop[ii]) - mgn(A$dem, A$gop)
  list(med = median(abs(e)), max = max(abs(e)),
       who = A$state[which.max(abs(e))], over = sum(abs(e) > 0.1),
       exact = sum(e == 0), n = length(e))
}
SE20 <- serr(of20, cm20); SE24 <- serr(of24, cm24)

# --- Connecticut: eight counties in the state's own returns, both years -----
ct20 <- of20[substr(of20$county_fips, 1, 2) == "09", ]
ct24 <- of24[substr(of24$county_fips, 1, 2) == "09", ]
ctm  <- match(ct20$county_fips, ct24$county_fips)
CTT  <- data.frame(county = sub(" County$", "", ct20$county_name),
                   m20 = mgn(ct20$votes_dem, ct20$votes_gop),
                   m24 = mgn(ct24$votes_dem[ctm], ct24$votes_gop[ctm]))
CTT$swing <- CTT$m24 - CTT$m20
CTT$two24 <- ct24$votes_dem[ctm] + ct24$votes_gop[ctm]
ct_agg <- mgn(sum(ct24$votes_dem), sum(ct24$votes_gop)) -
          mgn(sum(ct20$votes_dem), sum(ct20$votes_gop))
ct_two <- sum(ct24$votes_dem + ct24$votes_gop)
# the compilation's nine 2024 regions against the state's eight counties: the
# gap is the SOTS town export, which is where planning regions can be built
ct_cmp_d <- sum(cm24$votes_dem[substr(cm24$county_fips, 1, 2) == "09"])
ct_cmp_g <- sum(cm24$votes_gop[substr(cm24$county_fips, 1, 2) == "09"])
ct_gap_d <- sum(ct24$votes_dem) - ct_cmp_d
ct_gap_g <- sum(ct24$votes_gop) - ct_cmp_g

# --- the District: wards in both years, and no identifier for any of them ---
dc20 <- of20[of20$state_name == "District of Columbia", ]
dc24 <- of24[of24$state_name == "District of Columbia", ]
dc_off_sw <- mgn(sum(dc24$votes_dem), sum(dc24$votes_gop)) -
             mgn(sum(dc20$votes_dem), sum(dc20$votes_gop))
dcc20 <- cm20[substr(cm20$county_fips, 1, 2) == "11", ]
dcc24 <- cm24[substr(cm24$county_fips, 1, 2) == "11", ]
dc_agree <- sum(dcc20$votes_dem) == sum(dc20$votes_dem) &&
            sum(dcc20$votes_gop) == sum(dc20$votes_gop) &&
            sum(dcc24$votes_dem) == sum(dc24$votes_dem) &&
            sum(dcc24$votes_gop) == sum(dc24$votes_gop)
dc_nofips <- sum(dc20$county_fips == "") + sum(dc24$county_fips == "")

# --- Alaska: districts in both years, same surrogate codes, redrawn ground --
ak20 <- of20[substr(of20$county_fips, 1, 2) == "02", ]
ak24 <- of24[substr(of24$county_fips, 1, 2) == "02", ]
ak_join <- length(intersect(ak20$county_fips, ak24$county_fips))
ak_coll <- intersect(ak24$county_fips, cc$fips)
ak_fed  <- ak24$county_name[ak24$county_fips == "02099"]

# --- Maine's military and overseas ballots, which belong to no county -------
me_u20 <- of20[of20$state_name == "Maine" & of20$county_fips == "", ]
me_u24 <- of24[of24$state_name == "Maine" & of24$county_fips == "", ]
cb_c20 <- row_of(cm20, "23005"); cb_c24 <- row_of(cm24, "23005")
cb_o20 <- row_of(of20, "23005"); cb_o24 <- row_of(of24, "23005")
cb_sw_c <- swg(cb_c20, cb_c24); cb_sw_o <- swg(cb_o20, cb_o24)
cb_add_d <- cb_c24$votes_dem - cb_o24$votes_dem
cb_add_g <- cb_c24$votes_gop - cb_o24$votes_gop
cb_2020_same <- cb_c20$votes_dem == cb_o20$votes_dem &&
                cb_c20$votes_gop == cb_o20$votes_gop

# --- Kansas City, its own returning jurisdiction in Missouri's canvass ------
jk_o20 <- row_of(of20, "29095"); jk_o24 <- row_of(of24, "29095")
kc_20  <- of20[of20$county_name == "Kansas City", ]
kc_24  <- of24[of24$county_name == "Kansas City", ]
jk_c20 <- row_of(cm20, "29095"); jk_c24 <- row_of(cm24, "29095")
jk_sw_c <- swg(jk_c20, jk_c24)                       # the arrow on the map
jk_sw_o <- swg(jk_o20, jk_o24)                       # Jackson without the city
jk_sw_k <- swg(plus(jk_o20, kc_20), plus(jk_o24, kc_24))

# --- Kansas in 2020, where the compilation is short in 102 of 105 counties --
ks <- merge(cm20[cm20$state_name == "Kansas", ],
            of20[of20$state_name == "Kansas", ], by = "county_fips")
ks_diff  <- sum(ks$votes_dem.x != ks$votes_dem.y |
                ks$votes_gop.x != ks$votes_gop.y)
ks_short <- sum((ks$votes_dem.y + ks$votes_gop.y) -
                (ks$votes_dem.x + ks$votes_gop.x))
ks_lower <- sum(ks$votes_dem.x + ks$votes_gop.x <
                ks$votes_dem.y + ks$votes_gop.y)

# Every number in this document comes out of F(). Nothing is typed.
FV <- setNames(ff$value, ff$key)
F  <- function(k) {
  stopifnot(k %in% names(FV))
  z <- FV[[k]]
  n <- suppressWarnings(as.numeric(z))
  if (is.na(n)) z else n
}
pc <- function(x, k = 1) formatC(as.numeric(x), format = "f", digits = k)
n  <- function(x) format(round(as.numeric(x)), big.mark = ",")
sg <- function(x, k = 1) {                       # signed, R+ / D+ labeled
  v <- as.numeric(x)
  paste0(if (v >= 0) "R+" else "D+", formatC(abs(v), format = "f", digits = k))
}

usf <- us[us$in_frame == "TRUE" | us$in_frame == TRUE, ]
u16f <- u16[u16$in_frame == "TRUE" | u16$in_frame == TRUE, ]
u16f$state_name <- st_of(u16f$county_fips)
u24f <- u24[u24$in_frame == "TRUE" | u24$in_frame == TRUE, ]

DEGPP <- F("deg_per_point")
RED <- "#C41230"; BLU <- "#2c7fb8"; GRY <- "#8c8c8c"
# Figure 5 thresholds. Below CORR_MIN a dot is rounding, not a correction;
# above CORR_CAP a single county would set the size scale for five hundred.
CORR_MIN <- 0.25; CORR_CAP <- 20
INK <- if (HTMLOUT) "#8a8d95" else "#333333"

# ---- length, the same rule the CSVs were built with ------------------------
# Linear on every map: an arrow twice as long is twice the swing. Only the
# kilometres-per-point differs between frames, and each legend prints its own.
LMINF  <- F("len_min_frac")
LMAXUS <- F("len_max_km_us");        NVFUS <- F("net_votes_full_us")
LMAXGA <- F("len_max_km_ga_county"); NVFGA <- F("net_votes_full_ga_county")
len_us <- function(net) LMAXUS * (LMINF + (1-LMINF) * sqrt(pmin(net, NVFUS)/NVFUS))
len_ga <- function(net) LMAXGA * (LMINF + (1-LMINF) * sqrt(pmin(net, NVFGA)/NVFGA))

# ---- one arrow, one rule ---------------------------------------------------
# Every map in this chapter reads dx and dy straight out of its CSV. There is
# no second encoding and no per-figure geometry: what wind_geom() wrote is what
# gets drawn, which is why the four figures cannot disagree about what an arrow
# means.

# ---- the pair, verified rather than asserted -------------------------------
# "Which way did most counties move" has two answers, because "margin" has two
# definitions, so both are computed. TWO-PARTY margin is what every arrow in
# this document draws: gop minus dem over (gop + dem), which holds third parties
# constant. ALL-VOTE margin divides by every ballot cast, so a third-party
# collapse moves it even when no voter changed sides. The gap between the two
# answers is the subject of one whole section, and which definition a wind map
# drew is never printed on the wind map.
pair <- function(d, a, b) {
  vt  <- d[[paste0("votes_dem_", b)]] + d[[paste0("votes_gop_", b)]]
  m1  <- 100 * (d[[paste0("votes_gop_", a)]] - d[[paste0("votes_dem_", a)]]) /
               d[[paste0("total_votes_", a)]]
  m2  <- 100 * (d[[paste0("votes_gop_", b)]] - d[[paste0("votes_dem_", b)]]) /
               d[[paste0("total_votes_", b)]]
  sa  <- m2 - m1                                    # all-vote swing
  agg <- function(dm, gp) 100 * (sum(gp) - sum(dm)) / (sum(gp) + sum(dm))
  list(n        = nrow(d),
       cnt_R    = sum(d$swing > 0),
       pct_R    = 100 * mean(d$swing > 0),
       vsh_R    = 100 * sum(vt[d$swing > 0]) / sum(vt),
       cnt_D    = sum(d$swing <= 0),
       pct_D    = 100 * mean(d$swing <= 0),
       vsh_D    = 100 * sum(vt[d$swing <= 0]) / sum(vt),
       med      = median(d$swing),
       natl     = agg(d[[paste0("votes_dem_", b)]], d[[paste0("votes_gop_", b)]]) -
                  agg(d[[paste0("votes_dem_", a)]], d[[paste0("votes_gop_", a)]]),
       av_pct_R = 100 * mean(sa > 0),
       av_vsh_R = 100 * sum(vt[sa > 0]) / sum(vt),
       av_med   = median(sa))
}
P1 <- pair(u16f, "16", "20")      # 2016 -> 2020, Trump loses
P2 <- pair(usf,  "20", "24")      # 2020 -> 2024, Trump wins

# ---- one arrow, drawn the same way everywhere -----------------------------
# x, y, dx, dy come straight out of the CSVs. Nothing here projects or
# rescales anything, which is why the D3 figures and these base-R figures
# cannot drift apart.
arrow_at <- function(x, y, dx, dy, col, lwd = 1, head = 0.045) {
  # An arrow shorter than one device pixel has no drawable angle; arrows()
  # calls it zero-length and skips it. Draw those as dots instead, so the
  # county is still on the map and nothing is silently missing.
  eps <- diff(par("usr")[1:2]) / (par("pin")[1] * 96)
  z <- sqrt(dx^2 + dy^2) < eps
  cc <- if (length(col) > 1) col else rep(col, length(x))
  if (any(z)) points(x[z], y[z], pch = 19, cex = 0.11, col = cc[z])
  k <- !z
  if (any(k)) arrows(x[k], y[k], (x + dx)[k], (y + dy)[k], length = head,
                     angle = 22, lwd = if (length(lwd) > 1) lwd[k] else lwd,
                     col = cc[k])
}
swing_col <- function(s) ifelse(s > 0, RED, BLU)

# ---- render every data.frame as a TABLE, not as code output ---------------
knit_print.data.frame <- function(x, ...) {
  nm <- gsub("_", " ", names(x))
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- raw2024
# Verbatim captures of the first lines of two vintages of the same repository.
R20 <- c(
"state_name,county_fips,county_name,votes_gop,votes_dem,total_votes,",
"diff,per_gop,per_dem,per_point_diff",
"Alabama,01001,Autauga County,19838,7503,27770,12335,0.714368023046453,",
"0.27018365142239825,0.4441843716240547",
"Alabama,01003,Baldwin County,83544,24578,109679,58966,0.761713728243328,",
"0.22409029987509005,0.5376234283682382")
R16 <- c(
",votes_dem,votes_gop,total_votes,per_dem,per_gop,diff,per_point_diff,",
"state_abbr,county_name,combined_fips",
"0,93003.0,130413.0,246588.0,0.37715947248,0.528870018006,\"37,410\",",
"15.17%,AK,Alaska,2013",
"1,93003.0,130413.0,246588.0,0.37715947248,0.528870018006,\"37,410\",",
"15.17%,AK,Alaska,2016",
"2,93003.0,130413.0,246588.0,0.37715947248,0.528870018006,\"37,410\",",
"15.17%,AK,Alaska,2020")
# The captures above are wrapped at the page width; each record is two lines.
# Rejoin them, then let a CSV parser do the splitting so that a quoted comma
# inside "37,410" is not mistaken for a field boundary.
join2 <- function(x) vapply(seq(1, length(x), 2),
                            function(i) paste0(x[i], x[i + 1L]), character(1))
.wm <- c(
  state_name = "the state, spelled out",
  county_fips = "county identifier — text on disk, five characters",
  combined_fips = "the same identifier under a different name, and unpadded",
  county_name = "the county's name",
  state_abbr = "the state's two-letter code",
  votes_gop = "votes for the Republican", votes_dem = "votes for the Democrat",
  total_votes = "all votes cast",
  diff = "the two counts subtracted",
  per_gop = "the Republican share", per_dem = "the Democratic share",
  per_point_diff = "the two shares subtracted",
  `(unnamed)` = "an unnamed row counter the compilation exported by accident")
astab <- function(x, labs) {
  p  <- read.csv(text = paste(join2(x), collapse = "\n"), header = FALSE,
                 stringsAsFactors = FALSE, colClasses = "character")
  nm <- as.character(p[1, ]); nm[!nzchar(nm)] <- "(unnamed)"
  b  <- p[-1, , drop = FALSE]
  out <- data.frame(Column_as_it_arrives = nm,
                    What_it_holds = ifelse(nm %in% names(.wm),
                                           unname(.wm[nm]), "—"),
                    stringsAsFactors = FALSE)
  for (i in seq_len(nrow(b))) out[[labs[i]]] <- unname(unlist(b[i, ]))
  out
}
astab(R20, c("Row_1", "Row_2"))

## ---- raw2016
astab(R16, c("Row_1", "Row_2", "Row_3"))

## ---- wind-runtime
# The shared runtime for every wind map below: d3, the zoom controls and the
# tooltip formatter, defined once so the four maps cannot drift apart in how
# they behave. Emitted with cat() rather than sprintf() -- it carries no
# substitutions, and sprintf's format string is capped at 8,192 characters.
cat('
<script src="../../_lib/d3.v7.min.js"></script>
<script>
/* ---------------------------------------------------------------------
   Shared by every wind map in this document. Defined once, beside the
   first figure, so the three national maps and the Georgia map cannot
   drift apart in how they zoom or what they say on hover.
   --------------------------------------------------------------------- */
window.windMap = (function(){
  const fmt = d3.format(",");
  /* R+/D+ from a pair of vote counts, on the two-party margin the arrows
     draw. Computed here rather than shipped, because the votes are already
     on the wire and the margin is one subtraction. */
  function marg(dv,rv){
    const t = dv+rv; if(!t) return "";
    const m = 100*(rv-dv)/t;
    return (m>=0?"R+":"D+") + Math.abs(m).toFixed(1);
  }
  /* Signed vote difference, in the same shape as the counts beside it. */
  function dlt(v){ return (v>0?"+":"\u2212") + fmt(Math.abs(v)); }
  /* A card, not a pill. The reader is being asked to weigh two observations
     and a difference, which is a deliberative act, so the tooltip is laid out
     as a small table: parties down the side, the two elections across the top,
     the change in its own column, and the margin underneath. The headline
     restates the arrow in words, because that is the one thing the reader came
     for and it should not have to be computed from the table. */
  function prow(label, cls, v0, v1) {
    return `<tr><td><i class="tt-bar ${cls}-bg"></i>${label}</td>`
         + `<td>${fmt(v0)}</td><td>${fmt(v1)}</td>`
         + `<td>${dlt(v1 - v0)}</td></tr>`;
  }
  function tipHTML(name,state,y0,y1,d0,r0,d1,r1,swing){
    const dir = swing > 0 ? "Republican" : "Democratic";
    const cls = swing > 0 ? "gop" : "dem";
    return `<div class="tt-name">${name}${state ? ", " + state : ""}</div>`
      + `<div class="tt-lede ${cls}-txt">`
      + `${Math.abs(swing).toFixed(1)} pts. more ${dir} `
      + `<span>than ${y0}</span></div>`
      + `<table><thead><tr><th>Party</th><th>${y0}</th><th>${y1}</th>`
      + `<th>Change</th></tr></thead><tbody>`
      + prow("Dem.", "dem", d0, d1) + prow("Rep.", "gop", r0, r1)
      + `</tbody><tfoot><tr><td>Two-party margin</td>`
      + `<td>${marg(d0,r0)}</td><td>${marg(d1,r1)}</td>`
      + `<td><b>${(swing>0?"R+":"D+") + Math.abs(swing).toFixed(1)}</b></td>`
      + `</tr></tfoot></table>`;
  }

  /* Buttons, not the wheel: a wheel handler on a figure this tall makes the
     page impossible to scroll past. Drag still pans. */
  function zoom(id,svg,gz,W,H,y0,y1){
    const z = d3.zoom().scaleExtent([1,14])
      .extent([[0,y0],[W,y1]]).translateExtent([[0,y0],[W,y1]])
      .filter(e => e.type !== "wheel" && !e.button)
      .on("zoom", e => gz.attr("transform", e.transform));
    svg.call(z).style("cursor","grab")
       .on("mousedown.cur",function(){d3.select(this).style("cursor","grabbing");})
       .on("mouseup.cur",  function(){d3.select(this).style("cursor","grab");});
    const bar = d3.select("#"+id).append("div").attr("class","zoombar");
    const mk = (label,aria,fn) => bar.append("button").attr("type","button")
      .attr("aria-label",aria).text(label).on("click",fn);
    /* Animate, unless the reader asked not to or the tab is in the background
       -- a background tab gets no animation frames, so a transition there
       never starts and the button silently does nothing. */
    const still = () => document.hidden ||
      (window.matchMedia && matchMedia("(prefers-reduced-motion: reduce)").matches);
    const go = (fn,arg) => still() ? svg.call(fn,arg)
                                   : svg.transition().duration(220).call(fn,arg);
    const bIn  = mk("+","zoom in",()=>go(z.scaleBy,1.8));
    const bOut = mk("−","zoom out",()=>go(z.scaleBy,1/1.8));
    const bRst = mk("Reset","reset zoom",()=>go(z.transform,d3.zoomIdentity));
    z.on("zoom.ui", e => {
      const k = e.transform.k;
      bOut.attr("disabled", k<=1.001 ? "" : null);
      bRst.attr("disabled", k<=1.001 ? "" : null);
      bIn .attr("disabled", k>=13.99 ? "" : null);
    });
    bOut.attr("disabled",""); bRst.attr("disabled","");
    return z;
  }
  /* Nearest-arrow hover. Chasing a five-pixel target among 3,100 of them is
     not a thing anyone should have to do, so instead of one hit circle per
     county there is ONE capture rectangle over the map and a nearest-arrow
     search underneath it: whatever pixel the cursor is on, the arrow nearest
     that pixel answers. Nothing can be missed and nothing has to be aimed at.
     A ring marks whichever arrow is being read. */
  function hover(id, svg, gz, xy, html, W, y0, y1) {
    /* Nearest ARROW, not nearest arrow-origin. The pick used to run on the
       tails alone, so hovering the middle or the head of a long arrow could
       answer with a different county whose tail happened to be closer. xy may
       therefore be points [x,y] or segments [x0,y0,x1,y1]; segments are
       measured properly, by distance to the nearest point ON the arrow. */
    const seg = xy.length && xy[0].length === 4;
    const near = function (px, py) {
      let best = -1, bd = Infinity;
      for (let i = 0; i < xy.length; i++) {
        const p = xy[i];
        let dx, dy;
        if (seg) {
          const vx = p[2] - p[0], vy = p[3] - p[1];
          const L2 = vx * vx + vy * vy;
          let t = L2 ? ((px - p[0]) * vx + (py - p[1]) * vy) / L2 : 0;
          t = t < 0 ? 0 : t > 1 ? 1 : t;
          dx = px - (p[0] + t * vx); dy = py - (p[1] + t * vy);
        } else { dx = px - p[0]; dy = py - p[1]; }
        const d2 = dx * dx + dy * dy;
        if (d2 < bd) { bd = d2; best = i; }
      }
      return best;
    };
    const host = d3.select("#" + id).node();
    /* Anchored to the RING, not to the cursor. Two earlier tries failed on
       the same corner: clamping mixed viewBox units with CSS pixels, and
       flipping off the cursor still landed on the county, because near the
       right-hand edge the nearest county is always to the LEFT of where the
       pointer is. The ring is the thing being described, so offsetting from
       the ring is what actually guarantees the label never covers it. */
    const place = function (rx, ry, tip) {
      const b  = host.getBoundingClientRect();
      const r  = tip.node().getBoundingClientRect();  /* reflects what was just written */
      const tw = r.width, th = r.height, GAP = 18;
      let lx = rx + GAP;
      if (lx + tw > b.width) lx = rx - GAP - tw;      /* the side with room */
      const ly = ry - th / 2;
      tip.style("left", Math.max(0, Math.min(lx, b.width  - tw)) + "px")
         .style("top",  Math.max(0, Math.min(ly, b.height - th)) + "px");
    };
    const ring = gz.append("circle").attr("r", 7).attr("class", "hilite")
      .attr("fill", "none").attr("stroke-width", 1.6)
      .attr("vector-effect", "non-scaling-stroke").style("display", "none");
    const tip = d3.select("#" + id).append("div").attr("class", "windtip");
    /* Appended last, so it sits above the legends and receives every move.
       Events still bubble to the svg, so drag-to-pan keeps working. */
    const cap = svg.append("rect").attr("x", 0).attr("y", y0)
      .attr("width", W).attr("height", y1 - y0)
      .attr("fill", "transparent").style("cursor", "crosshair");
    cap.on("mousemove", function (e) {
      const t = d3.zoomTransform(svg.node());
      const [mx, my] = d3.pointer(e, svg.node());
      const [lx, ly] = t.invert([mx, my]);
      const i = near(lx, ly);
      if (i < 0) return;
      ring.attr("cx", xy[i][0]).attr("cy", xy[i][1]).style("display", null);
      tip.style("opacity", 1).html(html(i));   /* html first: place() measures it */
      const b  = host.getBoundingClientRect();
      const kk = b.width / svg.node().viewBox.baseVal.width;
      const [sx, sy] = t.apply([xy[i][0], xy[i][1]]);   /* stage -> viewBox */
      place(sx * kk, sy * kk, tip);             /* viewBox -> rendered pixels */
    }).on("mouseleave", function () {
      tip.style("opacity", 0); ring.style("display", "none");
    });
  }

  /* A clipped group that the zoom transform is applied to. Titles and legends
     stay outside it, so they neither move nor scale. The clip has a top edge
     and no real bottom one: it is there to keep zoomed content off the title,
     and everything appended after the stage -- the legends, the footer --
     paints over the map rather than needing to be masked from it. Giving it a
     bottom edge above the frame silently cut the Gulf coast off. */
  function stage(svg,id,W,y0,y1){
    svg.append("clipPath").attr("id",id+"-clip").append("rect")
       .attr("x",0).attr("y",y0).attr("width",W).attr("height",y1-y0);
    return svg.append("g").attr("clip-path",`url(#${id}-clip)`).append("g");
  }
  return {tip:tipHTML, zoom:zoom, stage:stage, hover:hover};
})();
</script>
')

## ---- key-strip-d3
# One row, because there is one encoding. Geometry comes from wind_geom() by
# way of the same constants the CSVs were built with, so the key cannot drift
# from what it documents.
W1 <- 760; H1 <- 190; SC <- 78
sw <- c(-15, -10, -5, 0, 5, 10, 15)
X1 <- function(i) 96 + (i - 1) * 96
ROW <- 118
LL  <- rep(SC, length(sw))   # one length: this key is about angle
th  <- pmax(-90, pmin(90, DEGPP * sw)) * pi / 180
A1 <- paste(sprintf('[%.1f,%.1f,%.1f,%.1f,%.1f]',
                    X1(seq_along(sw)), ROW, LL * sin(th), -LL * cos(th), sw),
            collapse = ",")
cat(sprintf('
<div id="keystrip" class="wind-fig" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const A=[%s],W=%d,H=%d,ROW=%d;
const svg=d3.select("#keystrip").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
svg.append("text").attr("x",W/2).attr("y",22).attr("text-anchor","middle")
  .attr("font-size","14px").attr("font-weight","600").attr("class","ttl")
  .text("Seven margin changes, drawn at one length");
svg.append("text").attr("x",W/2).attr("y",39).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("class","sub")
  .text("%s degrees per point of margin change \u00b7 length carries votes, keyed on each map");
const HB=0.42;
const g=svg.append("g").attr("fill","none").attr("stroke-width",2.4)
  .attr("stroke-linecap","round").attr("stroke-linejoin","round");
A.forEach(d=>{
  if(d[4]===0){
    svg.append("line").attr("x1",d[0]).attr("y1",ROW).attr("x2",d[0])
      .attr("y2",ROW-26).attr("class","key").attr("stroke-width",2.4);
    svg.append("path").attr("class","key").attr("stroke","none")
      .attr("fill","currentColor")
      .attr("d",`M${d[0]-4},${ROW-26}L${d[0]+4},${ROW-26}L${d[0]},${ROW-34}Z`);
  } else {
    const dx=d[2],dy=d[3],L=Math.hypot(dx,dy),h=Math.min(7,0.32*L);
    const ux=dx/L,uy=dy/L,c=Math.cos(HB),sn=Math.sin(HB),tx=d[0]+dx,ty=ROW+dy;
    g.append("path").attr("class",d[4]>0?"gop":"dem")
      .attr("d",`M${d[0]},${ROW}L${tx},${ty}`
        +`M${tx},${ty}l${h*(-ux*c+uy*sn)},${h*(-ux*sn-uy*c)}`
        +`M${tx},${ty}l${h*(-ux*c-uy*sn)},${h*( ux*sn-uy*c)}`);
  }
  svg.append("text").attr("x",d[0]).attr("y",ROW+30).attr("text-anchor","middle")
    .attr("font-size","11px").attr("font-weight","600")
    .attr("class",d[4]===0?"foot":(d[4]>0?"gop-txt":"dem-txt"))
    .text(d[4]===0?"none":(d[4]>0?"R+":"D+")+Math.abs(d[4]));
});
})();
</script>
', A1, W1, H1, ROW, pc(DEGPP, 0)))

## ---- key-strip-static
par(mar = c(0.4, 0.4, 2.0, 0.4))
sw <- c(-15, -10, -5, 0, 5, 10, 15)
LL <- rep(0.86, length(sw))  # one length: this key is about angle
plot(NA, xlim = c(-1.05, length(sw) - 0.15), ylim = c(-0.75, 1.05),
     asp = 1, axes = FALSE, ann = FALSE)
for (i in seq_along(sw)) {
  if (sw[i] == 0) {
    segments(i - 1, 0, i - 1, 0.30, lwd = 2.4, col = GRY)
    points(i - 1, 0.30, pch = 17, cex = 0.8, col = GRY)
  } else {
    th <- max(-90, min(90, DEGPP * sw[i])) * pi / 180
    arrow_at(i - 1, 0, LL[i] * sin(th), LL[i] * cos(th), swing_col(sw[i]),
             lwd = 2.4, head = 0.07)
  }
  text(i - 1, -0.46, if (sw[i] == 0) "none" else sg(sw[i], 0),
       cex = 0.7, col = if (sw[i] == 0) GRY else swing_col(sw[i]), font = 2)
}
title("Seven margin changes, drawn at one length", cex.main = 0.95, line = 0.6)
mtext(paste0("angle ", pc(DEGPP, 0),
             " degrees per point of margin change; length carries votes, keyed on each map"),
      side = 3, line = -0.4, cex = 0.66, col = "#666666")

## ---- us-d3
# The legends used to sit at H-92, which on this frame is on top of Florida.
# The map now gets a fixed height and the keys get a band of their own beneath
# it, so nothing is drawn over the country.
W <- 780; PAD <- 8; MAPT <- 52; MAPH <- 424; BAND <- 84
H <- MAPT + MAPH + BAND
xr <- range(uo$x); yr <- range(uo$y)
s  <- min((W - 2*PAD) / diff(xr), MAPH / diff(yr))
SX <- function(x) PAD + (x - xr[1]) * s
SY <- function(y) MAPT + (yr[2] - y) * s

# Rows are ARRAYS, not objects: the same 3,100 arrows cost about a third as
# many bytes. Arrow components keep one decimal,
# because the longest arrow is only about 15 pixels and rounding its components
# to whole pixels would bend a 60-degree arrow to 58 and a short one to 53.
paths <- vapply(split(uo, uo$part), function(z) {
  # Simplify in PIXEL space, not by index: round each vertex to the pixel and
  # drop the ones that land on top of their predecessor. The boundary can move
  # by at most half a pixel, and -- this is the part that matters -- two states
  # that share a border round it to the same pixels, so the border stays shared.
  # Dropping every sixth vertex instead moved some borders by 35 pixels and,
  # because each state was thinned independently, left gaps along every shared
  # edge. On white paper those gaps are white. On any other background they are
  # wedges of it, showing through the middle of the country.
  px <- round(SX(z$x)); py <- round(SY(z$y))
  k  <- c(TRUE, diff(px) != 0 | diff(py) != 0)
  paste0("M", paste(sprintf("%d,%d", px[k], py[k]), collapse = "L"), "Z")
}, character(1))
OUTL <- paste(sprintf('"%s"', paths), collapse = ",")

# Thin arrows first, heavy ones last, so the big counties are not buried under
# the small ones. The base-R figure below sorts the same way; when the two
# renderers disagree about draw order they disagree about the picture.
d <- usf[order(usf$lw), ]
ST <- sort(unique(d$state_name))            # 49 strings, sent once
# Both observations travel, not just their difference: the tooltip has to be
# able to show what the arrow was computed FROM. Four counts per county.
A <- paste(sprintf('[%.1f,%.1f,%.1f,%.1f,%.1f,%.2f,"%s",%d,%d,%d,%d,%d]',
                   SX(d$x), SY(d$y), d$dx * s, -d$dy * s, d$swing, d$lw,
                   gsub('"', "", sub(" (County|Parish|Borough|city|City).*$", "",
                                     d$county_name)),
                   match(d$state_name, ST) - 1L,
                   d$votes_dem_20, d$votes_gop_20,
                   d$votes_dem_24, d$votes_gop_24), collapse = ",")
STJ <- paste(sprintf('"%s"', ST), collapse = ",")
LEGN <- paste(sprintf('{"s":"%s","L":%.1f,"i":%d}',
                      c("1,000", "5,000", n(NVFUS)),
                      len_us(c(1000, 5000, NVFUS)) * s, 0:2), collapse = ",")
cat(sprintf('

<div id="usw" class="wind-fig" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const O=[%s],A=[%s],LG=[%s],ST=[%s];
const W=%d,H=%d,BAND=%d,RED="%s",BLU="%s";
const svg=d3.select("#usw").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
svg.append("text").attr("x",%d).attr("y",18).attr("text-anchor","middle")
  .attr("font-size","14px").attr("font-weight","600").attr("class","ttl")
  .text("How every county moved, 2020 to 2024");
svg.append("text").attr("x",%d).attr("y",34).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("class","sub")
  .text("presidential two-party margin \\u00b7 leaning right = Republican gain, left = Democratic gain");
const gz=windMap.stage(svg,svg.node().parentNode.id,W,42,H-BAND+2);
const g=gz.append("g");
g.selectAll("path").data(O).join("path").attr("d",d=>d)
  .attr("class","land").attr("stroke-width",0.6)
  .attr("vector-effect","non-scaling-stroke");
// ---- one arrow = one path: shaft plus two barbs -----------------------
// An arrow needs a HEAD. The alternative -- a bare line segment -- has no
// direction at all, and a dot at the far end is worse than nothing, because a
// dot reads as "the place", so the arrow appears to point back toward the
// county from somewhere out in the next state. The barbs are capped at 0.45 of
// the shaft so that a 6-pixel arrow does not become a 6-pixel arrowhead.
const HB=0.42;                                  // barb angle, radians (24 deg)
function arrowPath(d){
  const x=d[0],y=d[1],dx=d[2],dy=d[3],L=Math.hypot(dx,dy);
  if(L<1) return null;                          // sub-pixel: dot at the ORIGIN
  const h=Math.min(2.8,0.45*L),ux=dx/L,uy=dy/L;
  const c=Math.cos(HB),s=Math.sin(HB),tx=x+dx,ty=y+dy;
  const b1x=h*(-ux*c+uy*s),b1y=h*(-ux*s-uy*c);
  const b2x=h*(-ux*c-uy*s),b2y=h*( ux*s-uy*c);
  return `M${x},${y}L${tx},${ty}M${tx},${ty}l${b1x},${b1y}M${tx},${ty}l${b2x},${b2y}`;
}
const ar=gz.append("g").attr("fill","none");
ar.selectAll("path").data(A.filter(d=>arrowPath(d))).join("path")
  .attr("d",arrowPath)
  .attr("class",d=>d[4]>0?"gop":"dem")
  .attr("stroke-opacity",0.62)
  .attr("stroke-width",0.9)
  .attr("stroke-linecap","round").attr("stroke-linejoin","round")
  .attr("vector-effect","non-scaling-stroke");
// A county whose arrow is shorter than one pixel has no drawable angle. It is
// drawn as a dot AT ITS OWN CENTROID -- the same rule the print figure uses --
// so the county is still on the map and nothing is silently missing.
ar.selectAll("circle").data(A.filter(d=>!arrowPath(d))).join("circle")
  .attr("cx",d=>d[0]).attr("cy",d=>d[1]).attr("r",0.6)
  .attr("class",d=>d[4]>0?"gop-fill":"dem-fill").attr("fill-opacity",0.62);
windMap.hover(svg.node().parentNode.id,svg,gz,A.map(d=>[d[0],d[1],d[0]+d[2],d[1]+d[3]]),
  i=>windMap.tip(A[i][6],ST[A[i][7]],2020,2024,A[i][8],A[i][9],A[i][10],A[i][11],A[i][4]),
  W,42,H-BAND+2);
// ---- legend: the length scale, stated, not implied --------------------
const lg=svg.append("g").attr("transform",`translate(16,${H-66})`);
lg.append("text").attr("y",-4).attr("font-size","11px").attr("font-weight","600")
  .attr("class","lbl").text("net votes moved");
LG.forEach((d,i)=>{
  lg.append("line").attr("x1",0).attr("y1",8+i*17).attr("x2",d.L).attr("y2",8+i*17)
    .attr("class","key").attr("stroke-width",1.6).attr("stroke-linecap","round");
  lg.append("text").attr("x",d.L+7).attr("y",12+i*17).attr("font-size","10.5px")
    .attr("class","lbl").text(d.s+" votes"+(d.i===2?" or more":""));});
const lg2=svg.append("g").attr("transform",`translate(${W-206},${H-66})`);
[["gop","shift toward the Republican"],["dem","shift toward the Democrat"]]
 .forEach((r,i)=>{lg2.append("line").attr("x1",0).attr("y1",i*17).attr("x2",22)
   .attr("y2",i*17).attr("class",r[0]).attr("stroke-width",2.2);
  lg2.append("text").attr("x",28).attr("y",i*17+4).attr("font-size","10.5px")
   .attr("class","lbl").text(r[1]);});
svg.append("text").attr("x",16).attr("y",H-10).attr("font-size","10px")
  .attr("class","foot").text("angle = %s degrees per point of margin change \\u00b7 length = net votes that changed hands");
windMap.zoom(svg.node().parentNode.id,svg,gz,W,H,42,H-BAND+2);
})();
</script>
', OUTL, A, LEGN, STJ, W, H, BAND, RED, BLU, round(W/2), round(W/2), pc(DEGPP, 0)))

## ---- us-static
par(mar = c(0.2, 0.2, 2.2, 0.2))
plot(NA, xlim = range(uo$x), ylim = range(uo$y) + c(-330, 40), asp = 1,
     axes = FALSE, ann = FALSE)
for (p in split(uo, uo$part)) polygon(p$x, p$y, col = "#f6f6f6",
                                      border = "#c9c9c9", lwd = 0.4)
o <- usf[order(usf$lw), ]
arrow_at(o$x, o$y, o$dx, o$dy, adjustcolor(swing_col(o$swing), 0.62),
         lwd = 0.5, head = 0.012)
title("How every county moved, 2020 to 2024", cex.main = 1.0, line = 0.6)
mtext("two-party margin; leaning right = Republican gain, left = Democratic gain",
      side = 3, line = -0.4, cex = 0.66, col = "#666666")
# legend, absolute scale. Rows are spaced in MAP kilometers, so the gaps have
# to be map-sized: 95 km between rows, not 26.
# ly sits low enough that the whole key, including the party swatches at
# ly + 70, clears the bottom of the map. At -60 they landed on Florida.
lx <- min(uo$x) + 60; ly <- min(uo$y) - 105
text(lx, ly + 95, "net votes moved", cex = 0.62, font = 2, col = "#444444", pos = 4)
for (i in 1:3) {
  k  <- c(1000, 5000, NVFUS)[i]
  yy <- ly - (i - 1) * 95
  segments(lx + 6, yy, lx + 6 + len_us(k), yy, lwd = 1.8, col = "#555555")
  text(lx + 6 + len_us(k), yy,
       paste0("  ", n(k), " votes", if (i == 3) " or more" else ""),
       cex = 0.58, col = "#555555", pos = 4)
}
rx <- max(uo$x) - 1150
segments(rx, ly + 70, rx + 110, ly + 70, col = RED, lwd = 2.4)
text(rx + 110, ly + 70, "  shift toward the Republican", cex = 0.58, col = RED, pos = 4)
segments(rx, ly - 25, rx + 110, ly - 25, col = BLU, lwd = 2.4)
text(rx + 110, ly - 25, "  shift toward the Democrat", cex = 0.58, col = BLU, pos = 4)
mtext(paste0("angle = ", pc(DEGPP, 0),
             " degrees per point of margin change; length = net votes moved"),
      side = 1, line = -1.2, cex = 0.58, col = "#888888")

## ---- us-summary
data.frame(
  quantity = c("Counties drawn", "Median county shift", "Counties shifting right (%)",
               "Largest shift right", "Largest shift left",
               "Arrows at the length cap"),
  value = c(n(F("us_arrows")), sg(F("us_swing_median")), pc(F("us_share_R")),
            sg(F("us_swing_max")), sg(F("us_swing_min")), n(F("us_capped_len"))))

## ---- counts
data.frame(
  quantity = c("Rows in the 2020 file", "Rows in the 2024 file",
               "FIPS codes present in both", "Present in 2020 only",
               "Present in 2024 only"),
  value = c(n(F("rows_2020")), n(F("rows_2024")), n(F("in_both")),
            n(F("only_2020")), n(F("only_2024"))))

## ---- audit
a <- aud
a$reason <- sub("^([A-Za-z ]+):.*", "\\1", a$reason)
tab <- as.data.frame(table(a$reason, a$appears_in))
names(tab) <- c("where", "appears_in", "units")
tab <- tab[tab$units > 0, ]
tab <- tab[order(tab$where, tab$appears_in), ]
rownames(tab) <- NULL
tab

## ---- ak
data.frame(fips = c("02013", "02016", "02020"),
           in_the_returns = c("State House District 13", "State House District 16",
                              "State House District 20"),
           in_the_centroid_file = c("Aleutians East", "Aleutians West", "Anchorage"))

## ---- dc-d3
mk <- function(sw, L) {
  th <- DEGPP * sw * pi / 180
  c(u = L * sin(th), v = -L * cos(th))
}
nvw <- F("naive_dc_swing"); trw <- F("true_dc_swing")
a1 <- mk(nvw, 108); a2 <- mk(trw, 108)
cat(sprintf('
<div id="dcx" class="wind-fig" style="margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const W=760,H=286,RED="%s",BLU="%s";
const svg=d3.select("#dcx").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
svg.append("text").attr("x",W/2).attr("y",17).attr("text-anchor","middle")
  .attr("font-size","13.5px").attr("font-weight","600").attr("class","ttl")
  .text("One county, two joins, %s rows either way");
const P=[{x:196,t:"JOIN ON FIPS AND STOP",
          s:"2020 District of Columbia  vs  2024 Ward 1",
          u:%.2f,v:%.2f,lab:"%s",c:"%s",note:"the arrow the data hands you"},
         {x:564,t:"HARMONIZE FIRST",
          s:"2020 District of Columbia  vs  2024 all eight wards",
          u:%.2f,v:%.2f,lab:"%s",c:"%s",note:"the arrow that is true"}];
svg.append("defs").selectAll("marker").data(["mR","mB"]).join("marker")
  .attr("id",d=>d).attr("viewBox","0 0 10 10").attr("refX",8).attr("refY",5)
  .attr("markerWidth",5).attr("markerHeight",5).attr("orient","auto")
  .append("path").attr("d","M0,0L10,5L0,10Z").attr("class",d=>d==="mR"?"gop-fill":"dem-fill");
P.forEach(p=>{
  svg.append("text").attr("x",p.x).attr("y",48).attr("text-anchor","middle")
    .attr("font-size","11.5px").attr("font-weight","600").attr("class","gop-txt").text(p.t);
  svg.append("text").attr("x",p.x).attr("y",66).attr("text-anchor","middle")
    .attr("font-size","11px").attr("class","sub").text(p.s);
  svg.append("line").attr("x1",p.x).attr("y1",212).attr("x2",p.x+p.u)
    .attr("y2",212+p.v).attr("class",p.c).attr("stroke-width",4)
    .attr("stroke-linecap","round")
    .attr("marker-end",p.c==="gop"?"url(#mR)":"url(#mB)");
  svg.append("circle").attr("cx",p.x).attr("cy",212).attr("r",3.5).attr("class","ttl");
  svg.append("text").attr("x",p.x).attr("y",238).attr("text-anchor","middle")
    .attr("font-size","16px").attr("font-weight","700").attr("class",p.c+"-txt").text(p.lab);
  svg.append("text").attr("x",p.x).attr("y",258).attr("text-anchor","middle")
    .attr("font-size","11px").attr("class","sub").text(p.note);
});
svg.append("line").attr("x1",380).attr("y1",44).attr("x2",380).attr("y2",266)
  .attr("class","rule").attr("stroke-dasharray","4,4");
svg.append("line").attr("x1",56).attr("y1",212).attr("x2",704).attr("y2",212)
  .attr("class","rule-2");
svg.append("text").attr("x",W/2).attr("y",280).attr("text-anchor","middle")
  .attr("font-size","10.5px").attr("class","foot")
  .text("Both joins return the same row count. The row count cannot warn you.");
})();
</script>
', RED, BLU, n(F("naive_matched")), a1[["u"]], a1[["v"]], sg(nvw, 1),
   if (nvw > 0) "gop" else "dem", a2[["u"]], a2[["v"]], sg(trw, 1),
   if (trw > 0) "gop" else "dem"))

## ---- dc-static
par(mar = c(2.6, 0.4, 2.4, 0.4))
plot(NA, xlim = c(-1.55, 1.55), ylim = c(-0.34, 1.18), asp = 1,
     axes = FALSE, ann = FALSE)
draw <- function(cx, sw, top, sub, note) {
  th <- DEGPP * sw * pi / 180
  arrow_at(cx, 0, 0.72 * sin(th), 0.72 * cos(th), swing_col(sw), lwd = 3.4,
           head = 0.09)
  points(cx, 0, pch = 19, cex = 0.7, col = INK)
  text(cx, 1.12, top, cex = 0.7, font = 2, col = RED)
  text(cx, 1.00, sub, cex = 0.58, col = "#666666")
  text(cx, -0.15, sg(sw, 1), cex = 0.95, font = 2, col = swing_col(sw))
  text(cx, -0.28, note, cex = 0.58, col = "#666666")
}
draw(-0.80, F("naive_dc_swing"), "JOIN ON FIPS AND STOP",
     "2020 District vs 2024 Ward 1", "the arrow the data hands you")
draw( 0.80, F("true_dc_swing"), "HARMONIZE FIRST",
     "2020 District vs 2024 all wards", "the arrow that is true")
segments(0, -0.31, 0, 1.15, col = "#dddddd", lty = 2)
title(paste0("One county, two joins, ", n(F("naive_matched")),
             " rows either way"), cex.main = 0.92, line = 1.1)
mtext("Both joins return the same row count. The row count cannot warn you.",
      side = 1, line = 0.9, cex = 0.62, col = "#888888")

## ---- harm
data.frame(
  state = c("Alaska", "District of Columbia", "Connecticut", "Everywhere else"),
  problem = c("Same units, different pseudo-codes, and redrawn between elections",
              "One row in 2020, eight in 2024",
              "Eight counties replaced by nine planning regions",
              "Codes agree and mean the same place"),
  what_we_did = c(paste0("Dropped all ", n(F("ak_dropped_2024")), " units"),
                  paste0("Summed the ", n(F("dc_rows_folded")),
                         " wards back to one District"),
                  paste0("Dropped, losing ", n(F("ct_votes_lost")), " votes"),
                  "Kept"),
  check.names = FALSE)

## ---- verdicts
data.frame(
  unit = c("Alaska", "Connecticut", "District of Columbia"),
  `in the compilation` = c(
    paste0(n(F("ak_dropped_2020")), " pseudo-coded House districts each year, ",
           "renumbered between them"),
    paste0(n(F("ct_2020_units")), " counties in 2020, ",
           n(F("ct_current_units")), " planning regions in 2024"),
    paste0("1 citywide row in 2020, ", n(F("dc_rows_folded")),
           " wards in 2024, all eight given codes")),
  `in the state's own returns` = c(
    paste0(n(nrow(ak20)), " rows each year: ", n(nrow(ak20) - 1),
           " State House districts and one federal overseas district"),
    paste0(n(nrow(ct20)), " counties in 2020 and ", n(nrow(ct24)),
           " in 2024, the same eight"),
    paste0(n(nrow(dc20)), " wards in 2020 and ", n(nrow(dc24)),
           " in 2024, none of them with a FIPS code")),
  `whose change was it` = c("Alaska's", "The compiler's", "The compiler's"),
  check.names = FALSE)

## ---- ct-arrows
# sg() labels one number at a time; these are columns, so it is mapped.
sgc <- function(v) vapply(v, sg, character(1))
ctb <- CTT[order(-CTT$swing), ]
data.frame(county = ctb$county,
           `2020 margin` = sgc(ctb$m20), `2024 margin` = sgc(ctb$m24),
           swing = sgc(ctb$swing),
           `two-party votes, 2024` = vapply(ctb$two24, n, character(1)),
           check.names = FALSE)

## ---- divergence
data.frame(
  quantity = c("County rows checkable against the state's canvass",
               "Rows with no official counterpart",
               "Rows differing on a major-party count",
               "Median state error, two-party margin",
               "Worst state, two-party margin"),
  `2020` = c(n(DV20$checked), n(DV20$unmatched), n(DV20$differ),
             paste0(pc(SE20$med, 3), " pts"),
             paste0(SE20$who, "  ", pc(SE20$max, 2), " pts")),
  `2024` = c(n(DV24$checked), n(DV24$unmatched), n(DV24$differ),
             paste0(pc(SE24$med, 3), " pts"),
             paste0(SE24$who, "  ", pc(SE24$max, 2), " pts")),
  check.names = FALSE)

## ---- corr-d3
CW <- 780; CPAD <- 8; CTOP <- 52; CMAPH <- 424; CBAND <- 70
CH <- CTOP + CMAPH + CBAND
cxr <- range(uo$x); cyr <- range(uo$y)
cs  <- min((CW - 2*CPAD) / diff(cxr), CMAPH / diff(cyr))
CSX <- function(x) CPAD + (x - cxr[1]) * cs
CSY <- function(y) CTOP + (cyr[2] - y) * cs
cpaths <- vapply(split(uo, uo$part), function(z) {
  # Pixel-space simplification, same rule as the arrow maps: round to the pixel
  # and drop repeats, so shared borders stay shared and no wedges open up.
  px <- round(CSX(z$x)); py <- round(CSY(z$y))
  k  <- c(TRUE, diff(px) != 0 | diff(py) != 0)
  paste0("M", paste(sprintf("%d,%d", px[k], py[k]), collapse = "L"), "Z")
}, character(1))
COUTL <- paste(sprintf('"%s"', cpaths), collapse = ",")
cd <- crr[crr$in_frame %in% c(TRUE, "TRUE") & abs(crr$delta) > CORR_MIN, ]
cd$state_name <- st_of(cd$county_fips)
cd <- cd[order(abs(cd$delta)), ]                 # small first, large drawn on top
CST <- sort(unique(cd$state_name))
CPTS <- paste(sprintf('[%.1f,%.1f,%.3f,%.2f,%.2f,"%s",%d]',
                      CSX(cd$x), CSY(cd$y), cd$delta, cd$swing_old, cd$swing_new,
                      gsub('"', "", cd$county_name),
                      match(cd$state_name, CST) - 1L), collapse = ",")
CSTJ <- paste(sprintf('"%s"', CST), collapse = ",")
cat(sprintf('
<div id="corr" class="wind-fig" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const O=[%s],P=[%s],ST=[%s];
const W=%d,H=%d,BAND=%d,CAP=%.1f;
const svg=d3.select("#corr").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
svg.append("text").attr("x",%d).attr("y",18).attr("text-anchor","middle")
  .attr("font-size","14px").attr("font-weight","600").attr("class","ttl")
  .text("Where the old 2016 file was wrong");
svg.append("text").attr("x",%d).attr("y",34).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("class","sub")
  .text("each dot is a county whose 2016-to-2020 swing moved when the scrape was replaced");
const gz=windMap.stage(svg,"corr",W,42,H-BAND+2);
gz.append("g").selectAll("path").data(O).join("path").attr("d",d=>d)
  .attr("class","land").attr("stroke-width",0.6)
  .attr("vector-effect","non-scaling-stroke");
/* Area, not radius, carries the size of the correction: a dot twice as wide
   reads as four times as much, so the radius goes as the square root. */
const R=d=>0.9+3.4*Math.sqrt(Math.min(Math.abs(d),CAP)/CAP);
gz.append("g").selectAll("circle").data(P).join("circle")
  .attr("cx",d=>d[0]).attr("cy",d=>d[1]).attr("r",d=>R(d[2]))
  .attr("class",d=>d[2]>0?"gop-fill":"dem-fill").attr("fill-opacity",0.6);
const sgn=v=>(v>=0?"R+":"D+")+Math.abs(v).toFixed(1);
windMap.hover("corr",svg,gz,P.map(d=>[d[0],d[1]]),i=>{
  const p=P[i], up=p[2]>0;
  return `<div class="tt-name">${p[5]}, ${ST[p[6]]}</div>`
    +`<div class="tt-lede ${up?"gop-txt":"dem-txt"}">`
    +`the old file was ${Math.abs(p[2]).toFixed(1)} pts. too `
    +`${up?"Democratic":"Republican"} here</div>`
    +`<table><thead><tr><th>2016 source</th><th>swing it drew</th></tr></thead>`
    +`<tbody><tr><td>election-night scrape</td><td>${sgn(p[3])}</td></tr>`
    +`<tr><td>official state returns</td><td>${sgn(p[4])}</td></tr></tbody>`
    +`<tfoot><tr><td>Correction</td><td><b>${sgn(p[2])}</b></td></tr></tfoot></table>`;
},W,42,H-BAND+2);
const lg=svg.append("g").attr("transform",`translate(16,${H-52})`);
[["gop-fill","the old file understated the Republican shift"],
 ["dem-fill","the old file overstated it"]].forEach((r,i)=>{
  lg.append("circle").attr("cx",4).attr("cy",i*17).attr("r",4)
    .attr("class",r[0]).attr("fill-opacity",0.6);
  lg.append("text").attr("x",14).attr("y",i*17+4).attr("font-size","10.5px")
    .attr("class","lbl").text(r[1]);});
const lg2=svg.append("g").attr("transform",`translate(${W-232},${H-52})`);
lg2.append("text").attr("x",0).attr("y",-10).attr("font-size","11px")
  .attr("font-weight","600").attr("class","lbl").text("size of the correction");
[1,5,20].forEach((v,i)=>{const cx=i*74+8;
  lg2.append("circle").attr("cx",cx).attr("cy",6).attr("r",R(v))
    .attr("class","gop-fill").attr("fill-opacity",0.45);
  lg2.append("text").attr("x",cx+12).attr("y",10).attr("font-size","10.5px")
    .attr("class","lbl").text(v+(i===2?"+ pts":(v>1?" pts":" pt")));});
svg.append("text").attr("x",16).attr("y",H-10).attr("font-size","10px")
  .attr("class","foot")
  .text("counties that moved less than %s of a point are not drawn");
windMap.zoom("corr",svg,gz,W,H,42,H-BAND+2);
})();
</script>
', COUTL, CPTS, CSTJ, CW, CH, CBAND, CORR_CAP, round(CW/2), round(CW/2),
   CORR_MIN))

## ---- corr-static
par(mar = c(0.2, 0.2, 2.2, 0.2))
plot(NA, xlim = range(uo$x), ylim = range(uo$y) + c(-300, 40), asp = 1,
     axes = FALSE, ann = FALSE)
for (p in split(uo, uo$part)) polygon(p$x, p$y, col = "#f6f6f6",
                                      border = "#c9c9c9", lwd = 0.4)
cds <- crr[crr$in_frame %in% c(TRUE, "TRUE") & abs(crr$delta) > CORR_MIN, ]
cds <- cds[order(abs(cds$delta)), ]
points(cds$x, cds$y, pch = 19,
       cex = 0.18 + 1.45 * sqrt(pmin(abs(cds$delta), CORR_CAP) / CORR_CAP),
       col = adjustcolor(ifelse(cds$delta > 0, RED, BLU), 0.6))
title("Where the old 2016 file was wrong", cex.main = 1.0, line = 0.6)
mtext("each dot is a county whose 2016-to-2020 swing moved when the scrape was replaced",
      side = 3, line = -0.4, cex = 0.66, col = "#666666")
lx <- min(uo$x) + 60; ly <- min(uo$y) - 130
points(lx, ly + 60, pch = 19, cex = 1.0, col = adjustcolor(RED, 0.6))
text(lx, ly + 60, "  the old file understated the Republican shift",
     cex = 0.58, col = "#444444", pos = 4)
points(lx, ly - 35, pch = 19, cex = 1.0, col = adjustcolor(BLU, 0.6))
text(lx, ly - 35, "  the old file overstated it",
     cex = 0.58, col = "#444444", pos = 4)
# Spacing is set by the WIDEST swatch, not the average: at 330 apart the 20-point
# circle was printing on top of the "5 pts" label beside it.
rx <- max(uo$x) - 1620
text(rx, ly + 60, "size of the correction", cex = 0.62, font = 2,
     col = "#444444", pos = 4)
for (i in 1:3) {
  v <- c(1, 5, CORR_CAP)[i]
  points(rx + 60 + (i - 1) * 430, ly - 35, pch = 19,
         cex = 0.18 + 1.45 * sqrt(v / CORR_CAP), col = adjustcolor(RED, 0.45))
  text(rx + 60 + (i - 1) * 430, ly - 35,
       paste0("   ", v, if (i == 3) "+ pts" else if (v > 1) " pts" else " pt"),
       cex = 0.58, col = "#555555", pos = 4)
}
mtext(paste0("counties that moved less than ", CORR_MIN,
             " of a point are not drawn"),
      side = 1, line = -1.2, cex = 0.58, col = "#888888")

## ---- join1620
data.frame(
  vintage_problem = c("Alaska", "District of Columbia", "Connecticut",
                      "Everything else"),
  in_2016_to_2020 = c(
    paste0("Dropped. Reported as ", n(F("ak_2016_units")),
           " State House districts, which have no county to join to"),
    "One row in both years; folded anyway, and unchanged by it",
    paste0("No problem: ", n(F("ct_2016_units")), " counties in 2016, ",
           n(F("ct_2020_units")), " in 2020"),
    "Codes agree"),
  in_2020_to_2024 = c(
    paste0("Dropped. Two sets of pseudo-codes for ",
           n(F("ak_dropped_2024")), " redrawn House districts"),
    paste0("One row in 2020, ", n(F("dc_rows_folded")), " wards in 2024"),
    paste0("Dropped. Eight counties became nine planning regions, losing ",
           n(F("ct_votes_lost")), " votes"),
    "Codes agree"),
  check.names = FALSE)

## ---- us16-d3
# The legends used to sit at H-92, which on this frame is on top of Florida.
# The map now gets a fixed height and the keys get a band of their own beneath
# it, so nothing is drawn over the country.
W <- 780; PAD <- 8; MAPT <- 52; MAPH <- 424; BAND <- 84
H <- MAPT + MAPH + BAND
xr <- range(uo$x); yr <- range(uo$y)
s  <- min((W - 2*PAD) / diff(xr), MAPH / diff(yr))
SX <- function(x) PAD + (x - xr[1]) * s
SY <- function(y) MAPT + (yr[2] - y) * s
paths <- vapply(split(uo, uo$part), function(z) {
  # Simplify in PIXEL space, not by index: round each vertex to the pixel and
  # drop the ones that land on top of their predecessor. The boundary can move
  # by at most half a pixel, and -- this is the part that matters -- two states
  # that share a border round it to the same pixels, so the border stays shared.
  # Dropping every sixth vertex instead moved some borders by 35 pixels and,
  # because each state was thinned independently, left gaps along every shared
  # edge. On white paper those gaps are white. On any other background they are
  # wedges of it, showing through the middle of the country.
  px <- round(SX(z$x)); py <- round(SY(z$y))
  k  <- c(TRUE, diff(px) != 0 | diff(py) != 0)
  paste0("M", paste(sprintf("%d,%d", px[k], py[k]), collapse = "L"), "Z")
}, character(1))
OUTL <- paste(sprintf('"%s"', paths), collapse = ",")
d <- u16f[order(u16f$lw), ]                 # thin first, heavy last
ST <- sort(unique(d$state_name))
A <- paste(sprintf('[%.1f,%.1f,%.1f,%.1f,%.1f,%.2f,"%s",%d,%d,%d,%d,%d]',
                   SX(d$x), SY(d$y), d$dx * s, -d$dy * s, d$swing, d$lw,
                   gsub('"', "", sub(" (County|Parish|Borough|city|City).*$", "",
                                     d$county_name)),
                   match(d$state_name, ST) - 1L,
                   d$votes_dem_16, d$votes_gop_16,
                   d$votes_dem_20, d$votes_gop_20), collapse = ",")
STJ <- paste(sprintf('"%s"', ST), collapse = ",")
LEGN <- paste(sprintf('{"s":"%s","L":%.1f,"i":%d}',
                      c("1,000", "5,000", n(NVFUS)),
                      len_us(c(1000, 5000, NVFUS)) * s, 0:2), collapse = ",")
cat(sprintf('
<div id="usw16" class="wind-fig" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const O=[%s],A=[%s],LG=[%s],ST=[%s];
const W=%d,H=%d,BAND=%d,RED="%s",BLU="%s";
const svg=d3.select("#usw16").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
svg.append("text").attr("x",%d).attr("y",18).attr("text-anchor","middle")
  .attr("font-size","14px").attr("font-weight","600").attr("class","ttl")
  .text("How every county moved, 2016 to 2020");
svg.append("text").attr("x",%d).attr("y",34).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("class","sub")
  .text("same encoding as Figure 2 \\u00b7 leaning right = Republican gain, left = Democratic gain");
const gz=windMap.stage(svg,svg.node().parentNode.id,W,42,H-BAND+2);
const g=gz.append("g");
g.selectAll("path").data(O).join("path").attr("d",d=>d)
  .attr("class","land").attr("stroke-width",0.6)
  .attr("vector-effect","non-scaling-stroke");
// same shaft-plus-barbs arrow as the 2020-to-2024 figure, restated here
// because this script is self-contained
const HB=0.42;
function arrowPath(d){
  const x=d[0],y=d[1],dx=d[2],dy=d[3],L=Math.hypot(dx,dy);
  if(L<1) return null;
  const h=Math.min(2.8,0.45*L),ux=dx/L,uy=dy/L;
  const c=Math.cos(HB),s=Math.sin(HB),tx=x+dx,ty=y+dy;
  const b1x=h*(-ux*c+uy*s),b1y=h*(-ux*s-uy*c);
  const b2x=h*(-ux*c-uy*s),b2y=h*( ux*s-uy*c);
  return `M${x},${y}L${tx},${ty}M${tx},${ty}l${b1x},${b1y}M${tx},${ty}l${b2x},${b2y}`;
}
const ar=gz.append("g").attr("fill","none");
ar.selectAll("path").data(A.filter(d=>arrowPath(d))).join("path")
  .attr("d",arrowPath)
  .attr("class",d=>d[4]>0?"gop":"dem")
  .attr("stroke-opacity",0.62)
  .attr("stroke-width",0.9)
  .attr("stroke-linecap","round").attr("stroke-linejoin","round")
  .attr("vector-effect","non-scaling-stroke");
ar.selectAll("circle").data(A.filter(d=>!arrowPath(d))).join("circle")
  .attr("cx",d=>d[0]).attr("cy",d=>d[1]).attr("r",0.6)
  .attr("class",d=>d[4]>0?"gop-fill":"dem-fill").attr("fill-opacity",0.62);
windMap.hover(svg.node().parentNode.id,svg,gz,A.map(d=>[d[0],d[1],d[0]+d[2],d[1]+d[3]]),
  i=>windMap.tip(A[i][6],ST[A[i][7]],2016,2020,A[i][8],A[i][9],A[i][10],A[i][11],A[i][4]),
  W,42,H-BAND+2);
const lg=svg.append("g").attr("transform",`translate(16,${H-66})`);
lg.append("text").attr("y",-4).attr("font-size","11px").attr("font-weight","600")
  .attr("class","lbl").text("net votes moved");
LG.forEach((d,i)=>{
  lg.append("line").attr("x1",0).attr("y1",8+i*17).attr("x2",d.L).attr("y2",8+i*17)
    .attr("class","key").attr("stroke-width",1.6).attr("stroke-linecap","round");
  lg.append("text").attr("x",d.L+7).attr("y",12+i*17).attr("font-size","10.5px")
    .attr("class","lbl").text(d.s+" votes"+(d.i===2?" or more":""));});
const lg2=svg.append("g").attr("transform",`translate(${W-206},${H-66})`);
[["gop","shift toward the Republican"],["dem","shift toward the Democrat"]]
 .forEach((r,i)=>{lg2.append("line").attr("x1",0).attr("y1",i*17).attr("x2",22)
   .attr("y2",i*17).attr("class",r[0]).attr("stroke-width",2.2);
  lg2.append("text").attr("x",28).attr("y",i*17+4).attr("font-size","10.5px")
   .attr("class","lbl").text(r[1]);});
svg.append("text").attr("x",16).attr("y",H-10).attr("font-size","10px")
  .attr("class","foot").text("angle = %s degrees per point of margin change \\u00b7 length = net votes that changed hands");
windMap.zoom(svg.node().parentNode.id,svg,gz,W,H,42,H-BAND+2);
})();
</script>
', OUTL, A, LEGN, STJ, W, H, BAND, RED, BLU, round(W/2), round(W/2), pc(DEGPP, 0)))

## ---- us16-static
par(mar = c(0.2, 0.2, 2.2, 0.2))
plot(NA, xlim = range(uo$x), ylim = range(uo$y) + c(-330, 40), asp = 1,
     axes = FALSE, ann = FALSE)
for (p in split(uo, uo$part)) polygon(p$x, p$y, col = "#f6f6f6",
                                      border = "#c9c9c9", lwd = 0.4)
o <- u16f[order(u16f$lw), ]
arrow_at(o$x, o$y, o$dx, o$dy, adjustcolor(swing_col(o$swing), 0.62),
         lwd = 0.5, head = 0.012)
title("How every county moved, 2016 to 2020", cex.main = 1.0, line = 0.6)
mtext("same encoding as Figure 2; leaning right = Republican gain, left = Democratic gain",
      side = 3, line = -0.4, cex = 0.66, col = "#666666")
# ly sits low enough that the whole key, including the party swatches at
# ly + 70, clears the bottom of the map. At -60 they landed on Florida.
lx <- min(uo$x) + 60; ly <- min(uo$y) - 105
text(lx, ly + 95, "net votes moved", cex = 0.62, font = 2, col = "#444444", pos = 4)
for (i in 1:3) {
  k  <- c(1000, 5000, NVFUS)[i]
  yy <- ly - (i - 1) * 95
  segments(lx + 6, yy, lx + 6 + len_us(k), yy, lwd = 1.8, col = "#555555")
  text(lx + 6 + len_us(k), yy,
       paste0("  ", n(k), " votes", if (i == 3) " or more" else ""),
       cex = 0.58, col = "#555555", pos = 4)
}
rx <- max(uo$x) - 1150
segments(rx, ly + 70, rx + 110, ly + 70, col = RED, lwd = 2.4)
text(rx + 110, ly + 70, "  shift toward the Republican", cex = 0.58, col = RED, pos = 4)
segments(rx, ly - 25, rx + 110, ly - 25, col = BLU, lwd = 2.4)
text(rx + 110, ly - 25, "  shift toward the Democrat", cex = 0.58, col = BLU, pos = 4)
mtext(paste0("angle = ", pc(DEGPP, 0),
             " degrees per point of margin change; length = net votes moved"),
      side = 1, line = -1.2, cex = 0.58, col = "#888888")

## ---- pair-check
data.frame(
  measured_over = c("Counties drawn", "Counties moving toward the Republican",
                    "...as a share of counties", "...as a share of the votes cast",
                    "Counties moving toward the Democrat",
                    "...as a share of counties", "...as a share of the votes cast",
                    "Median county shift", "National two-party margin moved",
                    "Who won"),
  `2016 to 2020` = c(n(P1$n), n(P1$cnt_R), paste0(pc(P1$pct_R), "%"),
                     paste0(pc(P1$vsh_R), "%"), n(P1$cnt_D),
                     paste0(pc(P1$pct_D), "%"), paste0(pc(P1$vsh_D), "%"),
                     sg(P1$med), sg(P1$natl), "Biden"),
  `2020 to 2024` = c(n(P2$n), n(P2$cnt_R), paste0(pc(P2$pct_R), "%"),
                     paste0(pc(P2$vsh_R), "%"), n(P2$cnt_D),
                     paste0(pc(P2$pct_D), "%"), paste0(pc(P2$vsh_D), "%"),
                     sg(P2$med), sg(P2$natl), "Trump"),
  check.names = FALSE)

## ---- margin-defn
data.frame(
  `counties moving toward the Republican` = c(
    "Two-party margin (what every arrow above draws)",
    "Margin as a share of all votes cast"),
  `2016 to 2020` = c(paste0(pc(P1$pct_R), "%  of counties, ",
                            pc(P1$vsh_R), "% of votes"),
                     paste0(pc(P1$av_pct_R), "%  of counties, ",
                            pc(P1$av_vsh_R), "% of votes")),
  `2020 to 2024` = c(paste0(pc(P2$pct_R), "%  of counties, ",
                            pc(P2$vsh_R), "% of votes"),
                     paste0(pc(P2$av_pct_R), "%  of counties, ",
                            pc(P2$av_vsh_R), "% of votes")),
  check.names = FALSE)

## ---- halves-d3
# The screen twin of the print scatter. Same numbers, same frame, same two
# fitted lines; what the screen adds is the ability to ask a dot which county
# it is, which is the whole reason 3,099 anonymous points are worth plotting.
LO <- -20; HI <- 30
PL <- 58; PR <- 18; PT <- 60; PB <- 56; SQ <- 470
W3 <- PL + SQ + PR; H3 <- PT + SQ + PB
SXs <- function(v) PL + (v - LO) / (HI - LO) * SQ
SYs <- function(v) PT + (HI - v) / (HI - LO) * SQ

d  <- u24f[order(u24f$lw), ]
ST <- sort(unique(d$state_name))
PTS <- paste(sprintf('[%.1f,%.1f,%.1f,%.1f,%.1f,%.2f,"%s",%d,%d]',
                     SXs(pmin(pmax(d$swing_a, LO), HI)),
                     SYs(pmin(pmax(d$swing_b, LO), HI)),
                     d$swing_a, d$swing_b, d$swing, d$lw,
                     gsub('"', "", sub(" (County|Parish|Borough|city|City).*$", "",
                                       d$county_name)),
                     match(d$state_name, ST) - 1L,
                     d$votes_dem_24 + d$votes_gop_24), collapse = ",")
STJ  <- paste(sprintf('"%s"', ST), collapse = ",")
TICK <- paste(seq(LO, HI, by = 10), collapse = ",")
ln <- function(b0, b1) sprintf('[%.1f,%.1f,%.1f,%.1f]',
        SXs(LO), SYs(b0 + b1 * LO), SXs(HI), SYs(b0 + b1 * HI))
cat(sprintf('
<div id="halves" class="wind-fig" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const P=[%s],ST=[%s],TK=[%s];
const W=%d,H=%d,PL=%d,PT=%d,SQ=%d;
const svg=d3.select("#halves").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
svg.append("text").attr("x",W/2).attr("y",20).attr("text-anchor","middle")
  .attr("font-size","14px").attr("font-weight","600").attr("class","ttl")
  .text("One dot per county, one axis per half");
svg.append("text").attr("x",W/2).attr("y",37).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("class","sub")
  .text("per voter: slope %s, r %s \\u00b7 per county: slope %s, r %s");
const gx=v=>PL+(v+20)/50*SQ, gy=v=>PT+(30-v)/50*SQ;
// grid, axes, ticks
const ax=svg.append("g");
TK.forEach(t=>{
  ax.append("line").attr("x1",gx(t)).attr("y1",PT).attr("x2",gx(t)).attr("y2",PT+SQ)
    .attr("class","rule-2").attr("stroke-width",1);
  ax.append("line").attr("x1",PL).attr("y1",gy(t)).attr("x2",PL+SQ).attr("y2",gy(t))
    .attr("class","rule-2").attr("stroke-width",1);
  ax.append("text").attr("x",gx(t)).attr("y",PT+SQ+18).attr("text-anchor","middle")
    .attr("font-size","10.5px").attr("class","foot").text(t);
  ax.append("text").attr("x",PL-9).attr("y",gy(t)+3.5).attr("text-anchor","end")
    .attr("font-size","10.5px").attr("class","foot").text(t);});
ax.append("line").attr("x1",PL).attr("y1",gy(0)).attr("x2",PL+SQ).attr("y2",gy(0))
  .attr("class","key").attr("stroke-width",1);
ax.append("line").attr("x1",gx(0)).attr("y1",PT).attr("x2",gx(0)).attr("y2",PT+SQ)
  .attr("class","key").attr("stroke-width",1);
// where exact cancellation would put every county
svg.append("line").attr("x1",gx(-20)).attr("y1",gy(20)).attr("x2",gx(30)).attr("y2",gy(-30))
  .attr("class","key").attr("stroke-width",1.2).attr("stroke-dasharray","2,3");
// A county outside the frame is NOT drawn on the frame edge. A dot at a
// clamped coordinate is a mark claiming a position the county does not have;
// the print figure clips those points and so does this one, and the count of
// what was dropped is printed in the corner either way.
const inF=d=>d[2]>=-20&&d[2]<=30&&d[3]>=-20&&d[3]<=30;
const IN=P.filter(inF);
svg.append("g").selectAll("circle").data(IN).join("circle")
  .attr("cx",d=>d[0]).attr("cy",d=>d[1]).attr("r",d=>0.7+2.6*d[5])
  .attr("class",d=>d[4]>0?"gop-fill":"dem-fill").attr("fill-opacity",0.34);
// per-county fit, then per-voter fit on top: the weighted one is the steeper
const F1=%s, F2=%s;
svg.append("line").attr("x1",F1[0]).attr("y1",F1[1]).attr("x2",F1[2]).attr("y2",F1[3])
  .attr("class","key").attr("stroke-width",1.8).attr("stroke-dasharray","6,3");
svg.append("line").attr("x1",F2[0]).attr("y1",F2[1]).attr("x2",F2[2]).attr("y2",F2[3])
  .attr("class","ttl-stroke").attr("stroke-width",2.6);
svg.append("text").attr("x",W/2).attr("y",H-16).attr("text-anchor","middle")
  .attr("font-size","10.5px").attr("class","foot")
  .text("2016 to 2020 swing (points, + = toward the Republican)");
svg.append("text").attr("transform",`translate(14,${PT+SQ/2}) rotate(-90)`)
  .attr("text-anchor","middle").attr("font-size","10.5px").attr("class","foot")
  .text("2020 to 2024 swing (points)");
svg.append("text").attr("x",PL+SQ).attr("y",PT+SQ-8).attr("text-anchor","end")
  .attr("font-size","10px").attr("class","foot")
  .text("%s counties fall outside this frame");
// The two counties the map below is about. Inside the frame: an open circle.
// Outside it: an arrowhead ON the edge it left by, never a dot.
// Ten counties in this file are called Henry. Matching on the name alone
// picked the wrong one; the state index is part of the key.
[["%s","%s","gop"],["%s","%s","dem"]].forEach(([nm,st,cls])=>{
  const d=P.find(p=>p[6]===nm&&ST[p[7]]===st); if(!d) return;
  const g=svg.append("g");
  if(inF(d)){
    g.append("circle").attr("cx",d[0]).attr("cy",d[1]).attr("r",5.5)
      .attr("fill","none").attr("stroke-width",1.6)
      .attr("class",cls==="gop"?"gop":"dem");
    g.append("text").attr("x",d[0]+9).attr("y",d[1]+4).attr("font-size","11px")
      .attr("font-weight","600").attr("class",cls+"-txt").text(nm);
  } else {
    const dx=Math.sign(d[2]-Math.min(Math.max(d[2],-20),30));
    const dy=-Math.sign(d[3]-Math.min(Math.max(d[3],-20),30));
    const z=8, px=-dy, py=dx;
    g.append("path").attr("class",cls+"-fill")
      .attr("d",`M${d[0]},${d[1]}l${-dx*z-px*z*0.55},${-dy*z-py*z*0.55}`
               +`l${px*z*1.1},${py*z*1.1}Z`);
    g.append("text").attr("x",d[0]-(dx>0?11:-11)).attr("y",d[1]+4)
      .attr("text-anchor",dx>0?"end":"start").attr("font-size","11px")
      .attr("font-weight","600").attr("class",cls+"-txt").text(nm);
  }
});
const f=d3.format(",");
const gh=svg.append("g");
windMap.hover("halves",svg,gh,IN.map(d=>[d[0],d[1]]),i=>{const d=IN[i];
    const sg=v=>(v>0?"R+":"D+")+Math.abs(v).toFixed(1);
    const cls=d[4]>0?"gop":"dem", dir=d[4]>0?"Republican":"Democratic";
    return (`<div class="tt-name">${d[6]}, ${ST[d[7]]}</div>`
      +`<div class="tt-lede ${cls}-txt">${Math.abs(d[4]).toFixed(1)} pts. more ${dir} `
      +`<span>than 2016</span></div>`
      +`<table><thead><tr><th>Period</th><th>Margin change</th></tr></thead><tbody>`
      +`<tr><td>2016 to 2020</td><td>${sg(d[2])}</td></tr>`
      +`<tr><td>2020 to 2024</td><td>${sg(d[3])}</td></tr>`
      +`</tbody><tfoot><tr><td>2016 to 2024</td><td><b>${sg(d[4])}</b></td></tr>`
      +`<tr><td>Two-party votes</td><td>${f(d[8])}</td></tr></tfoot></table>`);},
  W,PT,PT+SQ);
})();
</script>
', PTS, STJ, TICK, W3, H3, PL, PT, SQ,
   pc(F("us1624_slope_w"), 2), pc(F("us1624_halves_cor_w"), 2),
   pc(F("us1624_slope"), 2),   pc(F("us1624_halves_cor"), 2),
   ln(F("us1624_int"), F("us1624_slope")),
   ln(F("us1624_int_w"), F("us1624_slope_w")),
   n(F("us1624_off_frame")),
   sub(" County", "", F("us1624_top_county")), F("us1624_top_state"),
   sub(" County", "", F("us1624_bot_county")), F("us1624_bot_state")))

## ---- halves-scatter-static
d <- u24f
LO <- -20; HI <- 30      # the same square frame the screen figure uses

par(mar = c(3.5, 3.5, 2.6, 0.8), mgp = c(2.1, 0.45, 0), tcl = -0.22)
plot(NA, xlim = c(LO, HI), ylim = c(LO, HI), asp = 1, axes = FALSE,
     xlab = "", ylab = "")
# The line every point would sit on if the two halves cancelled exactly.
abline(a = 0, b = -1, col = GRY, lty = 3, lwd = 1)
abline(h = 0, v = 0, col = GRY, lwd = 0.7)
o <- d[order(d$lw), ]
points(o$swing_a, o$swing_b, pch = 19, cex = 0.14 + 0.75 * o$lw,
       col = adjustcolor(swing_col(o$swing), 0.32))
# Two fits, because they answer different questions: one per county, one per
# voter. Grofman and Cervas (2024) name reporting only the first "failing to
# weight units", and the weighted line is the steeper one.
abline(a = F("us1624_int"),   b = F("us1624_slope"),   col = GRY, lwd = 1.6, lty = 2)
abline(a = F("us1624_int_w"), b = F("us1624_slope_w"), col = INK, lwd = 2.4)
axis(1, col = GRY, col.axis = INK, cex.axis = 0.72, lwd = 0.7)
axis(2, col = GRY, col.axis = INK, cex.axis = 0.72, lwd = 0.7, las = 1)
mtext("2016 to 2020 swing (points, + = toward the Republican)", 1,
      line = 2.1, cex = 0.72, col = INK)
mtext("2020 to 2024 swing (points)", 2, line = 2.2, cex = 0.72, col = INK)
title("One dot per county, one axis per half", cex.main = 0.95, line = 1.3,
      col.main = INK)
mtext(paste0("per voter: slope ", pc(F("us1624_slope_w"), 2), ", r ",
             pc(F("us1624_halves_cor_w"), 2), "   \u00b7   per county: slope ",
             pc(F("us1624_slope"), 2), ", r ", pc(F("us1624_halves_cor"), 2)),
      3, line = 0.15, cex = 0.64, col = GRY)
# The two counties the map is about, named on both figures. A county outside
# the frame gets an arrowhead ON the edge it left by, never a circle: a circle
# at a clamped coordinate is a mark claiming a position the county does not
# have, which is the one thing this chapter is about not doing.
lab <- function(nm, st, col) {
  i <- which(d$county_name == nm & d$state_name == st)
  if (!length(i)) return(invisible())
  ax <- d$swing_a[i]; ay <- d$swing_b[i]
  x  <- min(max(ax, LO), HI); y <- min(max(ay, LO), HI)
  nn <- sub(" County", "", nm)
  if (ax != x || ay != y) {                       # clamped: say which way
    dx <- sign(ax - x); dy <- sign(ay - y); z <- 0.95
    polygon(c(x, x - dx * z - dy * z * 0.6, x - dx * z + dy * z * 0.6),
            c(y, y - dy * z + dx * z * 0.6, y - dy * z - dx * z * 0.6),
            col = col, border = NA)
    text(x, y, paste0(nn, "  "), pos = if (dx > 0) 2 else 4,
         offset = 0.3, cex = 0.62, col = col, font = 2)
  } else {
    points(x, y, pch = 1, cex = 1.5, lwd = 1.4, col = col)
    text(x, y, paste0("  ", nn), pos = 4, offset = 0.35,
         cex = 0.62, col = col, font = 2)
  }
}
lab(F("us1624_top_county"), F("us1624_top_state"), RED)
lab(F("us1624_bot_county"), F("us1624_bot_state"), BLU)
text(HI, LO + 1.2, paste0(n(F("us1624_off_frame")), " counties fall outside this frame  "),
     adj = 1, cex = 0.58, col = GRY)

## ---- us24-d3
# The legends used to sit at H-92, which on this frame is on top of Florida.
# The map now gets a fixed height and the keys get a band of their own beneath
# it, so nothing is drawn over the country.
W <- 780; PAD <- 8; MAPT <- 52; MAPH <- 424; BAND <- 84
H <- MAPT + MAPH + BAND
xr <- range(uo$x); yr <- range(uo$y)
s  <- min((W - 2*PAD) / diff(xr), MAPH / diff(yr))
SX <- function(x) PAD + (x - xr[1]) * s
SY <- function(y) MAPT + (yr[2] - y) * s
paths <- vapply(split(uo, uo$part), function(z) {
  # Simplify in PIXEL space, not by index: round each vertex to the pixel and
  # drop the ones that land on top of their predecessor. The boundary can move
  # by at most half a pixel, and -- this is the part that matters -- two states
  # that share a border round it to the same pixels, so the border stays shared.
  # Dropping every sixth vertex instead moved some borders by 35 pixels and,
  # because each state was thinned independently, left gaps along every shared
  # edge. On white paper those gaps are white. On any other background they are
  # wedges of it, showing through the middle of the country.
  px <- round(SX(z$x)); py <- round(SY(z$y))
  k  <- c(TRUE, diff(px) != 0 | diff(py) != 0)
  paste0("M", paste(sprintf("%d,%d", px[k], py[k]), collapse = "L"), "Z")
}, character(1))
OUTL <- paste(sprintf('"%s"', paths), collapse = ",")
d <- u24f[order(u24f$lw), ]                 # thin first, heavy last
ST <- sort(unique(d$state_name))
A <- paste(sprintf('[%.1f,%.1f,%.1f,%.1f,%.1f,%.2f,"%s",%d,%d,%d,%d,%d]',
                   SX(d$x), SY(d$y), d$dx * s, -d$dy * s, d$swing, d$lw,
                   gsub('"', "", sub(" (County|Parish|Borough|city|City).*$", "",
                                     d$county_name)),
                   match(d$state_name, ST) - 1L,
                   d$votes_dem_16, d$votes_gop_16,
                   d$votes_dem_24, d$votes_gop_24), collapse = ",")
STJ <- paste(sprintf('"%s"', ST), collapse = ",")
LEGN <- paste(sprintf('{"s":"%s","L":%.1f,"i":%d}',
                      c("1,000", "5,000", n(NVFUS)),
                      len_us(c(1000, 5000, NVFUS)) * s, 0:2), collapse = ",")
cat(sprintf('
<div id="usw24" class="wind-fig" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const O=[%s],A=[%s],LG=[%s],ST=[%s];
const W=%d,H=%d,BAND=%d,RED="%s",BLU="%s";
const svg=d3.select("#usw24").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
svg.append("text").attr("x",%d).attr("y",18).attr("text-anchor","middle")
  .attr("font-size","14px").attr("font-weight","600").attr("class","ttl")
  .text("How every county moved, 2016 to 2024");
svg.append("text").attr("x",%d).attr("y",34).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("class","sub")
  .text("eight years, two pairs added together \\u00b7 leaning right = Republican gain, left = Democratic gain");
const gz=windMap.stage(svg,svg.node().parentNode.id,W,42,H-BAND+2);
const g=gz.append("g");
g.selectAll("path").data(O).join("path").attr("d",d=>d)
  .attr("class","land").attr("stroke-width",0.6)
  .attr("vector-effect","non-scaling-stroke");
// same shaft-plus-barbs arrow as the 2020-to-2024 figure, restated here
// because this script is self-contained
const HB=0.42;
function arrowPath(d){
  const x=d[0],y=d[1],dx=d[2],dy=d[3],L=Math.hypot(dx,dy);
  if(L<1) return null;
  const h=Math.min(2.8,0.45*L),ux=dx/L,uy=dy/L;
  const c=Math.cos(HB),s=Math.sin(HB),tx=x+dx,ty=y+dy;
  const b1x=h*(-ux*c+uy*s),b1y=h*(-ux*s-uy*c);
  const b2x=h*(-ux*c-uy*s),b2y=h*( ux*s-uy*c);
  return `M${x},${y}L${tx},${ty}M${tx},${ty}l${b1x},${b1y}M${tx},${ty}l${b2x},${b2y}`;
}
const ar=gz.append("g").attr("fill","none");
ar.selectAll("path").data(A.filter(d=>arrowPath(d))).join("path")
  .attr("d",arrowPath)
  .attr("class",d=>d[4]>0?"gop":"dem")
  .attr("stroke-opacity",0.62)
  .attr("stroke-width",0.9)
  .attr("stroke-linecap","round").attr("stroke-linejoin","round")
  .attr("vector-effect","non-scaling-stroke");
ar.selectAll("circle").data(A.filter(d=>!arrowPath(d))).join("circle")
  .attr("cx",d=>d[0]).attr("cy",d=>d[1]).attr("r",0.6)
  .attr("class",d=>d[4]>0?"gop-fill":"dem-fill").attr("fill-opacity",0.62);
windMap.hover(svg.node().parentNode.id,svg,gz,A.map(d=>[d[0],d[1],d[0]+d[2],d[1]+d[3]]),
  i=>windMap.tip(A[i][6],ST[A[i][7]],2016,2024,A[i][8],A[i][9],A[i][10],A[i][11],A[i][4]),
  W,42,H-BAND+2);
const lg=svg.append("g").attr("transform",`translate(16,${H-66})`);
lg.append("text").attr("y",-4).attr("font-size","11px").attr("font-weight","600")
  .attr("class","lbl").text("net votes moved");
LG.forEach((d,i)=>{
  lg.append("line").attr("x1",0).attr("y1",8+i*17).attr("x2",d.L).attr("y2",8+i*17)
    .attr("class","key").attr("stroke-width",1.6).attr("stroke-linecap","round");
  lg.append("text").attr("x",d.L+7).attr("y",12+i*17).attr("font-size","10.5px")
    .attr("class","lbl").text(d.s+" votes"+(d.i===2?" or more":""));});
const lg2=svg.append("g").attr("transform",`translate(${W-206},${H-66})`);
[["gop","shift toward the Republican"],["dem","shift toward the Democrat"]]
 .forEach((r,i)=>{lg2.append("line").attr("x1",0).attr("y1",i*17).attr("x2",22)
   .attr("y2",i*17).attr("class",r[0]).attr("stroke-width",2.2);
  lg2.append("text").attr("x",28).attr("y",i*17+4).attr("font-size","10.5px")
   .attr("class","lbl").text(r[1]);});
svg.append("text").attr("x",16).attr("y",H-10).attr("font-size","10px")
  .attr("class","foot").text("angle = %s degrees per point of margin change \\u00b7 length = net votes that changed hands");
windMap.zoom(svg.node().parentNode.id,svg,gz,W,H,42,H-BAND+2);
})();
</script>
', OUTL, A, LEGN, STJ, W, H, BAND, RED, BLU, round(W/2), round(W/2), pc(DEGPP, 0)))

## ---- us24-static
par(mar = c(0.2, 0.2, 2.2, 0.2))
plot(NA, xlim = range(uo$x), ylim = range(uo$y) + c(-330, 40), asp = 1,
     axes = FALSE, ann = FALSE)
for (p in split(uo, uo$part)) polygon(p$x, p$y, col = "#f6f6f6",
                                      border = "#c9c9c9", lwd = 0.4)
o <- u24f[order(u24f$lw), ]
arrow_at(o$x, o$y, o$dx, o$dy, adjustcolor(swing_col(o$swing), 0.62),
         lwd = 0.5, head = 0.012)
title("How every county moved, 2016 to 2024", cex.main = 1.0, line = 0.6)
mtext("eight years; leaning right = Republican gain, left = Democratic gain",
      side = 3, line = -0.4, cex = 0.66, col = "#666666")
# ly sits low enough that the whole key, including the party swatches at
# ly + 70, clears the bottom of the map. At -60 they landed on Florida.
lx <- min(uo$x) + 60; ly <- min(uo$y) - 105
text(lx, ly + 95, "net votes moved", cex = 0.62, font = 2, col = "#444444", pos = 4)
for (i in 1:3) {
  k  <- c(1000, 5000, NVFUS)[i]
  yy <- ly - (i - 1) * 95
  segments(lx + 6, yy, lx + 6 + len_us(k), yy, lwd = 1.8, col = "#555555")
  text(lx + 6 + len_us(k), yy,
       paste0("  ", n(k), " votes", if (i == 3) " or more" else ""),
       cex = 0.58, col = "#555555", pos = 4)
}
rx <- max(uo$x) - 1150
segments(rx, ly + 70, rx + 110, ly + 70, col = RED, lwd = 2.4)
text(rx + 110, ly + 70, "  shift toward the Republican", cex = 0.58, col = RED, pos = 4)
segments(rx, ly - 25, rx + 110, ly - 25, col = BLU, lwd = 2.4)
text(rx + 110, ly - 25, "  shift toward the Democrat", cex = 0.58, col = BLU, pos = 4)
mtext(paste0("angle = ", pc(DEGPP, 0),
             " degrees per point of margin change; length = net votes moved"),
      side = 1, line = -1.2, cex = 0.58, col = "#888888")

## ---- us24-summary
data.frame(
  quantity = c("Arrows drawn", "Counties leaning right (%)",
               "…as a share of the votes cast",
               "Median county shift", "National two-party margin moved",
               "Spread of the eight-year swing (sd)",
               "…if the two halves were unrelated",
               "Correlation between the two halves",
               "Biggest shift right", "Biggest shift left",
               "Arrows at the length cap"),
  value = c(n(F("us1624_arrows")), pc(F("us1624_share_R")),
            paste0(pc(F("us1624_vshare_R")), "%"),
            sg(F("us1624_swing_median")), sg(F("us1624_agg_swing")),
            pc(F("us1624_swing_sd")), pc(F("us1624_sd_if_indep")),
            pc(F("us1624_halves_cor"), 2),
            paste0(F("us1624_top_county"), ", ", F("us1624_top_state"), "  ",
                   sg(F("us1624_top_swing"))),
            paste0(F("us1624_bot_county"), ", ", F("us1624_bot_state"), "  ",
                   sg(F("us1624_bot_swing"))),
            n(F("us1624_capped"))))

## ---- weighting
data.frame(
  question = c("What did the typical county do?",
               "What did the typical county do? (median)",
               "What did the votes do?"),
  answer = c(paste0("mean county shift ", sg(F("us_swing_unweighted"))),
             paste0("median county shift ", sg(F("us_swing_median"))),
             paste0("national two-party margin moved ", sg(F("us_agg_swing")))),
  check.names = FALSE)

## ---- ga-d3
W2 <- 720; H2 <- 470; PD <- 10
gx <- range(go$x); gy <- range(go$y)
s2 <- min((W2 - 200 - 2*PD) / diff(gx), (H2 - 56 - 2*PD) / diff(gy))
GX <- function(x) PD + (x - gx[1]) * s2
GY <- function(y) PD + 44 + (gy[2] - y) * s2
gpaths <- vapply(split(go, go$part), function(z)
  paste0("M", paste(sprintf("%.0f,%.0f", GX(z$x), GY(z$y)), collapse = "L"), "Z"),
  character(1))
GO <- paste(sprintf('"%s"', gpaths), collapse = ",")
GA <- paste(sprintf('{"x":%.1f,"y":%.1f,"u":%.1f,"v":%.1f,"s":%.2f,"w":%.2f,"n":"%s","d0":%d,"r0":%d,"d1":%d,"r1":%d}',
                    GX(ga$x), GY(ga$y), ga$dx * s2, -ga$dy * s2, ga$swing, ga$lw,
                    ga$county, ga$dem_20, ga$gop_20, ga$dem_24, ga$gop_24),
            collapse = ",")
GL <- paste(sprintf('{"s":"%s","L":%.1f,"i":%d}', c("500", n(NVFGA)),
                    len_ga(c(500, NVFGA)) * s2, 0:1), collapse = ",")
cat(sprintf('
<div id="gaw" class="wind-fig" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const O=[%s],A=[%s],LG=[%s];
const W=%d,H=%d,RED="%s",BLU="%s";
const svg=d3.select("#gaw").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
svg.append("text").attr("x",%d).attr("y",18).attr("text-anchor","middle")
  .attr("font-size","14px").attr("font-weight","600").attr("class","ttl")
  .text("Georgia, county by county, 2020 to 2024");
svg.append("text").attr("x",%d).attr("y",34).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("class","sub")
  .text("Secretary of State returns, both years \\u00b7 %s counties, unchanged since 1945");
const gz=windMap.stage(svg,"gaw",W,42,H);
const g=gz.append("g");
g.selectAll("path").data(O).join("path").attr("d",d=>d)
  .attr("class","land").attr("stroke-width",0.5)
  .attr("vector-effect","non-scaling-stroke");
svg.append("defs").selectAll("marker").data(["gR","gB"]).join("marker")
  .attr("id",d=>d).attr("viewBox","0 0 10 10").attr("refX",8).attr("refY",5)
  .attr("markerWidth",4).attr("markerHeight",4).attr("orient","auto")
  .append("path").attr("d","M0,0L10,5L0,10Z").attr("class",d=>d==="gR"?"gop-fill":"dem-fill");
const ar=gz.append("g");
ar.selectAll("line").data(A).join("line")
  .attr("x1",d=>d.x).attr("y1",d=>d.y).attr("x2",d=>d.x+d.u).attr("y2",d=>d.y+d.v)
  .attr("class",d=>d.s>0?"gop":"dem").attr("stroke-opacity",0.85)
  .attr("stroke-width",1.1).attr("stroke-linecap","round")
  .attr("marker-end",d=>d.s>0?"url(#gR)":"url(#gB)");
windMap.hover("gaw",svg,gz,A.map(d=>[d.x,d.y,d.x+d.u,d.y+d.v]),
  i=>windMap.tip(A[i].n,"",2020,2024,A[i].d0,A[i].r0,A[i].d1,A[i].r1,A[i].s),
  W,42,H);
const lg=svg.append("g").attr("transform",`translate(${W-186},96)`);
lg.append("text").attr("y",-6).attr("font-size","11px").attr("font-weight","600")
  .attr("class","lbl").text("net votes moved");
LG.forEach((d,i)=>{
  lg.append("line").attr("x1",0).attr("y1",8+i*18).attr("x2",d.L).attr("y2",8+i*18)
    .attr("class","key").attr("stroke-width",1.8).attr("stroke-linecap","round");
  lg.append("text").attr("x",d.L+7).attr("y",12+i*18).attr("font-size","10.5px")
    .attr("class","lbl").text(d.s+" votes"+(d.i===1?" or more":""));});
[["gop","toward the Republican"],["dem","toward the Democrat"]].forEach((r,i)=>{
  lg.append("line").attr("x1",0).attr("y1",64+i*17).attr("x2",22).attr("y2",64+i*17)
    .attr("stroke",r[0]).attr("stroke-width",2.2);
  lg.append("text").attr("x",28).attr("y",68+i*17).attr("font-size","10.5px")
    .attr("class","lbl").text(r[1]);});
lg.append("text").attr("y",112).attr("font-size","10px").attr("class","foot")
  .text("%s degrees per point");
lg.append("text").attr("y",126).attr("font-size","10px").attr("class","foot")
  .text("(national map: full at %s)");
windMap.zoom("gaw",svg,gz,W,H,42,H);
})();
</script>
', GO, GA, GL, W2, H2, RED, BLU, round((W2 - 190)/2), round((W2 - 190)/2),
   n(F("ga_counties")), pc(DEGPP, 0), n(NVFUS)))

## ---- ga-static
par(mar = c(1.8, 0.2, 2.2, 0.2))
plot(NA, xlim = range(go$x) + c(0, 95), ylim = range(go$y), asp = 1,
     axes = FALSE, ann = FALSE)
for (p in split(go, go$part)) polygon(p$x, p$y, col = "#f7f7f7",
                                      border = "#cfcfcf", lwd = 0.35)
arrow_at(ga$x, ga$y, ga$dx, ga$dy, swing_col(ga$swing),
         lwd = 0.9, head = 0.028)
title("Georgia, county by county, 2020 to 2024", cex.main = 1.0, line = 0.6)
mtext(paste0("Secretary of State returns, both years; ", n(F("ga_counties")),
             " counties, unchanged since 1945"),
      side = 3, line = -0.4, cex = 0.66, col = "#666666")
lx <- max(go$x) + 6; ly <- max(go$y) - 30
text(lx, ly + 22, "arrow length", cex = 0.6, font = 2, col = "#444444", pos = 4)
for (i in 1:2) {
  k <- c(500, NVFGA)[i]
  segments(lx + 2, ly - (i - 1) * 20, lx + 2 + len_ga(k), ly - (i - 1) * 20,
           lwd = 1.7, col = "#555555")
  text(lx + 4 + len_ga(k), ly - (i - 1) * 20, paste0(n(k), " votes"), cex = 0.55,
       col = "#555555", pos = 4)
}
text(lx + 2, ly - 52, "toward the Republican", cex = 0.55, col = RED, pos = 4)
text(lx + 2, ly - 68, "toward the Democrat", cex = 0.55, col = BLU, pos = 4)
text(lx + 2, ly - 92, paste0(pc(DEGPP, 0), " degrees per point"), cex = 0.52,
     col = "#888888", pos = 4)
text(lx + 2, ly - 106, paste0("(national: full at ", n(NVFUS), ")"), cex = 0.52,
     col = "#888888", pos = 4)

## ---- ga-summary
data.frame(
  quantity = c("Counties drawn", "Georgia margin, 2020", "Georgia margin, 2024",
               "Statewide shift", "Median county shift",
               "Counties shifting right (%)",
               "Biggest shift right", "Biggest shift left"),
  value = c(n(F("ga_counties_drawn")), sg(F("ga_state_margin_20")),
            sg(F("ga_state_margin_24")), sg(F("ga_state_swing")),
            sg(F("ga_swing_median")), pc(F("ga_share_R")),
            paste0(F("ga_swing_max_county"), "  ", sg(F("ga_swing_max"))),
            paste0(F("ga_swing_min_county"), "  ", sg(F("ga_swing_min")))))

## ---- ladder
lad

## ---- ladder-note
data.frame(
  quantity = c("2020 precincts to place", "Placed after every repair",
               "Left unplaced", "Share unplaced (%)",
               "Counties affected", "Worst county",
               "Precincts unmatched there"),
  value = c(n(F("pj_precincts_2020")), n(F("pj_best")), n(F("pj_lost")),
            pc(F("pj_lost_pct")), n(F("pj_counties_affected")),
            paste0(F("pj_worst_county"), " (Savannah)"),
            paste0(n(F("pj_worst_unmatched")), " of ", n(F("pj_worst_total")))))

## ---- office
data.frame(
  quantity = c("Georgia margin, president", "Georgia margin, US Senate",
               "Gap between two offices on the same ballots",
               "Median precinct-level gap", "Precincts differing by over 2 points (%)",
               "Largest precinct gap",
               "For comparison: Georgia's whole 2020 to 2024 shift"),
  value = c(sg(F("og_state_pres")), sg(F("og_state_sen")),
            paste0(pc(F("og_state_gap")), " points"),
            paste0(pc(F("og_median_abs")), " points"),
            pc(F("og_share_over2")),
            paste0(pc(F("og_max_abs")), " points"),
            paste0(pc(abs(F("ga_state_swing"))), " points")))

## ---- office-fig-d3
# Same bins as the print figure: hist() decides them once and both renderers
# draw the identical histogram.
h  <- hist(og$gap, breaks = 60, plot = FALSE)
W8 <- 760; H8 <- 400; ML <- 56; MR <- 20; MT <- 62; MB <- 54
xr <- range(h$breaks); yr <- c(0, max(h$counts))
BINS <- paste(sprintf('[%.3f,%.3f,%d]', h$breaks[-length(h$breaks)],
                      h$breaks[-1], h$counts), collapse = ",")
xt <- pretty(xr, 8); yt <- pretty(yr, 5)
cat(sprintf('
<div id="offgap" class="wind-fig" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const B=[%s],XT=[%s],YT=[%s];
const W=%d,H=%d,ML=%d,MR=%d,MT=%d,MB=%d;
const x0=%.4f,x1=%.4f,y1=%d;
const PW=W-ML-MR,PH=H-MT-MB;
const X=v=>ML+(v-x0)/(x1-x0)*PW, Y=v=>MT+PH-(v/y1)*PH;
const svg=d3.select("#offgap").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
svg.append("text").attr("x",W/2).attr("y",22).attr("text-anchor","middle")
  .attr("font-size","14px").attr("font-weight","600").attr("class","ttl")
  .text("Two offices, one ballot, %s Georgia precincts");
svg.append("text").attr("x",W/2).attr("y",39).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("class","sub")
  .text("Nobody’s turnout changed. Nobody’s mind changed. The margin did.");
const ax=svg.append("g");
YT.forEach(t=>{
  ax.append("line").attr("x1",ML).attr("y1",Y(t)).attr("x2",ML+PW).attr("y2",Y(t))
    .attr("class","rule-2").attr("stroke-width",1);
  ax.append("text").attr("x",ML-9).attr("y",Y(t)+3.5).attr("text-anchor","end")
    .attr("font-size","10.5px").attr("class","foot").text(t);});
XT.forEach(t=>{
  ax.append("text").attr("x",X(t)).attr("y",MT+PH+18).attr("text-anchor","middle")
    .attr("font-size","10.5px").attr("class","foot").text(t);});
svg.append("g").selectAll("rect").data(B).join("rect")
  .attr("x",d=>X(d[0])).attr("y",d=>Y(d[2]))
  .attr("width",d=>Math.max(1,X(d[1])-X(d[0])-0.6))
  .attr("height",d=>MT+PH-Y(d[2]))
  .attr("class","bin").attr("fill-opacity",0.85);
// the three lines the figure exists to compare
const mark=(v,cls,dash,lab,dy)=>{
  svg.append("line").attr("x1",X(v)).attr("y1",MT).attr("x2",X(v)).attr("y2",MT+PH)
    .attr("class",cls).attr("stroke-width",v===0?1.2:2.2)
    .attr("stroke-dasharray",dash);
  if(lab) svg.append("text").attr("x",X(v)+7).attr("y",MT+dy)
    .attr("font-size","11px").attr("font-weight","600").attr("class",cls+"-txt")
    .text(lab);};
mark(0,"key",null,null,0);
mark(%.4f,"gop",null,"office effect, statewide: %s pts",22);
mark(%.4f,"built",("5,3"),"Georgia’s real 2020–2024 shift: %s pts",48);
svg.append("text").attr("x",ML+PW/2).attr("y",H-16).attr("text-anchor","middle")
  .attr("font-size","10.5px").attr("class","foot")
  .text("Senate margin minus presidential margin, same ballots (points)");
svg.append("text").attr("transform",`translate(15,${MT+PH/2}) rotate(-90)`)
  .attr("text-anchor","middle").attr("font-size","10.5px").attr("class","foot")
  .text("precincts");
const tip=d3.select("#offgap").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:6px 9px;"+
 "border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("rect").data(B).join("rect")
  .attr("x",d=>X(d[0])).attr("y",MT).attr("width",d=>Math.max(1,X(d[1])-X(d[0])))
  .attr("height",PH).attr("fill","transparent")
  .on("mousemove",function(e,d){tip.style("opacity",1)
    .html(`<b>${d[2]}</b> precinct${d[2]===1?"":"s"}<br>`
      +`<span style="opacity:.75">gap ${d[0].toFixed(1)} to ${d[1].toFixed(1)} pts</span>`)
    .style("left",Math.min(e.offsetX+14,W-190)+"px").style("top",(e.offsetY-10)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
', BINS, paste(xt, collapse = ","), paste(yt, collapse = ","),
   W8, H8, ML, MR, MT, MB, xr[1], xr[2], max(yr),
   n(F("og_n")),
   F("og_state_gap"), pc(F("og_state_gap")),
   F("ga_state_swing"), pc(F("ga_state_swing"))))

## ---- office-fig-static
par(mar = c(3.6, 3.4, 2.4, 0.8), mgp = c(2.1, 0.6, 0))
h <- hist(og$gap, breaks = 60, plot = FALSE)
plot(h, col = "#dfe8ef", border = "#9fb6c6",
     main = "", xlab = "Senate margin minus presidential margin, same ballots (points)",
     ylab = "precincts", las = 1, cex.axis = 0.75, cex.lab = 0.8)
abline(v = 0, col = GRY, lwd = 1.2)
abline(v = F("og_state_gap"), col = RED, lwd = 2.2)
abline(v = F("ga_state_swing"), col = "#4d9221", lwd = 2.2, lty = 2)
usr <- par("usr")
text(F("og_state_gap"), usr[4] * 0.93,
     paste0("  office effect, statewide: ", pc(F("og_state_gap")), " pts"),
     col = RED, cex = 0.68, pos = 4, font = 2)
text(F("ga_state_swing"), usr[4] * 0.78,
     paste0("  Georgia's real 2020-2024 shift: ", pc(F("ga_state_swing")), " pts"),
     col = "#4d9221", cex = 0.68, pos = 4, font = 2)
title(paste0("Two offices, one ballot, ", n(F("og_n")), " Georgia precincts"),
      cex.main = 0.95, line = 1.0)
mtext("Nobody's turnout changed. Nobody's mind changed. The margin did.",
      side = 3, line = -0.2, cex = 0.64, col = "#666666")

## ---- plan2026
data.frame(
  option = c("2024 president to 2026 governor", "2022 governor to 2026 governor"),
  office = c("different", "same"),
  electorate = c("presidential vs midterm", "midterm vs midterm"),
  verdict = c(paste0("carries at least ", pc(F("og_state_gap")),
                     " points of office effect before anything real"),
              "what this chapter uses"),
  check.names = FALSE)

## ---- base2026-d3
W9 <- 720; H9 <- 500; P9 <- 12
gx <- range(go$x); gy <- range(go$y)
s9 <- min((W9 - 2*P9) / diff(gx), (H9 - 74 - 2*P9) / diff(gy))
GX9 <- function(x) P9 + (W9 - 2*P9 - diff(gx)*s9)/2 + (x - gx[1]) * s9
GY9 <- function(y) P9 + 56 + (gy[2] - y) * s9
gp <- vapply(split(go, go$part), function(z) {
  px <- round(GX9(z$x)); py <- round(GY9(z$y))
  k  <- c(TRUE, diff(px) != 0 | diff(py) != 0)
  paste0("M", paste(sprintf("%d,%d", px[k], py[k]), collapse = "L"), "Z")
}, character(1))
GO9 <- paste(sprintf('"%s"', gp), collapse = ",")
d9  <- g22[order(g22$total), ]
D9  <- paste(sprintf('[%.1f,%.1f,%.2f,%.2f,"%s",%d]',
                     GX9(d9$x), GY9(d9$y),
                     0.5 + 13 * sqrt(d9$total) / sqrt(max(d9$total)),
                     d9$margin_from, d9$county, d9$total), collapse = ",")
cat(sprintf('
<div id="ga22" class="wind-fig" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const O=[%s],D=[%s],W=%d,H=%d;
const svg=d3.select("#ga22").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
svg.append("text").attr("x",W/2).attr("y",22).attr("text-anchor","middle")
  .attr("font-size","14px").attr("font-weight","600").attr("class","ttl")
  .text("The 2026 map, waiting: %s Georgia counties, 2022 governor");
svg.append("text").attr("x",W/2).attr("y",39).attr("text-anchor","middle")
  .attr("font-size","11.5px").attr("class","sub")
  .text("Kemp vs Abrams, %s statewide \\u00b7 every dot is an arrow with only one end");
const gz=windMap.stage(svg,"ga22",W,44,H);
gz.append("g").selectAll("path").data(O).join("path").attr("d",d=>d)
  .attr("class","land").attr("stroke-width",0.5)
  .attr("vector-effect","non-scaling-stroke");
gz.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>d[0]).attr("cy",d=>d[1]).attr("r",d=>d[2])
  .attr("class",d=>d[3]>0?"gop-fill":"dem-fill").attr("fill-opacity",0.55)
  .attr("stroke","var(--card)").attr("stroke-width",0.6)
  .attr("vector-effect","non-scaling-stroke");
svg.append("text").attr("x",W/2).attr("y",H-14).attr("text-anchor","middle")
  .attr("font-size","11px").attr("class","foot")
  .text("The second end arrives on 3 November 2026.");
const f=d3.format(",");
windMap.hover("ga22",svg,gz,D.map(d=>[d[0],d[1]]),i=>{const d=D[i];
  const cls=d[3]>0?"gop":"dem";
  return `<div class="tt-name">${d[4]} County</div>`
    +`<div class="tt-lede ${cls}-txt">${d[3]>0?"Kemp":"Abrams"} `
    +`<span>by ${Math.abs(d[3]).toFixed(1)} pts. in 2022</span></div>`
    +`<table><tfoot><tr><td>Votes cast</td><td>${f(d[5])}</td></tr>`
    +`<tr><td>Second observation</td><td>3 Nov 2026</td></tr></tfoot></table>`;},
  W,44,H);
windMap.zoom("ga22",svg,gz,W,H,44,H);
})();
</script>
', GO9, D9, W9, H9, n(F("ga22_counties")), sg(F("ga22_margin"))))

## ---- base2026-static
par(mar = c(0.4, 0.2, 2.4, 0.2))
plot(NA, xlim = range(go$x), ylim = range(go$y) + c(-24, 0), asp = 1,
     axes = FALSE, ann = FALSE)
for (p in split(go, go$part)) polygon(p$x, p$y, col = "#fafafa",
                                      border = "#d5d5d5", lwd = 0.35)
mg <- g22$margin_from
cl <- ifelse(mg > 0, RED, BLU)
points(g22$x, g22$y, pch = 21, bg = adjustcolor(cl, 0.55), col = "white",
       cex = 0.5 + 1.7 * sqrt(g22$total) / sqrt(max(g22$total)), lwd = 0.5)
title(paste0("The 2026 map, waiting: ", n(F("ga22_counties")),
             " Georgia counties, 2022 governor"), cex.main = 0.95, line = 0.9)
mtext(paste0("Kemp vs Abrams, ", sg(F("ga22_margin")),
             " statewide. Every dot is an arrow with only one end."),
      side = 3, line = -0.3, cex = 0.64, col = "#666666")
text(mean(range(go$x)), min(go$y) - 18,
     "The second end arrives on 3 November 2026.", cex = 0.68, col = "#888888")

## ---- knowable
data.frame(
  `By early December` = c("Unofficial county returns for most states",
                        "Georgia's own results export",
                        "Certified, canvassed returns everywhere",
                        "Every race called",
                        "Precinct-level returns"),
  expect = c("yes, from press compilations",
             "likely, and it is the source this chapter uses",
             "no; certification deadlines run into December and beyond",
             "no; close races and recounts will still be open",
             "rarely this fast"),
  check.names = FALSE)

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
