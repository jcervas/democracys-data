# gss-confidence-code.R -- chunk bodies for gss-confidence-brief.Rmd
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
cf <- read.csv("data/derived/confidence.csv",  stringsAsFactors = FALSE)
it <- read.csv("data/derived/institutions.csv", stringsAsFactors = FALSE)
Y1 <- min(cf$year); Y2 <- max(cf$year)
gg <- function(inst, yr, col) cf[[col]][cf$institution == inst & cf$year == yr]
pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",")
mil <- cf[cf$institution == "Military", ]
con <- cf[cf$institution == "Congress", ]

# ---- render every data.frame in this document as a TABLE, not code output ----
# These are front-facing documents. A data.frame printed the ordinary way comes
# out as a "##"-prefixed code block, which reads as machinery rather than as a
# result. Registering knit_print for data.frame turns all of them into real
# tables in both HTML and PDF without touching a single chunk.
knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)                       # fails_when -> fails when
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)   # sentence case the first letter
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

# ---- the Congress series, and the two dates the bands figure marks -----------
CG  <- con[order(con$year), ]
GD  <- "#1b7837"      # green  = a great deal of confidence, everywhere below
OS  <- "#e2e2e2"      # gray   = only some
HA  <- "#762a83"      # purple = hardly any
CROSS <- CG$year[which(CG$hardly_any > CG$great_deal)][1]   # 2nd survey year
MAJ   <- CG$year[which(CG$hardly_any > 50)][1]              # "hardly any" a majority
MID1  <- CG$only_some[1]; MID2 <- CG$only_some[nrow(CG)]

# ---- first reading against last, with the interval on each ------------------
# Each year's percentage is an estimate from a finite sample, so the difference
# between two of them carries the uncertainty of both. Computed once here and
# read by both renderers of the dumbbell figure below.
first_last <- function(k) {
  s <- cf[cf$institution == k, ]; s <- s[order(s$year), ]
  a <- s[1, ]; z <- s[nrow(s), ]
  se <- function(p, nn) 100 * sqrt((p / 100) * (1 - p / 100) / nn)
  data.frame(institution = k, y1 = a$year, y2 = z$year,
             p1 = a$great_deal, p2 = z$great_deal,
             s1 = se(a$great_deal, a$n), s2 = se(z$great_deal, z$n),
             stringsAsFactors = FALSE)
}
DB <- do.call(rbind, lapply(it$institution, first_last))
DB$change <- DB$p2 - DB$p1
DB$se_ch  <- sqrt(DB$s1^2 + DB$s2^2)
DB$sig    <- abs(DB$change) > 1.96 * DB$se_ch
DB        <- DB[order(DB$change), ]
NSIG      <- sum(!DB$sig)
NFELL     <- sum(DB$sig & DB$change < 0)
NROSE     <- sum(DB$sig & DB$change > 0)
# counts small enough to belong in a sentence as words rather than digits; still
# computed, so a revised file changes the prose rather than contradicting it
WORDS <- c("zero", "one", "two", "three", "four", "five", "six", "seven",
           "eight", "nine", "ten", "eleven", "twelve", "thirteen")
wd    <- function(k) WORDS[k + 1]
WIDEST    <- max(1.96 * c(DB$s1, DB$s2))
STARTS    <- sort(unique(DB$y1))

## ---- clean-gss
o <- cf[cf$institution == "Congress" & cf$year == Y1,
        c("institution", "variable", "year", "n", "great_deal", "only_some",
          "hardly_any")]
names(o) <- c("institution", "variable", "year", "answered", "great deal %",
              "only some %", "hardly any %")
o

## ---- one-record
o <- con[con$year == Y1, c("institution", "variable", "year", "n",
                           "great_deal", "only_some", "hardly_any")]
names(o) <- c("institution", "GSS variable", "year", "respondents",
              "great deal (%)", "only some (%)", "hardly any (%)")
o

## ---- scope
data.frame(
  quantity = c("Institutions asked about", "Survey years", "Period",
               "Institution-years in this file",
               "Respondents per item (smallest, largest)"),
  value = c(nrow(it), length(unique(cf$year)), paste(Y1, "to", Y2),
            n(nrow(cf)), paste(n(min(cf$n)), "to", n(max(cf$n)))))

## ---- congress
o <- con[con$year %in% c(1973, 1980, 1990, 2000, 2010, 2018, Y2),
         c("year", "n", "great_deal", "only_some", "hardly_any")]
names(o) <- c("year", "respondents", "great deal (%)", "only some (%)", "hardly any (%)")
o

## ---- bands-d3
# ---------------------------------------------------------------------------
# The three shares of the Congress item at every survey year, stacked. Both
# renderers read CG, built once in setup, and both mark the same year.
#
# This chunk carries the ONE d3 <script src> for the document. A second copy
# would silently double the payload; the later figures use the library loaded
# here.
# ---------------------------------------------------------------------------
rows <- paste0("[", CG$year, ",",
               formatC(CG$great_deal, format = "f", digits = 1), ",",
               formatC(CG$only_some,  format = "f", digits = 1), ",",
               formatC(CG$hardly_any, format = "f", digits = 1), "]",
               collapse = ",")
cat(paste0('
<div id="bands" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', rows, '].map(r=>({y:r[0],g:r[1],s:r[2],h:r[3]}));
const MAJ=', MAJ, ';
const W=770,H=380,M={t:16,r:150,b:40,l:46};
const svg=d3.select("#bands").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain(d3.extent(D,d=>d.y)).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,100]).range([H-M.b,M.t]);
const keys=[{k:"g",c:"', GD, '",lab:"a great deal"},
            {k:"s",c:"', OS, '",lab:"only some"},
            {k:"h",c:"', HA, '",lab:"hardly any"}];
const st=d3.stack().keys(["g","s","h"])(D);
const ar=d3.area().x((d,i)=>x(D[i].y)).y0(d=>y(d[0])).y1(d=>y(d[1]));
svg.selectAll("path.band").data(st).join("path").attr("class","band")
  .attr("d",ar).attr("fill",(d,i)=>keys[i].c);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(10));
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickFormat(d=>d+"%").ticks(5));
const last=D[D.length-1];
const mid={g:last.g/2, s:last.g+last.s/2, h:last.g+last.s+last.h/2};
keys.forEach(function(kk){
  svg.append("text").attr("x",W-M.r+8).attr("y",y(mid[kk.k])+4)
    .attr("font-size","12px").attr("font-weight","600")
    .attr("fill",kk.k==="s"?"#666":kk.c)
    .text(kk.lab+" "+last[kk.k].toFixed(1)+"%");
});
svg.append("line").attr("x1",x(MAJ)).attr("x2",x(MAJ)).attr("y1",M.t)
  .attr("y2",H-M.b).attr("stroke","#111").attr("stroke-dasharray","3 3");
svg.append("text").attr("x",x(MAJ)-5).attr("y",M.t+12).attr("font-size","11px")
  .attr("text-anchor","end").attr("fill","#111")
  .text(MAJ+": \\u201chardly any\\u201d becomes a majority");
svg.append("text").attr("x",M.l).attr("y",H-8).attr("font-size","11px")
  .attr("fill","#666").text("Confidence in the people running Congress; bands sum to 100% of respondents.");
})();
</script>'))

## ---- bands-static
par(mar = c(3.6, 4.0, 1.0, 9.6))
plot(NA, xlim = range(CG$year), ylim = c(0, 100), xlab = "", ylab = "",
     las = 1, xaxs = "i", yaxs = "i", cex.axis = 0.85)
mtext("% of respondents", 2, line = 2.6, cex = 0.85)
cum0 <- rep(0, nrow(CG))
for (z in list(list(CG$great_deal, GD), list(CG$only_some, OS),
               list(CG$hardly_any, HA))) {
  top <- cum0 + z[[1]]
  polygon(c(CG$year, rev(CG$year)), c(top, rev(cum0)), col = z[[2]], border = NA)
  cum0 <- top
}
abline(v = MAJ, lty = 2)
text(MAJ - 1.2, 92, paste0(MAJ, ": \"hardly any\"\nbecomes a majority"),
     adj = 1, cex = 0.62)
L <- CG[nrow(CG), ]
mids <- c(L$great_deal / 2, L$great_deal + L$only_some / 2, 100 - L$hardly_any / 2)
text(max(CG$year) + 1.2, mids,
     paste0(c("a great deal ", "only some ", "hardly any "),
            pc(c(L$great_deal, L$only_some, L$hardly_any)), "%"),
     xpd = NA, adj = 0, cex = 0.72, col = c(GD, "#666666", HA), font = 2)
mtext("Confidence in the people running Congress; bands sum to 100% of respondents.",
      side = 1, line = 2.3, cex = 0.72, col = "#555555")

## ---- changes
o <- it[order(it$change), c("institution", "first_year", "survey_years", "change")]
names(o) <- c("institution", "first asked", "survey years",
              "change in % 'great deal'")
o

## ---- dumb-d3
rows <- paste0("{\"k\":\"", DB$institution, "\",\"y1\":", DB$y1, ",\"y2\":", DB$y2,
               ",\"p1\":", formatC(DB$p1, format = "f", digits = 1),
               ",\"p2\":", formatC(DB$p2, format = "f", digits = 1),
               ",\"e1\":", formatC(1.96 * DB$s1, format = "f", digits = 2),
               ",\"e2\":", formatC(1.96 * DB$s2, format = "f", digits = 2),
               ",\"ch\":\"", ifelse(DB$change > 0, "+", ""),
               formatC(DB$change, format = "f", digits = 1),
               "\",\"sig\":", tolower(as.character(DB$sig)), "}", collapse = ",")
cat(paste0('
<div id="dumb" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const D=[', rows, '];
const GD="', GD, '";
const W=770,H=430,M={t:30,r:74,b:42,l:214};
const svg=d3.select("#dumb").append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,70]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.k)).range([M.t,H-M.b]).padding(0.35);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickFormat(d=>d+"%").ticks(8));
svg.append("g").attr("transform","translate("+M.l+",0)").call(d3.axisLeft(y).tickSize(0))
  .selectAll("text").attr("font-size","11px");
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("% with a great deal of confidence in the people running it");
const g=svg.append("g");
D.forEach(function(d){
  const yy=y(d.k)+y.bandwidth()/2;
  g.append("line").attr("x1",x(d.p1)).attr("x2",x(d.p2)).attr("y1",yy).attr("y2",yy)
   .attr("stroke",d.sig?"#999":"#ddd").attr("stroke-width",4);
  [[d.p1,d.e1],[d.p2,d.e2]].forEach(function(q){
    g.append("line").attr("x1",x(q[0]-q[1])).attr("x2",x(q[0]+q[1]))
     .attr("y1",yy).attr("y2",yy).attr("stroke","#555").attr("stroke-width",1);
  });
  g.append("circle").attr("cx",x(d.p1)).attr("cy",yy).attr("r",5)
   .attr("fill","#fff").attr("stroke",GD).attr("stroke-width",2);
  g.append("circle").attr("cx",x(d.p2)).attr("cy",yy).attr("r",5).attr("fill",GD);
  g.append("text").attr("x",W-M.r+8).attr("y",yy+4).attr("font-size","11px")
   .attr("fill",d.sig?"#222":"#999").text(d.ch+(d.sig?"":" ns"));
});
const lg=svg.append("g").attr("transform","translate("+M.l+",16)");
lg.append("circle").attr("cx",6).attr("cy",-4).attr("r",5).attr("fill","#fff")
  .attr("stroke",GD).attr("stroke-width",2);
lg.append("text").attr("x",16).attr("y",0).attr("font-size","11px")
  .text("first survey year (1973; 1975 for banks)");
lg.append("circle").attr("cx",256).attr("cy",-4).attr("r",5).attr("fill",GD);
lg.append("text").attr("x",266).attr("y",0).attr("font-size","11px").text("2024");
lg.append("text").attr("x",396).attr("y",0).attr("font-size","11px").attr("fill","#555")
  .text("thin rule = 95% interval on that single reading");
})();
</script>'))

## ---- dumb-static
par(mar = c(3.8, 12.6, 2.4, 3.6))
plot(NA, xlim = c(0, 70), ylim = c(0.5, nrow(DB) + 0.5), axes = FALSE,
     xlab = "", ylab = "")
axis(1, at = seq(0, 70, 10), labels = paste0(seq(0, 70, 10), "%"), cex.axis = 0.8)
mtext("% with a great deal of confidence in the people running it", 1, line = 2.2,
      cex = 0.82)
axis(2, at = seq_len(nrow(DB)), labels = DB$institution, las = 1, tick = FALSE,
     cex.axis = 0.72)
for (i in seq_len(nrow(DB))) {
  segments(DB$p1[i], i, DB$p2[i], i,
           col = if (DB$sig[i]) "#999999" else "#dddddd", lwd = 4)
  segments(DB$p1[i] - 1.96 * DB$s1[i], i, DB$p1[i] + 1.96 * DB$s1[i], i,
           col = "#555555", lwd = 1)
  segments(DB$p2[i] - 1.96 * DB$s2[i], i, DB$p2[i] + 1.96 * DB$s2[i], i,
           col = "#555555", lwd = 1)
  points(DB$p1[i], i, pch = 21, bg = "white", col = GD, cex = 1.1, lwd = 2)
  points(DB$p2[i], i, pch = 19, col = GD, cex = 1.1)
  text(71, i, paste0(ifelse(DB$change[i] > 0, "+", ""), pc(DB$change[i]),
                     ifelse(DB$sig[i], "", " ns")),
       xpd = NA, adj = 0, cex = 0.68,
       col = if (DB$sig[i]) "#222222" else "#999999")
}
legend("top", c("first survey year (1973; 1975 for banks)", "2024",
                "thin rule = 95% interval"),
       pch = c(21, 19, NA), lty = c(NA, NA, 1), col = c(GD, GD, "#555555"),
       pt.bg = "white", pt.lwd = 2, bty = "n", cex = 0.62, horiz = TRUE,
       inset = -0.07, xpd = NA)
mtext(paste("Intervals treat each year as a simple random sample; the GSS is",
            "clustered, so the true intervals are wider."),
      side = 1, line = 3.0, cex = 0.68, col = "#555555")

## ---- military
o <- mil[mil$year %in% c(1973, 1980, 1986, 1991, 2000, 2008, 2018, Y2),
         c("year", "great_deal", "hardly_any")]
names(o) <- c("year", "great deal (%)", "hardly any (%)")
o

## ---- current
y <- cf[cf$year == Y2, c("institution", "great_deal", "hardly_any")]
y <- y[order(-y$great_deal), ]
y$net <- y$great_deal - y$hardly_any
names(y) <- c("institution", "great deal (%)", "hardly any (%)", "net")
y

## ---- lines-static
ks <- c("Military", "Scientific community", "Medicine", "U.S. Supreme Court",
        "Press", "Congress")
cl <- c("#4d9221", "#2c7fb8", "#8856a7", "#e08214", "#777777", "#C41230")
plot(NA, xlim = c(Y1, Y2), ylim = c(0, 84), las = 1, xlab = "",
     ylab = "% with a great deal of confidence")
# every institution, faint, so the PDF shows the same population as the HTML
for (k in it$institution) {
  s <- cf[cf$institution == k, ]; s <- s[order(s$year), ]
  lines(s$year, s$great_deal, col = "#cccccc", lwd = 1)
}
# the six discussed above, drawn on top and named in the legend
for (i in seq_along(ks)) {
  s <- cf[cf$institution == ks[i], ]; s <- s[order(s$year), ]
  lines(s$year, s$great_deal, col = cl[i], lwd = 2.4)
}
legend("topright", ks, col = cl, lwd = 2.4, ncol = 2, cex = 0.75, bty = "n")
mtext(sprintf("All %d institutions; the six named above are drawn in color.",
              nrow(it)), side = 1, line = 2.6, cex = 0.75, col = "#555555")

## ---- d3-lines
pal <- c("#C41230","#2c7fb8","#4d9221","#e08214","#8856a7","#00868B","#B8860B",
         "#777777","#c51b7d","#3b6ea5","#7f7f00","#a05a2c","#5a5a8c")
ser <- paste(mapply(function(k, col) {
  s <- cf[cf$institution == k, ]; s <- s[order(s$year), ]
  sprintf('{"k":"%s","c":"%s","v":[%s]}', k, col,
          paste(sprintf('[%d,%.1f]', s$year, s$great_deal), collapse = ","))
}, it$institution, pal[seq_len(nrow(it))]), collapse = ",")
cat(sprintf('
<div id="gss" style="position:relative;margin:1em 0"></div>
<!-- d3 v7 is loaded once, by the first D3 figure above -->
<script>
(function(){
const S=[%s];
const W=770,H=450,M={t:18,r:24,b:40,l:52};
const svg=d3.select("#gss").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const allY=S.flatMap(s=>s.v.map(p=>p[0]));
const x=d3.scaleLinear().domain(d3.extent(allY)).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,70]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(11));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickFormat(d=>d+"%%").ticks(7));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",14)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("%% with a great deal of confidence");
const ln=d3.line().x(p=>x(p[0])).y(p=>y(p[1]));
const on={}; S.forEach(s=>on[s.k]=true);
const g=svg.append("g");
let focus=null;
function draw(){
  g.selectAll("path").data(S.filter(s=>on[s.k]),s=>s.k).join("path")
   .attr("fill","none").attr("stroke",s=>s.c).attr("d",s=>ln(s.v))
   .attr("stroke-width",s=>focus===s.k?3.6:2)
   .attr("opacity",s=>focus&&focus!==s.k?0.14:1);
}
draw();
const rule=svg.append("line").attr("y1",M.t).attr("y2",H-M.b).attr("stroke","#bbb").attr("opacity",0);
const dots=svg.append("g");
const tip=d3.select("#gss").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:11.5px;opacity:0;white-space:nowrap");
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l).attr("height",H-M.b-M.t)
  .attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(e){
    const px=d3.pointer(e,this)[0]+M.l;
    const yrs=S[0].v.map(p=>p[0]);
    const yr=yrs.reduce((a,c)=>Math.abs(x(c)-px)<Math.abs(x(a)-px)?c:a);
    rule.attr("x1",x(yr)).attr("x2",x(yr)).attr("opacity",1);
    const rows=S.filter(s=>on[s.k]).map(s=>{const p=s.v.find(q=>q[0]===yr);
      return p?{k:s.k,c:s.c,val:p[1]}:null;}).filter(Boolean).sort((a,b)=>b.val-a.val);
    dots.selectAll("circle").data(rows).join("circle")
      .attr("cx",x(yr)).attr("cy",r=>y(r.val)).attr("r",3.6).attr("fill",r=>r.c);
    tip.style("opacity",1).html(`<b>${yr}</b><br>`+
      rows.map(r=>`<span style="color:${r.c}">\\u25a0</span> ${r.k}: ${r.val}%%`).join("<br>"))
      .style("left",Math.min(x(yr)-M.l+18,W-300)+"px").style("top",(M.t+2)+"px");
  })
  .on("mouseleave",()=>{rule.attr("opacity",0);dots.selectAll("circle").remove();tip.style("opacity",0);});
const leg=d3.select("#gss").append("div").attr("style","margin-top:8px;font-size:11.5px;line-height:1.8");
leg.selectAll("span").data(S).join("span")
  .attr("style",s=>`display:inline-block;margin-right:10px;cursor:pointer;color:${s.c};font-weight:600`)
  .html(s=>`\\u25a0 ${s.k}`)
  .on("mouseenter",(e,s)=>{focus=s.k;draw();})
  .on("mouseleave",()=>{focus=null;draw();})
  .on("click",function(e,s){on[s.k]=!on[s.k];
    d3.select(this).style("opacity",on[s.k]?1:0.3);focus=null;draw();});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Move across for a year readout; hover a label to isolate a line, click to hide
it.</p>
', ser))

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so it gets a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# LIGHT PAGE ONLY: on the dark page the fill is lifted and already passes,
# and a --paper stroke would sit dark behind a dark ink there, because the
# checker scores the fill against the stroke it touches.
# Sites found by _lib/check-contrast.js --light.
cat('<style>
@media (prefers-color-scheme: light) {
#bands text[fill="#111" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
}
</style>')
