# historical-campaigns-code.R -- chunk bodies for historical-campaigns-brief.Rmd
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
p <- read.csv("data/derived/pres_states_1864_2024.csv", stringsAsFactors = FALSE)
p$d2 <- 100 * p$democrat / (p$democrat + p$republican)

c11 <- c("AL","AR","FL","GA","LA","MS","NC","SC","TN","TX","VA")
c17 <- c(c11, "KY","MO","MD","WV","OK","DE")

gap_for <- function(sts) {
  p$r <- ifelse(p$state_abbrev %in% sts, "S", "R")
  a <- aggregate(d2 ~ year + r, data = p, FUN = mean, na.rm = TRUE)
  w <- reshape(a, idvar = "year", timevar = "r", direction = "wide")
  names(w) <- c("year", "rest", "south")
  w$gap <- round(w$south - w$rest, 1)
  w[order(w$year), ]
}
g11 <- gap_for(c11); g17 <- gap_for(c17)
gv  <- function(w, y) w$gap[w$year == y]
first_neg <- function(w) min(w$year[!is.na(w$gap) & w$gap < 0])
perm_neg  <- function(w) {
  y <- w$year[!is.na(w$gap)]
  min(y[sapply(y, function(yy) all(w$gap[w$year >= yy & !is.na(w$gap)] < 0))])
}

sp <- aggregate(d2 ~ year, data = p, FUN = sd, na.rm = TRUE)
names(sp)[2] <- "sd"; sp <- sp[order(sp$year), ]

# how much of the state-to-state spread is South-versus-rest?
vd <- do.call(rbind, lapply(sort(unique(p$year)), function(y) {
  s <- p[p$year == y & !is.na(p$d2), ]
  g <- s$state_abbrev %in% c11
  if (sum(g) == 0 || sum(!g) == 0) return(NULL)
  gm <- tapply(s$d2, g, mean); nn <- table(g); tot <- mean(s$d2)
  data.frame(year = y, sd = sd(s$d2),
             pct_between = 100 * sum(nn * (gm - tot)^2) / sum((s$d2 - tot)^2))
}))
vb <- function(y) vd$pct_between[vd$year == y]

p$close <- abs(p$d2 - 50) < 5
ncl <- aggregate(close ~ year, data = p, FUN = sum, na.rm = TRUE)
tot <- as.data.frame(table(p$year)); names(tot) <- c("year", "n")
tot$year <- as.numeric(as.character(tot$year))
ncl <- merge(ncl, tot, by = "year"); ncl$pct <- 100 * ncl$close / ncl$n
cv <- function(y, v) ncl[[v]][ncl$year == y]

# how lopsided was the election overall? (unweighted mean state, the only
# national figure this file can support -- it carries no vote counts)
nt <- aggregate(d2 ~ year, data = p, FUN = mean, na.rm = TRUE)
names(nt)[2] <- "natl"
ncl <- merge(ncl, nt, by = "year")
ncl$tilt <- abs(ncl$natl - 50)
ncl$even <- ncl$tilt < 3            # "the election itself was close"
ev_pre  <- mean(ncl$pct[ncl$even & ncl$year <  2000])
ev_post <- mean(ncl$pct[ncl$even & ncl$year >= 2000])
sd_hi <- sp$year[which.max(sp$sd)]
sd_lo <- sp$year[which.min(sp$sd)]

absent <- p[is.na(p$d2), ]
pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
sv <- function(s, y, v) p[[v]][p$state_abbrev == s & p$year == y]

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

## ---- one-row
o <- p[p$state_abbrev == "MS" & p$year == 1948,
       c("year", "state_abbrev", "democrat", "republican", "other", "winner")]
names(o) <- c("year", "state", "Democratic %", "Republican %", "other %",
              "winner")
o

## ---- clean-hist
o <- p[(p$year == 1864 & p$state_abbrev == "CA") |
       (p$year == 1948 & p$state_abbrev == "AL"),
       c("year", "state_abbrev", "democrat", "republican", "other", "winner")]
names(o) <- c("year", "state", "Dem %", "Rep %", "other %", "winner")
o

## ---- coverage
data.frame(
  quantity = c("Rows", "Elections", "First and last year",
               "States in the first election", "States in the last election"),
  value = c(format(nrow(p), big.mark = ","), length(unique(p$year)),
            paste(min(p$year), "to", max(p$year)),
            sum(p$year == min(p$year)), sum(p$year == max(p$year))))

## ---- absent
o <- absent[absent$state_abbrev %in% c("AL", "CA"),
            c("year", "state_abbrev", "democrat", "republican", "winner")]
names(o) <- c("year", "state", "Democratic %", "Republican %", "winner")
o

## ---- south-static
plot(g11$year, g11$south, type = "l", lwd = 2.5, col = "#2166AC",
     ylim = c(20, 95), xlab = "",
     ylab = "Democratic share of two-party vote (%)", las = 1)
lines(g11$year, g11$rest, lwd = 2.5, col = "#B2182B")
abline(h = 50, lty = 3, col = "grey50")
abline(v = 1964, lty = 2, col = "grey30")
legend("bottomleft", c("11 Confederate states", "rest of the country"),
       col = c("#2166AC", "#B2182B"), lwd = 2.5, bty = "n", cex = 0.85)

## ---- south-d3
ser <- sprintf('[{"k":"11 Confederate states","c":"#2166AC","v":[%s]},{"k":"rest of the country","c":"#B2182B","v":[%s]}]',
  paste(sprintf('[%d,%.1f]', g11$year[!is.na(g11$south)], g11$south[!is.na(g11$south)]), collapse = ","),
  paste(sprintf('[%d,%.1f]', g11$year[!is.na(g11$rest)],  g11$rest[!is.na(g11$rest)]),  collapse = ","))
cat(sprintf('
<div id="south" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const S=%s;
const W=760,H=430,M={t:18,r:150,b:40,l:52};
const box=d3.select("#south");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([1864,2024]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([20,95]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(9));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickFormat(d=>d+"%%").ticks(6));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",14)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("Democratic share of the two-party vote");
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(50)).attr("y2",y(50))
  .attr("stroke","#bbb").attr("stroke-dasharray","3,3");
svg.append("line").attr("x1",x(1964)).attr("x2",x(1964)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#888").attr("stroke-dasharray","5,4");
svg.append("text").attr("x",x(1964)+5).attr("y",M.t+12).attr("font-size","11px")
  .attr("fill","#666").text("1964");
const ln=d3.line().x(d=>x(d[0])).y(d=>y(d[1]));
S.forEach(s=>{
  svg.append("path").attr("d",ln(s.v)).attr("fill","none")
    .attr("stroke",s.c).attr("stroke-width",2.4);
  const last=s.v[s.v.length-1];
  svg.append("text").attr("x",W-M.r+8).attr("y",y(last[1])+4)
    .attr("font-size","11.5px").attr("fill",s.c).text(s.k);
});
const rule=svg.append("line").attr("y1",M.t).attr("y2",H-M.b).attr("stroke","#bbb").attr("opacity",0);
const dots=svg.append("g");
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const yrs=S[0].v.map(d=>d[0]);
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(ev){
    const px=d3.pointer(ev,this)[0]+M.l;
    const yr=yrs.reduce((a,b)=>Math.abs(x(b)-px)<Math.abs(x(a)-px)?b:a);
    rule.attr("x1",x(yr)).attr("x2",x(yr)).attr("opacity",1);
    const rows=S.map(s=>{const f=s.v.find(d=>d[0]===yr); return f?{k:s.k,c:s.c,val:f[1]}:null}).filter(Boolean);
    dots.selectAll("circle").data(rows).join("circle")
      .attr("cx",x(yr)).attr("cy",r=>y(r.val)).attr("r",4).attr("fill",r=>r.c);
    const gap=rows.length===2?(rows[0].val-rows[1].val).toFixed(1):"n/a";
    tip.style("opacity",1).html(`<b>${yr}</b><br>`+
      rows.map(r=>`<span style="color:${r.c}">&#9632;</span> ${r.k}: ${r.val}%%`).join("<br>")+
      `<br>gap: ${gap}`)
      .style("left",Math.min(x(yr)-M.l+18,W-260)+"px").style("top",(M.t+6)+"px"); })
  .on("mouseleave",()=>{rule.attr("opacity",0);dots.selectAll("circle").remove();tip.style("opacity",0);});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Move across the chart to read both regions and the gap between them in any year.</p>
', ser))

## ---- gaps
yy <- c(1924, 1948, 1960, 1964, 1976, 1980, 1984, 2024)
o <- data.frame(year = yy,
                south = pc(g11$south[match(yy, g11$year)]),
                rest  = pc(g11$rest[match(yy, g11$year)]),
                gap   = pc(g11$gap[match(yy, g11$year)]))
names(o) <- c("year", "South", "rest of country", "gap (points)")
o

## ---- robust
m <- merge(g11[, c("year","gap")], g17[, c("year","gap")], by = "year",
           suffixes = c("_11", "_17"))
yy <- c(1924, 1948, 1960, 1964, 1976, 1992, 2024)
o <- m[match(yy, m$year), ]
names(o) <- c("year", "gap, 11 states", "gap, 17 states")
o

## ---- robust-static
par(mar = c(4, 4.4, 1, 1))
yl <- range(c(g11$gap, g17$gap), na.rm = TRUE)
plot(g11$year, g11$gap, type = "n", ylim = yl, xlab = "", las = 1,
     ylab = "South minus rest of country (points)")
abline(h = 0, col = "grey35")
abline(v = first_neg(g11), lty = 3, col = "#999")
lines(g11$year, g11$gap, lwd = 2.6, col = "#C41230")
lines(g17$year, g17$gap, lwd = 2.2, col = "#2c7fb8", lty = 2)
points(g11$year, g11$gap, pch = 19, cex = 0.6, col = "#C41230")
points(g17$year, g17$gap, pch = 2,  cex = 0.6, col = "#2c7fb8")
text(first_neg(g11), yl[2], paste0("  ", first_neg(g11), ": first year below zero"),
     adj = c(0, 1), cex = 0.75, col = "#555")
text(2024, 0.5, "same sign for both definitions", adj = c(1, 0), cex = 0.75,
     col = "#555")
legend("bottomleft", c("11 Confederate states (circles)",
                       "17 slave states (triangles)"),
       col = c("#C41230", "#2c7fb8"), lwd = c(2.6, 2.2), lty = c(1, 2),
       pch = c(19, 2), bty = "n", cex = 0.78)

## ---- robust-d3
ok11 <- g11[!is.na(g11$gap), ]; ok17 <- g17[!is.na(g17$gap), ]
cat(sprintf('
<div id="rob" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const A=[%s],B=[%s],FN=%d;
const W=760,H=420,M={t:20,r:150,b:40,l:56};
const box=d3.select("#rob");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([1864,2024]).range([M.l,W-M.r]);
const all=A.concat(B).map(d=>d[1]);
const y=d3.scaleLinear().domain([d3.min(all)-3,d3.max(all)+3]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(9));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).ticks(7));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("South minus rest of country (points)");
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(0)).attr("y2",y(0))
  .attr("stroke","#666");
svg.append("line").attr("x1",x(FN)).attr("x2",x(FN)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#bbb").attr("stroke-dasharray","4,4");
svg.append("text").attr("x",x(FN)+5).attr("y",M.t+11).attr("font-size","11px")
  .attr("fill","#666").text(FN+": first year below zero");
const ln=d3.line().x(d=>x(d[0])).y(d=>y(d[1]));
const S=[{k:"11 Confederate states",c:"#C41230",v:A,dash:null,sym:d3.symbolCircle},
         {k:"17 slave states",c:"#2c7fb8",v:B,dash:"6,4",sym:d3.symbolTriangle}];
S.forEach(s=>{
  svg.append("path").attr("d",ln(s.v)).attr("fill","none").attr("stroke",s.c)
    .attr("stroke-width",2.4).attr("stroke-dasharray",s.dash);
  svg.append("g").selectAll("path.m").data(s.v).join("path")
    .attr("d",d3.symbol().type(s.sym).size(34))
    .attr("transform",d=>`translate(${x(d[0])},${y(d[1])})`).attr("fill",s.c);
  const last=s.v[s.v.length-1];
  svg.append("text").attr("x",W-M.r+8).attr("y",y(last[1])+(s.c==="#C41230"?-2:12))
    .attr("font-size","11.5px").attr("fill",s.c).text(s.k);
});
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const rule=svg.append("line").attr("y1",M.t).attr("y2",H-M.b).attr("stroke","#ccc").attr("opacity",0);
const yrs=A.map(d=>d[0]);
svg.append("rect").attr("x",M.l).attr("y",M.t).attr("width",W-M.r-M.l)
  .attr("height",H-M.b-M.t).attr("fill","none").attr("pointer-events","all")
  .on("mousemove",function(ev){
    const px=d3.pointer(ev,this)[0]+M.l;
    const yr=yrs.reduce((a,b)=>Math.abs(x(b)-px)<Math.abs(x(a)-px)?b:a);
    rule.attr("x1",x(yr)).attr("x2",x(yr)).attr("opacity",1);
    const a=A.find(d=>d[0]===yr),b=B.find(d=>d[0]===yr);
    tip.style("opacity",1).html(`<b>${yr}</b><br>11 states: ${a?a[1].toFixed(1):"n/a"}`+
      `<br>17 states: ${b?b[1].toFixed(1):"n/a"}`)
      .style("left",Math.min(x(yr)-M.l+18,W-220)+"px").style("top",(M.t+6)+"px");})
  .on("mouseleave",()=>{rule.attr("opacity",0);tip.style("opacity",0);});
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Circles: the eleven Confederate states. Triangles: those plus the six border
states. Widening the definition lowers the whole curve without moving the year
it crosses zero.</p>
', paste(sprintf('[%d,%.1f]', ok11$year, ok11$gap), collapse = ","),
   paste(sprintf('[%d,%.1f]', ok17$year, ok17$gap), collapse = ","),
   first_neg(g11)))

## ---- truman
o <- p[p$year == 1960 & p$winner == "Harry S. Truman",
       c("year", "state_abbrev", "democrat", "republican", "other", "winner")]
names(o) <- c("year", "state", "Democratic %", "Republican %", "other %",
              "winner")
o

## ---- spread
yy <- c(1892, 1924, 1960, 1988, 2012, 2024)
o <- data.frame(year = yy, sd = pc(sp$sd[match(yy, sp$year)], 2))
names(o) <- c("year", "SD of state Democratic share (points)")
o

## ---- shapes-static
yrs <- c(sd_hi, sd_lo, 2024)
par(mar = c(4, 5.6, 0.6, 1))
plot(NA, xlim = c(5, 100), ylim = c(0.5, 3.5), yaxt = "n", xlab =
     "Democratic share of the two-party vote (%)", ylab = "", las = 1,
     bty = "n")
rect(45, 0.5, 55, 3.5, col = "#f0f0f0", border = NA)
abline(v = 50, col = "grey45", lty = 2)
axis(2, at = 3:1, labels = paste0(yrs, "\n(SD ", pc(sp$sd[match(yrs, sp$year)], 1),
     ")"), las = 1, tick = FALSE, cex.axis = 0.78)
cols <- c("#C41230", "#4d9221", "#2c7fb8")
for (i in seq_along(yrs)) {
  v <- p$d2[p$year == yrs[i] & !is.na(p$d2)]
  yy <- (4 - i) + seq(-0.16, 0.16, length.out = length(v))[rank(v, ties.method = "first")]
  points(v, yy, pch = 21, bg = cols[i], col = "white", cex = 0.85, lwd = 0.6)
}
text(5, 3.42, "gray band = within 5 points of an even split", adj = c(0, 1),
     cex = 0.72, col = "#555")

## ---- shapes-d3
yrs <- c(sd_hi, sd_lo, 2024)
rows <- paste(unlist(lapply(seq_along(yrs), function(i) {
  s <- p[p$year == yrs[i] & !is.na(p$d2), ]
  sprintf('{"r":%d,"y":%d,"s":"%s","v":%.1f}', i, yrs[i], s$state_abbrev, s$d2)
})), collapse = ",")
lab <- paste(sprintf('{"r":%d,"t":"%s"}', seq_along(yrs),
                     sprintf("%d (SD %s)", yrs, pc(sp$sd[match(yrs, sp$year)], 1))),
             collapse = ",")
cat(sprintf('
<div id="shp" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s],L=[%s];
const W=760,H=300,M={t:26,r:20,b:44,l:104};
const box=d3.select("#shp");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([5,100]).range([M.l,W-M.r]);
const yb=d3.scalePoint().domain(L.map(d=>d.r)).range([M.t+14,H-M.b-14]).padding(0.4);
svg.append("rect").attr("x",x(45)).attr("y",M.t).attr("width",x(55)-x(45))
  .attr("height",H-M.b-M.t).attr("fill","#f0f0f0");
svg.append("line").attr("x1",x(50)).attr("x2",x(50)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#999").attr("stroke-dasharray","4,4");
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(8).tickFormat(d=>d+"%%"));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("Democratic share of the two-party vote");
svg.append("text").attr("x",M.l).attr("y",M.t-8).attr("font-size","11px").attr("fill","#555")
  .text("gray band = within 5 points of an even split");
const cols={1:"#C41230",2:"#4d9221",3:"#2c7fb8"};
svg.append("g").selectAll("text").data(L).join("text")
  .attr("x",M.l-10).attr("y",d=>yb(d.r)+4).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("fill","#333").text(d=>d.t);
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:6px 9px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
const by={};D.forEach(d=>{(by[d.r]=by[d.r]||[]).push(d)});
Object.values(by).forEach(g=>{g.sort((a,b)=>a.v-b.v);
  g.forEach((d,i)=>{d.off=(i-(g.length-1)/2)/Math.max(1,g.length-1)*22;});});
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.v)).attr("cy",d=>yb(d.r)+d.off).attr("r",4)
  .attr("fill",d=>cols[d.r]).attr("stroke","#fff").attr("stroke-width",0.8)
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(`<b>${d.s} ${d.y}</b><br>${d.v.toFixed(1)}%% Democratic`)
      .style("left",Math.min(ev.offsetX+14,W-160)+"px").style("top",(ev.offsetY-34)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
One dot per state; hover for the state. In %d the country sits low and a
detached tail of Southern states sits far to the right. In 2024 the dots run
continuously from one end to the other. Both years give a large SD.</p>
', rows, lab, sd_hi))

## ---- decomp
yy <- c(1900, 1924, 1936, 1948, 1960, 1964, 1976, 1984, 2000, 2012, 2024)
o <- data.frame(year = yy,
                sd = pc(vd$sd[match(yy, vd$year)], 1),
                pb = pc(vd$pct_between[match(yy, vd$year)], 1))
names(o) <- c("year", "SD across states", "% of the spread that is South vs rest")
o

## ---- decomp-static
par(mar = c(4, 4.2, 1, 4.2))
plot(vd$year, vd$sd, type = "b", pch = 19, lwd = 2, col = "#4D4D4D",
     xlab = "", ylab = "SD of state Democratic share (points)", las = 1,
     ylim = c(0, 24))
par(new = TRUE)
plot(vd$year, vd$pct_between, type = "l", lwd = 2.6, col = "#C41230",
     axes = FALSE, xlab = "", ylab = "", ylim = c(0, 100))
axis(4, las = 1, col.axis = "#C41230")
mtext("% of spread that is South vs rest", side = 4, line = 2.8, cex = 0.85,
      col = "#C41230")
legend("topright", c("SD across states", "% South vs rest"),
       col = c("#4D4D4D", "#C41230"), lwd = 2.4, bty = "n", cex = 0.8)

## ---- decomp-d3
rows <- paste(sprintf('{"y":%d,"sd":%.2f,"pb":%.1f}',
                      vd$year, vd$sd, vd$pct_between), collapse = ",")
cat(sprintf('
<div id="dec" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=420,M={t:18,r:58,b:40,l:52};
const box=d3.select("#dec");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain(d3.extent(D,d=>d.y)).range([M.l,W-M.r]);
const yl=d3.scaleLinear().domain([0,24]).range([H-M.b,M.t]);
const yr=d3.scaleLinear().domain([0,100]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(9));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(yl).ticks(6));
svg.append("g").attr("transform",`translate(${W-M.r},0)`)
  .call(d3.axisRight(yr).ticks(5).tickFormat(d=>d+"%%"));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",14)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#4D4D4D")
  .text("SD of state Democratic share");
svg.append("text").attr("transform","rotate(90)").attr("x",(H-M.b+M.t)/2).attr("y",-(W-14))
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#C41230")
  .text("share of spread that is South vs rest");
svg.append("path").attr("d",d3.line().x(d=>x(d.y)).y(d=>yr(d.pb))(D))
  .attr("fill","none").attr("stroke","#C41230").attr("stroke-width",2.6);
svg.append("path").attr("d",d3.line().x(d=>x(d.y)).y(d=>yl(d.sd))(D))
  .attr("fill","none").attr("stroke","#4D4D4D").attr("stroke-width",2.2);
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.y)).attr("cy",d=>yl(d.sd)).attr("r",4).attr("fill","#4D4D4D")
  .on("mousemove",function(ev,d){
    tip.style("opacity",1).html(
      `<b>${d.y}</b><br>SD ${d.sd.toFixed(1)} points<br>South vs rest: ${d.pb.toFixed(1)}%% of it`)
      .style("left",Math.min(ev.offsetX+14,W-230)+"px").style("top",(ev.offsetY-10)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Gray: how far apart the states are. Red: how much of that is one region.
The two come apart after 1964.</p>
', rows))

## ---- close
yy <- c(1960, 1976, 1992, 2008, 2020, 2024)
o <- data.frame(year = yy,
                n = sapply(yy, function(y) cv(y, "close")),
                tot = sapply(yy, function(y) cv(y, "n")),
                pct = pc(sapply(yy, function(y) cv(y, "pct"))))
names(o) <- c("year", "competitive states", "states in the union",
              "% competitive")
o

## ---- battle-static
par(mar = c(4, 4.4, 0.6, 1))
plot(ncl$year, ncl$pct, type = "l", col = "#bbbbbb", lwd = 1.6, las = 1,
     xlab = "", ylab = "% of states within 5 points of an even split",
     ylim = c(0, 72))
e_yr <- ncl$year[ncl$even]
segments(min(e_yr), ev_pre, max(e_yr[e_yr < 2000]), ev_pre, col = "#C41230",
         lwd = 2.4, lty = 2)
segments(min(e_yr[e_yr >= 2000]), ev_post, max(e_yr), ev_post, col = "#C41230",
         lwd = 2.4, lty = 2)
points(ncl$year, ncl$pct, pch = 1, cex = 0.7, col = "#999999")
points(ncl$year[ncl$even], ncl$pct[ncl$even], pch = 19, cex = 1.05,
       col = "#C41230")
text(1900, ev_pre + 2.5, paste0("close elections before 2000 averaged ",
     pc(ev_pre), "%"), adj = c(0, 0), cex = 0.76, col = "#C41230")
text(1998, ev_post - 4.5, paste0("since 2000, ", pc(ev_post), "%"),
     adj = c(1, 1), cex = 0.76, col = "#C41230")
for (yr in c(1960, 1976)) text(yr, ncl$pct[ncl$year == yr] + 2.6, yr,
                               cex = 0.72, col = "#C41230")
legend("topleft", c("filled: the election itself was close",
                    "open: a landslide year"),
       pch = c(19, 1), col = c("#C41230", "#999999"), bty = "n", cex = 0.76)

## ---- battle-d3
rows <- paste(sprintf('{"y":%d,"p":%.1f,"n":%d,"t":%.1f,"e":%s}',
        ncl$year, ncl$pct, ncl$close, ncl$tilt,
        ifelse(ncl$even, "true", "false")), collapse = ",")
cat(sprintf('
<div id="btl" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s],PRE=%.1f,POST=%.1f;
const W=760,H=430,M={t:22,r:24,b:40,l:56};
const box=d3.select("#btl");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([1864,2024]).range([M.l,W-M.r]);
const y=d3.scaleLinear().domain([0,72]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).tickFormat(d3.format("d")).ticks(9));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(7).tickFormat(d=>d+"%%"));
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2).attr("y",15)
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#444")
  .text("%% of states within 5 points of an even split");
svg.append("path").attr("d",d3.line().x(d=>x(d.y)).y(d=>y(d.p))(D))
  .attr("fill","none").attr("stroke","#c9c9c9").attr("stroke-width",1.8);
const ev=D.filter(d=>d.e),eA=ev.filter(d=>d.y<2000),eB=ev.filter(d=>d.y>=2000);
[[eA[0].y,eA[eA.length-1].y,PRE],[eB[0].y,eB[eB.length-1].y,POST]].forEach(s=>{
  svg.append("line").attr("x1",x(s[0])).attr("x2",x(s[1])).attr("y1",y(s[2]))
    .attr("y2",y(s[2])).attr("stroke","#C41230").attr("stroke-width",2.2)
    .attr("stroke-dasharray","7,4");});
svg.append("text").attr("x",x(1900)).attr("y",y(PRE)-7).attr("font-size","11.5px")
  .attr("fill","#C41230").text("close elections before 2000 averaged "+PRE.toFixed(1)+"%%");
svg.append("text").attr("x",x(2024)).attr("y",y(POST)-9).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("fill","#C41230").text("since 2000, "+POST.toFixed(1)+"%%");
D.filter(d=>d.y===1960||d.y===1976).forEach(d=>{
  svg.append("text").attr("x",x(d.y)).attr("y",y(d.p)-11).attr("text-anchor","middle")
    .attr("font-size","11px").attr("fill","#C41230").text(d.y);});
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.y)).attr("cy",d=>y(d.p)).attr("r",d=>d.e?5:3.4)
  .attr("fill",d=>d.e?"#C41230":"#fff").attr("stroke",d=>d.e?"#C41230":"#999")
  .attr("stroke-width",1.4)
  .on("mousemove",function(ev2,d){
    tip.style("opacity",1).html(`<b>${d.y}</b><br>${d.n} competitive states `+
      `(${d.p.toFixed(1)}%%)<br>average state was ${d.t.toFixed(1)} points off 50<br>`+
      (d.e?"a close election":"a landslide"))
      .style("left",Math.min(ev2.offsetX+14,W-250)+"px").style("top",(ev2.offsetY-46)+"px");})
  .on("mouseleave",()=>tip.style("opacity",0));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Filled points are the %d elections in which the average state sat within three
points of an even split. Hollow points are landslides, where few states are
close anywhere. Hover for the count.</p>
', rows, ev_pre, ev_post, sum(ncl$even)))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
