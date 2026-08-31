# neutral-maps-code.R -- chunk bodies for neutral-maps-brief.Rmd
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

comp   <- read.csv("data/derived/competitive_by_state.csv", stringsAsFactors = FALSE)
compN  <- read.csv("data/derived/competitive_national.csv", stringsAsFactors = FALSE)
black  <- read.csv("data/derived/black_by_state.csv", stringsAsFactors = FALSE)
blackN <- read.csv("data/derived/black_national.csv", stringsAsFactors = FALSE)
facts  <- read.csv("data/derived/facts.csv", stringsAsFactors = FALSE)
fx <- function(k) facts$value[facts$key == k]
who    <- read.csv("data/derived/who_draws.csv", stringsAsFactors = FALSE)
whoS   <- read.csv("data/derived/who_draws_states.csv", stringsAsFactors = FALSE)
WD <- function(k) who$states[who$who_draws_it == k]
fxn <- function(k) as.numeric(fx(k))

# the House chapters this primer leans on, read across rather than re-derived
hc_year <- read.csv("../house-competition/data/derived/by_year.csv", stringsAsFactors = FALSE)
hc24    <- hc_year[hc_year$year == max(hc_year$year), ]
di_fact <- read.csv("../distributions/data/derived/facts.csv", stringsAsFactors = FALSE)
dfx <- function(k) di_fact$value[di_fact$key == k]

senate <- read.csv("../election-night/data/derived/senate_2026_landscape.csv", stringsAsFactors = FALSE)

pc <- function(x, k = 1) formatC(as.numeric(x), format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",")

# one palette, used in every figure in this chapter
ENACT <- "#4E5A63"   # the map a legislature actually drew
NEUT  <- "#1C4C5C"   # the mean across 5,000 maps with no one steering them
GERRY <- "#C41230"   # the most adversarial map the simulations contain
GRY   <- "#B9BEC4"

knit_print.data.frame <- function(x, ...) {
  nm <- names(x)
  nm <- gsub("_", " ", nm)
  nm <- sub("^(.)", "\\U\\1", nm, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = nm, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- fig1-static
op <- par(mar = c(4, 4.4, 1.4, 1), mgp = c(2.6, 0.7, 0))
x <- seq_along(compN$threshold)
w <- 0.36
plot(NA, xlim = c(0.5, length(x) + 0.5), ylim = c(0, max(compN$neutral_mean) * 1.18),
     axes = FALSE, xlab = "", ylab = "Competitive districts")
axis(2, cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
axis(1, at = x, labels = paste0("±", compN$threshold * 100, "%"),
     cex.axis = 0.88, lwd = 0)
rect(x - w, 0, x, compN$enacted, col = ENACT, border = NA)
rect(x, 0, x + w, compN$neutral_mean, col = NEUT, border = NA)
text(x - w/2, compN$enacted + max(compN$neutral_mean) * 0.03, compN$enacted,
     cex = 0.78, col = ENACT)
text(x + w/2, compN$neutral_mean + max(compN$neutral_mean) * 0.03,
     round(compN$neutral_mean), cex = 0.78, col = NEUT)
legend("topleft", c("enacted maps", "neutral maps (mean)"), fill = c(ENACT, NEUT),
       border = NA, bty = "n", cex = 0.85)
mtext("Threshold for calling a district competitive", 1, line = 2.5, cex = 0.85)
par(op)

## ---- fig1-d3
FR <- paste(sprintf('{"t":"±%s%%","e":%d,"n":%.2f}',
                    compN$threshold * 100, compN$enacted, compN$neutral_mean),
            collapse = ",")
cat(paste0('
<div id="f1" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', FR, '];
const ENACT="', ENACT, '", NEUT="', NEUT, '";
const W=720,H=340,M={t:20,r:20,b:44,l:56};
const box=d3.select("#f1");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const cap=box.append("p").attr("style","font-size:0.86em;color:#444;min-height:2.2em;margin:0.3em 0 0 0");
const DEF="Enacted maps against the average of 5,000 neutral maps, at four "+
  "thresholds for calling a district competitive. <i>Hover a bar for the count.</i>";
cap.html(DEF);
const x0=d3.scaleBand().domain(D.map(d=>d.t)).range([M.l,W-M.r]).padding(0.32);
const x1=d3.scaleBand().domain(["e","n"]).range([0,x0.bandwidth()]).padding(0.12);
const y=d3.scaleLinear().domain([0,d3.max(D,d=>Math.max(d.e,d.n))*1.15]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")").call(d3.axisBottom(x0));
svg.append("g").attr("transform","translate("+M.l+",0)").call(d3.axisLeft(y).ticks(6));
svg.append("text").attr("transform","translate(14,"+((M.t+H-M.b)/2)+") rotate(-90)")
  .attr("text-anchor","middle").attr("font-size","12px").attr("fill","#555")
  .text("Competitive districts");
const g=svg.selectAll("g.grp").data(D).join("g")
  .attr("transform",d=>"translate("+x0(d.t)+",0)");
function bar(sel,key,col,label){
  sel.append("rect").attr("x",x1(key)).attr("width",x1.bandwidth())
    .attr("y",d=>y(d[key])).attr("height",d=>y(0)-y(d[key])).attr("fill",col)
    .on("mouseenter",function(e,d){
      d3.select(this).attr("fill-opacity",0.75);
      cap.html("<b>"+d.t+"</b>, "+label+": <b>"+
        (key==="n"?d.n.toFixed(1):d.e)+"</b> competitive districts.");})
    .on("mouseleave",function(){d3.select(this).attr("fill-opacity",1);cap.html(DEF);});
}
bar(g,"e",ENACT,"enacted map"); bar(g,"n",NEUT,"mean of 5,000 neutral maps");
svg.append("rect").attr("x",M.l).attr("y",6).attr("width",10).attr("height",10).attr("fill",ENACT);
svg.append("text").attr("x",M.l+14).attr("y",15).attr("font-size","11.5px").attr("fill","#555").text("enacted");
svg.append("rect").attr("x",M.l+70).attr("y",6).attr("width",10).attr("height",10).attr("fill",NEUT);
svg.append("text").attr("x",M.l+84).attr("y",15).attr("font-size","11.5px").attr("fill","#555").text("neutral mean");
})();
</script>'))

## ---- fig2-static
c5 <- comp[comp$threshold == 0.05, ]
c5$gap <- c5$neutral_mean - c5$enacted
top <- head(c5[order(-c5$gap), ], 10)
top <- top[order(top$gap), ]
op <- par(mar = c(4, 4.2, 1, 1), mgp = c(2.6, 0.7, 0))
yy <- seq_len(nrow(top))
plot(NA, xlim = c(0, max(top$neutral_mean) * 1.12), ylim = range(yy) + c(-0.6, 0.6),
     axes = FALSE, xlab = "Competitive districts (±5%)", ylab = "")
axis(1, cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
segments(top$enacted, yy, top$neutral_mean, yy, col = "#999999", lwd = 1.6)
points(top$enacted, yy, pch = 21, bg = "#FFFFFF", col = ENACT, cex = 1.25)
points(top$neutral_mean, yy, pch = 19, col = NEUT, cex = 1.25)
text(-0.4, yy, top$state, xpd = NA, adj = 1, cex = 0.86)
legend("bottomright", c("enacted", "neutral mean"), pch = c(21, 19),
       col = c(ENACT, NEUT), pt.bg = c("#FFFFFF", NA), bty = "n", cex = 0.85)
par(op)

## ---- fig2-d3
c5 <- comp[comp$threshold == 0.05, ]
c5$gap <- c5$neutral_mean - c5$enacted
top <- head(c5[order(-c5$gap), ], 10)
top <- top[order(top$gap), ]
TR <- paste(sprintf('{"s":"%s","e":%d,"n":%.2f}', top$state, top$enacted, top$neutral_mean),
            collapse = ",")
cat(paste0('
<div id="f2" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', TR, '];
const ENACT="', ENACT, '", NEUT="', NEUT, '";
const W=680,H=420,M={t:16,r:24,b:38,l:56};
const box=d3.select("#f2");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const cap=box.append("p").attr("style","font-size:0.86em;color:#444;min-height:2.2em;margin:0.3em 0 0 0");
const DEF="The ten states with the largest gap between the enacted map and the "+
  "neutral average, at the 5-point threshold. <i>Hover a row for the counts.</i>";
cap.html(DEF);
const x=d3.scaleLinear().domain([0,d3.max(D,d=>d.n)*1.12]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.s)).range([H-M.b,M.t]).padding(0.34);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")").call(d3.axisBottom(x).ticks(6));
svg.append("text").attr("x",(M.l+W-M.r)/2).attr("y",H-6).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#555").text("Competitive districts (±5%)");
const rg=svg.selectAll("g.r").data(D).join("g")
  .on("mouseenter",function(e,d){
    d3.select(this).selectAll("circle").attr("r",6.5);
    cap.html("<b>"+d.s+"</b>: "+d.e+" competitive districts enacted, "+
      d.n.toFixed(1)+" expected under neutral maps.");})
  .on("mouseleave",function(){d3.select(this).selectAll("circle").attr("r",5);cap.html(DEF);});
rg.append("text").attr("x",M.l-10).attr("y",d=>y(d.s)+y.bandwidth()/2+4)
  .attr("text-anchor","end").attr("font-size","12px").attr("fill","#4E5A63").text(d=>d.s);
rg.append("line").attr("x1",d=>x(d.e)).attr("x2",d=>x(d.n))
  .attr("y1",d=>y(d.s)+y.bandwidth()/2).attr("y2",d=>y(d.s)+y.bandwidth()/2)
  .attr("stroke","#999").attr("stroke-width",1.6);
rg.append("circle").attr("cx",d=>x(d.e)).attr("cy",d=>y(d.s)+y.bandwidth()/2)
  .attr("r",5).attr("fill","#fff").attr("stroke",ENACT).attr("stroke-width",1.8);
rg.append("circle").attr("cx",d=>x(d.n)).attr("cy",d=>y(d.s)+y.bandwidth()/2)
  .attr("r",5).attr("fill",NEUT);
})();
</script>'))

## ---- fig3-static
b <- black[black$actual_black > 0 | black$neutral_black_mean > 0.01, ]
b <- b[order(-b$actual_black), ]
op <- par(mar = c(4, 4.2, 1.4, 1), mgp = c(2.6, 0.7, 0))
x <- seq_along(b$state); w <- 0.27
plot(NA, xlim = c(0.5, length(x) + 0.5), ylim = c(0, max(b$actual_black) * 1.2),
     axes = FALSE, xlab = "", ylab = "Black-plurality districts")
axis(2, cex.axis = 0.82, lwd = 0, lwd.ticks = 1, at = 0:max(b$actual_black))
axis(1, at = x, labels = b$state, cex.axis = 0.86, lwd = 0)
rect(x - 1.4*w, 0, x - 0.4*w, b$actual_black, col = ENACT, border = NA)
rect(x - 0.4*w, 0, x + 0.4*w, b$neutral_black_mean, col = NEUT, border = NA)
rect(x + 0.4*w, 0, x + 1.4*w, b$gerry_black, col = GERRY, border = NA)
legend("topright", c("enacted", "neutral mean", "maximized gerrymander"),
       fill = c(ENACT, NEUT, GERRY), border = NA, bty = "n", cex = 0.82)
par(op)

## ---- fig3-d3
b <- black[black$actual_black > 0 | black$neutral_black_mean > 0.01, ]
b <- b[order(-b$actual_black), ]
BR <- paste(sprintf('{"s":"%s","e":%d,"n":%.3f,"g":%d}',
                    b$state, b$actual_black, b$neutral_black_mean, b$gerry_black),
            collapse = ",")
cat(paste0('
<div id="f3" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', BR, '];
const ENACT="', ENACT, '", NEUT="', NEUT, '", GERRY="', GERRY, '";
const W=740,H=380,M={t:20,r:20,b:40,l:44};
const box=d3.select("#f3");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const cap=box.append("p").attr("style","font-size:0.86em;color:#444;min-height:2.4em;margin:0.3em 0 0 0");
const DEF="Black-plurality districts, three ways, in the 20 states Republicans "+
  "controlled after the 2020 census. <i>Hover a bar.</i>";
cap.html(DEF);
const x0=d3.scaleBand().domain(D.map(d=>d.s)).range([M.l,W-M.r]).padding(0.3);
const x1=d3.scaleBand().domain(["e","n","g"]).range([0,x0.bandwidth()]).padding(0.08);
const y=d3.scaleLinear().domain([0,d3.max(D,d=>d.e)*1.2]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")").call(d3.axisBottom(x0));
svg.append("g").attr("transform","translate("+M.l+",0)").call(d3.axisLeft(y).ticks(5).tickFormat(d3.format("d")));
const g=svg.selectAll("g.grp").data(D).join("g").attr("transform",d=>"translate("+x0(d.s)+",0)");
function bar(key,col,label){
  g.append("rect").attr("x",x1(key)).attr("width",x1.bandwidth())
    .attr("y",d=>y(d[key])).attr("height",d=>y(0)-y(d[key])).attr("fill",col)
    .on("mouseenter",function(e,d){
      d3.select(this).attr("fill-opacity",0.75);
      cap.html("<b>"+d.s+"</b>, "+label+": <b>"+
        (key==="n"?d.n.toFixed(2):d[key])+"</b> Black-plurality district(s).");})
    .on("mouseleave",function(){d3.select(this).attr("fill-opacity",1);cap.html(DEF);});
}
bar("e",ENACT,"enacted"); bar("n",NEUT,"neutral mean"); bar("g",GERRY,"maximized gerrymander");
const lg=[["enacted",ENACT],["neutral mean",NEUT],["maximized gerrymander",GERRY]];
lg.forEach((d,i)=>{
  svg.append("rect").attr("x",M.l+i*150).attr("y",4).attr("width",10).attr("height",10).attr("fill",d[1]);
  svg.append("text").attr("x",M.l+i*150+14).attr("y",13).attr("font-size","11px").attr("fill","#555").text(d[0]);
});
})();
</script>'))

## ---- whotab
data.frame(Who_draws_the_congressional_map = who$who_draws_it,
           States = who$states)

## ---- sen-facts
s26 <- senate[order(abs(senate$pres24_margin)), ]
CLOSE <- sum(abs(s26$pres24_margin) <= 5)

## ---- fig4-static
s <- senate
s$m <- ifelse(s$pres24_winner == "Trump", s$abs_margin, -s$abs_margin)
s <- s[order(s$m), ]
op <- par(mar = c(4, 6.4, 1, 1), mgp = c(2.6, 0.7, 0))
yy <- seq_len(nrow(s))
cl <- ifelse(s$m < 0, NEUT, GERRY)
plot(NA, xlim = c(-40, 40), ylim = range(yy) + c(-0.6, 0.6), axes = FALSE,
     xlab = "", ylab = "")
axis(1, at = seq(-40, 40, 20),
     labels = c("D+40", "D+20", "even", "R+20", "R+40"), cex.axis = 0.8, lwd = 0)
mtext("2024 presidential margin", 1, line = 2.4, cex = 0.85)
abline(v = 0, col = "#999999", lty = 3)
points(s$m, yy, pch = 19, col = cl, cex = 1.05)
text(-42, yy, s$state, xpd = NA, adj = 1, cex = 0.68)
par(op)

## ---- fig4-d3
s <- senate
s$m <- ifelse(s$pres24_winner == "Trump", s$abs_margin, -s$abs_margin)
s <- s[order(s$m), ]
SR <- paste(sprintf('{"s":"%s","m":%.2f,"w":"%s"}', s$state, s$m, s$pres24_winner),
            collapse = ",")
cat(paste0('
<div id="f4" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', SR, '];
const NEUT="', NEUT, '", GERRY="', GERRY, '";
const W=700,H=560,M={t:16,r:20,b:40,l:52};
const box=d3.select("#f4");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const cap=box.append("p").attr("style","font-size:0.86em;color:#444;min-height:2.2em;margin:0.3em 0 0 0");
const DEF="States with a U.S. Senate seat up in 2026, by their 2024 presidential "+
  "margin \\u2014 the only \\u201cdistrict\\u201d a senator answers to. "+
  "<i>Hover a row.</i>";
cap.html(DEF);
const x=d3.scaleLinear().domain([-40,40]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.s)).range([M.t,H-M.b]).padding(0.3);
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x).tickValues([-40,-20,0,20,40])
    .tickFormat(v=>v===0?"even":(v<0?"D+"+(-v):"R+"+v)));
svg.append("line").attr("x1",x(0)).attr("x2",x(0)).attr("y1",M.t).attr("y2",H-M.b)
  .attr("stroke","#999").attr("stroke-dasharray","2 3");
const rg=svg.selectAll("g.r").data(D).join("g")
  .on("mouseenter",function(e,d){
    d3.select(this).select("circle").attr("r",6);
    cap.html("<b>"+d.s+"</b>: 2024 margin "+
      (d.m<0?("D+"+(-d.m).toFixed(1)):("R+"+d.m.toFixed(1)))+".");})
  .on("mouseleave",function(){d3.select(this).select("circle").attr("r",4.4);cap.html(DEF);});
rg.append("text").attr("x",M.l-8).attr("y",d=>y(d.s)+y.bandwidth()/2+3.5)
  .attr("text-anchor","end").attr("font-size","10.5px").attr("fill","#4E5A63").text(d=>d.s);
rg.append("circle").attr("cx",d=>x(d.m)).attr("cy",d=>y(d.s)+y.bandwidth()/2)
  .attr("r",4.4).attr("fill",d=>d.m<0?NEUT:GERRY);
})();
</script>'))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
