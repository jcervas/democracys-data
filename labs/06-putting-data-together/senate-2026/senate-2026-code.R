# senate-2026-code.R -- chunk bodies for senate-2026-brief.Rmd
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

RACES <- read.csv("data/derived/races.csv",        stringsAsFactors = FALSE)
RAT   <- read.csv("data/derived/ratings_long.csv", stringsAsFactors = FALSE)
MATH  <- read.csv("data/derived/seat_math.csv",    stringsAsFactors = FALSE)
CLASS <- read.csv("data/derived/class_ratings.csv", stringsAsFactors = FALSE)
FACTS <- read.csv("data/derived/facts.csv",        stringsAsFactors = FALSE)
CK    <- read.csv("data/derived/checks.csv",       stringsAsFactors = FALSE)
FCS   <- read.csv("data/derived/forecasters.csv",   stringsAsFactors = FALSE)

fx <- function(k) {
  v <- FACTS$value[FACTS$key == k]
  if (length(v) != 1L) stop("facts.csv has no single value for '", k, "'")
  v
}
fxn <- function(k) as.numeric(fx(k))
mq  <- function(k) MATH$value[MATH$quantity == k]
pc  <- function(x, k = 1) formatC(as.numeric(x), format = "f", digits = k)

HAS_CLASS <- nrow(CLASS) > 0

# The seven categories, in the order the rating code numbers them. 1 is Safe D
# and 7 is Safe R, so a code reads left to right from one party to the other.
CATS   <- c("Safe D", "Likely D", "Lean D", "Toss-up", "Lean R", "Likely R", "Safe R")
SCORES <- c(3, 2, 1, 0, -1, -2, -3)
# The seven-class red-blue ramp the book uses everywhere a party margin is
# painted. Pinned in both themes: these are party colours, not page ink.
FILLS  <- c("#2166AC", "#67A9CF", "#D1E5F0", "#E6E6E6",
            "#FDDBC7", "#EF8A62", "#B2182B")
# Text drawn ON those fills. #2b2b2b rather than #333 on purpose: brief.css
# remaps a list of dark hexes to page ink under prefers-color-scheme: dark,
# which would turn a label on a pale slab nearly white. See the note in the
# figures below. Text that sits on the PAGE rather than on a mark uses #333333
# and #666666, which the same rule lifts, and #707070 labels the pale slab of a
# state with no race in it.
INK_ON  <- c("#ffffff", "#2b2b2b", "#2b2b2b", "#2b2b2b",
             "#2b2b2b", "#2b2b2b", "#ffffff")

# A continuous version of the same ramp, for the maps that paint an average
# rather than a category.
ramp <- colorRampPalette(c("#B2182B", "#EF8A62", "#FDDBC7", "#F2F2F2",
                           "#D1E5F0", "#67A9CF", "#2166AC"))(201)
score_fill <- function(s) ramp[round((pmax(pmin(s, 3), -3) + 3) / 6 * 200) + 1]

# The equal-weight state grid: one square per state, every state the same size.
# It is the right map for a chamber where every state elects exactly two
# senators regardless of how many people live in it.
GJ <- jsonlite::fromJSON("../../_lib/geo/us-grid.geojson", simplifyVector = FALSE)
GRID <- do.call(rbind, lapply(GJ$features, function(f) data.frame(
  st = f$properties$st, col = f$properties$col, row = f$properties$row)))
NCOL <- max(GRID$col); NROW <- max(GRID$row)
GRID$up <- GRID$st %in% RACES$state
G <- merge(GRID, RACES, by.x = "st", by.y = "state", all.x = TRUE)
G <- G[order(G$row, G$col), ]

CODE_ORDER <- RACES$state          # already alphabetical by abbreviation
D_HOLD <- mq("Democratic seats not on the ballot")
D_NEED <- mq("of the 35, Democrats must win")

# Draw the grid once, in base R, colouring by whatever the caller hands over.
# Used by every static twin in this chapter, so all three read the same.
grid_panel <- function(fill, main, sub = NULL, labels = NULL, cex_lab = 0.62) {
  op <- par(mar = c(if (is.null(sub)) 0.6 else 2.2, 0.4, 2.2, 0.4))
  on.exit(par(op), add = TRUE)
  plot(NULL, xlim = c(0.4, NCOL + 0.6), ylim = c(NROW + 0.6, 0.4),
       asp = 1, axes = FALSE, xlab = "", ylab = "", main = main, cex.main = 0.95)
  for (i in seq_len(nrow(G))) {
    x <- G$col[i]; y <- G$row[i]
    rect(x - 0.46, y - 0.46, x + 0.46, y + 0.46,
         col = fill[i], border = "#FFFFFF", lwd = 1.4)
    lab <- if (is.null(labels)) G$st[i] else labels[i]
    text(x, y, lab, cex = cex_lab,
         col = if (isTRUE(G$up[i])) "#2b2b2b" else "#707070")
  }
  if (!is.null(sub)) mtext(sub, side = 1, line = 0.6, cex = 0.68, col = "#555555")
  invisible(NULL)
}

# The seven-box key that goes under a static map.
cat_key <- function(note = NULL) {
  op <- par(mar = c(0.2, 0.4, 0.2, 0.4))
  on.exit(par(op), add = TRUE)
  plot(NULL, xlim = c(0, 7), ylim = c(0, 1.6), axes = FALSE, xlab = "", ylab = "")
  for (i in 1:7) {
    rect(i - 0.94, 0.75, i - 0.06, 1.35, col = FILLS[i], border = "#FFFFFF")
    text(i - 0.5, 1.05, CATS[i], cex = 0.6, col = INK_ON[i])
  }
  if (!is.null(note)) text(3.5, 0.35, note, cex = 0.66, col = "#555555")
  invisible(NULL)
}

# A state's forecaster ratings as one line of text, for the tooltips.
rat_line <- function(st) {
  r <- RAT[RAT$state == st, ]
  paste(sprintf("%s: %s", r$forecaster, r$rating), collapse = "; ")
}

# Render every data.frame in this document as a TABLE, not as console output.
# Without this the chapter's output depends on WHICH OTHER CHAPTERS rendered
# before it: render-brief.R builds every brief in one R session, and a sibling
# chapter's identical registration lands in knitr's namespace and stays there.
# This chapter printed raw `##` output when built alone and a table when built
# with the corpus. Registering it here makes the page the same either way.
knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- seat-math
knitr::kable(MATH, col.names = c("", "seats"), align = c("l", "r"))

## ---- scale-table
knitr::kable(data.frame(
  Category = CATS,
  Score    = SCORES,
  Meaning  = c("the other side is not going to win this",
               "a significant advantage, but not a certainty",
               "a slight advantage",
               "no advantage either way",
               "a slight advantage",
               "a significant advantage, but not a certainty",
               "the other side is not going to win this")),
  align = c("l", "r", "l"))

## ---- no-dem
nd <- RACES[is.na(RACES$dem_cand), c("state_name", "candidates")]
knitr::kable(nd, col.names = c("", "on the ballot"), row.names = FALSE)

## ---- rate-static
layout(matrix(1:2, ncol = 1), heights = c(1, 0.24))
grid_panel(ifelse(G$up, FILLS[match(G$base_rating, CATS)], "#F4F5F6"),
           "Where each of these seats stood at its last election",
           sub = paste("States with no Senate race in 2026 are left blank.",
                       "Rate the 35 in the table below."))
cat_key("your own ratings go in the table that follows")
layout(1)

## ---- rate-d3
rows <- paste(sprintf(
 '{"st":"%s","c":%d,"r":%d,"up":%s,"nm":%s,"sen":%s,"pty":"%s","sp":%s,"stat":%s,"dem":%s,"rep":%s,"base":%d,"fc":%.3f,"last":%s}',
  G$st, G$col, G$row, tolower(as.character(G$up)),
  ifelse(is.na(G$state_name), '""', paste0('"', G$state_name, '"')),
  ifelse(is.na(G$senator),    '""', paste0('"', G$senator, '"')),
  ifelse(is.na(G$seat_party), "",  G$seat_party),
  ifelse(is.na(G$special), "false", tolower(as.character(G$special))),
  ifelse(is.na(G$status), '""', paste0('"', G$status, '"')),
  ifelse(is.na(G$dem_cand), '"no Democrat filed"', paste0('"', G$dem_cand, '"')),
  ifelse(is.na(G$rep_cand), '"no Republican filed"', paste0('"', G$rep_cand, '"')),
  ifelse(is.na(G$base_score), 9, match(G$base_rating, CATS)),
  ifelse(is.na(G$fc_mean), 0, G$fc_mean),
  ifelse(is.na(G$last_share), '""',
         paste0('"', pc(G$last_share, 1), '% ', G$last_party, ' in ', G$last_year, '"'))),
  collapse = ",")

cat(sprintf('
<script src="../../_lib/d3.v7.min.js"></script>
<div id="rater" style="margin:1em 0"></div>
<script>
window.DD_SYNC=window.DD_SYNC||{fns:[],select(id){this.fns.forEach(f=>f(id));},
  subscribe(fn){this.fns.push(fn);}};
window.__SEN26=window.__SEN26||{};
(function(){
const D=[%s];
const CATS=%s, FILLS=%s, INK=%s, SCORES=[3,2,1,0,-1,-2,-3];
const ORDER=%s, DHOLD=%d, DNEED=%d;
window.__SEN26.D=D; window.__SEN26.CATS=CATS; window.__SEN26.FILLS=FILLS;
window.__SEN26.ORDER=ORDER; window.__SEN26.DHOLD=DHOLD;

// state -> 1..7, 4 (Toss-up) until the reader says otherwise
const KEY="dd-senate-2026-ratings";
let pick={}; try{ pick=JSON.parse(localStorage.getItem(KEY)||"{}"); }catch(e){ pick={}; }
ORDER.forEach(s=>{ if(!(pick[s]>=1&&pick[s]<=7)) pick[s]=4; });
let brush=4, sel=null;

const NC=%d, NR=%d, W=760, H=%d, CW=Math.min(W/NC, 84);
const box=d3.select("#rater");

// ---- the running total, which is the point of the whole figure ----------
const head=box.append("div").attr("style","margin-bottom:8px");
const tally=head.append("div").attr("style","font-size:15px;line-height:1.5");
const barw=520, barh=16;
const barsvg=head.append("svg").attr("width",barw).attr("height",barh+18)
  .attr("style","max-width:100%%;display:block;margin-top:4px");
barsvg.append("rect").attr("x",0).attr("y",0).attr("width",barw).attr("height",barh)
  .attr("fill","#E6E6E6");
const dbar=barsvg.append("rect").attr("x",0).attr("y",0).attr("height",barh)
  .attr("fill","#2166AC");
barsvg.append("line").attr("x1",barw*0.51).attr("x2",barw*0.51)
  .attr("y1",-2).attr("y2",barh+2).attr("stroke","#2b2b2b").attr("stroke-width",2);
barsvg.append("text").attr("x",barw*0.51+5).attr("y",barh+14)
  .attr("font-size","11px").attr("fill","#555").text("51 = majority");

// ---- the brush: pick a rating, then click states ------------------------
const pal=box.append("div").attr("style","margin:8px 0 6px 0;display:flex;flex-wrap:wrap;gap:4px");
const btns=CATS.map((c,i)=>pal.append("button").text(c)
  .attr("style",`padding:4px 10px;font:inherit;font-size:12px;cursor:pointer;`+
    `background:${FILLS[i]};color:${INK[i]};border:2px solid transparent;border-radius:3px`)
  .on("click",()=>{ brush=i+1; paintBtns(); }));
function paintBtns(){ btns.forEach((b,i)=>
  b.attr("style",`padding:4px 10px;font:inherit;font-size:12px;cursor:pointer;`+
    `background:${FILLS[i]};color:${INK[i]};border-radius:3px;border:2px solid `+
    (brush===i+1?"#2b2b2b":"transparent"))); }

const tools=box.append("div").attr("style","margin-bottom:8px;display:flex;flex-wrap:wrap;gap:6px");
function tool(label,fn){ tools.append("button").text(label)
  .attr("style","padding:3px 9px;font:inherit;font-size:12px;cursor:pointer")
  .on("click",fn); }
tool("all toss-up",()=>{ ORDER.forEach(s=>pick[s]=4); draw(); });
tool("fill from last election",()=>{ D.forEach(d=>{ if(d.up&&d.base<9) pick[d.st]=d.base; }); draw(); });
tool("fill from the forecasters",()=>{ D.forEach(d=>{ if(!d.up) return;
  const s=Math.max(-3,Math.min(3,Math.round(d.fc))); pick[d.st]=4-s; }); draw(); });

// ---- the map -------------------------------------------------------------
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const g=svg.append("g");
const tip=box.append("div").attr("style",
 "position:fixed;pointer-events:none;background:#1b1b1b;color:#fff;padding:7px 10px;"+
 "border-radius:4px;font-size:12px;max-width:280px;opacity:0;z-index:9;line-height:1.35");
const x0=(W-CW*NC)/2, y0=6;

function fillOf(d){ return d.up ? FILLS[pick[d.st]-1] : "#F4F5F6"; }
function inkOf(d){ return d.up ? INK[pick[d.st]-1] : "#707070"; }

g.selectAll("rect").data(D).join("rect")
  .attr("x",d=>x0+(d.c-1)*CW+CW*0.04).attr("y",d=>y0+(d.r-1)*CW+CW*0.04)
  .attr("width",CW*0.92).attr("height",CW*0.92)
  .attr("stroke","#FFFFFF").attr("stroke-width",1.6)
  .attr("cursor",d=>d.up?"pointer":"default")
  .on("click",function(ev,d){ if(!d.up) return;
    pick[d.st]=brush; sel=d.st; window.DD_SYNC.select(d.st); draw(); })
  .on("mousemove",function(ev,d){
    if(!d.up){ tip.style("opacity",0); return; }
    tip.style("opacity",1).html(
      `<b>${d.nm}</b>${d.sp?" (special)":""}<br>`+
      `held by ${d.pty==="D"?"a Democrat":"a Republican"}: ${d.sen} (${d.stat})<br>`+
      `${d.dem} vs ${d.rep}<br>`+
      (d.last?`last election: ${d.last}<br>`:"")+
      `<span style="opacity:.8">you: ${CATS[pick[d.st]-1]}</span>`)
      .style("left",Math.min(ev.clientX+14,window.innerWidth-300)+"px")
      .style("top",(ev.clientY+14)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0));
g.selectAll("text").data(D).join("text")
  .attr("x",d=>x0+(d.c-1)*CW+CW/2).attr("y",d=>y0+(d.r-1)*CW+CW/2+4)
  .attr("text-anchor","middle").attr("font-size",Math.min(CW*0.26,13)+"px")
  .attr("pointer-events","none").text(d=>d.st);

// ---- the code, and where it goes ----------------------------------------
const out=box.append("div").attr("style","margin-top:10px;font-size:13px");
out.append("div").attr("style","margin-bottom:4px").html(
  "<b>Your code.</b> Copy this and paste it into the class form. "+
  "One digit per race, alphabetical by state: 1 is Safe D, 7 is Safe R.");
const codebox=out.append("input").attr("readonly",true)
  .attr("style","width:100%%;max-width:520px;font-family:ui-monospace,SFMono-Regular,Menlo,monospace;"+
    "font-size:13px;padding:5px 7px");
out.append("button").text("copy").attr("style","margin-left:6px;padding:4px 10px;font:inherit;font-size:12px;cursor:pointer")
  .on("click",function(){ const c=ORDER.map(s=>pick[s]).join("");
    if(navigator.clipboard) navigator.clipboard.writeText(c);
    else { codebox.node().select(); document.execCommand("copy"); }
    d3.select(this).text("copied"); setTimeout(()=>d3.select(this).text("copy"),1200); });

function draw(){
  g.selectAll("rect").attr("fill",fillOf)
    .attr("stroke",d=>d.st===sel?"#2b2b2b":"#FFFFFF")
    .attr("stroke-width",d=>d.st===sel?2.6:1.6);
  g.selectAll("text").attr("fill",inkOf);
  const sc=ORDER.map(s=>SCORES[pick[s]-1]);
  const d=sc.filter(v=>v>0).length, r=sc.filter(v=>v<0).length, t=sc.filter(v=>v===0).length;
  const lo=DHOLD+d, hi=DHOLD+d+t;
  tally.html(`<b>You have called ${d} of ${ORDER.length} races for the Democrats`+
    (t?`, and left ${t} as toss-ups`:"")+`.</b><br>`+
    `That is ${lo===hi?lo:lo+" to "+hi} Democratic seats out of 100 `+
    `(${DHOLD} not on the ballot, plus ${d}${t?" to "+(d+t):""}). `+
    (hi>=51 ? (lo>=51?"<span style=\'color:#2166AC\'>A Democratic majority.</span>"
                    :"<span>A majority only if the toss-ups break their way.</span>")
            : "<span style=\'color:#B2182B\'>Short of 51.</span>"));
  dbar.attr("width",Math.max(0,barw*lo/100));
  codebox.property("value",ORDER.map(s=>pick[s]).join(""));
  try{ localStorage.setItem(KEY,JSON.stringify(pick)); }catch(e){}
  window.__SEN26.pick=pick;
}
window.DD_SYNC.subscribe(function(id){ if(id===sel) return; sel=id; draw(); });
paintBtns(); draw();
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.4em">
Click a rating, then click states. Hover a state for the incumbent, the
candidates and its last result. Your ratings are kept in this browser, so the
page can be closed and reopened. Sixteen states have no Senate race in 2026 and
are left blank.</p>
', rows,
   jsonlite::toJSON(CATS), jsonlite::toJSON(FILLS), jsonlite::toJSON(INK_ON),
   jsonlite::toJSON(CODE_ORDER), D_HOLD, D_NEED,
   NCOL, NROW, round(6 + NROW * min(760 / NCOL, 84) + 6)))

## ---- worksheet
ws <- data.frame(
  code_pos   = RACES$state,
  state      = RACES$state_name,
  seat_party = ifelse(RACES$seat_party == "D", "Democratic", "Republican"),
  # one column, because the two facts are only useful together: an incumbent
  # who is not running is a different race from one who is.
  incumbent  = paste0(RACES$senator, " (", RACES$status, ")"),
  dem_cand   = ifelse(is.na(RACES$dem_cand), "—", RACES$dem_cand),
  rep_cand   = ifelse(is.na(RACES$rep_cand), "—", RACES$rep_cand),
  rating     = "")
knitr::kable(ws, row.names = FALSE, align = c("l","l","l","l","l","l","l"),
  col.names = c("", "state", "seat held by", "sitting senator",
                "Democrat", "Republican", "your rating"))

## ---- compare-static
layout(matrix(1:3, ncol = 1), heights = c(1, 1, 0.3))
grid_panel(ifelse(G$up, score_fill(G$fc_mean), "#F4F5F6"),
           paste(fx("n_forecasters"), "forecasters, averaged"))
grid_panel(ifelse(G$up, FILLS[match(G$base_rating, CATS)], "#F4F5F6"),
           "The same seats, at their last election")
cat_key("blue is Democratic, red Republican; pale is close")
layout(1)

## ---- compare-d3
crows <- paste(sprintf(
 '{"st":"%s","c":%d,"r":%d,"up":%s,"nm":%s,"fc":%.3f,"sd":%.3f,"base":%.0f,"cl":%s,"cln":%s,"rl":%s}',
  G$st, G$col, G$row, tolower(as.character(G$up)),
  ifelse(is.na(G$state_name), '""', paste0('"', G$state_name, '"')),
  ifelse(is.na(G$fc_mean), 0, G$fc_mean),
  ifelse(is.na(G$fc_sd), 0, G$fc_sd),
  ifelse(is.na(G$base_score), 0, G$base_score),
  vapply(G$st, function(s) if (HAS_CLASS && s %in% CLASS$state)
           sprintf("%.3f", CLASS$cl_mean[CLASS$state == s]) else "null", ""),
  vapply(G$st, function(s) if (HAS_CLASS && s %in% CLASS$state)
           sprintf("%d", CLASS$n[CLASS$state == s]) else "0", ""),
  ifelse(G$up, paste0('"', vapply(G$st, function(s)
           if (s %in% RACES$state) rat_line(s) else "", ""), '"'), '""')),
  collapse = ",")

cat(sprintf('
<div id="cmp" style="margin:1em 0"></div>
<script>
window.DD_SYNC=window.DD_SYNC||{fns:[],select(id){this.fns.forEach(f=>f(id));},
  subscribe(fn){this.fns.push(fn);}};
(function(){
const D=[%s], NC=%d, NR=%d, W=760, CW=Math.min(W/NC,84), H=%d;
const HASCLASS=%s, ORDER=%s;
let mode=HASCLASS?"class":"base", sel=null;
const box=d3.select("#cmp");
const bar=box.append("div").attr("style","margin-bottom:6px;display:flex;flex-wrap:wrap;gap:6px");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const g=svg.append("g"), x0=(W-CW*NC)/2, y0=6;
const tip=box.append("div").attr("style",
 "position:fixed;pointer-events:none;background:#1b1b1b;color:#fff;padding:7px 10px;"+
 "border-radius:4px;font-size:12px;max-width:320px;opacity:0;z-index:9;line-height:1.35");
const cap=box.append("div").attr("style","font-size:12.5px;color:#555;margin-top:6px;min-height:2.2em");

// the same ramp the static twin uses, as a function
function fill(v){ if(v===null||v===undefined) return "#F4F5F6";
  const t=(Math.max(-3,Math.min(3,v))+3)/6;
  return d3.interpolateRgbBasis(["#B2182B","#EF8A62","#FDDBC7","#F2F2F2",
                                 "#D1E5F0","#67A9CF","#2166AC"])(t); }
function val(d){
  if(!d.up) return null;
  if(mode==="fc") return d.fc;
  if(mode==="base") return d.base;
  if(mode==="class") return d.cl;
  if(mode==="you"){ const p=(window.__SEN26&&window.__SEN26.pick)||{};
    return p[d.st]?4-p[d.st]:null; }
  return null;
}
const MODES=[["you","your map"],["fc","the %d forecasters, averaged"],
             ["base","last time this seat was up"],["class","this class, averaged"]];
const LABEL={you:"Your map",fc:"The forecasters\u2019 average",
             base:"The last election",class:"The class average"};
const btn={};
MODES.forEach(([k,lab])=>{ btn[k]=bar.append("button").text(lab)
  .attr("style","padding:3px 9px;font:inherit;font-size:12px;cursor:pointer")
  .on("click",()=>{ mode=k; draw(); }); });

g.selectAll("rect").data(D).join("rect")
  .attr("x",d=>x0+(d.c-1)*CW+CW*0.04).attr("y",d=>y0+(d.r-1)*CW+CW*0.04)
  .attr("width",CW*0.92).attr("height",CW*0.92)
  .attr("stroke","#FFFFFF").attr("stroke-width",1.6)
  .on("mousemove",function(ev,d){ if(!d.up){tip.style("opacity",0);return;}
    const v=val(d);
    tip.style("opacity",1).html(`<b>${d.nm}</b><br>`+
      (v===null?"no value on this layer":`score ${v>0?"+":""}${v.toFixed(2)}`)+
      (mode==="fc"?`<br>spread ${d.sd.toFixed(2)}<br><span style="opacity:.75">${d.rl}</span>`:"")+
      (mode==="class"&&d.cln?`<br>${d.cln} classmates`:""))
      .style("left",Math.min(ev.clientX+14,window.innerWidth-340)+"px")
      .style("top",(ev.clientY+14)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0))
  .on("click",(ev,d)=>{ if(d.up){ sel=d.st; window.DD_SYNC.select(d.st); draw(); } });
g.selectAll("text").data(D).join("text")
  .attr("x",d=>x0+(d.c-1)*CW+CW/2).attr("y",d=>y0+(d.r-1)*CW+CW/2+4)
  .attr("text-anchor","middle").attr("font-size",Math.min(CW*0.26,13)+"px")
  .attr("pointer-events","none").text(d=>d.st);

// ---- pasting codes in, so the class map works without a rebuild ----------
const pastewrap=box.append("details").attr("style","margin-top:10px;font-size:13px");
pastewrap.append("summary").attr("style","cursor:pointer").text("paste the class codes");
const ta=pastewrap.append("textarea").attr("rows",4)
  .attr("placeholder","one 35-digit code per line")
  .attr("style","width:100%%;font-family:ui-monospace,Menlo,monospace;font-size:12px;margin-top:6px");
pastewrap.append("button").text("average them").attr("style","margin-top:5px;padding:3px 9px;font:inherit;font-size:12px;cursor:pointer")
  .on("click",()=>{
    const codes=(ta.property("value")||"").split(/[^1-7]+/).filter(c=>c.length===ORDER.length);
    if(!codes.length){ cap.text("No usable codes. Each has to be "+ORDER.length+" digits, 1 to 7."); return; }
    const sums={}; ORDER.forEach(s=>sums[s]=0);
    codes.forEach(c=>ORDER.forEach((s,i)=>sums[s]+=4-(+c[i])));
    D.forEach(d=>{ if(d.up){ d.cl=sums[d.st]/codes.length; d.cln=codes.length; } });
    mode="class"; draw(); });

function draw(){
  g.selectAll("rect").attr("fill",d=>fill(val(d)))
    .attr("stroke",d=>d.st===sel?"#2b2b2b":"#FFFFFF")
    .attr("stroke-width",d=>d.st===sel?2.6:1.6);
  g.selectAll("text").attr("fill",d=>{ const v=val(d);
    return (v===null||!d.up)?"#707070":(Math.abs(v)>2.2?"#ffffff":"#2b2b2b"); });
  MODES.forEach(([k])=>btn[k].style("outline",mode===k?"2px solid #2b2b2b":"none"));
  const live=D.filter(d=>d.up&&val(d)!==null);
  const dd=live.filter(d=>val(d)>0).length, tt=live.filter(d=>val(d)===0).length;
  if(!live.length){ cap.html("Nothing on this layer yet."); return; }
  cap.html("<b>"+LABEL[mode]+"</b> calls "+dd+" of "+live.length+
    " races Democratic"+(tt?", with "+tt+" exactly even":"")+
    ", which is "+(%d+dd)+(tt?" to "+(%d+dd+tt):"")+
    " Democratic seats out of 100.");
}
window.DD_SYNC.subscribe(function(id){ if(id===sel) return; sel=id; draw(); });
draw();
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.4em">
Four layers on the same squares. Blue is Democratic, red Republican, pale is
close. Hover a state for the numbers behind it; on the forecaster layer the
tooltip lists all %d ratings.</p>
', crows, NCOL, NROW, round(6 + NROW * min(760 / NCOL, 84) + 6),
   tolower(as.character(HAS_CLASS)), jsonlite::toJSON(CODE_ORDER),
   as.numeric(fx("n_forecasters")), D_HOLD, D_HOLD,
   as.numeric(fx("n_forecasters"))))

## ---- spread-static
sp <- RACES[order(RACES$fc_mean), ]
op <- par(mar = c(3.4, 5.6, 2.2, 1.0))
plot(NULL, xlim = c(-3.2, 3.2), ylim = c(0.5, nrow(sp) + 0.5),
     axes = FALSE, xlab = "", ylab = "",
     main = paste(nrow(RAT), "ratings, one row per race"), cex.main = 0.95)
abline(v = 0, col = "#BBBBBB", lwd = 1)
for (i in seq_len(nrow(sp))) {
  rr <- RAT$score[RAT$state == sp$state[i]]
  segments(min(rr), i, max(rr), i, col = "#CCCCCC", lwd = 2)
  points(jitter(rr, amount = 0.06), rep(i, length(rr)),
         pch = 19, cex = 0.55, col = "#8899A6")
  points(sp$fc_mean[i], i, pch = 18, cex = 1.15,
         col = ifelse(sp$fc_mean[i] > 0, "#2166AC", "#B2182B"))
}
axis(2, at = seq_len(nrow(sp)), labels = sp$state_name, las = 1,
     cex.axis = 0.62, tick = FALSE, line = -0.6)
axis(1, at = -3:3, labels = c("Safe R", "Likely R", "Lean R", "Toss-up",
                              "Lean D", "Likely D", "Safe D"),
     cex.axis = 0.6, tick = FALSE, line = -0.7)
par(op)

## ---- spread-d3
sp <- RACES[order(RACES$fc_mean), ]
srows <- paste(sprintf('{"st":"%s","nm":"%s","m":%.3f,"v":%s,"f":%s}',
  sp$state, sp$state_name, sp$fc_mean,
  vapply(sp$state, function(s) jsonlite::toJSON(RAT$score[RAT$state == s]), ""),
  vapply(sp$state, function(s)
    jsonlite::toJSON(paste0(RAT$forecaster[RAT$state == s], ": ",
                            RAT$rating[RAT$state == s])), "")),
  collapse = ",")
cat(sprintf('
<div id="spread" style="margin:1em 0"></div>
<script>
window.DD_SYNC=window.DD_SYNC||{fns:[],select(id){this.fns.forEach(f=>f(id));},
  subscribe(fn){this.fns.push(fn);}};
(function(){
const D=[%s];
const W=760,RH=17,M={t:26,r:16,b:34,l:112},H=M.t+M.b+D.length*RH;
const svg=d3.select("#spread").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([-3.35,3.35]).range([M.l,W-M.r]);
const tip=d3.select("#spread").append("div").attr("style",
 "position:fixed;pointer-events:none;background:#1b1b1b;color:#fff;padding:7px 10px;"+
 "border-radius:4px;font-size:12px;max-width:300px;opacity:0;z-index:9;line-height:1.4");
let sel=null;
svg.append("line").attr("x1",x(0)).attr("x2",x(0)).attr("y1",M.t-8)
  .attr("y2",H-M.b+4).attr("stroke","#BBBBBB");
[[-3,"Safe R"],[-2,"Likely R"],[-1,"Lean R"],[0,"Toss-up"],
 [1,"Lean D"],[2,"Likely D"],[3,"Safe D"]].forEach(([v,l])=>
  svg.append("text").attr("x",x(v)).attr("y",H-M.b+20).attr("text-anchor","middle")
    .attr("font-size","10.5px").attr("fill","#666").text(l));
const rows=svg.selectAll("g.rw").data(D).join("g").attr("class","rw")
  .attr("transform",(d,i)=>`translate(0,${M.t+i*RH})`)
  .attr("cursor","pointer")
  .on("mousemove",function(ev,d){ tip.style("opacity",1)
    .html(`<b>${d.nm}</b><br>average ${d.m>0?"+":""}${d.m.toFixed(2)}<br>`+
          `<span style="opacity:.8">${d.f.join("<br>")}</span>`)
    .style("left",Math.min(ev.clientX+14,window.innerWidth-320)+"px")
    .style("top",Math.min(ev.clientY+14,window.innerHeight-250)+"px"); })
  .on("mouseleave",()=>tip.style("opacity",0))
  .on("click",(ev,d)=>{ sel=d.st; window.DD_SYNC.select(d.st); mark(); });
rows.append("rect").attr("x",0).attr("y",-RH/2).attr("width",W).attr("height",RH)
  .attr("fill","transparent");
rows.append("text").attr("x",M.l-8).attr("y",4).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#333333").text(d=>d.nm);
rows.append("line").attr("x1",d=>x(d3.min(d.v))).attr("x2",d=>x(d3.max(d.v)))
  .attr("stroke","#CCCCCC").attr("stroke-width",2.4);
rows.each(function(d){
  const counts={}; d.v.forEach(v=>counts[v]=(counts[v]||0)+1);
  const seen={};
  d3.select(this).selectAll("circle.p").data(d.v).join("circle").attr("class","p")
    .attr("cx",v=>x(v))
    .attr("cy",v=>{ seen[v]=(seen[v]||0)+1;
      return (seen[v]-1-(counts[v]-1)/2)*3.4; })
    .attr("r",2.6).attr("fill","#8899A6").attr("opacity",0.85);
});
rows.append("path").attr("d","M0,-5L4.6,0L0,5L-4.6,0Z")
  .attr("transform",d=>`translate(${x(d.m)},0)`)
  .attr("fill",d=>d.m>0?"#2166AC":"#B2182B");
function mark(){ rows.select("text").attr("font-weight",d=>d.st===sel?"700":"400"); }
window.DD_SYNC.subscribe(function(id){ if(id===sel) return; sel=id; mark(); });
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.4em">
Each grey dot is one forecaster; the diamond is the average of the %d. Hover a
row to read every rating behind it. Rows are sorted by the average.</p>
', srows, as.numeric(fx("n_forecasters"))))

## ---- forecaster-table
ft <- FCS
ft$mean_gap <- pc(ft$mean_gap, 2)
knitr::kable(ft, row.names = FALSE, align = c("l", "l", "r", "r"),
  col.names = c("forecaster", "ratings last changed", "races rated",
                "average distance from the other eleven"))

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
