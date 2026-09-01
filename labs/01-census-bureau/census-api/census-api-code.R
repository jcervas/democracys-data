# census-api-code.R -- chunk bodies for census-api-brief.Rmd
#
# Each `## ---- label` block below is the body of the chunk with that
# label in the brief. knitr::read_chunk() pairs them up at render time;
# the brief carries the labels and options, this file carries the code.
# Edit here, not there. A label added here needs a matching empty chunk
# in the brief to appear, and vice versa.

## ---- setup
source("../../../../../_syllabus-template/syllabus-helpers.R")
source("../../_lib/dd-charts.R")
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 7.2, fig.height = 4.6,
                      dpi = 96, fig.retina = 1)
options(scipen = 999)

v <- read.csv("data/derived/api_variables.csv",  stringsAsFactors = FALSE)
g <- read.csv("data/derived/api_geography.csv",  stringsAsFactors = FALSE)

v$table <- sub("_.*", "", v$variable)
subst   <- c("P1", "P2", "P3", "P4", "P5", "H1")
v$is_data <- v$table %in% subst
n_data  <- sum(v$is_data)
n_ident <- sum(!v$is_data)
adult   <- v$is_data & grepl("18 YEARS AND OVER", v$concept)
n_adult <- sum(adult)

tbl <- as.data.frame(table(v$table[v$is_data]), stringsAsFactors = FALSE)
names(tbl) <- c("table", "variables")

g$part   <- grepl("(or part)", g$name, fixed = TRUE)
lvl      <- unique(g$name[!g$part])
n_part   <- sum(g$part)
below    <- c("block", "block group", "tract", "county subdivision",
              "subminor civil division", "place", "voting district")
n_below  <- sum(lvl %in% below)

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(x, big.mark = ",")

# ---- inputs the figures share, so both output paths draw the same thing ----

# (a) the label hierarchy of table P1, laid out as an icicle
prt <- lapply(strsplit(sub("^[ ]*!!", "", v$label[v$table == "P1"]), "!!"),
              function(x) sub(":$", "", x))
ice <- data.frame(key   = vapply(prt, paste, "", collapse = " > "),
                  depth = lengths(prt),
                  leaf  = vapply(prt, function(x) x[length(x)], ""),
                  stringsAsFactors = FALSE)
ice$isleaf <- !vapply(ice$key,
                      function(k) any(startsWith(ice$key, paste0(k, " > "))),
                      logical(1))
li  <- cumsum(ice$isleaf) - 1L
spn <- t(vapply(ice$key, function(k) {
  d <- ice$isleaf & (ice$key == k | startsWith(ice$key, paste0(k, " > ")))
  c(min(li[d]), max(li[d]) + 1L) }, numeric(2)))
ice$x0 <- spn[, 1]; ice$x1 <- spn[, 2]
ice$col <- ifelse(ice$depth == 1, "#999999",
           ifelse(grepl("^Total > Population of one race", ice$key),
                  "#2c7fb8", "#e08214"))
white_key <- "Total > Population of one race > White alone"
ice$col[ice$key == white_key] <- "#C41230"
n_leaf  <- sum(ice$isleaf)
white_d <- ice$depth[ice$key == white_key]

# (b) how many label strings any two tables share
shr <- outer(subst, subst, Vectorize(function(a, b)
  length(intersect(v$label[v$table == a], v$label[v$table == b]))))
dimnames(shr) <- list(subst, subst)
twins <- which(shr > 1 & row(shr) != col(shr), arr.ind = TRUE)
twins <- twins[twins[, 1] < twins[, 2], , drop = FALSE]

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- doors
data.frame(
  door = c("data.census.gov", "www2.census.gov", "the API"),
  what_it_is = c("the website: search, click, download",
                 "the published files, exactly as released",
                 "a URL that returns data"),
  needs_a_key = c("no", "no", "yes"),
  can_you_automate_it = c("not really", "yes, if you can parse the format",
                          "yes — that is the point"))

## ---- one-var
o <- v[v$variable == "P1_003N", c("variable", "label", "concept")]
o$label <- gsub("!!", " > ", o$label)
names(o) <- c("variable", "what it counts", "which table it belongs to")
o

## ---- icicle-static
md <- max(ice$depth)
par(mar = c(1.6, 0.3, 0.3, 0.3))
plot(NA, xlim = c(-13, n_leaf), ylim = c(md, 0), axes = FALSE,
     xlab = "", ylab = "", yaxs = "i")
rect(ice$x0 + 0.05, ice$depth - 0.94, ice$x1 - 0.05, ice$depth - 0.14,
     col = ice$col, border = NA)
wd <- (ice$x1 - ice$x0) / (n_leaf + 13)
lb <- ifelse(nchar(ice$leaf) > wd * 108,
             paste0(substr(ice$leaf, 1, floor(wd * 108) - 2), ".."), ice$leaf)
sh <- wd * 108 > 6
text((ice$x0[sh] + ice$x1[sh]) / 2, ice$depth[sh] - 0.54, lb[sh],
     col = "white", cex = 0.62)
wk <- ice[ice$key == white_key, ]
arrows(-1.2, wk$depth - 0.54, wk$x0 - 0.2, wk$depth - 0.54, length = 0.055,
       col = "#C41230", lwd = 1.4)
text(-1.6, wk$depth - 0.54, "P1_003N", pos = 2, cex = 0.68, col = "#C41230")
mtext(paste0("depth 1 to ", md, ", top to bottom; ", n_leaf,
             " bottom-level categories across"), side = 1, line = 0.3,
      cex = 0.72, col = "#666666")

## ---- icicle-d3
rows <- paste(sprintf('{"k":"%s","l":"%s","d":%d,"a":%d,"b":%d,"c":"%s"}',
                      ice$key, ice$leaf, ice$depth, ice$x0, ice$x1, ice$col),
              collapse = ",")
cat(sprintf('
<div id="ice" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[%s], NL=%d, MD=%d;
const W=760,H=MD*46+30,M={t:8,r:8,b:8,l:8};
const svg=d3.select("#ice").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,NL]).range([M.l,W-M.r]);
const cap=d3.select("#ice").append("p")
  .attr("style","font-size:0.85em;color:#555;min-height:2.6em;margin-top:0.3em");
const g=svg.append("g").selectAll("g").data(D).join("g");
g.append("rect").attr("x",d=>x(d.a)+0.6).attr("y",d=>M.t+(d.d-1)*46)
  .attr("width",d=>Math.max(x(d.b)-x(d.a)-1.2,1)).attr("height",38)
  .attr("fill",d=>d.c).attr("rx",2).style("cursor","pointer")
  .on("mousemove",(e,d)=>cap.html("<b>"+d.k.replace(/ > /g," \\u203a ")+"</b>"))
  .on("mouseleave",()=>cap.html("<b>Hover a block.</b> The red block is P1_003N, \\u201cWhite alone\\u201d \\u2014 %d levels down, and it excludes everyone who ticked two boxes."));
g.filter(d=>x(d.b)-x(d.a)>78).append("text")
  .attr("x",d=>(x(d.a)+x(d.b))/2).attr("y",d=>M.t+(d.d-1)*46+23)
  .attr("text-anchor","middle").attr("fill","#fff").attr("font-size","11px")
  .attr("pointer-events","none")
  .text(d=>{const c=Math.floor((x(d.b)-x(d.a))/6.2);
            return d.l.length>c?d.l.slice(0,c-1)+"\\u2026":d.l;});
cap.html("<b>Hover a block.</b> The red block is P1_003N, \\u201cWhite alone\\u201d \\u2014 %d levels down, and it excludes everyone who ticked two boxes.");
})();
</script>
', rows, n_leaf, max(ice$depth), white_d, white_d))

## ---- tables
o <- tbl[order(-tbl$variables), ]
o$what_it_holds <- c(
  P2 = "Hispanic origin crossed with race",
  P4 = "Hispanic origin crossed with race, adults only",
  P1 = "race",
  P3 = "race, adults only",
  P5 = "group quarters",
  H1 = "occupied and vacant housing units")[o$table]
o

## ---- adult
data.frame(
  quantity = c("Substantive variables",
               "of which restricted to the population 18 years and over",
               "Share of the substantive file that is an adults-only twin"),
  value = c(n_data, n_adult, paste0(pc(100 * n_adult / n_data, 0), "%")))

## ---- matrix-static
k <- nrow(shr)
par(mar = c(0.6, 3.2, 3.2, 0.6))
plot(NA, xlim = c(0.5, k + 0.5), ylim = c(k + 0.5, 0.5), axes = FALSE,
     xlab = "", ylab = "")
ramp <- colorRampPalette(c("#f2f2f2", "#2c7fb8"))(101)
for (i in 1:k) for (j in 1:k) {
  sh <- shr[i, j] / max(shr)
  hot <- i != j && shr[i, j] > 1
  rect(j - 0.45, i - 0.45, j + 0.45, i + 0.45,
       col = if (hot) "#C41230" else ramp[round(100 * sh) + 1], border = "white")
  text(j, i, shr[i, j], cex = 0.9,
       col = if (hot || sh > 0.55) "white" else "#333333")
}
axis(3, at = 1:k, labels = colnames(shr), tick = FALSE, line = -0.7, font = 2)
axis(2, at = 1:k, labels = rownames(shr), tick = FALSE, las = 1, line = -0.5,
     font = 2)
mtext("labels two tables have in common", side = 3, line = 1.8, cex = 0.8,
      col = "#666666")

## ---- matrix-d3
cells <- expand.grid(i = seq_len(nrow(shr)), j = seq_len(ncol(shr)))
cells$v <- shr[cbind(cells$i, cells$j)]
rows <- paste(sprintf('{"i":%d,"j":%d,"v":%d,"a":"%s","b":"%s"}',
                      cells$i, cells$j, cells$v,
                      rownames(shr)[cells$i], colnames(shr)[cells$j]),
              collapse = ",")
cat(sprintf('
<div id="mat" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s],K=%d,MX=%d;
const W=470,H=430,M={t:52,r:12,b:12,l:52},S=(W-M.l-M.r)/K;
const svg=d3.select("#mat").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:470px;height:auto;font:12px inherit");
const ramp=d3.scaleLinear().domain([0,MX]).range(["#f2f2f2","#2c7fb8"]);
const hot=d=>d.i!==d.j&&d.v>1;
const cap=d3.select("#mat").append("p")
  .attr("style","font-size:0.85em;color:#555;min-height:2.6em;margin-top:0.3em");
const g=svg.append("g").selectAll("g").data(D).join("g");
g.append("rect").attr("x",d=>M.l+(d.j-1)*S+1).attr("y",d=>M.t+(d.i-1)*S+1)
  .attr("width",S-2).attr("height",S-2)
  .attr("fill",d=>hot(d)?"#C41230":ramp(d.v)).style("cursor","pointer")
  .on("mousemove",(e,d)=>cap.html("<b>"+d.a+" and "+d.b+"</b> share "+d.v+
    " of the label strings the API publishes."+(hot(d)?
    " That is every row of both tables but one.":"")));
g.append("text").attr("x",d=>M.l+(d.j-0.5)*S).attr("y",d=>M.t+(d.i-0.5)*S+5)
  .attr("text-anchor","middle").attr("font-size","13px").attr("pointer-events","none")
  .attr("fill",d=>(hot(d)||d.v>MX*0.55)?"#fff":"#333").text(d=>d.v);
const nm=[%s];
nm.forEach((t,i)=>{
  svg.append("text").attr("x",M.l+(i+0.5)*S).attr("y",M.t-10)
    .attr("text-anchor","middle").attr("font-weight","600").text(t);
  svg.append("text").attr("x",M.l-10).attr("y",M.t+(i+0.5)*S+5)
    .attr("text-anchor","end").attr("font-weight","600").text(t);});
cap.html("<b>Hover a cell.</b> Two red blocks, and nothing else off the diagonal: %s and %s are the same %d labels, %s and %s the same %d.");
})();
</script>
', rows, nrow(shr), max(shr),
   paste(sprintf('"%s"', rownames(shr)), collapse = ","),
   rownames(shr)[twins[1, 1]], colnames(shr)[twins[1, 2]],
   shr[twins[1, 1], twins[1, 2]],
   rownames(shr)[twins[2, 1]], colnames(shr)[twins[2, 2]],
   shr[twins[2, 1], twins[2, 2]]))

## ---- geo
data.frame(
  quantity = c("Rows in the geography file",
               "Rows that are '(or part)' fragments of a level already listed",
               "Distinct geographic levels",
               "of which sit below the county"),
  value = c(nrow(g), n_part, length(lvl), n_below))

## ---- shape-static
vals <- c(`identifiers & keywords` = n_ident,
          `adults-only twins` = n_adult,
          `everything else` = n_data - n_adult)
par(mar = c(4.4, 11.5, 1.4, 2))
bp <- barplot(rev(vals), horiz = TRUE, las = 1, cex.names = 0.85,
              col = rev(c("#BDBDBD", "#C41230", "#2c7fb8")), border = NA,
              xlim = c(0, 180), xlab = "rows in api_variables.csv")
text(rev(vals), bp, labels = rev(vals), pos = 4, cex = 0.8, xpd = NA)
mtext(paste0("The middle bar is the finding: ", n_adult, " of ", n_data,
             " substantive variables are the same tables run again on adults."),
      side = 1, line = 3.2, cex = 0.62, col = "#666666")

## ---- shape-d3
# Drawn with the shared library. d3 itself is loaded once, by the icicle
# figure above, so dd_fig() is told not to emit it a second time; it still
# emits dd-charts.js, which rides beside whatever loaded d3 first.
D <- data.frame(
  group = c("identifiers & keywords", "adults-only twins", "everything else"),
  rows  = c(n_ident, n_adult, n_data - n_adult),
  stringsAsFactors = FALSE)
dd_fig("shape", "bar", D, d3 = FALSE,
  x = list(field = "rows", domain = c(0, 180), ticks = 6, fmt = "d"),
  y = list(field = "group", band = TRUE),
  series = list(field = "group",
                classes = list(`identifiers & keywords` = "series-7",
                               `adults-only twins` = "series-2",
                               `everything else` = "series-1")),
  rowHeight = 46, valueLabels = TRUE,
  tip = dd_tip(c(rows = "rows of api_variables.csv"), fmt = c(rows = "d"),
               title = "group"))
cat('
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Hover a bar for the exact count.</p>')

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#mat text[fill="#333" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- on-mark-halo
# White labels drawn on pale and mid-toned marks, under 3:1 in BOTH themes.
# A halo fixes them, but the stroke must be dark against a white glyph in
# both themes, and no single token is: on the light page that is var(--ink),
# and on the dark page it is var(--paper). A --paper stroke on the light page
# would make white text worse, not better.
# Sites found by _lib/check-contrast.js --light.
cat('<style>
#ice text[fill="#fff" i],
#ice text[fill="#ffffff" i]
  { paint-order:stroke; stroke:var(--ink); stroke-width:3px;
    stroke-linejoin:round; }
@media (prefers-color-scheme: dark) {
#ice text[fill="#fff" i],
#ice text[fill="#ffffff" i]
  { stroke:var(--paper); }
}
</style>')
