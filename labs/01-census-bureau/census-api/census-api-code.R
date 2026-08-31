# census-api-code.R -- chunk bodies for census-api-brief.Rmd
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

# (b) a treemap of the six substantive tables
tmap <- function(z, x0, y0, x1, y1) {
  if (length(z) == 1L)
    return(data.frame(table = names(z), x0 = x0, y0 = y0, x1 = x1, y1 = y1,
                      stringsAsFactors = FALSE))
  cs <- cumsum(z) / sum(z)
  k  <- max(1L, min(which.min(abs(cs - 0.5)), length(z) - 1L))
  f  <- sum(z[seq_len(k)]) / sum(z)
  if ((x1 - x0) >= (y1 - y0)) {
    xm <- x0 + f * (x1 - x0)
    rbind(tmap(z[seq_len(k)], x0, y0, xm, y1), tmap(z[-seq_len(k)], xm, y0, x1, y1))
  } else {
    ym <- y0 + f * (y1 - y0)
    rbind(tmap(z[seq_len(k)], x0, y0, x1, ym), tmap(z[-seq_len(k)], x0, ym, x1, y1))
  }
}
tsz <- setNames(tbl$variables, tbl$table); tsz <- tsz[order(-tsz)]
tm  <- tmap(tsz, 0, 0, 1, 1)
tm$variables <- as.integer(tsz[tm$table])
tm$adult <- grepl("18 YEARS AND OVER",
                  v$concept[match(paste0(tm$table, "_001N"), v$variable)])
tm$col <- c(P1 = "#2c7fb8", P3 = "#2c7fb8", P2 = "#4d9221", P4 = "#4d9221",
            P5 = "#e08214", H1 = "#999999")[tm$table]
tm$what <- c(P1 = "race", P2 = "Hispanic origin by race",
             P3 = "race, adults only", P4 = "Hispanic origin by race, adults only",
             P5 = "group quarters", H1 = "housing occupancy")[tm$table]

# (c) how many label strings any two tables share
shr <- outer(subst, subst, Vectorize(function(a, b)
  length(intersect(v$label[v$table == a], v$label[v$table == b]))))
dimnames(shr) <- list(subst, subst)
twins <- which(shr > 1 & row(shr) != col(shr), arr.ind = TRUE)
twins <- twins[twins[, 1] < twins[, 2], , drop = FALSE]

# (d) which levels the "(or part)" rows are fragments of
gbase <- sub(" (or part)", "", g$name, fixed = TRUE)
frag  <- as.data.frame(sort(table(gbase[g$part])), stringsAsFactors = FALSE)
names(frag) <- c("level", "fragments")

# --- the request, dissected, and the two objects that come back -------------
URLSEG <- c("api.census.gov/data", "/2020/dec/pl", "?get=NAME,P1_001N",
            "&for=state:42")
URLCOL <- c("#777777", "#2c7fb8", "#54278F", "#4d9221")
URLLAB <- c("the host", "the 2020 PL 94-171 file", "what you want",
            "where you want it")

# the response object on the left is read out of the committed metadata file,
# so the figure shows real bytes rather than a paraphrase of them
JSONL <- local({
  txt <- paste(readLines("data/raw/api_variables_2020pl.json", warn = FALSE),
               collapse = "")
  m <- regmatches(txt, regexpr('"P1_003N":\\s*\\{[^}]*\\}', txt))
  inner <- sub('^"P1_003N":\\s*\\{', "", sub("\\}$", "", m))
  kv <- trimws(strsplit(inner, ',\\s*(?=")', perl = TRUE)[[1]])
  kv <- head(kv[nzchar(kv)], 4)
  kv <- ifelse(nchar(kv) > 44, paste0(substr(kv, 1, 43), "..."), kv)
  c('"P1_003N": {',
    paste0("   ", kv, c(rep(",", length(kv) - 1), "")), "}")
})

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

## ---- api-raw-var
LV <- readLines("data/raw/api_variables_2020pl.json", warn = FALSE)
LG <- readLines("data/raw/api_geography_2020pl.json", warn = FALSE)
nb <- function(s, p) lengths(regmatches(s, gregexpr(p, s)))
slice <- function(L, i0) {
  i1 <- i0; d <- 0L
  repeat {
    d <- d + nb(L[i1], "[{]") - nb(L[i1], "[}]")
    if (d <= 0L || i1 >= length(L)) break
    i1 <- i1 + 1L
  }
  L[i0:i1]
}
nvar_json <- length(grep('^    "[A-Za-z0-9_]+": [{]', LV))
ngeo_json <- length(grep('"geoLevelDisplay"', LG))
bv <- sub("^    ", "", slice(LV, grep('"P1_003N": [{]', LV)[1]))
bg <- sub("^    ", "", slice(LG, grep('"name": "block",', LG, fixed = TRUE)[1] - 1L))

# the columns the BUILD wrote, before this document derived any of its own
cv <- names(read.csv("data/derived/api_variables.csv", nrows = 1))
cg <- names(read.csv("data/derived/api_geography.csv", nrows = 1))
fv <- length(grep('": ', bv, fixed = TRUE))          # fields on the raw object
fg <- length(grep('": ', bg, fixed = TRUE))

# The response is already one object per thing with named fields, so the
# honest table is exactly that: the names it arrives with, and their values.
# Nesting is kept by qualifying a nested key with its parent.
kv <- function(block) {
  keys <- character(0); vals <- character(0)
  parent <- ""; i <- 1L
  while (i <= length(block)) {
    m <- regmatches(block[i],
                    regexec('^\\s*"([^"]+)":\\s*(.*?),?\\s*$', block[i]))[[1]]
    if (!length(m)) { i <- i + 1L; next }
    k <- m[2]; v <- trimws(m[3])
    if (v == "{") { parent <- paste0(k, "."); i <- i + 1L; next }
    if (v == "[") {                       # an array runs to its closing bracket
      j <- i + 1L; acc <- character(0)
      while (j <= length(block) && !grepl("^\\s*\\]", block[j])) {
        acc <- c(acc, gsub('["]', "", sub(",$", "", trimws(block[j]))))
        j <- j + 1L
      }
      acc <- acc[nzchar(acc)]
      keys <- c(keys, paste0(parent, k))
      vals <- c(vals, if (length(acc)) paste(acc, collapse = ", ")
                      else "(empty list)")
      i <- j + 1L; next
    }
    v <- sub('^"', "", sub('"$', "", v))
    if (nzchar(v)) { keys <- c(keys, paste0(parent, k)); vals <- c(vals, v) }
    i <- i + 1L
  }
  data.frame(key = keys, value = vals, stringsAsFactors = FALSE)
}
kvv <- kv(bv)
data.frame(Field_as_it_arrives = kvv$key, Value = kvv$value)

## ---- api-raw-geo
kvg <- kv(bg)
data.frame(Field_as_it_arrives = kvg$key, Value = kvg$value)

## ---- api-clean-var
v[v$variable == "P1_003N", cv]

## ---- api-clean-geo
g[g$name == "block", cg]

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

## ---- files
data.frame(
  file = c("api_variables.csv", "api_geography.csv"),
  rows = c(nrow(v), nrow(g)),
  what_it_lists = c("every published column of the 2020 PL 94-171 file",
                    "every geographic level you may ask for"),
  needs_a_key = c("no", "no"))

## ---- concepts
o <- as.data.frame(sort(table(v$concept[!is.na(v$concept)]), decreasing = TRUE),
                   stringsAsFactors = FALSE)
names(o) <- c("concept", "variables")
o$concept <- ifelse(nchar(o$concept) > 62,
                    paste0(substr(o$concept, 1, 60), "..."), o$concept)
head(o, 6)

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

## ---- treemap-static
par(mar = c(1.8, 0.3, 0.3, 0.3))
plot(NA, xlim = c(0, 1), ylim = c(1, 0), axes = FALSE, xlab = "", ylab = "",
     xaxs = "i", yaxs = "i")
rect(tm$x0, tm$y0, tm$x1, tm$y1, col = tm$col, border = "white", lwd = 2)
big <- (tm$x1 - tm$x0) * (tm$y1 - tm$y0) > 0.04
text((tm$x0 + tm$x1) / 2, (tm$y0 + tm$y1) / 2 - 0.035, tm$table,
     col = "white", font = 2, cex = 1.5)
text((tm$x0[big] + tm$x1[big]) / 2, (tm$y0[big] + tm$y1[big]) / 2 + 0.03,
     paste(tm$variables[big], "variables"), col = "white", cex = 0.78)
text((tm$x0[big] + tm$x1[big]) / 2, (tm$y0[big] + tm$y1[big]) / 2 + 0.085,
     tm$what[big], col = "white", cex = 0.72)
mtext(paste0("Area is the number of published columns. ", tm$table[5], " and ",
             tm$table[6], " together are ", sum(tm$variables[5:6]), " of the ",
             n_data, " substantive variables."),
      side = 1, line = 0.5, cex = 0.62, col = "#666666")

## ---- treemap-d3
rows <- paste(sprintf(
  '{"t":"%s","x0":%.4f,"y0":%.4f,"x1":%.4f,"y1":%.4f,"v":%d,"c":"%s","w":"%s"}',
  tm$table, tm$x0, tm$y0, tm$x1, tm$y1, tm$variables, tm$col, tm$what),
  collapse = ",")
cat(sprintf('
<div id="tmap" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s];
const W=760,H=380;
const svg=d3.select("#tmap").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const X=d=>d*W, Y=d=>d*H;
const g=svg.append("g").selectAll("g").data(D).join("g");
g.append("rect").attr("x",d=>X(d.x0)).attr("y",d=>Y(d.y0))
  .attr("width",d=>X(d.x1-d.x0)).attr("height",d=>Y(d.y1-d.y0))
  .attr("fill",d=>d.c).attr("stroke","#fff").attr("stroke-width",3)
  .attr("opacity",0).transition().delay((d,i)=>i*110).duration(400)
  .attr("opacity",1);
const cx=d=>X((d.x0+d.x1)/2), cy=d=>Y((d.y0+d.y1)/2);
g.append("text").attr("x",cx).attr("y",d=>cy(d)-4).attr("text-anchor","middle")
  .attr("fill","#fff").attr("font-size","22px").attr("font-weight","700")
  .attr("pointer-events","none").text(d=>d.t);
g.filter(d=>(d.x1-d.x0)*(d.y1-d.y0)>0.04).append("text")
  .attr("x",cx).attr("y",d=>cy(d)+15).attr("text-anchor","middle")
  .attr("fill","#fff").attr("font-size","12px").attr("pointer-events","none")
  .text(d=>d.v+" variables");
g.filter(d=>(d.x1-d.x0)*(d.y1-d.y0)>0.04).append("text")
  .attr("x",cx).attr("y",d=>cy(d)+33).attr("text-anchor","middle")
  .attr("fill","#fff").attr("font-size","12px").attr("pointer-events","none")
  .text(d=>d.w);
})();
</script>
', rows))

## ---- ident
data.frame(
  category = c("Substantive variables (counts of people or housing)",
               "Geographic identifiers and query keywords",
               "Total rows in the file"),
  rows = c(n_data, n_ident, nrow(v)))

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
vals <- c(`geographic identifiers` = n_ident,
          `adults-only duplicates` = n_adult,
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
cat(sprintf('
<div id="shape" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[{k:"geographic identifiers",v:%d,c:"#BDBDBD",
          t:"Not counts of anybody. STATE, COUNTY, BLOCK, VTD and the API\\u2019s own query keywords."},
         {k:"adults-only duplicates",v:%d,c:"#C41230",
          t:"P3 and P4 \\u2014 P1 and P2 run again on the population 18 and over. They exist because the Voting Rights Act is about voters."},
         {k:"everything else",v:%d,c:"#2c7fb8",
          t:"Race, Hispanic origin, group quarters and housing occupancy, for the whole population. This is what the census actually asked."}];
const W=740,H=250,M={t:16,r:24,b:44,l:172};
const svg=d3.select("#shape").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,180]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.k)).range([M.t,H-M.b]).padding(0.28);
svg.append("g").attr("transform",`translate(0,${H-M.b})`).call(d3.axisBottom(x).ticks(6));
svg.append("g").attr("transform",`translate(${M.l},0)`).call(d3.axisLeft(y).tickSize(0));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-8).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444").text("rows in api_variables.csv");
const cap=d3.select("#shape").append("p")
  .attr("style","font-size:0.9em;color:#444;min-height:3.2em;margin-top:0.4em");
svg.append("g").selectAll("rect").data(D).join("rect")
  .attr("x",x(0)).attr("y",d=>y(d.k)).attr("height",y.bandwidth())
  .attr("fill",d=>d.c).attr("rx",2).attr("width",0)
  .on("mousemove",(e,d)=>cap.html(`<b>${d.k}: ${d.v}</b> \\u2014 ${d.t}`))
  .transition().duration(700).attr("width",d=>x(d.v)-x(0));
svg.append("g").selectAll("text").data(D).join("text")
  .attr("x",d=>x(d.v)+7).attr("y",d=>y(d.k)+y.bandwidth()/2+4)
  .attr("font-size","12px").attr("fill","#333").attr("opacity",0).text(d=>d.v)
  .transition().delay(700).duration(300).attr("opacity",1);
cap.html("<b>Hover a bar.</b> The middle bar is the finding: %s of %d substantive variables are the same tables run again on adults.");
})();
</script>
', n_ident, n_adult, n_data - n_adult, n_adult, n_data))

## ---- small
o <- data.frame(level = lvl[lvl %in% below])
o$this_is <- c(
  "block" = "often one city block; many contain nobody",
  "block group" = "a cluster of blocks",
  "tract" = "roughly a neighborhood",
  "county subdivision" = "townships and equivalents",
  "subminor civil division" = "sub-township units in a few states",
  "place" = "an incorporated city or town",
  "voting district" = "a precinct")[o$level]
o

## ---- frag-static
lb <- ifelse(nchar(frag$level) > 40, paste0(substr(frag$level, 1, 38), ".."),
             frag$level)
par(mar = c(5.4, 15.5, 1, 2))
plot(NA, xlim = c(0, max(frag$fragments) + 0.6), ylim = c(0.5, nrow(frag) + 0.5),
     yaxt = "n", bty = "n", ylab = "", las = 1,
     xlab = "times the level reappears as an \"(or part)\" row")
segments(0, seq_len(nrow(frag)), frag$fragments, seq_len(nrow(frag)),
         col = "#cccccc", lwd = 1.6)
points(frag$fragments, seq_len(nrow(frag)), pch = 19, cex = 1.5,
       col = ifelse(frag$level %in% lvl, "#2c7fb8", "#8856a7"))
axis(2, at = seq_len(nrow(frag)), labels = lb, las = 1, tick = FALSE,
     cex.axis = 0.7, line = -0.4)
legend("bottomright", bty = "n", cex = 0.74, pch = 19,
       col = c("#2c7fb8", "#8856a7"),
       legend = c("also published as a level in its own right",
                  "only ever appears as a fragment"))
mtext(paste0(nrow(frag), " levels generate the ", n_part, " fragment rows, and ",
             "\"", frag$level[nrow(frag)], "\" alone accounts for ",
             frag$fragments[nrow(frag)], " of them."),
      side = 1, line = 4.0, cex = 0.6, col = "#666666")

## ---- frag-d3
rows <- paste(sprintf('{"l":"%s","v":%d,"o":%d}', frag$level, frag$fragments,
                      as.integer(frag$level %in% lvl)), collapse = ",")
cat(sprintf('
<div id="frg" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[%s].reverse();
const W=760,H=440,M={t:16,r:26,b:66,l:290};
const svg=d3.select("#frg").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,d3.max(D,d=>d.v)+0.6]).range([M.l,W-M.r]);
const y=d3.scaleBand().domain(D.map(d=>d.l)).range([M.t,H-M.b]).padding(0.35);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(7).tickFormat(d3.format("d")));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickSize(0)).call(g=>g.select(".domain").remove())
  .selectAll("text").attr("font-size","10px")
  .text(d=>d.length>44?d.slice(0,42)+"\\u2026":d);
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-30).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("times the level reappears as an \\u201c(or part)\\u201d row");
const cy=d=>y(d.l)+y.bandwidth()/2;
const col=d=>d.o?"#2c7fb8":"#8856a7";
svg.append("g").selectAll("line").data(D).join("line")
  .attr("x1",x(0)).attr("y1",cy).attr("y2",cy).attr("stroke","#ccc")
  .attr("stroke-width",1.6).attr("x2",x(0))
  .transition().duration(700).attr("x2",d=>x(d.v));
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",x(0)).attr("cy",cy).attr("r",5.5).attr("fill",col)
  .transition().duration(700).attr("cx",d=>x(d.v));
const lg=svg.append("g").attr("transform",`translate(${M.l},${H-8})`);
[["#2c7fb8","also published as a level in its own right"],
 ["#8856a7","only ever appears as a fragment"]].forEach((r,i)=>{
  lg.append("circle").attr("cx",i*260+5).attr("r",5).attr("fill",r[0]);
  lg.append("text").attr("x",i*260+15).attr("y",4).attr("font-size","11px")
    .attr("fill","#333").text(r[1]);});
})();
</script>
', rows))

## ---- api-static
par(mar = c(0.3, 0.3, 0.3, 0.3))
plot(NA, xlim = c(0, 100), ylim = c(100, 0), axes = FALSE, xlab = "", ylab = "",
     xaxs = "i", yaxs = "i")
x0 <- 3
wseg <- sapply(URLSEG, function(s) strwidth(s, cex = 0.7, family = "mono"))
xs <- x0 + cumsum(c(0, head(wseg, -1)))
text(x0, 5, "The request", pos = 4, offset = 0, cex = 0.8, font = 2)
for (i in seq_along(URLSEG)) {
  text(xs[i], 12, URLSEG[i], pos = 4, offset = 0, cex = 0.7, family = "mono",
       col = URLCOL[i])
  segments(xs[i] + 0.2, 14.6, xs[i] + wseg[i] - 0.3, 14.6, col = URLCOL[i],
           lwd = 2.4)
  segments(xs[i] + 0.4, 14.6, xs[i] + 0.4, 17.2 + (i %% 2) * 4.4,
           col = URLCOL[i])
  text(xs[i] + 0.4, 19.4 + (i %% 2) * 4.4, URLLAB[i], pos = 4, offset = 0.1,
       cex = 0.62, col = URLCOL[i])
}
card <- function(bx, ttl, sub, lines, accent, sty) {
  rect(bx, 36, bx + 45, 90, border = "#999999")
  rect(bx, 36, bx + 45, 43, col = accent, border = accent)
  text(bx + 1.4, 40.2, ttl, pos = 4, offset = 0, cex = 0.68, col = "white",
       font = 2)
  text(bx + 1.4, 48, sub, pos = 4, offset = 0, cex = 0.58, col = "#666666",
       family = "mono")
  for (j in seq_along(lines))
    text(bx + sty$x[j], sty$y[j], lines[j], pos = 4, offset = 0,
         cex = sty$cex[j], family = sty$fam[j], col = sty$col[j],
         font = sty$font[j])
}
nj <- length(JSONL)
card(3, "Ask the catalog: no key needed", "GET /data/2020/dec/pl/variables.json",
     JSONL, "#2c7fb8",
     list(x = rep(2, nj), y = 56 + (seq_len(nj) - 1) * 5.4,
          cex = rep(0.56, nj), fam = rep("mono", nj),
          col = rep("#333333", nj), font = rep(1, nj)))
card(52, "Ask for a number: key required", "GET /data/2020/dec/pl?get=...&for=...",
     c("Missing Key", "(a web page, not data)"), "#C41230",
     list(x = c(12, 8.5), y = c(64, 72), cex = c(1.1, 0.66),
          fam = c("", ""), col = c("#C41230", "#666666"), font = c(2, 1)))
arrows(25, 30, 25, 35, length = 0.06, col = "#2c7fb8")
arrows(74, 30, 74, 35, length = 0.06, col = "#C41230")
text(3, 94, paste0("Left: real bytes from data/api_variables_2020pl.json, one of ",
                   nrow(v), " entries. Right: what the same host returns when the",
                   " key is missing -"), pos = 4, offset = 0, cex = 0.6,
     col = "#666666")
text(3, 98, paste0("valid HTML that a program will happily parse into nothing."),
     pos = 4, offset = 0, cex = 0.6, col = "#666666")

## ---- api-d3
seg <- paste(sprintf('{"t":"%s","c":"%s","l":"%s"}', URLSEG, URLCOL, URLLAB),
             collapse = ",")
jl  <- paste(sprintf('"%s"', gsub('"', '\\\\"', JSONL)), collapse = ",")
cat(paste0('
<div id="api" style="margin:1em 0"></div>
<script>
(function(){
const SEG=[', seg, '],JL=[', jl, '];
const W=760,H=430,CW=8.05;
const svg=d3.select("#api").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
svg.append("text").attr("x",14).attr("y",20).attr("font-weight","600")
  .text("The request");
let cx=14;
SEG.forEach((s,i)=>{
  const w=s.t.length*CW;
  svg.append("text").attr("x",cx).attr("y",48).attr("fill",s.c)
    .attr("font-family","ui-monospace,Menlo,monospace").attr("font-size","13.5px")
    .text(s.t);
  svg.append("line").attr("x1",cx).attr("x2",cx+w-3).attr("y1",56).attr("y2",56)
    .attr("stroke",s.c).attr("stroke-width",2.6);
  svg.append("line").attr("x1",cx+1).attr("x2",cx+1).attr("y1",56)
    .attr("y2",62+(i%2)*16).attr("stroke",s.c);
  svg.append("text").attr("x",cx+5).attr("y",66+(i%2)*16).attr("font-size","11.5px")
    .attr("fill",s.c).text(s.l);
  cx+=w;});
function card(bx,ttl,sub,accent,draw){
  svg.append("rect").attr("x",bx).attr("y",128).attr("width",344).attr("height",232)
    .attr("fill","#fff").attr("stroke","#999").attr("rx",4);
  svg.append("rect").attr("x",bx).attr("y",128).attr("width",344).attr("height",28)
    .attr("fill",accent);
  svg.append("text").attr("x",bx+10).attr("y",147).attr("fill","#fff")
    .attr("font-size","12.5px").attr("font-weight","600").text(ttl);
  svg.append("text").attr("x",bx+10).attr("y",176).attr("fill","#666")
    .attr("font-size","11.5px").attr("font-family","ui-monospace,Menlo,monospace")
    .text(sub);
  draw(bx);
  svg.append("path").attr("d",`M${bx+172},104 L${bx+172},124`)
    .attr("stroke",accent).attr("marker-end","url(#am)");
}
card(14,"Ask the catalog: no key needed","GET /data/2020/dec/pl/variables.json",
  "#2c7fb8",bx=>{JL.forEach((t,j)=>
    svg.append("text").attr("x",bx+16).attr("y",204+j*20)
      .attr("font-family","ui-monospace,Menlo,monospace").attr("font-size","11.5px")
      .attr("fill","#333").text(t));});
card(402,"Ask for a number: key required","GET /data/2020/dec/pl?get=...&for=...",
  "#C41230",bx=>{
    svg.append("text").attr("x",bx+172).attr("y",250).attr("text-anchor","middle")
      .attr("font-size","26px").attr("font-weight","700").attr("fill","#C41230")
      .text("Missing Key");
    svg.append("text").attr("x",bx+172).attr("y",280).attr("text-anchor","middle")
      .attr("font-size","12px").attr("fill","#666").text("(a web page, not data)");});
const df=svg.append("defs").append("marker").attr("id","am").attr("viewBox","0 0 8 8")
  .attr("refX",7).attr("refY",4).attr("markerWidth",6).attr("markerHeight",6)
  .attr("orient","auto");
df.append("path").attr("d","M0,0 L8,4 L0,8 Z").attr("fill","#888");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Left: real bytes from <code>data/api_variables_2020pl.json</code>, one of ',
nrow(v), ' entries the open half will hand anybody. Right: what the same host
returns when the key is missing — valid HTML that a program will happily parse
into nothing.</p>
'))

## ---- urls
data.frame(
  piece = c("api.census.gov/data", "2020/dec/pl", "get=NAME,P1_001N",
            "for=state:42", "for=county:*&in=state:42"),
  means = c("the host",
            "the 2020 decennial census, Public Law 94-171 file",
            "the place name, and total population",
            "state FIPS 42, which is Pennsylvania",
            "every county in Pennsylvania instead"))

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#api text[fill="#333" i],
#api text[fill="#c41230" i],
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
#ice text[fill="#ffffff" i],
#tmap text[fill="#fff" i],
#tmap text[fill="#ffffff" i]
  { paint-order:stroke; stroke:var(--ink); stroke-width:3px;
    stroke-linejoin:round; }
@media (prefers-color-scheme: dark) {
#ice text[fill="#fff" i],
#ice text[fill="#ffffff" i],
#tmap text[fill="#fff" i],
#tmap text[fill="#ffffff" i]
  { stroke:var(--paper); }
}
</style>')
