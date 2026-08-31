# ces-class-code.R -- chunk bodies for ces-class-brief.Rmd
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

b <- read.csv("data/derived/ces2024_benchmarks.csv", stringsAsFactors = FALSE)
b$shift <- b$pct_weighted - b$pct_unweighted

cls_file    <- if (file.exists("data/raw/class_responses.csv"))
                 "data/raw/class_responses.csv" else
                 "data/derived/class_responses_EXAMPLE.csv"
PLACEHOLDER <- grepl("EXAMPLE", cls_file)
cls         <- read.csv(cls_file, stringsAsFactors = FALSE)
n_class     <- max(table(cls$variable))

N_CES <- max(tapply(b$n, b$variable, sum))
pc    <- function(x, k = 1) formatC(x, format = "f", digits = k)
cnt   <- function(x) format(round(x), big.mark = ",")
gv    <- function(v, cat_, col) b[[col]][b$variable == v & b$category == cat_]
moe   <- function(n) 100 * 1.96 * sqrt(0.25 / n)

# class shares for one variable, against both CES columns
share <- function(v) {
  x   <- cls$code[cls$variable == v]
  bb  <- b[b$variable == v, ]
  cn  <- as.vector(table(factor(x, levels = bb$code)))
  data.frame(category       = bb$category,
             class_pct      = round(100 * cn / sum(cn), 1),
             ces_unweighted = bb$pct_unweighted,
             ces_weighted   = bb$pct_weighted,
             stringsAsFactors = FALSE)
}

# plain-English name for each question, in file order
qlab <- c(gender4 = "gender", educ = "education", race = "race",
          pid7 = "party ID (7 point)", ideo5 = "ideology (5 point)",
          newsint = "news attention", votereg = "voter registration")

# how much of the sample each question's weights move, in total
# (half the sum of absolute category shifts = share of respondents reallocated)
adj <- data.frame(variable = names(tapply(abs(b$shift), b$variable, sum)),
                  idx = as.vector(tapply(abs(b$shift), b$variable, sum)) / 2,
                  stringsAsFactors = FALSE)
adj$label <- qlab[adj$variable]
adj <- adj[order(adj$idx), ]

# class-versus-CES gap for every answer category of every question
gaps <- do.call(rbind, lapply(names(qlab), function(v) {
  s <- share(v)
  s$variable <- v
  s$label    <- qlab[[v]]
  s$gap      <- s$class_pct - s$ces_weighted
  s
}))
gaps$out <- abs(gaps$gap) > moe(n_class)

# sample size a class would need to reach a +/- 3 point margin
n_for3 <- 0.25 * (1.96 / 0.03)^2

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- d3-load
# the one and only copy of d3 in this document
cat('<script src="../../_lib/d3.v7.min.js"></script>\n')

## ---- placeholder-warning
if (PLACEHOLDER) cat(
"> **A caveat about the class column, before any of it is read.** The class
> file currently committed to this folder is `class_responses_EXAMPLE.csv`.
> **It is placeholder data. The responses in it are invented and are not
> yours.** They exist so that the tables have the right shape while the real
> first-day survey is being processed. Everything drawn from the CES is real
> and can be checked; nothing in the class column is, until the real file
> replaces the placeholder. Where this document reports a class figure, treat
> it as an illustration of the arithmetic and not as a fact about this room.\n")
if (!PLACEHOLDER) cat(
"> The class file in use is `", basename(cls_file), "` — the real first-day
> responses.\n", sep = "")

## ---- ces-raw-class
CL <- readLines(cls_file, warn = FALSE)
# The file's own header line becomes the header row, and the eight lines under
# it become eight rows. Nothing is renamed or recoded: these are the digits as
# they sit on disk, which is the point the paragraph below makes.
.hdr  <- strsplit(CL[1], ",")[[1]]
.body <- do.call(rbind, lapply(CL[2:9], function(l) strsplit(l, ",")[[1]]))
.raw  <- as.data.frame(.body, stringsAsFactors = FALSE)
names(.raw) <- .hdr
.raw

## ---- ces-shape
RNG <- vapply(names(qlab), function(v) {
  k <- sort(b$code[b$variable == v])
  paste0(min(k), "..", max(k))
}, character(1))
data.frame(Column = names(qlab),
           Codes_that_occur = RNG,
           What_it_asks = unlist(qlab, use.names = FALSE))

## ---- ces-mislabel
r <- b[b$variable == "race", ]
o <- data.frame(
  code = c(6, 8),
  `by the Guide's printed order` = c("Middle Eastern", "Two or more races"),
  `by matching counts` = r$category[match(c(6, 8), r$code)],
  respondents = cnt(r$n[match(c(6, 8), r$code)]),
  check.names = FALSE)
o

## ---- schema
data.frame(
  file = c("ces2024_benchmarks.csv", "class_responses(...).csv"),
  one_row_is = c("one answer category of one question",
                 "one student's answer to one question"),
  columns = c("variable, code, category, n, pct_unweighted, pct_weighted",
              "variable, code"),
  rows = c(nrow(b), nrow(cls)),
  check.names = FALSE)

## ---- one-row
o <- b[b$variable == "votereg" & b$category == "Yes", ]
names(o) <- c("variable", "code", "category", "respondents",
              "% unweighted", "% weighted", "shift")
o[, 1:6]

## ---- pid7
o <- share("pid7")
names(o) <- c("party identification", "class (%)", "CES unweighted (%)",
              "CES weighted (%)")
o

## ---- pid7-caveat
if (PLACEHOLDER) cat(
"**Reminder: the first column is placeholder data, not this class.** The two\n",
"CES columns are real.\n")

## ---- shifts
o <- head(b[order(-abs(b$shift)),
            c("variable", "category", "pct_unweighted", "pct_weighted", "shift")], 8)
o$shift <- sprintf("%+.1f", o$shift)
names(o) <- c("variable", "category", "unweighted (%)", "weighted (%)",
              "shift (points)")
o

## ---- collapse
p   <- b[b$variable == "pid7", ]
grp <- ifelse(p$code %in% 1:3, "Democrat",
       ifelse(p$code == 4,     "Independent",
       ifelse(p$code %in% 5:7, "Republican", "Not sure")))
a   <- aggregate(cbind(pct_unweighted, pct_weighted) ~ grp,
                 data = cbind(p, grp), sum)
names(a) <- c("group", "CES unweighted (%)", "CES weighted (%)")
a

## ---- margin-calc
dr_raw <- a[a$group == "Democrat", 2] - a[a$group == "Republican", 2]
dr_wtd <- a[a$group == "Democrat", 3] - a[a$group == "Republican", 3]

## ---- alluv-prep
pp   <- b[b$variable == "pid7", ]
ordg <- c("Democrat", "Independent", "Republican", "Not sure")
pg   <- ifelse(pp$code %in% 1:3, "Democrat",
        ifelse(pp$code == 4,     "Independent",
        ifelse(pp$code %in% 5:7, "Republican", "Not sure")))
oo   <- order(match(pg, ordg), pp$code)
pp   <- pp[oo, ]; pg <- pg[oo]
lu1  <- cumsum(pp$pct_unweighted); lu0 <- c(0, head(lu1, -1))
lw1  <- cumsum(pp$pct_weighted);   lw0 <- c(0, head(lw1, -1))
gcol <- c(Democrat = "#2c7fb8", Independent = "#999999",
          Republican = "#C41230", `Not sure` = "#8856a7")
gsum <- sapply(ordg, function(z) c(u = sum(pp$pct_unweighted[pg == z]),
                                   w = sum(pp$pct_weighted[pg == z])))
gu1  <- cumsum(gsum["u", ]); gu0 <- c(0, head(gu1, -1))
gw1  <- cumsum(gsum["w", ]); gw0 <- c(0, head(gw1, -1))
sigy <- function(a, b_, n = 48) {
  tt <- seq(0, 1, length.out = n); a + (b_ - a) * (1 - cos(pi * tt)) / 2
}
sigx <- function(n = 48) seq(0, 1, length.out = n)

## ---- alluv-static
par(mar = c(0.6, 12.4, 2.4, 11.0))
plot(NA, xlim = c(0, 1), ylim = c(100, 0), axes = FALSE, ann = FALSE)
xx <- sigx()
for (i in seq_len(nrow(pp))) {
  polygon(c(xx, rev(xx)),
          c(sigy(lu0[i], lw0[i]), rev(sigy(lu1[i], lw1[i]))),
          col = adjustcolor(gcol[pg[i]], 0.55), border = "white", lwd = 0.6)
}
text(-0.03, (lu0 + lu1) / 2,
     paste0(pp$category, "  ", pc(pp$pct_unweighted)), pos = 2, cex = 0.66,
     col = gcol[pg], xpd = NA)
rect(1.0, gw0, 1.045, gw1, col = gcol[ordg], border = "white", xpd = NA)
text(1.055, (gw0 + gw1) / 2, paste0(ordg, "  ", pc(gsum["w", ])), pos = 4,
     cex = 0.72, col = gcol[ordg], xpd = NA)
text(c(0, 1), c(-3, -3), c("collected", "published"), cex = 0.8, font = 2,
     col = "#444444", xpd = NA)

## ---- alluv-d3
rib <- paste(sprintf('{"c":"%s","g":"%s","col":"%s","u0":%.2f,"u1":%.2f,"w0":%.2f,"w1":%.2f,"u":%.1f,"w":%.1f}',
                     pp$category, pg, gcol[pg], lu0, lu1, lw0, lw1,
                     pp$pct_unweighted, pp$pct_weighted), collapse = ",")
grp <- paste(sprintf('{"g":"%s","col":"%s","y0":%.2f,"y1":%.2f,"w":%.1f,"u":%.1f}',
                     ordg, gcol[ordg], gw0, gw1, gsum["w", ], gsum["u", ]),
             collapse = ",")
cat(sprintf('
<div id="alv" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const R=[%s], G=[%s];
const W=760,H=460,M={t:44,r:196,b:16,l:212};
const box=d3.select("#alv");
const svg=box.append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const y=d3.scaleLinear().domain([0,100]).range([M.t,H-M.b]);
const x0=M.l, x1=W-M.r, N=48;
function ribbon(d){
  const pts=[];
  for(let i=0;i<N;i++){const t=i/(N-1), e=(1-Math.cos(Math.PI*t))/2;
    pts.push([x0+(x1-x0)*t, y(d.u0+(d.w0-d.u0)*e)]);}
  for(let i=N-1;i>=0;i--){const t=i/(N-1), e=(1-Math.cos(Math.PI*t))/2;
    pts.push([x0+(x1-x0)*t, y(d.u1+(d.w1-d.u1)*e)]);}
  return "M"+pts.map(p=>p.join(",")).join("L")+"Z";
}
const tip=box.append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:7px 10px;border-radius:4px;font-size:11.5px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("path").data(R).join("path")
  .attr("d",ribbon).attr("fill",d=>d.col).attr("fill-opacity",0.55)
  .attr("stroke","#fff").attr("stroke-width",0.8)
  .on("mousemove",function(e,d){
    d3.select(this).attr("fill-opacity",0.85);
    tip.style("opacity",1).html(`<b>${d.c}</b><br>collected ${d.u}%%<br>`+
      `published ${d.w}%%<br>${d3.format("+.1f")(d.w-d.u)} points<br>`+
      `counted as ${d.g}`)
      .style("left",Math.min(e.offsetX+14,W-230)+"px").style("top",(e.offsetY-10)+"px");
  })
  .on("mouseleave",function(){d3.select(this).attr("fill-opacity",0.55);
    tip.style("opacity",0);});
svg.append("g").selectAll("text").data(R).join("text")
  .attr("x",x0-10).attr("y",d=>y((d.u0+d.u1)/2)+4).attr("text-anchor","end")
  .attr("font-size","11.5px").attr("fill",d=>d.col)
  .text(d=>d.c+"  "+d.u.toFixed(1)+"%%");
svg.append("g").selectAll("rect").data(G).join("rect")
  .attr("x",x1).attr("y",d=>y(d.y0)).attr("width",13)
  .attr("height",d=>y(d.y1)-y(d.y0)).attr("fill",d=>d.col);
svg.append("g").selectAll("text.g").data(G).join("text")
  .attr("x",x1+20).attr("y",d=>y((d.y0+d.y1)/2)+4).attr("font-size","12.5px")
  .attr("font-weight","600").attr("fill",d=>d.col)
  .text(d=>d.g+"  "+d.w.toFixed(1)+"%%");
[[x0,"collected"],[x1,"published"]].forEach(p=>
  svg.append("text").attr("x",p[0]).attr("y",M.t-16).attr("text-anchor","middle")
    .attr("font-size","13px").attr("font-weight","600").attr("fill","#444").text(p[1]));
})();
</script>
', rib, grp))

## ---- moe-table
o <- data.frame(
  survey = c("This class", "A typical national poll", "CES 2024"),
  n = c(cnt(n_class), "1,000", cnt(N_CES)),
  margin = c(pc(moe(n_class)), pc(moe(1000)), pc(moe(N_CES), 2)))
names(o) <- c("survey", "respondents", "margin of error (± points)")
o

## ---- two-failures
data.frame(
  problem = c("Bias", "Imprecision"),
  what_goes_wrong = c("the sample is systematically unlike the population",
                      "the sample is too small to resolve anything"),
  where_you_saw_it = c(paste0("CES: registration off by ",
                              pc(abs(gv("votereg", "Yes", "shift"))),
                              " points before weighting"),
                       paste0("this class: ±", pc(moe(n_class)),
                              " points on every estimate")),
  does_a_bigger_sample_fix_it = c("No", "Yes, slowly"),
  does_weighting_fix_it = c("Only for variables you measured", "No"),
  check.names = FALSE)

## ---- placeholder-close
if (PLACEHOLDER) cat(
"\nNone of that is yet a statement about any particular room, because the class\n",
"file in use is the placeholder. The arithmetic is correct; the responses it\n",
"was applied to are invented.\n")

## ---- weak-placeholder
if (PLACEHOLDER) cat(
"\nTo which one more must be added here: **the class responses in use are\n",
"placeholder data**, invented to give the tables their shape. Every class\n",
"figure in this document is illustrative only.\n")

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt"), tone = "frozen"))
