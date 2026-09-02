# voter-files-source-code.R -- chunk bodies for voter-files-source-brief.Rmd
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

sch <- read.csv("data/derived/schemas.csv",    stringsAsFactors = FALSE)
val <- read.csv("data/derived/validated.csv",  stringsAsFactors = FALSE)
sr  <- read.csv("data/derived/selfreport.csv", stringsAsFactors = FALSE)
att <- read.csv("data/derived/attrition.csv",  stringsAsFactors = FALSE)
st  <- read.csv("data/derived/status.csv",     stringsAsFactors = FALSE)

nn <- function(x) format(round(x), big.mark = ",")
p1 <- function(x) formatC(x, format = "f", digits = 1)

NGA <- as.integer(sch$georgia[sch$field == "Columns in the extract"])
NNJ <- as.integer(sch$new_jersey[sch$field == "Columns in the extract"])

REC24 <- val$pct_of_current_file[val$election == "2024 general"]
SAY24 <- sr$pct_said_they_voted[sr$year == 2024]
REC20 <- val$pct_of_current_file[val$election == "2020 general"]
SAY20 <- sr$pct_said_they_voted[sr$year == 2020]
INFILE <- val$in_file[1]

LOST20 <- att$voters_no_longer_in_file[att$election == "2020 general"]
LOSTP  <- att$pct_of_recorded[att$election == "2020 general"]
# the attrition arithmetic, walked in the prose: 2020 voters still in the file,
# and everyone the state's history file names for that election
REC20N  <- val$recorded_voting[val$election == "2020 general"]
VOTED20 <- REC20N + LOST20

ACTIVE <- st$voters[st$status == "ACTIVE"][1]
INACT  <- sum(st$voters[st$status == "INACTIVE"])

knit_print.data.frame <- function(x, ...) {
  n <- names(x)
  n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

ACC <- "#1C4C5C"; WARN <- "#8A3B2C"; GRY <- "#8A8F94"

## ---- cmp
data.frame(
  Election = val$election,
  Registrants_in_the_file = nn(val$in_file),
  Recorded_as_voting = nn(val$recorded_voting),
  Share_of_the_file = paste0(p1(val$pct_of_current_file), "%"))

## ---- fig1-static
op <- par(mar = c(3.2, 4.4, 1.2, 6.6), mgp = c(2.9, 0.7, 0))
m <- rbind(c(SAY20, SAY24), c(REC20, REC24))
b <- barplot(m, beside = TRUE, col = c(GRY, ACC), border = NA,
             ylim = c(0, 100), axes = FALSE, names.arg = c("2020", "2024"))
axis(2, las = 1, cex.axis = 0.82, lwd = 0, lwd.ticks = 1)
mtext("% voting", 2, line = 2.9, cex = 0.9)
text(as.vector(b), as.vector(m) + 3.5, paste0(p1(as.vector(m)), "%"),
     cex = 0.72, col = "#4E5A63")
legend(par("usr")[2], 90, c("said they voted\n(survey)", "recorded voting\n(state file)"),
       fill = c(GRY, ACC), border = NA, bty = "n", cex = 0.72, xpd = NA, y.intersp = 1.6)
par(op)

## ---- fig1-d3
# ---------------------------------------------------------------------------
# Four bars, and the thing that matters about them is not their heights but
# their denominators. The static twin has to push that into a caution below the
# figure; here it is attached to the bars themselves, so a reader who
# interrogates the comparison finds the caveat rather than having to be told.
#
# This chunk carries the ONE d3 <script src> for the document.
# ---------------------------------------------------------------------------
rows <- paste0(
  '{yr:"', c("2020", "2024", "2020", "2024"), '",',
  'k:"', c("survey", "survey", "file", "file"), '",',
  'v:', c(SAY20, SAY24, REC20, REC24), ',',
  'note:"', c("ANES respondents who said they voted",
              "ANES respondents who said they voted",
              paste0("of ", nn(INFILE), " current Houston County registrants"),
              paste0("of ", nn(INFILE), " current Houston County registrants")),
  '"}', collapse = ",")
cat(paste0('
<div id="vfs" style="position:relative;margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const D=[', rows, '];
const GRY="', GRY, '", ACC="', ACC, '";
const W=770,H=400,M={t:16,r:186,b:44,l:58};
const box=d3.select("#vfs");
const svg=box.append("svg").attr("viewBox","0 0 "+W+" "+H)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const yrs=["2020","2024"], ks=["survey","file"];
const x0=d3.scaleBand().domain(yrs).range([M.l,W-M.r]).padding(0.28);
const x1=d3.scaleBand().domain(ks).range([0,x0.bandwidth()]).padding(0.12);
const y=d3.scaleLinear().domain([0,100]).range([H-M.b,M.t]);
svg.append("g").attr("transform","translate("+M.l+",0)")
  .call(d3.axisLeft(y).tickFormat(d=>d+"%").ticks(5));
svg.append("g").attr("transform","translate(0,"+(H-M.b)+")")
  .call(d3.axisBottom(x0).tickSize(0));
svg.append("text").attr("transform","rotate(-90)")
  .attr("x",-(M.t+(H-M.b))/2).attr("y",15).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#4E5A63").text("% voting");
const tip=box.append("div").attr("style","position:absolute;pointer-events:none;'
, 'opacity:0;background:#fff;border:1px solid #CBD3D8;border-radius:3px;'
, 'padding:6px 8px;font:11.5px inherit;max-width:230px;'
, 'box-shadow:0 1px 4px rgba(0,0,0,.14)");
svg.selectAll("rect.b").data(D).join("rect").attr("class","b")
  .attr("x",d=>x0(d.yr)+x1(d.k)).attr("width",x1.bandwidth())
  .attr("y",d=>y(d.v)).attr("height",d=>y(0)-y(d.v))
  .attr("fill",d=>d.k==="survey"?GRY:ACC)
  .on("mousemove",function(e,d){
    const r=box.node().getBoundingClientRect();
    tip.style("opacity",1)
       .style("left",(e.clientX-r.left+14)+"px")
       .style("top",(e.clientY-r.top-8)+"px")
       .html("<b>"+d.yr+" &middot; "+
         (d.k==="survey"?"what people said":"what the record shows")+"</b><br>"+
         d.v.toFixed(1)+"%<br><span style=\\"color:#4E5A63\\">"+d.note+
         "</span><br><i>different denominators \\u2014 not a measure of "+
         "overreporting</i>");
  })
  .on("mouseleave",function(){tip.style("opacity",0);});
svg.selectAll("text.v").data(D).join("text").attr("class","v")
  .attr("x",d=>x0(d.yr)+x1(d.k)+x1.bandwidth()/2)
  .attr("y",d=>y(d.v)-6).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#4E5A63")
  .text(d=>d.v.toFixed(1)+"%");
const leg=svg.append("g").attr("transform","translate("+(W-M.r+14)+",60)");
[["said they voted (survey)",GRY],
 ["recorded voting (state file)",ACC]].forEach(function(s,i){
  leg.append("rect").attr("x",0).attr("y",i*34).attr("width",12).attr("height",12)
     .attr("fill",s[1]);
  leg.append("text").attr("x",18).attr("y",i*34+10).attr("font-size","11px")
     .attr("fill","#4E5A63").text(s[0]);
});
})();
</script>'))

## ---- raw
cat(paste(readLines("data/raw/two-states.txt"), collapse = "\n"))

## ---- schtab
s <- sch[sch$field != "Columns in the extract", ]
data.frame(Does_the_file_record = s$field, Georgia = s$georgia,
           New_Jersey = s$new_jersey)

## ---- atttab
data.frame(Election = att$election,
           Voted_then_and_gone_now = nn(att$voters_no_longer_in_file),
           Share_of_everyone_who_voted = paste0(p1(att$pct_of_recorded), "%"))

## ---- sttab
data.frame(Status = st$status, Reason = ifelse(nzchar(st$reason), st$reason, "—"),
           Registrations = nn(st$voters))
