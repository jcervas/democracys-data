# eavs-code.R -- chunk bodies for eavs-brief.Rmd
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

nat <- read.csv("data/derived/national.csv", stringsAsFactors = FALSE)
st  <- read.csv("data/derived/states.csv",   stringsAsFactors = FALSE)
rsn <- read.csv("data/derived/reasons.csv",  stringsAsFactors = FALSE)
u   <- read.csv("data/derived/units.csv",    stringsAsFactors = FALSE)

g  <- function(v) nat$total[nat$variable == v]
rb <- function(v) nat$reported_by[nat$variable == v]
pr <- function(v) nat$pct_reporting[nat$variable == v]

JUR   <- sum(st$jurisdictions)
MRATE <- 100 * g("C9a") / g("C1b")
PRATE <- 100 * g("E1d") / g("E1a")

st$rate  <- 100 * st$mail_rejected / st$mail_returned
big      <- st[st$mail_returned > 100000, ]
OK <- big$rate[big$state == "OK"]; VT <- big$rate[big$state == "VT"]

hole  <- st[st$mail_returned == 0, ]
NOJOIN <- rb("C9a") - rb("C1b")

pv <- st[st$prov_cast > 10000, ]
pv$prate <- 100 * pv$prov_rejected / pv$prov_cast

pc <- function(x, k = 2) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",", trim = TRUE)

# --- the mail ballot's journey, as a flow ----------------------------------
SENT <- g("C1a"); RET <- g("C1b"); REJ <- g("C9a")
NORET <- SENT - RET; CNT <- RET - REJ
top   <- head(rsn[order(-rsn$ballots), ], 5)
RREST <- REJ - sum(top$ballots)
MAG   <- SENT / REJ            # how far the rejected sliver has to be blown up

# --- reasons: how many, against how well reported --------------------------
rs <- rsn[order(-rsn$ballots), ]
rs$short <- c("Signature did not match" = "signature mismatch",
              "Received after the deadline" = "arrived late",
              "Other (1)" = "other (1)",
              "Voter already voted" = "already voted",
              "Voter signature missing" = "no voter signature",
              "Missing documentation" = "missing documentation",
              "Witness signature missing" = "no witness signature",
              "Voter not eligible" = "not eligible",
              "Voter deceased" = "deceased",
              "No secrecy envelope" = "no secrecy envelope",
              "Envelope not sealed" = "envelope unsealed",
              "Other (2)" = "other (2)",
              "Ballot missing from envelope" = "no ballot in envelope",
              "Other (3)" = "other (3)",
              "Unofficial envelope" = "unofficial envelope",
              "No postmark" = "no postmark",
              "Multiple ballots in one envelope" = "multiple in envelope",
              "No resident address on envelope" = "no resident address",
              "No ballot application on file" = "no application")[rs$reason]
# the two groups the text names, and nothing else invented
AUTO  <- c("Received after the deadline", "Voter already voted",
           "Voter not eligible")
JUDGE <- c("No postmark", "No resident address on envelope",
           "Unofficial envelope")
rs$grp <- ifelse(rs$reason == "Signature did not match", "sig",
          ifelse(rs$reason %in% AUTO, "auto",
          ifelse(rs$reason %in% JUDGE, "judge", "other")))
rs$cov  <- 100 * rs$reported_by / JUR
GCOL <- c(sig = "#C41230", auto = "#4d9221", judge = "#54278F",
          other = "#BDBDBD")
COVA  <- mean(rs$cov[rs$grp == "auto"])
COVJ  <- mean(rs$cov[rs$grp == "judge"])

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- instrument
knitr::include_graphics("../../_lib/assets/eavs-2024-items-c1-c9.png")

## ---- clean-states
o <- st[st$state %in% c("AL", "GA", "TX", "WI"),
        c("state", "jurisdictions", "mail_sent", "mail_returned",
          "mail_rejected")]
o$rate <- ifelse(o$mail_returned > 0,
                 paste0(pc(100 * o$mail_rejected / o$mail_returned), "%"),
                 "cannot be computed")
o$mail_sent     <- n(o$mail_sent)
o$mail_returned <- n(o$mail_returned)
o$mail_rejected <- n(o$mail_rejected)
names(o) <- c("state", "jurisdictions", "mail sent", "mail returned",
              "mail rejected", "rejection rate")
o

## ---- shape
data.frame(
  stage = c("Downloaded", "After cleaning"),
  rows = c(n(JUR), n(nrow(st))),
  columns = c("535", "8"),
  one_row_is = c("one election office", "one state or territory"))

## ---- rows
data.frame(
  quantity = c("Rows in the survey", "States and territories covered",
               "Columns in the source file"),
  value = c(n(JUR), nrow(st), "535"))

## ---- units
o <- rbind(head(u, 3), tail(u, 3))
o$voters <- n(o$voters)
o$voters_per_jurisdiction <- n(o$voters_per_jurisdiction)
names(o) <- c("state", "jurisdictions", "registered voters",
              "voters per jurisdiction")
o

## ---- national
o <- nat[, c("item", "total", "reported_by", "pct_reporting")]
o$total <- n(o$total); o$reported_by <- n(o$reported_by)
names(o) <- c("item", "national total", "jurisdictions reporting it", "% reporting")
o

## ---- mail
data.frame(
  quantity = c("Mail ballots transmitted to voters", "Returned by voters",
               "Never returned", "Rejected", "Rejection rate"),
  value = c(n(g("C1a")), n(g("C1b")), n(g("C1a") - g("C1b")),
            n(g("C9a")), paste0(pc(MRATE), "% of those returned")))

## ---- journey-static
FC <- c(sent = "#cfe3f2", ret = "#2c7fb8", noret = "#D9D9D9",
        rej = "#C41230")
ry <- rbind(c(0.04, 0.13), c(0.23, 0.32), c(0.42, 0.51), c(0.72, 0.81))
par(mar = c(2.6, 0.4, 0.6, 0.4))
plot(NA, xlim = c(0, 1), ylim = c(1, 0), axes = FALSE, xlab = "", ylab = "",
     xaxs = "i", yaxs = "i")
seg <- function(r, x0, x1, col) rect(x0, ry[r, 1], x1, ry[r, 2], col = col,
                                     border = "white", lwd = 1.2)
mid <- function(r) mean(ry[r, ])
seg(1, 0, 1, FC[["sent"]])
text(0.5, mid(1), paste0("transmitted to voters   ", n(SENT)), cex = 0.72)
xr <- RET / SENT
seg(2, 0, xr, FC[["ret"]]); seg(2, xr, 1, FC[["noret"]])
text(xr / 2, mid(2), paste0("returned by voters   ", n(RET)), cex = 0.72,
     col = "white")
text((1 + xr) / 2, mid(2), paste0("never returned   ", n(NORET)), cex = 0.72)
xc <- CNT / SENT
seg(3, 0, xc, FC[["ret"]]); seg(3, xc, xr, FC[["rej"]]); seg(3, xr, 1, FC[["noret"]])
text(xc / 2, mid(3), paste0("counted   ", n(CNT)), cex = 0.72, col = "white")
text((1 + xr) / 2, mid(3), paste0("never returned   ", n(NORET)), cex = 0.72,
     col = "#777777")
polygon(c(xc, xr, 1, 0), c(ry[3, 2], ry[3, 2], ry[4, 1], ry[4, 1]),
        col = "#C4123022", border = NA)
text(0.5, 0.615, paste0("rejected: ", n(REJ), " ballots, ", pc(MRATE),
                        "% of those returned - magnified about ",
                        pc(MAG, 0), " times"), cex = 0.72, col = "#C41230")
xx <- c(0, cumsum(c(top$ballots, RREST)) / REJ)
lab <- c(top$reason, "all other stated reasons")
pct <- c(top$ballots, RREST) / REJ * 100
for (i in seq_along(lab)) {
  rect(xx[i], ry[4, 1], xx[i + 1], ry[4, 2],
       col = if (i == 1) FC[["rej"]] else "#e8918a", border = "white")
  yy <- ry[4, 2] + ifelse(i %% 2, 0.05, 0.115)
  segments((xx[i] + xx[i + 1]) / 2, ry[4, 2], (xx[i] + xx[i + 1]) / 2,
           yy - 0.028, col = "#999999")
  text((xx[i] + xx[i + 1]) / 2, yy,
       paste0(lab[i], "\n", n(c(top$ballots, RREST)[i]), "  (", pc(pct[i], 0),
              "%)"), cex = 0.6)
}
mtext(paste0("Widths are exact shares of the ", n(SENT),
             " ballots transmitted. Every total is a floor: the four items ",
             "come from between ", pc(min(nat$pct_reporting), 0), "% and ",
             pc(max(nat$pct_reporting), 0), "% of jurisdictions."),
      side = 1, line = 1.1, cex = 0.6, col = "#666666")

## ---- journey-d3
rsegs <- paste0("[", paste(sprintf('{"l":"%s","v":%d,"p":"%s","n":"%s"}',
  c(top$reason, "all other stated reasons"), c(top$ballots, RREST),
  pc(100 * c(top$ballots, RREST) / REJ, 0),
  n(c(top$ballots, RREST))), collapse = ","), "]")
cat(paste0('
<div id="jrn" style="margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const SENT=', SENT, ',RET=', RET, ',NORET=', NORET, ',CNT=', CNT, ',REJ=', REJ, ';
const LBL={sent:"', n(SENT), '",ret:"', n(RET), '",noret:"', n(NORET),
  '",cnt:"', n(CNT), '",rej:"', n(REJ), '"};
const RS=', rsegs, ';
const W=760,H=430,M={l:8,r:8};
const svg=d3.select("#jrn").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([0,SENT]).range([M.l,W-M.r]);
const RY=[[18,54],[104,140],[190,226],[330,366]];
const C={sent:"#cfe3f2",ret:"#2c7fb8",noret:"#D9D9D9",rej:"#C41230"};
const bar=(r,a,b,c)=>svg.append("rect").attr("x",x(a)).attr("y",RY[r][0])
  .attr("width",x(b)-x(a)).attr("height",RY[r][1]-RY[r][0]).attr("fill",c)
  .attr("stroke","#fff");
// on-mark: lab() only ever writes inside a bar segment drawn by bar(), so the
// colour is chosen against that segment and must not follow the page.
const lab=(r,a,b,t,c)=>svg.append("text").attr("x",(x(a)+x(b))/2)
  .attr("y",(RY[r][0]+RY[r][1])/2+4).attr("text-anchor","middle")
  .attr("class","on-mark")
  .attr("font-size","12px").attr("fill",c).text(t);
bar(0,0,SENT,C.sent); lab(0,0,SENT,"transmitted to voters   "+LBL.sent,"#333");
bar(1,0,RET,C.ret); bar(1,RET,SENT,C.noret);
lab(1,0,RET,"returned by voters   "+LBL.ret,"#fff");
lab(1,RET,SENT,"never returned   "+LBL.noret,"#333");
bar(2,0,CNT,C.ret); bar(2,CNT,RET,C.rej); bar(2,RET,SENT,C.noret);
lab(2,0,CNT,"counted   "+LBL.cnt,"#fff");
lab(2,RET,SENT,"never returned   "+LBL.noret,"#777");
svg.append("path").attr("d",`M${x(CNT)},${RY[2][1]} L${x(RET)},${RY[2][1]}
  L${W-M.r},${RY[3][0]} L${M.l},${RY[3][0]} Z`)
  .attr("fill","#C41230").attr("opacity",0.13);
svg.append("text").attr("x",W/2).attr("y",RY[3][0]-24).attr("text-anchor","middle")
  .attr("font-size","12.5px").attr("fill","#C41230")
  .text("rejected: "+LBL.rej+" ballots, ', pc(MRATE), '% of those returned — magnified about ',
  pc(MAG, 0), ' times");
const cap=d3.select("#jrn").append("p")
  .attr("style","font-size:0.85em;color:#555;min-height:2.6em;margin-top:0.3em");
let acc=0;
RS.forEach((d,i)=>{
  const a=acc,b=acc+d.v; acc=b;
  const xa=M.l+(W-M.l-M.r)*a/REJ, xb=M.l+(W-M.l-M.r)*b/REJ;
  svg.append("rect").attr("x",xa).attr("y",RY[3][0]).attr("width",xb-xa)
    .attr("height",RY[3][1]-RY[3][0]).attr("fill",i?"#e8918a":"#C41230")
    .attr("stroke","#fff").style("cursor","pointer")
    .on("mousemove",()=>cap.html("<b>"+d.l+"</b>: "+d.n+" ballots, "+d.p+
      "% of every mail ballot rejected in the country."))
    .on("mouseleave",()=>cap.html("<b>Hover the bottom bar.</b> The red block is a handwriting judgment."));
  const cx=(xa+xb)/2, yy=RY[3][1]+(i%2?18:44);
  svg.append("line").attr("x1",cx).attr("x2",cx).attr("y1",RY[3][1])
    .attr("y2",yy-11).attr("stroke","#bbb");
  svg.append("text").attr("x",cx).attr("y",yy).attr("text-anchor","middle")
    .attr("font-size","10.5px").attr("fill","#444").text(d.l);
  svg.append("text").attr("x",cx).attr("y",yy+12).attr("text-anchor","middle")
    .attr("font-size","10.5px").attr("fill","#777").text(d.n+"  ("+d.p+"%)");
});
cap.html("<b>Hover the bottom bar.</b> The red block is a handwriting judgment.");
})();
</script>
'))

## ---- by-state
o <- rbind(head(big[order(-big$rate), ], 5), head(big[order(big$rate), ], 4))
o <- o[, c("state", "mail_returned", "mail_rejected", "rate")]
o$mail_returned <- n(o$mail_returned); o$mail_rejected <- n(o$mail_rejected)
o$rate <- pc(o$rate)
names(o) <- c("state", "mail ballots returned", "rejected", "rate (%)")
o

## ---- d3-states
b <- big[order(-big$rate), ]
rows <- paste(sprintf('{"s":"%s","v":%.2f,"n":%d}', b$state, b$rate,
                      b$mail_rejected), collapse = ",")
cat(sprintf('
<div id="eav" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const d=[%s], nat=%s;
const W=760,H=380,M={t:18,r:20,b:44,l:52};
const svg=d3.select("#eav").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleBand().domain(d.map(q=>q.s)).range([M.l,W-M.r]).padding(0.18);
const y=d3.scaleLinear().domain([0,d3.max(d,q=>q.v)*1.1]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x)).selectAll("text").attr("font-size","9px")
  .attr("transform","rotate(-60)").attr("text-anchor","end").attr("dy","0.1em").attr("dx","-0.4em");
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).ticks(6).tickFormat(v=>v+"%%"));
svg.append("text").attr("x",M.l).attr("y",M.t-4).attr("font-size","11px")
  .attr("fill","#666").text("%% of returned mail ballots rejected");
const tip=d3.select("#eav").append("div").attr("style",
 "position:absolute;pointer-events:none;background:#111;color:#fff;padding:6px 9px;border-radius:4px;font-size:12px;opacity:0;white-space:nowrap");
svg.append("g").selectAll("rect").data(d).join("rect")
  .attr("x",q=>x(q.s)).attr("y",q=>y(q.v)).attr("width",x.bandwidth())
  .attr("height",q=>y(0)-y(q.v)).attr("rx",1)
  .attr("fill",q=>q.v>2?"#C41230":"#2c7fb8")
  .on("mousemove",function(e,q){ d3.select(this).attr("opacity",0.7);
    tip.style("opacity",1).html(`<b>${q.s} &mdash; ${q.v.toFixed(2)}%%</b><br>${d3.format(",")(q.n)} ballots rejected`)
      .style("left",Math.min(e.offsetX+12,W-190)+"px").style("top",(e.offsetY-6)+"px"); })
  .on("mouseleave",function(){ d3.select(this).attr("opacity",1); tip.style("opacity",0); });
svg.append("line").attr("x1",M.l).attr("x2",W-M.r).attr("y1",y(nat)).attr("y2",y(nat))
  .attr("stroke","#333").attr("stroke-dasharray","4 3");
svg.append("text").attr("x",W-M.r).attr("y",y(nat)-5).attr("text-anchor","end")
  .attr("font-size","11px").attr("fill","#333").text(`national ${nat.toFixed(2)}%%`);
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
States handling more than 100,000 returned mail ballots. Hover for counts.
%d states and territories are missing from this chart entirely.</p>
', rows, pc(MRATE), nrow(hole)))

## ---- state-static
b <- big[order(-big$rate), ]
par(mar = c(4, 4.2, 1, 1))
barplot(b$rate, names.arg = b$state, las = 2, cex.names = 0.55,
        col = ifelse(b$rate > 2, "#C41230", "#2c7fb8"),
        ylab = "% of returned mail ballots rejected")
abline(h = MRATE, lty = 2)
mtext(paste0("States handling more than 100,000 returned mail ballots. The ",
             "dashed line is the national ", pc(MRATE), "%. ", nrow(hole),
             " states and territories are missing from this chart entirely."),
      side = 1, line = 3.4, cex = 0.6, col = "#666666")

## ---- reasons
o <- head(rsn[, c("reason", "ballots", "pct_of_rejected")], 6)
o$ballots <- n(o$ballots)
names(o) <- c("reason recorded", "ballots", "% of all rejections")
o

## ---- reason-check
data.frame(
  quantity = c("Stated national total rejected", "Sum of the stated reasons",
               "Unexplained"),
  value = c(n(g("C9a")), n(sum(rsn$ballots)), n(g("C9a") - sum(rsn$ballots))))

## ---- reporting
o <- rsn[order(-rsn$reported_by), c("reason", "reported_by", "ballots")]
o <- rbind(head(o, 3), tail(o, 3))
o$reported_by <- n(o$reported_by); o$ballots <- n(o$ballots)
names(o) <- c("reason recorded", "jurisdictions reporting it", "ballots")
o

## ---- reasons-static
par(mar = c(6.4, 5.2, 0.6, 1.2))
plot(rs$cov, rs$ballots, log = "y", pch = 19, cex = 1.4, col = GCOL[rs$grp],
     bty = "n", las = 1, xlim = c(8, 102), ylim = c(90, 4e5), axes = FALSE,
     xlab = "", ylab = "")
axis(1, at = seq(20, 100, 20), labels = paste0(seq(20, 100, 20), "%"))
tk <- c(100, 1000, 10000, 1e5)
axis(2, at = tk, labels = c("100", "1,000", "10,000", "100,000"), las = 1)
mtext("ballots rejected for this reason (log scale)", side = 2, line = 3.8,
      cex = 0.8)
mtext("% of the country's jurisdictions that reported this reason", side = 1,
      line = 2.4, cex = 0.8)
k <- rs$grp != "other"
text(rs$cov[k], rs$ballots[k], rs$short[k],
     pos = ifelse(rs$cov[k] > 70, 2, 4), cex = 0.66, col = GCOL[rs$grp[k]])
legend("topleft", bty = "n", cex = 0.64, pch = 19,
       col = c(GCOL[["sig"]], GCOL[["auto"]], GCOL[["judge"]], GCOL[["other"]]),
       legend = c("signature mismatch: the largest reason of all",
                  "falls out of the system automatically",
                  "someone had to write down a judgment",
                  paste("the other", sum(!k), "stated reasons")))
mtext(paste0("The three automatic reasons are reported by ", pc(COVA, 0),
             "% of jurisdictions on average; the three judgment reasons by ",
             pc(COVJ, 0), "%."),
      side = 1, line = 4.0, cex = 0.58, col = "#666666")
mtext(paste0("Signature mismatch, the largest category of all, sits at ",
             pc(rs$cov[1], 0), "%. The gray dots are the remaining ", sum(!k),
             " reasons, named in the table above."),
      side = 1, line = 4.8, cex = 0.58, col = "#666666")

## ---- reasons-d3
rows <- paste(sprintf('{"s":"%s","x":%.2f,"y":%d,"g":"%s","n":"%s","j":"%s"}',
                      rs$short, rs$cov, rs$ballots, rs$grp, n(rs$ballots),
                      n(rs$reported_by)), collapse = ",")
cat(paste0('
<div id="rsn" style="position:relative;margin:1em 0"></div>
<script>
(function(){
const D=[', rows, '];
const C={sig:"', GCOL[["sig"]], '",auto:"', GCOL[["auto"]], '",judge:"',
  GCOL[["judge"]], '",other:"', GCOL[["other"]], '"};
const W=760,H=400,M={t:16,r:26,b:52,l:74};
const svg=d3.select("#rsn").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const x=d3.scaleLinear().domain([8,102]).range([M.l,W-M.r]);
const y=d3.scaleLog().domain([90,4e5]).range([H-M.b,M.t]);
svg.append("g").attr("transform",`translate(0,${H-M.b})`)
  .call(d3.axisBottom(x).ticks(6).tickFormat(v=>v+"%"));
svg.append("g").attr("transform",`translate(${M.l},0)`)
  .call(d3.axisLeft(y).tickValues([100,1000,10000,100000])
    .tickFormat(d3.format(",")));
svg.append("text").attr("x",(W-M.r+M.l)/2).attr("y",H-16).attr("text-anchor","middle")
  .attr("font-size","12px").attr("fill","#444")
  .text("% of the country’s jurisdictions that reported this reason");
svg.append("text").attr("transform","rotate(-90)").attr("x",-(H-M.b+M.t)/2)
  .attr("y",16).attr("text-anchor","middle").attr("font-size","12px")
  .attr("fill","#444").text("ballots rejected for this reason (log scale)");
const cap=d3.select("#rsn").append("p")
  .attr("style","font-size:0.85em;color:#555;min-height:2.6em;margin-top:0.3em");
svg.append("g").selectAll("circle").data(D).join("circle")
  .attr("cx",d=>x(d.x)).attr("cy",d=>y(d.y)).attr("r",6)
  .attr("fill",d=>C[d.g]).style("cursor","pointer")
  .on("mousemove",(e,d)=>cap.html("<b>"+d.s+"</b>: "+d.n+" ballots, reported by "+
    d.j+" jurisdictions."))
  .on("mouseleave",()=>cap.html("<b>Hover a dot.</b> Up the page is how many. Across is how well we know."));
svg.append("g").selectAll("text").data(D.filter(d=>d.g!=="other")).join("text")
  .attr("x",d=>d.x>70?x(d.x)-10:x(d.x)+10).attr("y",d=>y(d.y)+4)
  .attr("text-anchor",d=>d.x>70?"end":"start").attr("font-size","11px")
  .attr("fill",d=>C[d.g]).text(d=>d.s);
const lg=svg.append("g").attr("transform",`translate(${M.l+10},${M.t+6})`);
[["sig","signature mismatch: the largest reason of all"],
 ["auto","falls out of the system automatically"],
 ["judge","someone had to write down a judgment"],
 ["other","the other ', sum(rs$grp == 'other'), ' stated reasons (hover for names)"]].forEach((r,i)=>{
  lg.append("circle").attr("cx",5).attr("cy",i*16).attr("r",5).attr("fill",C[r[0]]);
  lg.append("text").attr("x",16).attr("y",i*16+4).attr("font-size","11px")
    .attr("fill","#333").text(r[1]);});
cap.html("<b>Hover a dot.</b> Up the page is how many. Across is how well we know.");
})();
</script>
'))

## ---- denominator-gap
data.frame(
  item = c("Mail ballots rejected (C9a) — the numerator",
           "Mail ballots returned (C1b) — the denominator",
           "Jurisdictions reporting a rejection count but no returned count"),
  jurisdictions = c(n(rb("C9a")), n(rb("C1b")), n(NOJOIN)),
  `% of the country` = c(paste0(pc(pr("C9a"), 1), "%"),
                         paste0(pc(pr("C1b"), 1), "%"),
                         paste0(pc(100 * NOJOIN / JUR, 1), "%")),
  check.names = FALSE)

## ---- hole
o <- hole[, c("state", "voters", "mail_sent", "mail_returned", "mail_rejected")]
o$voters <- n(o$voters); o$mail_sent <- n(o$mail_sent)
o$mail_returned <- n(o$mail_returned); o$mail_rejected <- n(o$mail_rejected)
names(o) <- c("state", "registered voters", "mail ballots sent",
              "returned", "rejected")
o

## ---- prov
data.frame(
  quantity = c("Provisional ballots cast", "Counted in full", "Rejected",
               "Rejection rate", "Reported by"),
  value = c(n(g("E1a")), n(g("E1b")), n(g("E1d")),
            paste0(pc(PRATE, 1), "%"), paste0(pc(pr("E1d"), 1), "% of jurisdictions")))

## ---- prov-state
o <- rbind(head(pv[order(-pv$prate), ], 4), head(pv[order(pv$prate), ], 3))
o <- o[, c("state", "prov_cast", "prov_rejected", "prate")]
o$prov_cast <- n(o$prov_cast); o$prov_rejected <- n(o$prov_rejected)
o$prate <- pc(o$prate, 1)
names(o) <- c("state", "provisional cast", "rejected", "rate (%)")
o

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#jrn text[fill="#333" i],
#jrn text[fill="#c41230" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))

## ---- on-mark-halo
# A label lying across a saturated or mid-toned mark, where neither the
# authored colour nor the lifted one reaches 3:1. Recolouring cannot fix a
# label the same colour as the thing it labels, so these get a halo instead:
# paint-order draws a --paper outline behind the glyph. It is invisible where
# the text sits on the page, so scoping by figure and fill is safe.
# Sites found by _lib/check-contrast.js.
# The light-only block: the on-mark chunk pins that fill for the dark page,
# so a --paper stroke there would sit dark behind a dark ink, and the checker
# scores the fill against the stroke it touches.
cat('<style>
#jrn text[fill="#777" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
@media (prefers-color-scheme: light) {
#jrn text[fill="#c41230" i]
  { paint-order:stroke; stroke:var(--paper); stroke-width:3px;
    stroke-linejoin:round; }
}
</style>')
