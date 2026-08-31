# cast-vote-records-code.R -- chunk bodies for cast-vote-records-brief.Rmd
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

s    <- read.csv("data/derived/sequences.csv",          stringsAsFactors = FALSE)
off  <- read.csv("data/derived/official_rounds.csv",    stringsAsFactors = FALSE)
tr   <- read.csv("data/derived/official_transfers.csv", stringsAsFactors = FALSE)

cols <- grep("^rank", names(s), value = TRUE)
ru   <- rowSums(s[, cols] != "")
BAL  <- sum(s$ballots)
SEQ  <- nrow(s)
BLANK <- sum(s$ballots[ru == 0])
ONE   <- sum(s$ballots[ru == 1])
VOTED <- BAL - BLANK
BULLET <- 100 * ONE / VOTED

MAJ <- c("Begich, Nick", "Peltola, Mary S.")
CAND <- c(MAJ, "Howe, John Wayne", "Hafner, Eric")

# --- our own instant-runoff tabulation, from the sequences ------------------
irv <- function(dat, eliminated = character(0)) {
  pick <- rep(NA_character_, nrow(dat))
  for (i in seq_len(nrow(dat))) {
    for (cl in cols) {
      v <- dat[i, cl]
      if (v == "") next
      if (v == "OVERVOTE") break
      if (v %in% eliminated) next
      pick[i] <- v; break
    }
  }
  tapply(dat$ballots, pick, sum)
}
elim <- character(0); rounds <- list()
repeat {
  cnt <- sort(irv(s, elim), decreasing = TRUE)
  rounds[[length(rounds) + 1]] <- cnt
  if (cnt[1] > sum(cnt) / 2 || length(cnt) <= 2) break
  elim <- c(elim, names(cnt)[length(cnt)])
}
fin <- rounds[[length(rounds)]]
ACTIVE1 <- sum(rounds[[1]])
ACTIVEF <- sum(fin)
WINPCT  <- 100 * fin[1] / ACTIVEF

offf <- off[off$round == max(off$round), ]
offf <- offf[order(-offf$votes), ]
GAP  <- ACTIVEF - sum(offf$votes)

# --- pairs, crossover, duplicates ------------------------------------------
pairs <- aggregate(ballots ~ rank1 + rank2, data = s[s$rank2 != "", ], sum)
pairs <- pairs[order(-pairs$ballots), ]
cross <- pairs[pairs$rank1 %in% MAJ & pairs$rank2 %in% MAJ &
               pairs$rank1 != pairs$rank2, ]
dup   <- pairs[pairs$rank1 == pairs$rank2, ]

# --- bullet voting by first choice -----------------------------------------
bul <- data.frame(candidate = CAND, stringsAsFactors = FALSE)
bul$voters <- sapply(CAND, function(c_) sum(s$ballots[s$rank1 == c_]))
bul$bullet <- sapply(CAND, function(c_)
  100 * sum(s$ballots[s$rank1 == c_ & ru == 1]) / sum(s$ballots[s$rank1 == c_]))
bul <- bul[order(-bul$voters), ]

EXH <- tr$ballots[tr$eliminated == "Howe, John Wayne" & tr$transferred_to == "Exhausted"]
HOWE <- sum(tr$ballots[tr$eliminated == "Howe, John Wayne"])

# --- the ballot itself: the two sequences drawn in Figure 2 ------------------
RK    <- c("rank1", "rank2", "rank3", "rank4")
b_one <- s[order(-s$ballots), ][1, ]
s4    <- s[ru == 4, ]
b_all <- s4[which.max(s4$ballots), ]
marks <- function(row) {
  m <- matrix(FALSE, length(CAND), length(RK), dimnames = list(CAND, RK))
  for (j in seq_along(RK)) {
    v <- row[[RK[j]]]
    if (!is.na(v) && v != "" && v %in% CAND) m[v, j] <- TRUE
  }
  m
}
M_one <- marks(b_one); M_all <- marks(b_all)
csvrow <- function(row) {
  v <- vapply(RK, function(k) {
    x <- row[[k]]; if (is.na(x) || x == "") "" else paste0('"', x, '"')
  }, character(1))
  f <- c(format(row$ballots, big.mark = ",", trim = TRUE), v)
  one <- paste(f, collapse = ",")
  if (nchar(one) <= 36) c(one, "") else
    c(paste0(paste(f[1:3], collapse = ","), ","), paste(f[4:5], collapse = ","))
}

# --- the official count as a flow: rounds, transfers, and what leaves -------
OUTN <- "Left the count"
rd   <- lapply(split(off, off$round), function(d) setNames(d$votes, d$candidate))
R1   <- rd[["Round 1"]]; R2 <- rd[["Round 2"]]; R3 <- rd[["Round 3"]]
TOT  <- sum(R1)
e1   <- setdiff(names(R1), names(R2))
e2   <- setdiff(names(R2), names(R3))
ORD  <- c(names(sort(R1, decreasing = TRUE)), OUTN)
dst  <- function(x) ifelse(x %in% c("Exhausted", "Overvotes"), OUTN, x)
t1   <- tr[tr$eliminated == e1, ]; t2 <- tr[tr$eliminated == e2, ]
OUT2 <- sum(t1$ballots[dst(t1$transferred_to) == OUTN])
OUT3 <- OUT2 + sum(t2$ballots[dst(t2$transferred_to) == OUTN])
OVR  <- sum(tr$ballots[tr$transferred_to == "Overvotes"])

stay <- function(keep, vals) data.frame(from = keep, to = keep,
                                        v = as.numeric(vals[keep]),
                                        stringsAsFactors = FALSE)
rib1 <- rbind(stay(setdiff(names(R1), e1), R1),
              data.frame(from = e1, to = dst(t1$transferred_to), v = t1$ballots,
                         stringsAsFactors = FALSE))
rib2 <- rbind(stay(setdiff(names(R2), e2), R2),
              data.frame(from = e2, to = dst(t2$transferred_to), v = t2$ballots,
                         stringsAsFactors = FALSE),
              data.frame(from = OUTN, to = OUTN, v = OUT2,
                         stringsAsFactors = FALSE))
rib1 <- aggregate(v ~ from + to, rib1, sum)
rib2 <- aggregate(v ~ from + to, rib2, sum)

GAP  <- 0.035
npos <- function(vals) {
  vals <- vals[ORD[ORD %in% names(vals)]]
  h <- as.numeric(vals) / TOT
  y0 <- cumsum(c(0, head(h + GAP, -1)))
  data.frame(node = names(vals), y0 = y0, y1 = y0 + h, v = as.numeric(vals),
             stringsAsFactors = FALSE)
}
P1 <- npos(R1)
P2 <- npos(c(R2, setNames(OUT2, OUTN)))
P3 <- npos(c(R3, setNames(OUT3, OUTN)))

geom <- function(rib, P, Q) {
  rib <- rib[order(match(rib$from, ORD), match(rib$to, ORD)), ]
  rib$sy0 <- NA_real_; rib$sy1 <- NA_real_
  for (nd in unique(rib$from)) {
    k  <- which(rib$from == nd)
    cs <- P$y0[P$node == nd] + cumsum(c(0, rib$v[k] / TOT))
    rib$sy0[k] <- head(cs, -1); rib$sy1[k] <- tail(cs, -1)
  }
  rib <- rib[order(match(rib$to, ORD), match(rib$from, ORD)), ]
  rib$ty0 <- NA_real_; rib$ty1 <- NA_real_
  for (nd in unique(rib$to)) {
    k  <- which(rib$to == nd)
    cs <- Q$y0[Q$node == nd] + cumsum(c(0, rib$v[k] / TOT))
    rib$ty0[k] <- head(cs, -1); rib$ty1[k] <- tail(cs, -1)
  }
  rib[order(-rib$v), ]
}
G1 <- geom(rib1, P1, P2); G2 <- geom(rib2, P2, P3)
FCOL <- c("#54278F", "#e6550d", "#4d9221", "#8c564b", "#9e9e9e")
names(FCOL) <- ORD
sname <- function(x) ifelse(x == OUTN, x, sub(",.*", "", x))

pc <- function(x, k = 1) formatC(x, format = "f", digits = k)
n  <- function(x) format(round(x), big.mark = ",", trim = TRUE)

# ---- render every data.frame in this document as a TABLE, not code output ----
knit_print.data.frame <- function(x, ...) {
  n <- names(x); n <- gsub("_", " ", n)
  n <- sub("^(.)", "\\U\\1", n, perl = TRUE)
  knitr::knit_print(knitr::kable(x, col.names = n, row.names = FALSE,
                                 align = table_align(x)), ...)
}
registerS3method("knit_print", "data.frame", knit_print.data.frame,
                 envir = asNamespace("knitr"))

## ---- ballot-photo
knitr::include_graphics("../../_lib/assets/alaska-rcv-sample-ballot.png")

## ---- one-row
o <- head(s[order(-s$ballots), ], 5)
o$ballots <- n(o$ballots)
o <- o[, c("ballots", "rank1", "rank2", "rank3")]
names(o) <- c("ballots", "1st ranking", "2nd ranking", "3rd ranking")
o

## ---- ballot-static
oval <- function(cx, cy, rx, ry, fill) {
  a <- seq(0, 2 * pi, length.out = 48)
  polygon(cx + rx * cos(a), cy + ry * sin(a), col = fill, border = "#555555")
}
panel <- function(x0, M, cnt, row, head) {
  w <- 46; nm <- 21
  text(x0, 3, head, pos = 4, cex = 0.78, font = 2)
  rect(x0, 6, x0 + w, 44, border = "#888888")
  cx <- x0 + nm + c(4, 10, 16, 22)
  text(cx, 12, c("1st", "2nd", "3rd", "4th"), cex = 0.62, col = "#444444")
  for (i in seq_len(nrow(M))) {
    yy <- 18 + (i - 1) * 6.4
    text(x0 + 2, yy, rownames(M)[i], pos = 4, cex = 0.6)
    for (j in 1:4) oval(cx[j], yy, 2.1, 1.5,
                        if (M[i, j]) "#C41230" else "white")
  }
  arrows(x0 + w / 2, 46, x0 + w / 2, 53, length = 0.06, col = "#888888")
  text(x0 + w / 2, 57, "the record it becomes", cex = 0.62, col = "#666666")
  cl <- csvrow(row)
  text(x0 + w / 2, 62, cl[1], cex = 0.55, family = "mono")
  if (nzchar(cl[2])) text(x0 + w / 2, 65.6, cl[2], cex = 0.55, family = "mono")
  text(x0 + w / 2, 71, paste(n(cnt), "ballots looked exactly like this"),
       cex = 0.66, col = "#C41230")
}
par(mar = c(0.2, 0.2, 0.2, 0.2))
plot(NA, xlim = c(0, 100), ylim = c(74, 0), axes = FALSE, xlab = "", ylab = "",
     xaxs = "i", yaxs = "i")
panel(2,  M_one, b_one$ballots, b_one, "One ranking used")
panel(52, M_all, b_all$ballots, b_all, "All four rankings used")

## ---- ballot-d3
cell <- function(M) paste(apply(which(M, arr.ind = TRUE), 1, function(k)
  paste0("[", k[1] - 1, ",", k[2] - 1, "]")), collapse = ",")
cat(paste0('
<div id="bal" style="margin:1em 0"></div>
<script src="../../_lib/d3.v7.min.js"></script>
<script>
(function(){
const NM=', paste0("[", paste(sprintf('"%s"', CAND), collapse = ","), "]"), ';
const P=[{h:"One ranking used",m:[', cell(M_one), '],r:[\'',
  csvrow(b_one)[1], '\',\'', csvrow(b_one)[2], '\'],c:"', n(b_one$ballots), '"},
         {h:"All four rankings used",m:[', cell(M_all), '],r:[\'',
  csvrow(b_all)[1], '\',\'', csvrow(b_all)[2], '\'],c:"', n(b_all$ballots), '"}];
const W=760,H=300,PW=360;
const svg=d3.select("#bal").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
P.forEach((p,pi)=>{
  const x0=pi*(PW+40)+6;
  svg.append("text").attr("x",x0).attr("y",14).attr("font-weight","600").text(p.h);
  svg.append("rect").attr("x",x0).attr("y",24).attr("width",PW).attr("height",152)
    .attr("fill","#fff").attr("stroke","#999").attr("rx",3);
  const cx=j=>x0+186+j*44;
  // on-mark: everything from here to the bottom of the card is printed ON the
  // white ballot above, so it stays dark when the page does not. The heading
  // and the caption below the card are on the page and are left to follow it.
  ["1st","2nd","3rd","4th"].forEach((t,j)=>
    svg.append("text").attr("x",cx(j)).attr("y",48).attr("text-anchor","middle")
      .attr("class","on-mark")
      .attr("font-size","11px").attr("fill","#555").text(t));
  NM.forEach((nm,i)=>{
    const y=72+i*26;
    svg.append("text").attr("x",x0+14).attr("y",y+4).attr("font-size","12px")
      .attr("class","on-mark").text(nm);
    d3.range(4).forEach(j=>{
      svg.append("ellipse").attr("cx",cx(j)).attr("cy",y).attr("rx",13).attr("ry",8)
        .attr("fill","#fff").attr("stroke","#666");
    });
  });
  p.m.forEach(q=>{
    svg.append("ellipse").attr("cx",cx(q[1])).attr("cy",72+q[0]*26).attr("rx",13)
      .attr("ry",8).attr("fill","#C41230").attr("stroke","#666")
      .attr("opacity",0).transition().delay(400+pi*300).duration(500).attr("opacity",1);
  });
  svg.append("path").attr("d",`M${x0+PW/2},180 L${x0+PW/2},200`)
    .attr("stroke","#999").attr("marker-end","url(#ah)");
  svg.append("text").attr("x",x0+PW/2).attr("y",216).attr("text-anchor","middle")
    .attr("font-size","11px").attr("fill","#666").text("the record it becomes");
  p.r.filter(t=>t.length).forEach((t,k)=>
    svg.append("text").attr("x",x0+PW/2).attr("y",238+k*17).attr("text-anchor","middle")
      .attr("font-size","12px").attr("font-family","ui-monospace,Menlo,monospace")
      .text(t));
  svg.append("text").attr("x",x0+PW/2).attr("y",276).attr("text-anchor","middle")
    .attr("font-size","12px").attr("fill","#C41230")
    .text(p.c+" ballots looked exactly like this");
});
const df=svg.append("defs").append("marker").attr("id","ah").attr("viewBox","0 0 8 8")
  .attr("refX",7).attr("refY",4).attr("markerWidth",6).attr("markerHeight",6)
  .attr("orient","auto");
df.append("path").attr("d","M0,0 L8,4 L0,8 Z").attr("fill","#999");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
The ballot on the left is the one ', n(b_one$ballots), ' Alaskans cast \u2014 the
single commonest piece of paper in the election. Three of its four columns are
empty, and that is what the rest of this chapter is about.</p>
'))

## ---- cvr-envelope
L   <- readLines("data/raw/cvr-session-example.json", warn = FALSE)
shp <- read.csv("data/raw/export-shape.csv", stringsAsFactors = FALSE)
SH  <- function(k) shp$value[shp$name == k]
ncon <- length(grep('^            "Id": ', L))     # contests on this card

# the dictionary that turns a CandidateId into a person
cm <- local({
  x   <- paste(readLines("data/raw/CandidateManifest.json", warn = FALSE),
               collapse = "")
  rec <- regmatches(x, gregexpr("\\{[^{}]*\\}", x))[[1]]
  data.frame(id   = as.integer(sub('.*"Id":([0-9]+).*', "\\1", rec)),
             cid  = as.integer(sub('.*"ContestId":([0-9]+).*', "\\1", rec)),
             name = sub('.*"Description":"([^"]*)".*', "\\1", rec),
             stringsAsFactors = FALSE)
})

# The record is nested JSON, and the nesting is part of what it says: a
# session contains a card, a card contains contests. Flattening to dotted
# paths keeps that while making each field readable on its own line.
jpath <- function(block) {
  keys <- character(0); vals <- character(0); stack <- character(0)
  for (ln in block) {
    m <- regmatches(ln, regexec('^\\s*"([^"]+)"\\s*:\\s*(.*?),?\\s*$', ln))[[1]]
    if (length(m)) {
      k <- m[2]; v <- trimws(m[3])
      if (v %in% c("{", "[", "[{")) { stack <- c(stack, k); next }
      v <- sub('^"', "", sub('"$', "", v))
      keys <- c(keys, paste(c(stack, k), collapse = "."))
      vals <- c(vals, v)
    } else if (grepl("^\\s*[]}]", ln) && length(stack)) {
      stack <- stack[-length(stack)]
    }
  }
  data.frame(Field = keys, Value = vals, stringsAsFactors = FALSE)
}
jp <- jpath(L[1:14])
jp <- rbind(jp, data.frame(
  Field = "…",
  Value = paste0(ncon, " contests on this ballot, one after another"),
  stringsAsFactors = FALSE))
jp

## ---- cvr-contest
i0 <- grep('"Id": 7,', L, fixed = TRUE)[1]
nb <- function(s, p) lengths(regmatches(s, gregexpr(p, s)))
i1 <- i0 - 1L; depth <- 0L
repeat {
  depth <- depth + nb(L[i1], "[{[]") - nb(L[i1], "[]}]")
  if (depth <= 0L || i1 >= length(L)) break
  i1 <- i1 + 1L
}
blk <- sub("^      ", "", L[(i0 - 1L):i1])
jpath(blk)

## ---- cvr-lookup
mk  <- grep("CandidateId", blk, value = TRUE)
ids <- as.integer(sub('.*"CandidateId": ([0-9]+).*', "\\1", mk))
rks <- as.integer(sub('.*"Rank": ([0-9]+).*', "\\1",
                      grep('"Rank"', blk, value = TRUE)))
data.frame(rank = rks, CandidateId = ids,
           `who that is` = cm$name[match(ids, cm$id)], check.names = FALSE)

## ---- cvr-clean
want <- c(cm$name[match(ids, cm$id)],
          rep("", length(cols) - length(ids)))
o <- s[apply(s[, cols], 1, function(r) identical(unname(r), want)), ]
stopifnot(nrow(o) == 1)
o <- o[, c("ballots", "rank1", "rank2", "rank3")]
names(o) <- c("ballots", "1st ranking", "2nd ranking", "3rd ranking")
o

## ---- shape
data.frame(
  quantity = c("Ballots in the cast vote record",
               "Distinct ranking sequences any Alaskan produced",
               "Ballots per distinct sequence, on average",
               "Sequences used by exactly one ballot"),
  value = c(n(BAL), n(SEQ), n(BAL / SEQ), n(sum(s$ballots == 1))))

## ---- ranks
o <- data.frame(rankings_used = names(tapply(s$ballots, ru, sum)),
                ballots = as.vector(tapply(s$ballots, ru, sum)),
                stringsAsFactors = FALSE)
o$share <- pc(100 * o$ballots / BAL)
o$ballots <- n(o$ballots)
names(o) <- c("candidates ranked", "ballots", "% of all ballots")
o

## ---- our-count
o <- data.frame(round = seq_along(rounds),
                continuing = sapply(rounds, function(r) n(sum(r))),
                leader = sapply(rounds, function(r) names(r)[1]),
                leader_pct = sapply(rounds, function(r) pc(100*r[1]/sum(r), 2)),
                eliminated = c(sapply(rounds[-length(rounds)],
                                      function(r) names(r)[length(r)]), "—"),
                stringsAsFactors = FALSE)
names(o) <- c("round", "continuing ballots", "leader", "leader's %",
              "eliminated after this round")
o

## ---- official
o <- offf
o$votes <- n(o$votes)
names(o) <- c("round", "candidate", "official votes", "official %")
o

## ---- gap
data.frame(
  quantity = c("Ballots continuing in our final round",
               "Ballots continuing in the state's final round",
               "Difference", "As a share of the count"),
  value = c(n(ACTIVEF), n(sum(offf$votes)), n(GAP),
            paste0(pc(100 * GAP / ACTIVEF, 3), "%")))

## ---- transfers
o <- tr
o$ballots <- n(o$ballots)
names(o) <- c("eliminated candidate", "transferred to", "ballots")
o

## ---- flow-static
XS <- c(0.02, 0.5, 0.98); BW <- 0.02
ribbon <- function(g, xs, xt) {
  tt <- seq(0, 1, length.out = 40); ww <- tt * tt * (3 - 2 * tt)
  for (i in seq_len(nrow(g))) {
    xx <- xs + (xt - xs) * tt
    tp <- g$sy0[i] + (g$ty0[i] - g$sy0[i]) * ww
    bt <- g$sy1[i] + (g$ty1[i] - g$sy1[i]) * ww
    polygon(c(xx, rev(xx)), c(tp, rev(bt)),
            col = paste0(FCOL[[g$from[i]]], "77"), border = NA)
  }
}
nodes <- function(P, x, side) {
  rect(x - BW / 2, P$y0, x + BW / 2, P$y1, col = FCOL[P$node], border = NA)
  lab <- paste0(sname(P$node), "  ", n(P$v))
  if (side == 3) {
    yy <- P$y0 - 0.004
    for (i in seq_along(lab)) {
      w <- abs(strwidth(lab[i], cex = 0.62))
      h <- abs(strheight(lab[i], cex = 0.62))
      rect(x - w / 2 - 0.012, yy[i] - 2.1 * h, x + w / 2 + 0.012, yy[i] - 0.05 * h,
           col = "white", border = NA)
    }
    text(x, yy, lab, pos = 3, cex = 0.62, col = "#333333", xpd = NA)
  }
  else text(x + ifelse(side == 4, BW, -BW), (P$y0 + P$y1) / 2, lab, pos = side,
            cex = 0.62, col = "#333333", xpd = NA)
}
par(mar = c(1.4, 5.2, 2.6, 5.2))
plot(NA, xlim = c(0, 1), ylim = c(max(P1$y1, P2$y1, P3$y1) + 0.01, -0.05),
     axes = FALSE, xlab = "", ylab = "", xaxs = "i")
ribbon(G1, XS[1] + BW / 2, XS[2] - BW / 2)
ribbon(G2, XS[2] + BW / 2, XS[3] - BW / 2)
nodes(P1, XS[1], 2); nodes(P2, XS[2], 3); nodes(P3, XS[3], 4)
mtext(c("Round 1", "Round 2", "Round 3"), side = 3, at = XS, line = 0.9,
      cex = 0.82, font = 2)
mtext(paste0("Alaska's official rounds, all to the same scale. The gray band is ",
             n(OUT3), " ballots (", pc(100 * OUT3 / TOT),
             "% of ", n(TOT), ") that ran out of rankings: ", n(OUT3 - OVR),
             " exhausted plus ", n(OVR), " overvotes."),
      side = 1, line = 0.2, cex = 0.62, col = "#666666")

## ---- flow-d3
js <- function(g) paste(sprintf(
  '{"f":"%s","t":"%s","v":%.0f,"a":%.5f,"b":%.5f,"c":%.5f,"d":%.5f,"L":"%s"}',
  g$from, g$to, g$v, g$sy0, g$sy1, g$ty0, g$ty1,
  paste0(n(g$v), " ballots: ", sname(g$from), " to ",
         ifelse(g$from == g$to, "the next round", sname(g$to)))),
  collapse = ",")
nd <- function(P, i) paste(sprintf(
  '{"s":%d,"k":"%s","y0":%.5f,"y1":%.5f,"l":"%s"}', i, P$node, P$y0, P$y1,
  paste0(sname(P$node), "  ", n(P$v))), collapse = ",")
cat(paste0('
<div id="flw" style="margin:1em 0"></div>
<script>
(function(){
const R1=[', js(G1), '],R2=[', js(G2), '];
const N=[', nd(P1, 0), ',', nd(P2, 1), ',', nd(P3, 2), '];
const COL={', paste(sprintf('"%s":"%s"', names(FCOL), FCOL), collapse = ","), '};
const W=760,H=380,M={t:44,r:130,b:14,l:118},BW=9;
const X=[M.l,(M.l+W-M.r)/2,W-M.r];
const svg=d3.select("#flw").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%;height:auto;font:12px inherit");
const y=d3.scaleLinear().domain([0,', sprintf("%.5f", max(P1$y1, P2$y1, P3$y1)),
  ']).range([M.t,H-M.b]);
const cap=d3.select("#flw").append("p")
  .attr("style","font-size:0.85em;color:#555;min-height:2.6em;margin-top:0.3em");
const path=(d,xs,xt)=>{
  const T=d3.range(0,41).map(i=>i/40),P=[];
  T.forEach(t=>{const w=t*t*(3-2*t);
    P.push([xs+(xt-xs)*t, y(d.a+(d.c-d.a)*w)]);});
  T.slice().reverse().forEach(t=>{const w=t*t*(3-2*t);
    P.push([xs+(xt-xs)*t, y(d.b+(d.d-d.b)*w)]);});
  return "M"+P.map(p=>p[0].toFixed(1)+","+p[1].toFixed(1)).join("L")+"Z";};
[[R1,X[0]+BW/2,X[1]-BW/2],[R2,X[1]+BW/2,X[2]-BW/2]].forEach(q=>{
  svg.append("g").selectAll("path").data(q[0]).join("path")
    .attr("d",d=>path(d,q[1],q[2])).attr("fill",d=>COL[d.f]).attr("opacity",0.45)
    .style("cursor","pointer")
    .on("mousemove",function(e,d){d3.select(this).attr("opacity",0.8);
      cap.html("<b>"+d.L+"</b>");})
    .on("mouseleave",function(){d3.select(this).attr("opacity",0.45);
      cap.html("<b>Hover a band.</b> The gray one is ballots that ran out of rankings.");});
});
svg.append("g").selectAll("rect").data(N).join("rect")
  .attr("x",d=>X[d.s]-BW/2).attr("y",d=>y(d.y0)).attr("width",BW)
  .attr("height",d=>y(d.y1)-y(d.y0)).attr("fill",d=>COL[d.k]);
svg.append("g").selectAll("text").data(N).join("text")
  .attr("x",d=>d.s===1?X[1]:(d.s===2?X[2]+BW:X[0]-BW))
  .attr("y",d=>d.s===1?y(d.y0)-5:(y(d.y0)+y(d.y1))/2+4)
  .attr("text-anchor",d=>d.s===1?"middle":(d.s===2?"start":"end"))
  // halo class rather than a hardcoded white stroke: on the dark page the
  // fill lifts and a white outline left the label invisible against itself.
  .attr("font-size","11.5px").attr("fill","#333").attr("class","halo")
  .text(d=>d.l);
["Round 1","Round 2","Round 3"].forEach((t,i)=>
  svg.append("text").attr("x",X[i]).attr("y",26).attr("text-anchor","middle")
    .attr("font-size","13px").attr("font-weight","600").text(t));
svg.append("text").attr("x",X[0]).attr("y",H-2).attr("text-anchor","middle")
  .attr("font-size","11px").attr("fill","#777")
  .text("all rounds to the same scale");
cap.html("<b>Hover a band.</b> The gray one is ballots that ran out of rankings.");
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Alaska’s official rounds. Every band is drawn to the same scale, so the
', n(OUT3), ' ballots that leave the count are as wide as they truly are:
', pc(100 * OUT3 / TOT), '% of the ', n(TOT), ' that started. The gray node is
', n(OUT3 - OVR), ' exhausted ballots plus ', n(OVR), ' overvotes.</p>
'))

## ---- crossover
o <- cross
o$ballots <- n(o$ballots)
names(o) <- c("first ranking", "second ranking", "ballots")
o

## ---- dupes
o <- head(dup[order(-dup$ballots), ], 4)
o$ballots <- n(o$ballots)
o <- o[, c("rank1", "ballots")]
names(o) <- c("candidate named at both rank 1 and rank 2", "ballots")
o

## ---- bullet-table
o <- bul
o$voters <- n(o$voters); o$bullet <- pc(o$bullet)
names(o) <- c("first choice", "voters", "% who ranked only that candidate")
o

## ---- d3-bullet
rowsA <- paste(sprintf('{"k":"%s","v":%.1f,"lab":"%s%% of %s voters"}',
                       sub(",.*", "", bul$candidate), bul$bullet,
                       pc(bul$bullet), n(bul$voters)), collapse = ",")
uv <- tapply(s$ballots, ru, sum)
rowsB <- paste(sprintf('{"k":"%s ranked","v":%.1f,"lab":"%s ballots"}',
                       names(uv), 100 * as.vector(uv) / BAL,
                       n(as.vector(uv))), collapse = ",")
cat(sprintf('
<div id="cvr" style="position:relative;margin:1em 0">
 <div style="margin-bottom:6px">
  <button id="cA" style="font:12px inherit;padding:4px 10px;margin-right:4px;cursor:pointer">Bullet voting, by first choice</button>
  <button id="cB" style="font:12px inherit;padding:4px 10px;cursor:pointer">How many rankings were used</button>
 </div>
</div>
<script>
(function(){
const A=[%s], B=[%s];
const W=760,H=360,M={t:14,r:190,b:36,l:150};
const svg=d3.select("#cvr").append("svg").attr("viewBox",`0 0 ${W} ${H}`)
  .attr("style","max-width:100%%;height:auto;font:12px inherit");
const x=d3.scaleLinear().range([M.l,W-M.r]);
const y=d3.scaleBand().range([M.t,H-M.b]).padding(0.22);
const gx=svg.append("g").attr("transform",`translate(0,${H-M.b})`);
const gy=svg.append("g").attr("transform",`translate(${M.l},0)`);
const bars=svg.append("g"), labs=svg.append("g");
function draw(d,color){
  x.domain([0,d3.max(d,q=>q.v)*1.15]); y.domain(d.map(q=>q.k));
  gx.transition().duration(500).call(d3.axisBottom(x).ticks(6).tickFormat(v=>v+"%%"));
  gy.transition().duration(500).call(d3.axisLeft(y).tickSize(0))
    .selectAll("text").attr("font-size","11px");
  bars.selectAll("rect").data(d,q=>q.k).join(
    e=>e.append("rect").attr("x",M.l).attr("y",q=>y(q.k)).attr("height",y.bandwidth()).attr("rx",2).attr("width",0),
    u=>u, ex=>ex.transition().duration(250).attr("width",0).remove())
    .transition().duration(600)
    .attr("y",q=>y(q.k)).attr("height",y.bandwidth())
    .attr("width",q=>x(q.v)-M.l).attr("fill",color);
  labs.selectAll("text").data(d,q=>q.k).join(
    e=>e.append("text").attr("font-size","11px").attr("fill","#555").attr("opacity",0),
    u=>u, ex=>ex.remove())
    .transition().duration(600)
    .attr("x",q=>x(q.v)+6).attr("y",q=>y(q.k)+y.bandwidth()/2+4)
    .attr("opacity",1).text(q=>q.lab);
}
draw(A,"#C41230");
d3.select("#cA").on("click",()=>draw(A,"#C41230"));
d3.select("#cB").on("click",()=>draw(B,"#2c7fb8"));
})();
</script>
<p style="font-size:0.85em;color:#666;margin-top:0.2em">
Candidates are ordered by how many first-preference votes they received.</p>
', rowsA, rowsB))

## ---- bullet-static
par(mar = c(4, 8.5, 1, 2))
barplot(rev(bul$bullet), horiz = TRUE,
        names.arg = rev(sub(",.*", "", bul$candidate)), las = 1,
        cex.names = 0.85, col = "#C41230", xlim = c(0, 90),
        xlab = "% of that candidate's voters who ranked nobody else")
mtext(paste0("Candidates are ordered by how many first-preference votes they ",
             "received: ", n(bul$voters[1]), " down to ", n(bul$voters[nrow(bul)]),
             "."), side = 1, line = 3.4, cex = 0.62, col = "#666666")

## ---- denominator
data.frame(
  quantity = c("Ballots in the cast vote record",
               "Continuing in our first round",
               "Continuing in the final round",
               "Winner's final-round total",
               "As a share of continuing ballots",
               "As a share of all ballots cast"),
  value = c(n(BAL), n(ACTIVE1), n(ACTIVEF), n(fin[1]),
            paste0(pc(WINPCT, 2), "%"),
            paste0(pc(100 * fin[1] / BAL, 1), "%")))

## ---- on-mark
# Labels drawn ON a mark, not on the page. brief.css lifts dark text fills for
# the dark page; over a light bar or cell that would give near-white on
# near-white, so these pin the ink tokens back to the values the figure was
# drawn with. Listed per figure and per fill, because the same hex elsewhere in
# the chapter IS on the page and does want lifting.
# Sites found by _lib/check-contrast.js; re-run it after touching these figures.
cat('<style>
#bal text[fill="#555" i]
  { --ink:#12181D; --ink-2:#4E5A63; --ink-3:#76838C;
    --map-gop:#C41230; --map-dem:#2C7FB8; }
</style>')

## ---- ai-prompt
cat(ai_prompt(readLines("data/ai-prompt.txt")))
