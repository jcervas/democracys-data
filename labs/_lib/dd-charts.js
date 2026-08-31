// dd-charts.js -- the shared chart library for the book's D3 figures.
//
// Every brief used to hand-write its D3: the same 760px frame, the same
// scales, the same tooltip div, four hundred times. This file is that
// boilerplate written once. A chapter cat()s a <div class="dd-fig"> and a
// one-call <script>DD.fig("#id", {...})</script>, and the config carries only
// what is particular to the figure: the data, the fields, the labels.
//
// Plain D3 v7, no build step. Load order does not matter relative to this
// file's own definition -- d3 is only touched when DD.fig() runs -- but both
// scripts must be on the page before the first DD.fig() call, which is what
// dd_libs() in _lib/dd-charts.R guarantees.
//
// COLOURS ARE CLASSES, NEVER ATTRIBUTES. brief.css restyles the page for
// dark mode with media queries, and a media query cannot reach an SVG
// presentation attribute. So nothing in this file ever writes a hex fill or
// stroke. Every mark and every text carries a class from the .dd-fig
// vocabulary, and brief.css says what each class means in each theme:
//
//   ttl sub lbl foot          title, subtitle, label, footnote text
//   axis                      a d3 axis group (ticks + text, currentColor)
//   rule zero band            gridlines, the zero line, a shaded region
//   line area pt              marks (geometry only; colour from series-*)
//   series-1 .. series-8      categorical strokes
//   series-N-fill             the same eight, as fills
//   series-N-txt              the same eight, as text
//   gop dem gop-fill dem-fill gop-txt dem-txt    the party pair
//   land geo edge             map ground and boundaries
//   ramp-d5..d1 r1..r5        the shared diverging choropleth bins
//   hoverline hilite          hover furniture
//   on-mark halo              text drawn ON a light or saturated mark
//                             (defined once in brief.css, honoured here)
//
// MAPS ARE PRE-PROJECTED. A choropleth takes path d-strings computed in R
// from _lib/geo (the fixed 1152 x 748.8 y-down frame). Nothing here projects
// anything: DD just appends the paths, which is how the D3 map and the base-R
// map in the same chapter cannot disagree about the country's shape.
(function () {
  "use strict";

  var DD = {};
  var FRAME_W = 1152, FRAME_H = 748.8;   // the _lib/geo frame, exactly

  // ---- number formats -----------------------------------------------------
  // Named, because a config that travels through JSON cannot carry a
  // function. dd_fig() in R passes the name; DD.fmtByName() resolves it.
  DD.fmt = {
    comma:   function (x) { return (+x).toLocaleString("en-US"); },
    d:       function (x) { return String(Math.round(+x)); },
    f0:      function (x) { return (+x).toFixed(0); },
    f1:      function (x) { return (+x).toFixed(1); },
    f2:      function (x) { return (+x).toFixed(2); },
    pct0:    function (x) { return (+x).toFixed(0) + "%"; },
    pct1:    function (x) { return (+x).toFixed(1) + "%"; },
    pct2:    function (x) { return (+x).toFixed(2) + "%"; },
    signed0: function (x) { return (x > 0 ? "+" : "") + (+x).toFixed(0); },
    signed1: function (x) { return (x > 0 ? "+" : "") + (+x).toFixed(1); },
    signed2: function (x) { return (x > 0 ? "+" : "") + (+x).toFixed(2); },
    plain:   function (x) { return String(x); }
  };
  DD.fmtByName = function (name) {
    if (typeof name === "function") return name;
    return DD.fmt[name] || DD.fmt.plain;
  };

  // ---- the frame ----------------------------------------------------------
  // One SVG, one viewBox, scaled to the column: the corpus convention.
  // Returns everything a hand-written figure needs, which is why the
  // showpiece chapters can call this directly and skip DD.fig().
  DD.frame = function (sel, opt) {
    opt = opt || {};
    var W = opt.w || 760, H = opt.h || 420;
    var M = Object.assign({ t: 18, r: 24, b: 44, l: 52 }, opt.m || {});
    // sel may be a selector string, a node, or a selection DD.fig already made
    var box = (sel && typeof sel.node === "function") ? sel : d3.select(sel);
    if (box.empty()) { console.error("DD.frame: no element for", sel); return null; }
    box.classed("dd-fig", true);
    var svg = box.append("svg")
      .attr("viewBox", "0 0 " + W + " " + H)
      .attr("style", "max-width:100%;height:auto;font:12px inherit");
    return { box: box, svg: svg, W: W, H: H, M: M,
             innerW: W - M.l - M.r, innerH: H - M.t - M.b };
  };

  // ---- the tooltip --------------------------------------------------------
  // One HTML div per figure, positioned against the .dd-fig container (which
  // brief.css keeps position:relative). Styled entirely by .dd-fig .dd-tip.
  DD.tip = function (box) {
    var div = box.append("div").attr("class", "dd-tip");
    var node = box.node();
    return {
      show: function (html, evt) {
        var r = node.getBoundingClientRect();
        var x = evt.clientX - r.left, y = evt.clientY - r.top;
        div.style("opacity", 1).html(html)
           .style("left", Math.min(x + 14, Math.max(r.width - 240, 0)) + "px")
           .style("top", (y - 12) + "px");
      },
      hide: function () { div.style("opacity", 0); },
      node: div
    };
  };

  // Build the standard tooltip body from a declarative spec:
  //   { title: "state", fields: [["harris","Harris","pct1"], ...] }
  // or accept a function(d) and use it as-is.
  function tipHtml(spec, d) {
    if (typeof spec === "function") return spec(d);
    var out = [];
    if (spec.title) out.push("<b>" + d[spec.title] + "</b>");
    (spec.fields || []).forEach(function (f) {
      var field = f[0], label = f[1] || field, fmt = DD.fmtByName(f[2]);
      var v = d[field];
      out.push(label + ": " + (v === null || v === undefined ? "—" : fmt(v)));
    });
    return out.join("<br>");
  }

  // ---- axes ---------------------------------------------------------------
  // The axis group is classed "axis" and left at currentColor, which
  // brief.css turns into secondary ink in both themes. `grid` clones the
  // ticks across the panel as .rule hairlines.
  DD.axis = {
    bottom: function (f, x, o) {
      o = o || {};
      var ax = d3.axisBottom(x);
      if (o.ticks) ax.ticks(o.ticks);
      if (o.fmt) ax.tickFormat(DD.fmtByName(o.fmt));
      var g = f.svg.append("g").attr("class", "axis")
        .attr("transform", "translate(0," + (f.H - f.M.b) + ")").call(ax);
      if (o.grid) g.selectAll(".tick line").clone()
        .attr("y2", -(f.innerH)).attr("class", "rule-2");
      return g;
    },
    top: function (f, x, o) {
      o = o || {};
      var ax = d3.axisTop(x);
      if (o.ticks) ax.ticks(o.ticks);
      if (o.fmt) ax.tickFormat(DD.fmtByName(o.fmt));
      var g = f.svg.append("g").attr("class", "axis")
        .attr("transform", "translate(0," + f.M.t + ")").call(ax);
      if (o.grid) g.selectAll(".tick line").clone()
        .attr("y2", f.innerH).attr("class", "rule-2");
      return g;
    },
    left: function (f, y, o) {
      o = o || {};
      var ax = d3.axisLeft(y);
      if (o.ticks) ax.ticks(o.ticks);
      if (o.fmt) ax.tickFormat(DD.fmtByName(o.fmt));
      var g = f.svg.append("g").attr("class", "axis")
        .attr("transform", "translate(" + f.M.l + ",0)").call(ax);
      if (o.grid) g.selectAll(".tick line").clone()
        .attr("x2", f.innerW).attr("class", "rule-2");
      return g;
    },
    // the rotated y-axis caption and the x-axis caption, as .lbl text
    xlab: function (f, text) {
      if (!text) return;
      f.svg.append("text").attr("class", "lbl")
        .attr("x", (f.M.l + f.W - f.M.r) / 2).attr("y", f.H - 8)
        .attr("text-anchor", "middle").attr("font-size", "12px").text(text);
    },
    ylab: function (f, text) {
      if (!text) return;
      f.svg.append("text").attr("class", "lbl")
        .attr("transform", "rotate(-90)")
        .attr("x", -(f.M.t + f.H - f.M.b) / 2).attr("y", 14)
        .attr("text-anchor", "middle").attr("font-size", "12px").text(text);
    }
  };

  // ---- scales -------------------------------------------------------------
  function linScale(spec, values, range) {
    spec = spec || {};
    var dom = spec.domain;
    if (!dom) {
      dom = d3.extent(values.filter(function (v) { return v !== null && v !== undefined; }));
      if (spec.zero) dom[0] = Math.min(0, dom[0]);
    }
    var s = spec.log ? d3.scaleLog() : d3.scaleLinear();
    return s.domain(dom).range(range);
  }

  // ---- series -------------------------------------------------------------
  // Two shapes, matching the two shapes the data takes in the corpus:
  //   wide: series.fields = [{field,label,class?}, ...] -- one column each
  //   long: series.field  = "region", series.classes = {value: class}
  // Either way, a series that names no class gets series-1..series-8.
  function wideSeries(cfg) {
    var s = (cfg.series && cfg.series.fields) || [{ field: cfg.y.field, label: cfg.y.label }];
    return s.map(function (d, i) {
      var cls = d["class"] || ("series-" + ((i % 8) + 1));
      return { field: d.field, label: d.label || d.field, cls: cls,
               endLabel: d.endLabel };
    });
  }
  function rowClass(cfg, d, i) {
    var s = cfg.series || {};
    if (s.field && s.classes) return s.classes[d[s.field]] || "series-1";
    if (d.cls) return d.cls;
    return s["class"] || "series-1";
  }

  // ---- annotations --------------------------------------------------------
  // Drawn after the marks, in data units, so a chapter can pin a threshold or
  // a caption without touching the geometry.
  function annotate(f, x, y, list) {
    (list || []).forEach(function (a) {
      var cls = a["class"];
      if (a.type === "vline") {
        f.svg.append("line").attr("class", cls || "rule")
          .attr("x1", x(a.x)).attr("x2", x(a.x))
          .attr("y1", f.M.t).attr("y2", f.H - f.M.b)
          .attr("stroke-dasharray", a.dash === false ? null : "4 3");
      } else if (a.type === "hline") {
        f.svg.append("line").attr("class", cls || "rule")
          .attr("x1", f.M.l).attr("x2", f.W - f.M.r)
          .attr("y1", y(a.y)).attr("y2", y(a.y))
          .attr("stroke-dasharray", a.dash === false ? null : "4 3");
      } else if (a.type === "band") {
        var r = f.svg.append("rect").attr("class", cls || "band").lower();
        if (a.axis === "y") {
          r.attr("x", f.M.l).attr("width", f.innerW)
           .attr("y", y(a.to)).attr("height", Math.abs(y(a.from) - y(a.to)));
        } else {
          r.attr("y", f.M.t).attr("height", f.innerH)
           .attr("x", x(a.from)).attr("width", Math.abs(x(a.to) - x(a.from)));
        }
      } else if (a.type === "text") {
        f.svg.append("text").attr("class", cls || "lbl")
          .attr("x", (a.px ? a.x : x(a.x)) + (a.dx || 0))
          .attr("y", (a.px ? a.y : y(a.y)) + (a.dy || 0))
          .attr("text-anchor", a.anchor || "start")
          .attr("font-size", (a.size || 11) + "px")
          .attr("font-weight", a.weight || null)
          .text(a.text);
      } else if (a.type === "rule") {
        f.svg.append("line").attr("class", cls || "rule")
          .attr("x1", x(a.x1)).attr("x2", x(a.x2))
          .attr("y1", y(a.y1)).attr("y2", y(a.y2));
      }
    });
  }

  // ---- the slider ---------------------------------------------------------
  // An HTML range input above the SVG, screen-only by nature. The callback
  // gets the finished fig object, so it can re-select marks by class and
  // redraw whatever the slider governs.
  function addSlider(box, cfg, later) {
    if (!cfg.slider) return;
    var s = cfg.slider;
    var bar = box.append("div").attr("class", "dd-slider");
    if (s.label) bar.append("span").attr("class", "dd-slider-label").text(s.label);
    var val = bar.append("span").attr("class", "dd-slider-value").text(s.value);
    var inp = bar.append("input").attr("type", "range")
      .attr("min", s.min).attr("max", s.max)
      .attr("step", s.step || 1).attr("value", s.value);
    later.push(function (fig) {
      inp.on("input", function () {
        var v = +this.value;
        val.text(v);
        if (typeof s.onchange === "function") s.onchange(fig, v);
      });
      if (typeof s.onchange === "function") s.onchange(fig, +s.value);
    });
  }

  // ---- the legend ---------------------------------------------------------
  // HTML, under the SVG, one swatch character per series -- the corpus form.
  function addLegend(box, items) {
    if (!items || !items.length) return;
    var lg = box.append("div").attr("class", "dd-legend");
    lg.selectAll("span").data(items).join("span")
      .attr("class", function (d) { return "dd-legend-item " + (d.txtcls || d.cls.replace(/(-fill)?$/, "-txt")); })
      .html(function (d) { return "■ " + d.label; });
  }

  // ---- shared hover-by-x for line charts ----------------------------------
  // A vertical rule follows the pointer to the nearest x; the tooltip reads
  // every series off at that x. This is the interaction three quarters of the
  // corpus's line charts hand-roll.
  function hoverX(f, x, y, data, series, cfg, tip) {
    var xs = data.map(function (d) { return d[cfg.x.field]; });
    var xfmt = DD.fmtByName((cfg.x && cfg.x.fmt) || "plain");
    var rule = f.svg.append("line").attr("class", "hoverline")
      .attr("y1", f.M.t).attr("y2", f.H - f.M.b).attr("opacity", 0);
    f.svg.append("rect")
      .attr("x", f.M.l).attr("y", f.M.t)
      .attr("width", f.innerW).attr("height", f.innerH)
      .attr("fill", "transparent")
      .on("mousemove", function (e) {
        var px = d3.pointer(e, this)[0] + f.M.l;
        var i = 0, best = Infinity;
        xs.forEach(function (v, j) {
          var dd = Math.abs(x(v) - px);
          if (dd < best) { best = dd; i = j; }
        });
        var d = data[i];
        rule.attr("x1", x(xs[i])).attr("x2", x(xs[i])).attr("opacity", 0.55);
        var html;
        if (typeof cfg.tip === "function") {
          html = cfg.tip(d);
        } else if (cfg.tip && cfg.tip.fields) {
          html = tipHtml(cfg.tip, d);
        } else {
          var rows = series.map(function (s) {
            var v = d[s.field];
            return v === null || v === undefined ? null :
              "<span class=\"" + s.cls.replace(/(-fill)?$/, "-txt") + "\">■</span> " +
              s.label + ": " + DD.fmtByName((cfg.y && cfg.y.fmt) || "f1")(v);
          }).filter(Boolean);
          html = "<b>" + xfmt(d[cfg.x.field]) + "</b><br>" + rows.join("<br>");
        }
        tip.show(html, e);
      })
      .on("mouseleave", function () { tip.hide(); rule.attr("opacity", 0); });
  }

  // ---- per-mark tooltip ---------------------------------------------------
  function hoverMarks(selection, cfg, tip) {
    if (cfg.tip === false || !cfg.tip) return;
    selection
      .on("mousemove", function (e, d) { tip.show(tipHtml(cfg.tip, d), e); })
      .on("mouseleave", function () { tip.hide(); });
  }

  // ==========================================================================
  // The figure types. Each takes (box, cfg) and returns the fig object that
  // hooks and sliders receive: { box, svg, x, y, W, H, M, data, cfg, tip }.
  // ==========================================================================
  var types = {};

  // ---- line / step / area -------------------------------------------------
  // One frame, n series read from wide columns. `band` shades the region
  // between two columns (the disagreement between two definitions, in the
  // chapters that need it). `points` dots the observations; `endLabels`
  // names each series at its last point instead of in a legend.
  function lineLike(box, cfg, curve, asArea) {
    var f = DD.frame(box, sizeOf(cfg, { h: 420, m: { t: 16, r: 24, b: 40, l: 52 } }));
    var data = cfg.data;
    var series = wideSeries(cfg);
    var x = linScale(cfg.x, data.map(function (d) { return d[cfg.x.field]; }),
                     [f.M.l, f.W - f.M.r]);
    var yvals = [];
    series.forEach(function (s) { data.forEach(function (d) { yvals.push(d[s.field]); }); });
    if (cfg.band) data.forEach(function (d) {
      yvals.push(d[cfg.band.y0]); yvals.push(d[cfg.band.y1]);
    });
    var y = linScale(cfg.y, yvals, [f.H - f.M.b, f.M.t]);

    if (cfg.band) {
      f.svg.append("path").attr("class", (cfg.band["class"] || "band"))
        .attr("d", d3.area()
          .defined(function (d) { return ok(d[cfg.band.y0]) && ok(d[cfg.band.y1]); })
          .x(function (d) { return x(d[cfg.x.field]); })
          .y0(function (d) { return y(d[cfg.band.y0]); })
          .y1(function (d) { return y(d[cfg.band.y1]); })(data));
    }

    DD.axis.bottom(f, x, cfg.x);
    DD.axis.left(f, y, cfg.y);
    DD.axis.xlab(f, cfg.x.label);
    DD.axis.ylab(f, cfg.y.label);

    series.forEach(function (s) {
      var gen = asArea
        ? d3.area().y0(y(Math.max(0, y.domain()[0])))
            .defined(function (d) { return ok(d[s.field]); })
            .x(function (d) { return x(d[cfg.x.field]); })
            .y1(function (d) { return y(d[s.field]); })
        : d3.line()
            .defined(function (d) { return ok(d[s.field]); })
            .x(function (d) { return x(d[cfg.x.field]); })
            .y(function (d) { return y(d[s.field]); });
      if (curve) gen.curve(curve);
      f.svg.append("path")
        .attr("class", (asArea ? "area " + s.cls + "-fill" : "line " + s.cls))
        .attr("fill", asArea ? null : "none")
        .attr("stroke-width", asArea ? null : (s.width || 2.5))
        .attr("d", gen(data));
      if (cfg.points) {
        f.svg.append("g").selectAll("circle").data(data.filter(function (d) { return ok(d[s.field]); }))
          .join("circle").attr("class", "pt " + s.cls + "-fill")
          .attr("cx", function (d) { return x(d[cfg.x.field]); })
          .attr("cy", function (d) { return y(d[s.field]); })
          .attr("r", cfg.points === true ? 2.8 : cfg.points);
      }
      if (cfg.endLabels) {
        var last = null;
        data.forEach(function (d) { if (ok(d[s.field])) last = d; });
        if (last) {
          var lines = [].concat(s.endLabel || s.label);
          var t = f.svg.append("text").attr("class", s.cls + "-txt")
            .attr("x", f.W - f.M.r + 8).attr("y", y(last[s.field]) + 4)
            .attr("font-size", "11.5px").attr("font-weight", 600);
          lines.forEach(function (ln, i) {
            t.append("tspan").attr("x", f.W - f.M.r + 8)
             .attr("dy", i === 0 ? "0" : "1.15em").text(ln);
          });
        }
      }
    });

    var tip = DD.tip(f.box);
    if (cfg.tip !== false) hoverX(f, x, y, data, series, cfg, tip);
    annotate(f, x, y, cfg.annotations);
    if (cfg.legend) addLegend(f.box, series.map(function (s) {
      return { cls: s.cls, label: s.label };
    }));
    return finish({ box: f.box, svg: f.svg, x: x, y: y, W: f.W, H: f.H, M: f.M,
                    data: data, series: series, cfg: cfg, tip: tip }, cfg);
  }
  types.line = function (box, cfg) { return lineLike(box, cfg, null, false); };
  types.step = function (box, cfg) { return lineLike(box, cfg, d3.curveStepAfter, false); };
  types.area = function (box, cfg) { return lineLike(box, cfg, null, true); };

  // ---- bar ----------------------------------------------------------------
  // Horizontal when y is the band (y.band = true), vertical when x is. A
  // domain spanning zero draws its bars from the zero line and, when
  // catLabels is "inline", parks each category's name on the other side of
  // that line -- the diverging-bar layout the corpus uses for gains and
  // losses.
  types.bar = function (box, cfg) {
    var horiz = !!(cfg.y && cfg.y.band);
    var data = cfg.data;
    var catSpec = horiz ? cfg.y : cfg.x;
    var valSpec = horiz ? cfg.x : cfg.y;
    var cats = data.map(function (d) { return d[catSpec.field]; });
    var vfmt = DD.fmtByName(valSpec.fmt || "d");

    var f, band, val;
    if (horiz) {
      // height follows the row count, so forty categories are forty readable
      // rows rather than forty slivers of a fixed panel
      var rh = cfg.rowHeight || 17;
      var hm = { t: 26, r: 30, b: 8, l: cfg.catLabels === "inline" ? 12 : 130 };
      f = DD.frame(box, sizeOf(cfg, { h: hm.t + hm.b + data.length * rh, m: hm }));
      band = d3.scaleBand().domain(cats).range([f.M.t, f.H - f.M.b])
        .padding(cfg.padding || 0.18);
      val = linScale(Object.assign({ zero: true }, valSpec),
                     data.map(function (d) { return d[valSpec.field]; }),
                     [f.M.l, f.W - f.M.r]);
      DD.axis.top(f, val, Object.assign({ grid: true }, valSpec));
      if (cfg.catLabels !== "inline") {
        f.svg.append("g").attr("class", "axis")
          .attr("transform", "translate(" + f.M.l + ",0)")
          .call(d3.axisLeft(band).tickSize(0));
      }
      var bars = f.svg.append("g").selectAll("rect").data(data).join("rect")
        .attr("class", function (d, i) { return "bar " + rowClass(cfg, d, i) + "-fill"; })
        .attr("x", function (d) { return val(Math.min(0, d[valSpec.field])); })
        .attr("y", function (d) { return band(d[catSpec.field]); })
        .attr("width", function (d) { return Math.abs(val(d[valSpec.field]) - val(0)); })
        .attr("height", band.bandwidth());
      if (cfg.catLabels === "inline") {
        f.svg.append("g").selectAll("text").data(data).join("text")
          .attr("class", "lbl")
          .attr("x", function (d) { return val(0) + (d[valSpec.field] > 0 ? -6 : 6); })
          .attr("y", function (d) { return band(d[catSpec.field]) + band.bandwidth() / 2 + 4; })
          .attr("text-anchor", function (d) { return d[valSpec.field] > 0 ? "end" : "start"; })
          .attr("font-size", "11px")
          .text(function (d) { return d[catSpec.field]; });
      }
      if (cfg.valueLabels) {
        f.svg.append("g").selectAll("text").data(data).join("text")
          .attr("class", function (d, i) { return rowClass(cfg, d, i) + "-txt"; })
          .attr("x", function (d) { return val(d[valSpec.field]) + (d[valSpec.field] > 0 ? 5 : -5); })
          .attr("y", function (d) { return band(d[catSpec.field]) + band.bandwidth() / 2 + 4; })
          .attr("text-anchor", function (d) { return d[valSpec.field] > 0 ? "start" : "end"; })
          .attr("font-size", "10.5px")
          .text(function (d) { return vfmt(d[valSpec.field]); });
      }
      // the zero line, over the bars, so the two directions stay separated
      f.svg.append("line").attr("class", "zero")
        .attr("x1", val(0)).attr("x2", val(0))
        .attr("y1", f.M.t).attr("y2", f.H - f.M.b);
    } else {
      f = DD.frame(box, sizeOf(cfg, { h: 420, m: { t: 16, r: 16, b: 60, l: 52 } }));
      band = d3.scaleBand().domain(cats).range([f.M.l, f.W - f.M.r])
        .padding(cfg.padding || 0.22);
      val = linScale(Object.assign({ zero: true }, valSpec),
                     data.map(function (d) { return d[valSpec.field]; }),
                     [f.H - f.M.b, f.M.t]);
      DD.axis.left(f, val, valSpec);
      DD.axis.ylab(f, valSpec.label);
      var gx = f.svg.append("g").attr("class", "axis")
        .attr("transform", "translate(0," + (f.H - f.M.b) + ")")
        .call(d3.axisBottom(band).tickSize(0));
      if (cfg.tiltLabels) gx.selectAll("text")
        .attr("transform", "rotate(-38)").attr("text-anchor", "end")
        .attr("dx", "-0.6em").attr("dy", "0.4em");
      var bars = f.svg.append("g").selectAll("rect").data(data).join("rect")
        .attr("class", function (d, i) { return "bar " + rowClass(cfg, d, i) + "-fill"; })
        .attr("x", function (d) { return band(d[catSpec.field]); })
        .attr("width", band.bandwidth())
        .attr("y", function (d) { return val(Math.max(0, d[valSpec.field])); })
        .attr("height", function (d) { return Math.abs(val(d[valSpec.field]) - val(0)); });
      if (cfg.valueLabels) {
        f.svg.append("g").selectAll("text").data(data).join("text")
          .attr("class", "lbl")
          .attr("x", function (d) { return band(d[catSpec.field]) + band.bandwidth() / 2; })
          .attr("y", function (d) { return val(Math.max(0, d[valSpec.field])) - 5; })
          .attr("text-anchor", "middle").attr("font-size", "11px")
          .text(function (d) { return vfmt(d[valSpec.field]); });
      }
    }

    var tip = DD.tip(f.box);
    hoverMarks(f.svg.selectAll("rect.bar"), cfg, tip);
    var x = horiz ? val : band, y = horiz ? band : val;
    annotate(f, x, y, cfg.annotations);
    if (cfg.legend) addLegend(f.box, legendItems(cfg));
    return finish({ box: f.box, svg: f.svg, x: x, y: y, W: f.W, H: f.H, M: f.M,
                    data: data, cfg: cfg, tip: tip }, cfg);
  };

  // ---- dot ----------------------------------------------------------------
  // A categorical strip of points: one band per category on the y side, the
  // value on x. Rows sharing a category simply overplot; pass r to size them.
  types.dot = function (box, cfg) {
    var data = cfg.data;
    var cats = uniq(data.map(function (d) { return d[cfg.y.field]; }));
    var f = DD.frame(box, sizeOf(cfg, {
      h: 40 + cats.length * (cfg.rowHeight || 48) + 44,
      m: { t: 24, r: 26, b: 48, l: 96 }
    }));
    var y = d3.scalePoint().domain(cats)
      .range([f.M.t + 20, f.H - f.M.b - 20]);
    var x = linScale(cfg.x, data.map(function (d) { return d[cfg.x.field]; }),
                     [f.M.l, f.W - f.M.r]);
    DD.axis.bottom(f, x, cfg.x);
    DD.axis.xlab(f, cfg.x.label);
    f.svg.append("g").attr("class", "axis")
      .attr("transform", "translate(" + f.M.l + ",0)")
      .call(d3.axisLeft(y).tickSize(0));
    var pts = f.svg.append("g").selectAll("circle").data(data).join("circle")
      .attr("class", function (d, i) { return "pt " + rowClass(cfg, d, i) + "-fill"; })
      .attr("cx", function (d) { return x(d[cfg.x.field]); })
      .attr("cy", function (d) { return y(d[cfg.y.field]) + (d.dy || 0); })
      .attr("r", cfg.r || 4.5);
    var tip = DD.tip(f.box);
    hoverMarks(pts, cfg, tip);
    annotate(f, x, y, cfg.annotations);
    if (cfg.legend) addLegend(f.box, legendItems(cfg));
    return finish({ box: f.box, svg: f.svg, x: x, y: y, W: f.W, H: f.H, M: f.M,
                    data: data, cfg: cfg, tip: tip }, cfg);
  };

  // ---- scatter ------------------------------------------------------------
  // x against y, one point per row. A row with a `lbl` property gets its
  // label beside the point; a row with side:"left" gets it on the left, for
  // the pairs that would otherwise print on top of one another.
  types.scatter = function (box, cfg) {
    var data = cfg.data;
    var f = DD.frame(box, sizeOf(cfg, { h: 430, m: { t: 18, r: 22, b: 46, l: 52 } }));
    var x = linScale(cfg.x, data.map(function (d) { return d[cfg.x.field]; }),
                     [f.M.l, f.W - f.M.r]);
    var y = linScale(cfg.y, data.map(function (d) { return d[cfg.y.field]; }),
                     [f.H - f.M.b, f.M.t]);
    DD.axis.bottom(f, x, cfg.x);
    DD.axis.left(f, y, cfg.y);
    DD.axis.xlab(f, cfg.x.label);
    DD.axis.ylab(f, cfg.y.label);
    annotate(f, x, y, cfg.annotations);
    var pts = f.svg.append("g").selectAll("circle").data(data).join("circle")
      .attr("class", function (d, i) { return "pt " + rowClass(cfg, d, i) + "-fill"; })
      .attr("cx", function (d) { return x(d[cfg.x.field]); })
      .attr("cy", function (d) { return y(d[cfg.y.field]); })
      .attr("r", cfg.r || 5).attr("fill-opacity", cfg.opacity || 0.75);
    f.svg.append("g").selectAll("text").data(data.filter(function (d) { return d.lbl; }))
      .join("text").attr("class", "lbl")
      .attr("x", function (d) { return x(d[cfg.x.field]) + (d.side === "left" ? -8 : 8); })
      .attr("y", function (d) { return y(d[cfg.y.field]) + 4; })
      .attr("text-anchor", function (d) { return d.side === "left" ? "end" : "start"; })
      .attr("font-size", "10.5px").text(function (d) { return d.lbl; });
    var tip = DD.tip(f.box);
    hoverMarks(pts, cfg, tip);
    if (cfg.legend) addLegend(f.box, legendItems(cfg));
    return finish({ box: f.box, svg: f.svg, x: x, y: y, W: f.W, H: f.H, M: f.M,
                    data: data, cfg: cfg, tip: tip }, cfg);
  };

  // ---- slope --------------------------------------------------------------
  // Two rails, one line per row from a to b, dashed where the value fell.
  // Rows may carry pre-dodged label positions (la, lb) computed in R, with
  // leader lines back to the true points -- the same apparatus the static
  // twin uses, so the two cannot disagree about which label went where.
  types.slope = function (box, cfg) {
    var data = cfg.data;
    var f = DD.frame(box, sizeOf(cfg, { h: 430, m: { t: 34, r: 150, b: 14, l: 150 } }));
    var vals = [];
    data.forEach(function (d) {
      vals.push(d[cfg.a.field], d[cfg.b.field]);
      if (ok(d.la)) vals.push(d.la);
      if (ok(d.lb)) vals.push(d.lb);
    });
    var y = linScale(cfg.y || {}, vals, [f.H - f.M.b, f.M.t]);
    var xa = f.M.l, xb = f.W - f.M.r;
    [[xa, cfg.a.label], [xb, cfg.b.label]].forEach(function (r) {
      if (r[1]) f.svg.append("text").attr("class", "ttl")
        .attr("x", r[0]).attr("y", 20).attr("text-anchor", "middle")
        .attr("font-weight", 600).text(r[1]);
      f.svg.append("line").attr("class", "rule")
        .attr("x1", r[0]).attr("x2", r[0]).attr("y1", f.M.t).attr("y2", f.H - f.M.b);
    });
    var g = f.svg.append("g").selectAll("g").data(data).join("g");
    g.append("line")
      .attr("class", function (d, i) { return rowClass(cfg, d, i); })
      .attr("x1", xa).attr("x2", xb)
      .attr("y1", function (d) { return y(d[cfg.a.field]); })
      .attr("y2", function (d) { return y(d[cfg.b.field]); })
      .attr("stroke-width", 2.2)
      .attr("stroke-dasharray", function (d) {
        return d[cfg.b.field] >= d[cfg.a.field] ? null : "5,3"; });
    [["a", xa], ["b", xb]].forEach(function (side) {
      g.append("circle")
        .attr("class", function (d, i) { return rowClass(cfg, d, i) + "-fill"; })
        .attr("cx", side[1]).attr("r", 4)
        .attr("cy", function (d) { return y(d[cfg[side[0]].field]); });
    });
    g.each(function (d, i) {
      var sel = d3.select(this), cls = rowClass(cfg, d, i);
      var la = ok(d.la) ? d.la : d[cfg.a.field];
      var lb = ok(d.lb) ? d.lb : d[cfg.b.field];
      sel.append("line").attr("class", cls).attr("stroke-width", 0.7)
        .attr("x1", xa - 8).attr("x2", xa).attr("y1", y(la)).attr("y2", y(d[cfg.a.field]));
      sel.append("line").attr("class", cls).attr("stroke-width", 0.7)
        .attr("x1", xb + 8).attr("x2", xb).attr("y1", y(lb)).attr("y2", y(d[cfg.b.field]));
      sel.append("text").attr("class", cls + "-txt")
        .attr("x", xa - 12).attr("y", y(la) + 4).attr("text-anchor", "end")
        .attr("font-size", "11.5px")
        .text(d[cfg.label] + " " + d[cfg.a.field]);
      sel.append("text").attr("class", cls + "-txt")
        .attr("x", xb + 12).attr("y", y(lb) + 4)
        .attr("font-size", "11.5px")
        .text(d[cfg.b.field] + " " + d[cfg.label]);
    });
    var tip = DD.tip(f.box);
    return finish({ box: f.box, svg: f.svg, x: null, y: y, W: f.W, H: f.H, M: f.M,
                    data: data, cfg: cfg, tip: tip }, cfg);
  };

  // ---- dumbbell -----------------------------------------------------------
  // One row per category, two values joined by a rule: before and after,
  // measure one and measure two. Classes aClass/bClass name the two ends.
  types.dumbbell = function (box, cfg) {
    var data = cfg.data;
    var rh = cfg.rowHeight || 22;
    var f = DD.frame(box, sizeOf(cfg, {
      h: 40 + data.length * rh + 44, m: { t: 28, r: 26, b: 44, l: 130 }
    }));
    var cats = data.map(function (d) { return d[cfg.y.field]; });
    var y = d3.scalePoint().domain(cats).range([f.M.t + 10, f.H - f.M.b - 10]);
    var vals = [];
    data.forEach(function (d) { vals.push(d[cfg.a.field], d[cfg.b.field]); });
    var x = linScale(cfg.x || {}, vals, [f.M.l, f.W - f.M.r]);
    DD.axis.bottom(f, x, cfg.x || {});
    DD.axis.xlab(f, (cfg.x || {}).label);
    f.svg.append("g").attr("class", "axis")
      .attr("transform", "translate(" + f.M.l + ",0)")
      .call(d3.axisLeft(y).tickSize(0));
    var g = f.svg.append("g").selectAll("g").data(data).join("g");
    g.append("line").attr("class", "rule")
      .attr("x1", function (d) { return x(d[cfg.a.field]); })
      .attr("x2", function (d) { return x(d[cfg.b.field]); })
      .attr("y1", function (d) { return y(d[cfg.y.field]); })
      .attr("y2", function (d) { return y(d[cfg.y.field]); })
      .attr("stroke-width", 2);
    [["a", cfg.aClass || "series-1"], ["b", cfg.bClass || "series-2"]]
      .forEach(function (side) {
        g.append("circle").attr("class", "pt " + side[1] + "-fill")
          .attr("cx", function (d) { return x(d[cfg[side[0]].field]); })
          .attr("cy", function (d) { return y(d[cfg.y.field]); })
          .attr("r", cfg.r || 4.5);
      });
    var tip = DD.tip(f.box);
    hoverMarks(g, cfg, tip);
    annotate(f, x, y, cfg.annotations);
    addLegend(f.box, [
      { cls: cfg.aClass || "series-1", label: cfg.a.label || cfg.a.field },
      { cls: cfg.bClass || "series-2", label: cfg.b.label || cfg.b.field }]);
    return finish({ box: f.box, svg: f.svg, x: x, y: y, W: f.W, H: f.H, M: f.M,
                    data: data, cfg: cfg, tip: tip }, cfg);
  };

  // ---- small multiples ----------------------------------------------------
  // One little line panel per value of facet.field, on shared scales unless
  // told otherwise. Long data: every row carries the facet value, the x and
  // the y. The point of the form is comparability, so sharedY defaults on.
  types.smallmult = function (box, cfg) {
    var data = cfg.data;
    var facets = uniq(data.map(function (d) { return d[cfg.facet.field]; }));
    var cols = cfg.facet.cols || 3;
    var rows = Math.ceil(facets.length / cols);
    var W = (cfg.size && cfg.size.w) || 760;
    var ph = cfg.facet.height || 130;
    var H = (cfg.size && cfg.size.h) || (rows * ph + 30);
    var f = DD.frame(box, { w: W, h: H, m: { t: 0, r: 0, b: 0, l: 0 } });
    var pw = W / cols;
    var pm = { t: 26, r: 12, b: 22, l: 36 };
    var xall = data.map(function (d) { return d[cfg.x.field]; });
    var yall = data.map(function (d) { return d[cfg.y.field]; });
    var xdom = (cfg.x && cfg.x.domain) || d3.extent(xall);
    var ydom = (cfg.y && cfg.y.domain) || d3.extent(yall);
    var tip = DD.tip(f.box);
    facets.forEach(function (fc, i) {
      var ox = (i % cols) * pw, oy = Math.floor(i / cols) * ph;
      var sub = data.filter(function (d) { return d[cfg.facet.field] === fc; });
      var x = d3.scaleLinear().domain(xdom).range([ox + pm.l, ox + pw - pm.r]);
      var ydomi = (cfg.sharedY === false)
        ? d3.extent(sub, function (d) { return d[cfg.y.field]; }) : ydom;
      var y = d3.scaleLinear().domain(ydomi).range([oy + ph - pm.b, oy + pm.t]);
      f.svg.append("text").attr("class", "lbl")
        .attr("x", ox + pm.l).attr("y", oy + 15)
        .attr("font-size", "11.5px").attr("font-weight", 600).text(fc);
      f.svg.append("g").attr("class", "axis")
        .attr("transform", "translate(0," + (oy + ph - pm.b) + ")")
        .call(d3.axisBottom(x).ticks(3).tickFormat(DD.fmtByName(cfg.x.fmt || "d")));
      f.svg.append("g").attr("class", "axis")
        .attr("transform", "translate(" + (ox + pm.l) + ",0)")
        .call(d3.axisLeft(y).ticks(3).tickFormat(DD.fmtByName(cfg.y.fmt || "d")));
      f.svg.append("path")
        .attr("class", "line " + ((cfg.series && cfg.series["class"]) || "series-1"))
        .attr("fill", "none").attr("stroke-width", 1.8)
        .attr("d", d3.line()
          .defined(function (d) { return ok(d[cfg.y.field]); })
          .x(function (d) { return x(d[cfg.x.field]); })
          .y(function (d) { return y(d[cfg.y.field]); })(sub));
    });
    return finish({ box: f.box, svg: f.svg, x: null, y: null, W: W, H: H,
                    M: pm, data: data, cfg: cfg, tip: tip }, cfg);
  };

  // ---- choropleth ---------------------------------------------------------
  // The map recipe: R read a _lib/geo file, built the d-strings on the fixed
  // 1152 x 748.8 y-down frame, and quantised each unit's value to a CLASS.
  // This function appends paths and classes and nothing else -- no geoPath,
  // no projection, no computed hex anywhere.
  //
  //   geo:    { paths: [{id, d, cls?, lx?, ly?, lab?}], frame: [1152, 748.8] }
  //   values: { id: {cls: "ramp-r3", ...tooltip fields} }   (or rows with .id)
  //   key:    { title, items: [{cls, label}] }              (optional)
  types.choropleth = function (box, cfg) {
    var geo = cfg.geo || {};
    var frame = geo.frame || [FRAME_W, FRAME_H];
    if (Math.abs(frame[0] - FRAME_W) > 1e-6 || Math.abs(frame[1] - FRAME_H) > 1e-6) {
      // Draw anyway -- a wrong frame is a wrong overlay, not a blank page --
      // but say so where a developer will see it.
      console.error("DD choropleth: frame is [" + frame + "], not [" +
                    FRAME_W + ", " + FRAME_H + "]; every _lib/geo file uses the fixed frame.");
    }
    // a map drawn at half the column (two panels side by side) needs its
    // titles set larger in viewBox units to land at the same size on screen
    var tSize = cfg.titleSize || 14, sSize = cfg.subtitleSize || 11.5;
    var keyH = cfg.key ? 64 : 0;
    var titleH = (cfg.title ? tSize + 12 : 0) + (cfg.subtitle ? sSize + 5 : 0);
    var W = (cfg.size && cfg.size.w) || 760;
    var mapH = W * frame[1] / frame[0];
    var H = (cfg.size && cfg.size.h) || (titleH + mapH + keyH);
    var f = DD.frame(box, { w: W, h: H, m: { t: titleH, r: 0, b: keyH, l: 0 } });
    if (cfg.title) f.svg.append("text").attr("class", "ttl")
      .attr("x", W / 2).attr("y", tSize + 4).attr("text-anchor", "middle")
      .attr("font-size", tSize + "px").attr("font-weight", 600).text(cfg.title);
    if (cfg.subtitle) f.svg.append("text").attr("class", "sub")
      .attr("x", W / 2).attr("y", titleH - 6).attr("text-anchor", "middle")
      .attr("font-size", sSize + "px").text(cfg.subtitle);

    // values arrive either keyed by id or as rows carrying an id
    var vals = {};
    if (Array.isArray(cfg.values)) {
      cfg.values.forEach(function (v) { vals[v.id] = v; });
    } else if (cfg.values) {
      vals = cfg.values;
    }

    var s = W / frame[0];
    var g = f.svg.append("g")
      .attr("transform", "translate(0," + titleH + ") scale(" + s + ")");
    var paths = g.selectAll("path").data(geo.paths || []).join("path")
      .attr("d", function (d) { return d.d; })
      .attr("class", function (d) {
        var v = vals[d.id];
        return "geo " + ((v && v.cls) || d.cls || "land");
      })
      .attr("stroke-width", 1)
      .attr("vector-effect", "non-scaling-stroke");

    var tip = DD.tip(f.box);
    if (cfg.tip) {
      paths
        .on("mousemove", function (e, d) {
          var v = vals[d.id] || { id: d.id };
          d3.select(this).classed("hilite", true).raise();
          tip.show(typeof cfg.tip === "function" ? cfg.tip(v, d)
                                                 : tipHtml(cfg.tip, v), e);
        })
        .on("mouseleave", function () {
          d3.select(this).classed("hilite", false);
          tip.hide();
        });
    }

    // labels: either passed in, or read off the geo file's label anchors
    var labs = cfg.geoLabels;
    if (labs === true) labs = (geo.paths || [])
      .filter(function (d) { return ok(d.lx) && d.lab; })
      .map(function (d) { return { x: d.lx, y: d.ly, text: d.lab, id: d.id }; });
    (labs || []).forEach(function (L) {
      f.svg.append("text")
        .attr("class", (L.cls || cfg.labelClass || "lbl") + (L.onMark ? " on-mark" : ""))
        .attr("x", L.x * s)
        // the geo files' label anchors are centre points, so sit the
        // baseline a third of the type size below the anchor
        .attr("y", titleH + L.y * s + (L.size || cfg.labelSize || 9.5) * 0.35)
        .attr("text-anchor", "middle").attr("pointer-events", "none")
        .attr("font-size", (L.size || cfg.labelSize || 9.5) + "px").text(L.text);
    });

    // the key: one swatch per class, because a classed map carries no ramp
    // the reader can decode without one
    if (cfg.key) {
      var items = cfg.key.items || [];
      var sw = cfg.key.swatch || 26, gap = 2;
      var kw = items.length * (sw + gap) - gap;
      var kx = (W - kw) / 2, ky = H - keyH + 18;
      if (cfg.key.title) f.svg.append("text").attr("class", "lbl")
        .attr("x", W / 2).attr("y", ky - 6).attr("text-anchor", "middle")
        .attr("font-size", "11px").text(cfg.key.title);
      items.forEach(function (it, i) {
        f.svg.append("rect").attr("class", it.cls)
          .attr("x", kx + i * (sw + gap)).attr("y", ky)
          .attr("width", sw).attr("height", 12);
        if (it.label !== undefined && it.label !== null && it.label !== "")
          f.svg.append("text").attr("class", "lbl")
            .attr("x", kx + i * (sw + gap) + sw / 2).attr("y", ky + 26)
            .attr("text-anchor", "middle").attr("font-size", "10px")
            .text(it.label);
      });
      if (cfg.key.left) f.svg.append("text").attr("class", "dem-txt")
        .attr("x", kx - 10).attr("y", ky + 10).attr("text-anchor", "end")
        .attr("font-size", "11px").text(cfg.key.left);
      if (cfg.key.right) f.svg.append("text").attr("class", "gop-txt")
        .attr("x", kx + kw + 10).attr("y", ky + 10)
        .attr("font-size", "11px").text(cfg.key.right);
    }

    return finish({ box: f.box, svg: f.svg, g: g, x: null, y: null,
                    W: W, H: H, M: f.M, scale: s, values: vals,
                    data: cfg.data, cfg: cfg, tip: tip }, cfg);
  };

  // ---- shared plumbing ----------------------------------------------------
  function ok(v) { return v !== null && v !== undefined && !Number.isNaN(v); }
  function uniq(a) {
    var seen = {}, out = [];
    a.forEach(function (v) { if (!seen[v]) { seen[v] = 1; out.push(v); } });
    return out;
  }
  function sizeOf(cfg, def) {
    var size = cfg.size || {};
    return { w: size.w || 760,
             h: size.h || (typeof def.h === "function" ? def.h(cfg) : def.h),
             m: Object.assign({}, def.m, size.m || {}) };
  }
  function legendItems(cfg) {
    var s = cfg.series || {};
    if (s.field && s.classes) {
      return Object.keys(s.classes).map(function (k) {
        return { cls: s.classes[k], label: k };
      });
    }
    if (s.fields) return wideSeries(cfg).map(function (d) {
      return { cls: d.cls, label: d.label };
    });
    return [];
  }
  // The hook is the escape hatch: a function(fig) run after everything above,
  // with the svg, the scales and the config in hand, for the one flourish a
  // chapter needs that the library should not grow an option for.
  var pendingSliders = [];
  function finish(fig, cfg) {
    pendingSliders.forEach(function (fn) { fn(fig); });
    pendingSliders = [];
    if (typeof cfg.hook === "function") cfg.hook(fig);
    return fig;
  }

  // ---- quantising to classes ----------------------------------------------
  // The choropleth's colours are ten fixed bins, dem side and gop side, and
  // the bin is decided by data, not by a colour scale: the class name is the
  // encoding. Mirrored by dd_ramp_class() in dd-charts.R so a map quantised
  // in R and a legend quantised here cannot disagree.
  DD.rampClass = function (v, cap, n) {
    cap = cap || 30; n = n || 5;
    if (v === null || v === undefined || Number.isNaN(v)) return "land";
    var side = v > 0 ? "r" : "d";
    var k = Math.min(n, Math.max(1, Math.ceil(Math.abs(v) / (cap / n))));
    if (v === 0) { side = "d"; k = 1; }
    return "ramp-" + side + k;
  };
  // The matching key items, extreme-dem on the left through extreme-gop.
  DD.rampKey = function (cap, n, fmt) {
    cap = cap || 30; n = n || 5;
    var f = DD.fmtByName(fmt || "signed0");
    var items = [];
    for (var i = n; i >= 1; i--)
      items.push({ cls: "ramp-d" + i, label: f(-(i * cap / n)) });
    for (var j = 1; j <= n; j++)
      items.push({ cls: "ramp-r" + j, label: f(j * cap / n) });
    return items;
  };

  // ---- dispatch -----------------------------------------------------------
  DD.fig = function (sel, cfg) {
    if (typeof d3 === "undefined") {
      console.error("DD.fig: d3 is not loaded; dd_libs() emits it first.");
      return null;
    }
    var box = d3.select(sel);
    if (box.empty()) { console.error("DD.fig: no element matches", sel); return null; }
    var fn = types[cfg.type];
    if (!fn) {
      console.error("DD.fig: unknown type \"" + cfg.type + "\"; have: " +
                    Object.keys(types).join(", "));
      return null;
    }
    addSlider(box, cfg, pendingSliders);
    return fn(box, cfg);
  };
  DD.types = types;   // so a chapter can add one of its own

  window.DD = DD;
})();
