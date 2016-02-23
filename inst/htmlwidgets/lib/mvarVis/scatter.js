
var drawSetup = function(el, x, index) {
  var group = d3.select(el)
      .selectAll("div")
      .filter(function(d) { return d == index; });
  var svg = group.select("#scatter")
  var scales = getScales(getDomain(x), svg.attr("width"), svg.attr("height"));
  return {"group": group, "svg": svg, "scales": scales};
}

var drawScatter = function(el, x, index, opts) {
  switch(opts["type"]) {
  case "point":
    drawCircles(el, x, index, opts);
    break;
  case "text":
    drawText(el, x, index, opts);
    break;
  case "arrow":
    drawArrow(el, x, index, opts);
    break;
  }
}
