
var drawSetup = function(el, x, index) {
  var group = d3.select(el)
      .selectAll("div")
      .filter(function(d) { return d == index; });
  var svg = group.select("#scatter")
  var scales = getScales(getDomain(x), svg.attr("width"), svg.attr("height"));
  return {"group": group, "svg": svg, "scales": scales};
}

var drawScatter = function(el, x, index, opts) {
  if($.inArray("point", opts["type"]) != -1) {
    drawCircles(el, x, index, opts);
  } else {
    drawCircles(el, [], index, opts);
  }
  if($.inArray("text", opts["type"]) != -1) {
    drawText(el, x, index, opts);
  } else {
    drawText(el, [], index, opts);
  }
  if($.inArray("arrow", opts["type"]) != -1) {
    drawArrow(el, x, index, opts);
  }
}
