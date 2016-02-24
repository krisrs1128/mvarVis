
var drawSetup = function(el, x, index) {
  var group = d3.select(el)
      .selectAll(".mvar-table")
      .filter(function(d) { return d == index; })
      .select("#scatterplot")
  var svg = group.select("#scatter")
  var scales = getScales(getDomain(x), svg.attr("width"), svg.attr("height"));
  return {"group": group, "svg": svg, "scales": scales};
}

var drawScatter = function(el, x, index, opts) {
  // draw (or remove) circles
  if($.inArray("point", opts["type"]) != -1) {
    drawCircles(el, x, index, opts);
  } else {
    drawCircles(el, [], index, opts);
  }

  // draw (or remove) text
  if($.inArray("text", opts["type"]) != -1) {
    drawText(el, x, index, opts);
  } else {
    drawText(el, [], index, opts);
  }

  // draw (or remove) arrows
  if($.inArray("arrow", opts["type"]) != -1) {
    drawArrow(el, x, index, opts);
  } else {
    drawArrow(el, [], index, opts);
  }
}
