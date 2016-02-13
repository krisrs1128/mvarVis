
var drawText = function(el, x, index, colInfo, sizeInfo) {
  var setup = drawSetup(el, x, index);
  setup.svg.append("g")
    .attr("class", "mvar_text")
    .selectAll("text")
    .data(x)
    .enter()
    .append("text")
    .text(function(d) {
      return d.label;
    })
    .attr({
      "x": function(d) { return setup.scales.xScale(d.axis1); },
      "y": function(d) { return setup.scales.yScale(d.axis2); },
      "fill": function(d) { return colInfo.colorScale(d[colInfo.curCol]); },
      "opacity": .7,
      "font-size": function(d) { return 1.2 * (sizeInfo.sizeScale(d[sizeInfo.curSize]) * .7 + .3 * 8); },
      "index": index
    });

  // define interactivity for the circles
  setup.svg.selectAll(".mvar_text > text")
    .on("mouseover", function(d) {
      sizeInfo = getSizeInfo(el, x, index);
      d3.select(this)
	.transition()
	.duration(75)
	.attr({"font-size": function(z) { return 1.6 * (sizeInfo.sizeScale(d[sizeInfo.curSize]) * .7 + .3 * 8); },
	       "opacity": 1});
      hoverTable(el, d, d3.select(this).attr("index"));
    });
  setup.svg.selectAll(".mvar_text > text")
    .on("mouseout", function(d) {
      sizeInfo = getSizeInfo(el, x, index);
      d3.select(this)
	.transition()
	.duration(75)
	.attr({"font-size": function(z) { return 1.2 * (sizeInfo.sizeScale(d[sizeInfo.curSize]) * .7 + .3 * 8) },
	       "opacity": .8});
    });
}

var updateText = function(el, x, index, opts) {
  var group = d3.select(el)
      .selectAll("div")
      .filter(function(d) { return d == index; })
  var colInfo = getColorInfo(el, x, index, opts);
  var sizeInfo = getSizeInfo(el, x, index);
  group.selectAll(".mvar_text > text")
    .attr({"fill": function(d) { return colInfo.colorScale(d[colInfo.curCol]); },
	   "font-size": function(d) { return 1.2 * (sizeInfo.sizeScale(d[sizeInfo.curSize]) * .7 + .3 * 8) }})
}
