
var drawText = function(el, x, index, opts) {
  var colInfo = getColorInfo(el, x, index, opts);
  var sizeInfo = getSizeInfo(el, x, index, opts);

  var setup = drawSetup(el, x, index);
  setup.svg.selectAll(".mvar_text")
    .data(x).enter()
    .append("text")
    .classed("mvar_text", true)
    .text(function(d) {
      return d.label;
    })
    .attr({
      "x": function(d) { return setup.scales.xScale(d.axis1); },
      "y": function(d) { return setup.scales.yScale(d.axis2); },
      "fill": function(d) { return colInfo.colorScale(d[colInfo.curCol]); },
      "stroke-width": ".4px",
      "stroke": "black",
      "opacity": 0.8,
      "font-size": function(d) { return 1.8 * (sizeInfo.sizeScale(d[sizeInfo.curSize]) * .7 + .5 * 8); },
      "index": index
    })

  // define interactivity for the text
  setup.svg.selectAll(".mvar_text")
    .on("mouseover", function(d) {
      sizeInfo = getSizeInfo(el, x, index, opts);
      d3.select(this)
	.transition()
	.duration(75)
	.attr({"font-size": function(z) {return 1.8 * (sizeInfo.sizeScale(d[sizeInfo.curSize]) * .7 + 8); },
	       "opacity": 1});
      hoverTable(el, d, d3.select(this).attr("index"));
    });

  setup.svg.selectAll(".mvar_text")
    .on("mouseout", function(d) {
      sizeInfo = getSizeInfo(el, x, index, opts);
      d3.select(this)
	.transition()
	.duration(75)
	.attr({"font-size": function(z)  { return 1.8 * (sizeInfo.sizeScale(d[sizeInfo.curSize]) * .7 + .5 * 8); },
	       "opacity": .8});
    });

  setup.svg.selectAll(".mvar_text")
    .data(x).exit()
    .remove();

  setup.svg.selectAll(".mvar_text")
    .transition()
    .duration(750)
    .attr({"fill": function(d) { return colInfo.colorScale(d[colInfo.curCol]); },
	   "font-size": function(d) { return 1.8 * (sizeInfo.sizeScale(d[sizeInfo.curSize]) * .7 + .5 * 8) }})
}
