
var drawCircles = function(el, x, index, opts) {
  var colInfo = getColorInfo(el, x, index, opts);
  var sizeInfo = getSizeInfo(el, x, index, opts);

  var setup = drawSetup(el, x, index);
  setup.svg.selectAll("circle")
    .data(x)
    .enter()
    .append("circle")
    .classed("circle", true)
    .attr({"cx": function (d) { return setup.scales.xScale(d.axis1); },
           "cy": function (d) { return setup.scales.yScale(d.axis2); },
           "r": function(d) { return sizeInfo.sizeScale(d[sizeInfo.curSize]) },
	   "fill": function(d) { return colInfo.colorScale(d[colInfo.curCol]); },
	   "opacity": 0.7,
	   "index": index
	  });


  // define interactivity for the circles
  setup.svg.selectAll("circle")
    .on("mouseover", function(d) {
      sizeInfo = getSizeInfo(el, x, index, opts);
      d3.select(this)
	.transition()
	.duration(75)
	.attr({"r": function(z) { return 1.5 * sizeInfo.sizeScale(z[sizeInfo.curSize]) },
	       "opacity": 1});
      hoverTable(el, d, d3.select(this).attr("index"));
    });
  setup.svg.selectAll("circle")
    .on("mouseout", function(d) {
      sizeInfo = getSizeInfo(el, x, index, opts);
      d3.select(this)
	.transition()
	.duration(75)
	.attr({"r": function(z) { return sizeInfo.sizeScale(z[sizeInfo.curSize]) },
	       "opacity": 0.7})
    });

  // remove points that have been removed from x
  setup.svg.selectAll("circle")
    .data(x)
    .exit()
    .remove()

  // update the circles with new attributes
  setup.svg.selectAll("circle")
    .transition()
    .duration(750)
    .attr({"fill": function(d) { return colInfo.colorScale(d[colInfo.curCol]); },
	   "r": function(d) { return sizeInfo.sizeScale(d[sizeInfo.curSize]); }})
}
