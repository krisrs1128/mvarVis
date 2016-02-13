
// setup the arrow tips
var marker = function(svg, color) {
  svg.append("svg:defs").selectAll("marker")
    .data([color])      // Different link/path types can be defined here
    .enter().append("svg:marker")    // This section adds in the arrows
    .attr({ "id": String,
	    "viewBox": "0 -5 10 10",
	    "markerWidth": 4,
	    "markerHeight": 4,
	    "orient": "auto",
	    "fill": color
	  })
    .append("svg:path")
    .attr("d", "M0,-5L10,0L0,5");
  return "url(#" + color + ")"
}

var drawArrow = function(el, x, index, colInfo, sizeInfo) {
  var setup = drawSetup(el, x, index);

  // draw the arrows
  setup.svg.append("g")
    .attr("class", "mvar_arrow")
    .selectAll("line")
    .data(x)
    .enter()
    .append("line")
    .attr({
      x1: setup.scales.xScale(0),
      y1: setup.scales.yScale(0),
      x2: function(d) { return setup.scales.xScale(d.axis1); },
      y2: function(d) { return setup.scales.yScale(d.axis2); },
      "stroke": function(d) { return colInfo.colorScale(d[colInfo.curCol])},
      "stroke-width": function(d) { return .3 * sizeInfo.sizeScale(d[sizeInfo.curSize]) },
      "opacity": 0.7,
      "index": index,
      "marker-end": function(d) {
	return marker(setup.svg, colInfo.colorScale(d[colInfo.curCol]))
      }
    });

  // define interactivity for the arrow
  setup.svg.selectAll(".mvar_arrow > line")
    .on("mouseover", function(d) {
      sizeInfo = getSizeInfo(el, x, index);
      d3.select(this)
	.transition()
	.duration(75)
	.attr({"stroke-width": function(z) { return .6 * sizeInfo.sizeScale(z[sizeInfo.curSize]) },

	       "opacity": 1});
      hoverTable(el, d, d3.select(this).attr("index"));
    });
  setup.svg.selectAll(".mvar_arrow > line")
    .on("mouseout", function(d) {
      sizeInfo = getSizeInfo(el, x, index);
      d3.select(this)
	.transition()
	.duration(75)
	.attr({"stroke-width": function(z) { return .3 * sizeInfo.sizeScale(z[sizeInfo.curSize]) },
	       "opacity": .7});
    });
}

var updateArrows = function(el, x, index, opts) {
  var group = d3.select(el)
      .selectAll("div")
      .filter(function(d) { return d == index; })
  var colInfo = getColorInfo(el, x, index, opts);
  var sizeInfo = getSizeInfo(el, x, index);
  group.selectAll(".mvar_arrow > line")
    .attr({"stroke": function(d) { return colInfo.colorScale(d[colInfo.curCol]); },
	   "stroke-width": function(d) { return .3 * sizeInfo.sizeScale(d[sizeInfo.curSize]) },
	   "marker-end": function(d) {
	     return marker(group.select("svg"), colInfo.colorScale(d[colInfo.curCol]));
	   }
	  });
}
