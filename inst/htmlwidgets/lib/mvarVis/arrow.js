
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

var drawArrow = function(x, index, colInfo) {
    var setup = drawSetup(x, index);

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
	    "stroke-width": 3,
	    "opacity": 0.7,
	    "index": index,
	    "marker-end": function(d) {
		return marker(setup.svg, colInfo.colorScale(d[colInfo.curCol]))
	    }
	});

    // define interactivity for the arrow
    d3.selectAll(".mvar_arrow > line")
	.on("mouseover", function(d) {
	    d3.select(this)
		.transition()
		.duration(75)
		.attr({ "opacity": 1});
	    hoverTable(d, d3.select(this).attr("index"));
	});
    d3.selectAll(".mvar_arrow > line")
	.on("mouseout", function(d) {
	    d3.select(this)
		.transition()
		.duration(75)
		.attr({ "opacity": .7});
	});
}

var updateArrows = function(x, index) {
    var group = d3.selectAll("div")
	.filter(function(d) { return d == index; })
    var colInfo = getColorInfo(x, index);
    group.selectAll(".mvar_arrow > line")
	.attr({"stroke": function(d) { return colInfo.colorScale(d[colInfo.curCol]); },
	       "marker-end": function(d) {
		   return marker(group.select("svg"), colInfo.colorScale(d[colInfo.curCol]));
	       }
	      });
}
