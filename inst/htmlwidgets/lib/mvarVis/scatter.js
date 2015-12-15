
var drawSetup = function(index) {
    var group = d3.selectAll("div")
	.filter(function(d) { return d == index; });

    // draw the circles
    var svg = group.select("svg")
    var scales = getScales(svg.attr("width"), svg.attr("height"));
    return {"group": group, "svg": svg, "scales": scales};
}

drawCircles = function(x, index, colInfo) {
    var setup = drawSetup(index);
    setup.svg.append("g")
	.attr("class", "circle")
	.selectAll("circle")
	.data(x)
	.enter()
	.append("circle")
	.attr({cx: function (d) { return setup.scales.xScale(d.axis1); },
               cy: function (d) { return setup.scales.yScale(d.axis2); },
               r: 4,
	       fill: function(d) { return colInfo.colorScale(d[colInfo.curCol]); },
	       opacity: 0.7,
	       index: index
	      });

    // define interactivity for the circles
    d3.selectAll("circle")
	.on("mouseover", function(d) {
	    d3.select(this)
		.transition()
		.duration(75)
		.attr({r: 8,
		       opacity: 1});
	    hoverTable(d, d3.select(this).attr("index"));
	});
    d3.selectAll("circle")
	.on("mouseout", function(d) {
	    d3.select(this)
		.transition()
		.duration(75)
		.attr({r: 4,
		       opacity: 0.7})
	});
}

updateCircles = function(x, index){
    var group = d3.selectAll("div")
	.filter(function(d) { return d == index; })
    var colInfo = getColorInfo(x, index);
    group.selectAll("circle")
	.attr("fill", function(d) { return colInfo.colorScale(d[colInfo.curCol]); })
}

drawText = function(x, index, colInfo) {
    var setup = drawSetup(index);
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
	    x: function(d) { return setup.scales.xScale(d.axis1); },
	    y: function(d) { return setup.scales.yScale(d.axis2); },
	    fill: function(d) { return colInfo.colorScale(d[colInfo.curCol]); },
	    opacity: .7,
	    "font-size": 13,
	    index: index
	});

    // define interactivity for the circles
    d3.selectAll(".mvar_text")
	.on("mouseover", function(d) {
	    d3.select(this)
		.transition()
		.duration(75)
		.attr({"font-size": 16,
		       opacity: 1});
	    hoverTable(d, d3.select(this).attr("index"));
	});
    d3.selectAll(".mvar_text")
	.on("mouseout", function(d) {
	    d3.select(this)
		.transition()
		.duration(75)
		.attr({opacity: .7,
		       "font-size": 13,
		      })
	});
}

updateText = function(x, index) {
    var group = d3.selectAll("div")
	.filter(function(d) { return d == index; })
    var colInfo = getColorInfo(x, index);
    group.selectAll(".mvar_text")
	.attr("fill", function(d) { return colInfo.colorScale(d[colInfo.curCol]); })
}

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

drawArrow = function(x, index, colInfo) {
    var setup = drawSetup(index);


    // draw the arrows
    setup.svg.append("g")
	.attr("class", "line")
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
    d3.selectAll("line")
	.on("mouseover", function(d) {
	    d3.select(this)
		.transition()
		.duration(75)
		.attr({ "opacity": 1});
	    hoverTable(d, d3.select(this).attr("index"));
	});
    d3.selectAll("line")
	.on("mouseout", function(d) {
	    d3.select(this)
		.transition()
		.duration(75)
		.attr({ "opacity": .7});
	});
}

updateArrows = function(x, index) {
    var group = d3.selectAll("div")
	.filter(function(d) { return d == index; })
    var colInfo = getColorInfo(x, index);
    group.selectAll("line")
	.attr("stroke", function(d) { return colInfo.colorScale(d[colInfo.curCol]); })
}


drawScatter = function(x, index, type) {
    var colInfo = getColorInfo(x, index);
    switch(type) {
      case "point":
  	drawCircles(x, index, colInfo);
	break;
      case "text":
	drawText(x, index, colInfo);
	break;
    case "arrow":
	drawArrow(x, index, colInfo);
	break;
    }
}
