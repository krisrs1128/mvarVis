
drawCircles = function(x, index, colInfo) {
    var group = d3.selectAll("div")
	.filter(function(d) { return d == index; });

    // draw the circles
    var svg = group.select("svg")
    var scales = getScales(svg.attr("width"), svg.attr("height"));
    svg.append("g")
	.attr("class", "circle")
	.selectAll("circle")
	.data(x)
	.enter()
	.append("circle")
	.attr({cx: function (d) { return scales.xScale(d.axis1); },
               cy: function (d) { return scales.yScale(d.axis2); },
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
    console.log("updating circles")
    var group = d3.selectAll("div")
	.filter(function(d) { return d == index; })
    console.log("getting colors...")
    var colInfo = getColorInfo(x, index);
    console.log(colInfo);
    group.selectAll("circle")
	.attr("fill", function(d) { return colInfo.colorScale(d[colInfo.curCol]); })
}

drawScatter = function(x, index) {
    var colInfo = getColorInfo(x, index);
    drawCircles(x, index, colInfo);
}
