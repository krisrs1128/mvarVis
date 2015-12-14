
drawScatter = function(x) {
    // get the selected color scale
    var select = d3.selectAll("select")
    var options = d3.selectAll("option")
    var selectedIndex = select.property("selectedIndex")
    var curCol = options[0][selectedIndex].__data__;
    var colorDomain = uniqueValues(x, curCol)
    var colorScale = d3.scale.category20b()
	.domain(colorDomain);

    // draw the circles
    var svg = d3.select("svg")
    var scales = getScales(svg.attr("width"), svg.attr("height"));
    svg.append("g")
	.attr("class", "circle")
	.selectAll("circle")
	.data(x)
	.enter()
	.append("circle")
	.attr({cx: function (d) { return scales.xScale(d.layer_1); },
               cy: function (d) { return scales.yScale(d.layer_2); },
               r: 4,
	       fill: function(d) { return colorScale(d[curCol]); }
	      });

    // define interactivity for the circles
    d3.selectAll("circle")
	.on("mouseover", function(d) {
	    d3.select(this)
		.transition()
		.duration(75)
		.attr({ r: 8 });
	    hoverTable(d);
	});
    d3.selectAll("circle")
	.on("mouseout", function(d) {
	    d3.select(this)
		.transition()
		.duration(75)
		.attr({ r: 4 })
	});
}
