
drawScatter = function(x, index) {
    // get the selected color scale
    var group = d3.selectAll("div")
	.filter(function(d) { return d == index; })
    var select = group.selectAll("select")
    var options = group.selectAll("option")
    var selectedIndex = select.property("selectedIndex")
    var curCol = options[0][selectedIndex].__data__;
    var colorDomain = uniqueValues(x, curCol)
    if(isNumeric(colorDomain[0])) {
	var colorScale = d3.scale
	    .quantize([d3.min(colorDomain), d3.max(colorDomain)])
	    .range(colorbrewer.YlGn[5])
    } else {
	var colorScale = d3.scale.ordinal()
	    .domain(colorDomain)
	    .range(colorbrewer.Set3[d3.min([colorDomain.length, 12])]) // limit on number of ordinal colors
    }

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
	       fill: function(d) { return colorScale(d[curCol]); },
	       index: index
	      });

    // define interactivity for the circles
    d3.selectAll("circle")
	.on("mouseover", function(d) {
	    d3.select(this)
		.transition()
		.duration(75)
		.attr({ r: 8 });
	    hoverTable(d, d3.select(this).attr("index"));
	});
    d3.selectAll("circle")
	.on("mouseout", function(d) {
	    d3.select(this)
		.transition()
		.duration(75)
		.attr({ r: 4 })
	});
}
