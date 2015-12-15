
var drawText = function(x, index, colInfo) {
    var setup = drawSetup(x, index);
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
    d3.selectAll(".mvar_text > text")
	.on("mouseover", function(d) {
	    d3.select(this)
		.transition()
		.duration(75)
		.attr({"font-size": 16,
		       opacity: 1});
	    hoverTable(d, d3.select(this).attr("index"));
	});
    d3.selectAll(".mvar_text > text")
	.on("mouseout", function(d) {
	    d3.select(this)
		.transition()
		.duration(75)
		.attr({opacity: .7,
		       "font-size": 13,
		      })
	});
}

var updateText = function(x, index) {
    var group = d3.selectAll("div")
	.filter(function(d) { return d == index; })
    var colInfo = getColorInfo(x, index);
    group.selectAll(".mvar_text > text")
	.attr("fill", function(d) { return colInfo.colorScale(d[colInfo.curCol]); })
}
