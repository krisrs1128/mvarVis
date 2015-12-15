
var drawCircles = function(el, x, index, colInfo) {
    var setup = drawSetup(el, x, index);
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
    setup.svg.selectAll("circle")
	.on("mouseover", function(d) {
	    d3.select(this)
		.transition()
		.duration(75)
		.attr({r: 8,
		       opacity: 1});
	    hoverTable(el, d, d3.select(this).attr("index"));
	});
    setup.svg.selectAll("circle")
	.on("mouseout", function(d) {
	    d3.select(this)
		.transition()
		.duration(75)
		.attr({r: 4,
		       opacity: 0.7})
	});
}

var updateCircles = function(el, x, index){
    var group = d3.select(el)
	.selectAll("div")
	.filter(function(d) { return d == index; })
    var colInfo = getColorInfo(el, x, index);
    group.selectAll("circle")
	.attr("fill", function(d) { return colInfo.colorScale(d[colInfo.curCol]); })
}
