
var drawCircles = function(el, x, index, colInfo, sizeInfo) {
    var setup = drawSetup(el, x, index);
    setup.svg.append("g")
	.attr("class", "circle")
	.selectAll("circle")
	.data(x)
	.enter()
	.append("circle")
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
	    var newR = 2 * d3.select(this).attr("r");
	    d3.select(this)
		.transition()
		.duration(75)
		.attr({"r": newR, "opacity": 1});
	    hoverTable(el, d, d3.select(this).attr("index"));
	});
    setup.svg.selectAll("circle")
	.on("mouseout", function(d) {
	    var newR = .5 * d3.select(this).attr("r");
	    d3.select(this)
		.transition()
		.duration(75)
		.attr({"r": newR, "opacity": 0.7})
	});
}

var updateCircles = function(el, x, index){
    var group = d3.select(el)
	.selectAll("div")
	.filter(function(d) { return d == index; })
    var colInfo = getColorInfo(el, x, index);
    var sizeInfo = getSizeInfo(el, x, index);
    group.selectAll("circle")
	.attr({"fill": function(d) { return colInfo.colorScale(d[colInfo.curCol]); },
	       "r": function(d) { return sizeInfo.sizeScale(d[sizeInfo.curSize]); }})
}
