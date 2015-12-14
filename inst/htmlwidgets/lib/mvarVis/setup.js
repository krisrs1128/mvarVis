var padding = 20;
var getScales = function(width, height) {
    //Create scale functions
    var xScale = d3.scale.linear()
	.domain([-5, 5])
	.range([padding, width - padding]);

    var yScale = d3.scale.linear()
	.domain([-5, 5])
	.range([height - padding, padding]);
    return {"xScale": xScale, "yScale": yScale}
}

var setupSVG = function(el, width, height) {

    var scales = getScales(width, height);

    // Define axes
    var xAxis = d3.svg.axis()
	.scale(scales.xScale)
	.orient("bottom");
    var yAxis = d3.svg.axis()
	.scale(scales.yScale)
	.orient("right");

    //Create SVG element
    var svg = d3.select(el)
	.append("svg")
	.attr("width", width)
	.attr("height", height);

    svg.append("g")
	.attr("class", "axis")
	.call(xAxis)
	.attr("transform", "translate(0, " +  height / 2 + ")")
	.selectAll("text")

    svg.append("g")
	.attr("class", "axis")
	.call(yAxis)
	.attr("transform", "translate(" + width / 2 + ", 0)")
}

