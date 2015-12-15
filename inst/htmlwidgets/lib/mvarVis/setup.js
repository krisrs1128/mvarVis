var padding = 20
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

var setupSVG = function(el, width, height, index, number) {
    var scales = getScales(width / number, height);
    // Define axes
    var xAxis = d3.svg.axis()
	.scale(scales.xScale)
	.orient("bottom");
    var yAxis = d3.svg.axis()
	.scale(scales.yScale)
	.orient("right");

    //Create SVG element
    var svg = d3.selectAll("div")
	.filter(function(d) { return d == index; })
	.append("svg")
	.attr("width", width / number)
	.attr("height", height);

    svg.append("g")
	.attr("class", "axis")
	.call(xAxis)
	.attr("transform", "translate(0, " +  height / 2 + ")")
	.selectAll("text")

    svg.append("g")
	.attr("class", "axis")
	.call(yAxis)
	.attr("transform", "translate(" + width / (2 * number) + ", 0)")
}

var setupElems = function(el, number, width) {
    var div = d3.select(el)
	.selectAll("div")
	.data(d3.range(number))
	.enter()
	.append("div")
	.attr("class", "container")
	.style("width", width / number + "px");
}

