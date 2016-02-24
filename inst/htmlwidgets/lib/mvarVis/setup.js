
var getScales = function(domain, width, height) {
  //Create scale functions
  var padding = 20
  var xScale = d3.scale.linear()
      .domain(domain["y_domain"])
      .range([padding, width - padding]);
  var yScale = d3.scale.linear()
      .domain(domain["y_domain"])
      .range([height - padding, padding]);
  return {"xScale": xScale, "yScale": yScale}
}

var getDomain = function(x) {
  var axis1 = x.map(function(z) { return Math.abs(z.axis1) })
  var axis2 = x.map(function(z) { return Math.abs(z.axis2) })
  var axis_max = 1.2 * d3.max(axis1.concat(axis2))
  return {"x_domain": [-axis_max, axis_max],
	  "y_domain": [-axis_max, axis_max]}
}

var setupSVG = function(el, x, width, height, index, number) {
  var scales = getScales(getDomain(x), width, height);

  // Define axes
  var xAxis = d3.svg.axis()
      .scale(scales.xScale)
      .orient("bottom");
  var yAxis = d3.svg.axis()
      .scale(scales.yScale)
      .orient("right");

  //Create SVG element
  var svg = d3.select(el)
      .selectAll(".mvar-table")
      .filter(function(d) { return d == index; })
      .select("#scatterplot")
      .append("svg")
      .attr({"width": width,
	     "id": "scatter",
	     "height": height});

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

var setupElems = function(el, number, width) {
  var divs = d3.select(el)
      .selectAll("div")
      .data(d3.range(number))
      .enter()
      .append("div")
      .classed("mvar-table", true)

  divs.append("div")
    .classed("row-fluid", true)
    .attr({"id": "table"})
  var viewport = divs.append("div")
    .classed("row-fluid", true)
    .attr("id", "viewport")
  viewport.append("div")
    .classed("span9", true)
    .attr({"id": "scatterplot"})
  viewport.append("div")
    .classed("span3", true)
    .append("form")
    .classed("well", true)
    .attr({"id": "allInputs"})
}
