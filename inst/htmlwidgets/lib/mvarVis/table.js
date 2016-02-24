
var makeTable = function(el, width, x, index) {
  // make a table
  var table = d3.select(el)
      .selectAll("div")
      .filter(function(d) { return d == index; } )
      .select("#table")
      .append("table")
      .attr({"class": "fixed",
	     "width": width * .99
	    })
      .style("visibility", "hidden")
  var x_keys = Object.keys(x[0]);

  table.append("tr")
    .selectAll("th")
    .data(x_keys)
    .enter()
    .append("th")
    .text(function(d) { return d; });
  table.append("tr")
    .selectAll("td")
    .data(x_keys)
    .enter()
    .append("td")
    .attr("id", function(d) { return d; })
}

var hoverTable = function(el, d, index) {
  var group = d3.select(el)
      .selectAll("div")
      .filter(function(d) { return d == index; })
  group.select("table")
    .style("visibility", "visible");

  ids = Object.keys(d)
  ids.forEach(function(x) {
    if(isNumeric(d[x])) {
      var printVal = d3.round(d[x], 2)
    } else {
      var printVal = d[x]
    }
    group.select("#" + x)
      .html(printVal)
  });
}
