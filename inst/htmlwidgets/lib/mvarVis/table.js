
var makeTable = function(el, x) {
    // make a table
    var table = d3.select(el).
	append("table")
	.attr("class", "fixed");
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
	.attr("id", function(d) { return d; });
}

var hoverTable = function(d) {
    ids = Object.keys(d)
    ids.forEach(function(x) {
	d3.select("#" + x)
	    .html(d[x])
    });
}
