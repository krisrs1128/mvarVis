
var makeTable = function(el, x, index) {
    // make a table
    var table = d3.selectAll("div")
	.filter(function(d) { return d == index; } )
	.append("table")
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

var hoverTable = function(d, index) {
    var group = d3.selectAll("div")
	.filter(function(d) { return d == index; })

    console.log(group)

    ids = Object.keys(d)
    ids.forEach(function(x) {
	group.select("#" + x)
	    .html(d[x])
    });
}
