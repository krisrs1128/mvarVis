
var createInput = function(el, x) {
    // create a button for coloring the points
    var select = d3.select(el)
	.append("select")
	.on("change", function(z) { return drawScatter(x); });
    var options = select
	.selectAll("option")
	.data(Object.keys(x[0]))
	.enter()
	.append("option")
	.text(function(d) { return d; });
}
