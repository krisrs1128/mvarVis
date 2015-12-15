
var createInput = function(el, x, index) {
    // create a button for coloring the points
    var select = d3.selectAll("div")
	.filter(function(d) { return d == index; } )
	.append("select")
	.on("change", function(z) { return drawScatter(x, index); });
    var options = select
	.selectAll("option")
	.data(Object.keys(x[0]))
	.enter()
	.append("option")
	.text(function(d) { return d; });
}
