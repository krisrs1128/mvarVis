
var drawSetup = function(x, index) {
    var group = d3.selectAll("div")
	.filter(function(d) { return d == index; });

    // draw the circles
    var svg = group.select("svg")
    var scales = getScales(getDomain(x), svg.attr("width"), svg.attr("height"));
    return {"group": group, "svg": svg, "scales": scales};
}
