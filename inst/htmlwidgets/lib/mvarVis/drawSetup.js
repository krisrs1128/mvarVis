
var drawSetup = function(x, index) {
    var group = d3.selectAll("div")
	.filter(function(d) { return d == index; });

    // draw the circles
    var svg = group.select("svg")
    console.log(x);
    var scales = getScales(getDomain(x), svg.attr("width"), svg.attr("height"));
    console.log(scales);
    return {"group": group, "svg": svg, "scales": scales};
}
