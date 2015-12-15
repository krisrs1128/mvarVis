
var createInput = function(el, x, index) {
    // create a button for coloring the points
    var select = d3.selectAll("div")
	.filter(function(d) { return d == index; } )
	.append("select")
	.on("change", function(z) {
	    updateCircles(x, index);
	    updateText(x, index);
	    updateArrows(x, index);
	});
    var options = select
	.selectAll("option")
	.data(Object.keys(x[0]))
	.enter()
	.append("option")
	.text(function(d) { return d; });
}

var getColorInfo = function(x, index) {
    // get the selected color scale
    var group = d3.selectAll("div")
	.filter(function(d) { return d == index; })
    var select = group.selectAll("select")
    var options = group.selectAll("option")
    var selectedIndex = select.property("selectedIndex")
    var curCol = options[0][selectedIndex].__data__;
    var colorDomain = uniqueValues(x, curCol)
    var colorScale;
    if(isNumeric(colorDomain[0])) {
	colorDomain = colorDomain.map(parseInt)
	colorScale = d3.scale
	    .quantize()
	    .domain([d3.min(colorDomain), d3.max(colorDomain)])
	    .range(colorbrewer.Purples[5])
    } else {
	if(colorDomain.length < 3) {
	    colorDomain = colorDomain.concat(["dummyColor1", "dummyColor2"])
	}
	colorScale = d3.scale.ordinal()
	    .domain(colorDomain)
	    .range(colorbrewer.Set2[d3.min([colorDomain.length, 8])]) // limit on number of ordinal colors
    }
    return {"curCol": curCol, "colorScale": colorScale};
}
