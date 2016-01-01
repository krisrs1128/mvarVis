// Extract quantitative variables from an array
// Args:
//   x0: An object with some quantitative keys, potentially.
// Returns:
//   An array whose elements are the keys of quantiative variables in x.
var getQuantiVars = function(x0) {
    var quantiVars = [];
    for(curKey in x0) {
	var z = x0[curKey];
	if(isNumeric(z)) {
	    quantiVars = quantiVars.concat(curKey);
	}
    }
    return quantiVars;
}

// Create an input selection for only quantitative variables
// Args:
//   el: The element onto which to add the input.
//   x: An array containing the data.
//   index: The panel within el onto which to draw the current input.
// Returns:
//   null, but as a side effect adds a input to el.
var createQuantiInput = function(el, x, index) {
    // create a dropdown selection for quantitative features
    var quantiVars = getQuantiVars(x[0]);
    var quantiX = d3.range(x.length).map(function(z) { return {} });
    for(var j in quantiVars) {
	for(var k in d3.range(x.length)) {
	    quantiX[k][quantiVars[j]] = x[k][quantiVars[j]];
	}
    }
    createInput(el, quantiX, index);
}

var createInput = function(el, x, index) {
    // create a dropdown selection
    var select = d3.select(el)
	.selectAll("div")
	.filter(function(d) { return d == index; } )
	.append("select")
	.on("change", function(z) {
	    updateCircles(el, x, index);
	    updateText(el, x, index);
	    updateArrows(el, x, index);
	});
    var options = select
	.selectAll("option")
	.data(Object.keys(x[0]))
	.enter()
	.append("option")
	.text(function(d) { return d; });
}

var getColorInfo = function(el, x, index) {
    // get the selected color scale
    var group = d3.select(el)
	.selectAll("div")
	.filter(function(d) { return d == index; })
    var select = group.selectAll("select")
    var options = group.selectAll("option")
    var selectedIndex = select.property("selectedIndex")
    var curCol = options[0][selectedIndex].__data__;
    var colorDomain = uniqueValues(x, curCol)
    var colorScale;
    if(isNumeric(colorDomain[0])) {
	colorDomain = colorDomain.map(parseFloat)
	colorScale = d3.scale
	    .quantize()
	    .domain([d3.min(colorDomain), d3.max(colorDomain)])
	    .range(colorbrewer.RdBu[11])
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
