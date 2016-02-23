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

var createAllInputs = function(el, x, index, opts) {
      sizeVars = getQuantiVars(x[0]);
      sizeVars.unshift("NULL");
      colVars = Object.keys(x[0]);
      colVars.unshift("NULL");
      createInput(el, x, index, opts, colVars);
      createInput(el, x, index, opts, sizeVars);
}

// Create an input selection for all variables in x
var createInput = function(el, x, index, opts, selectVars) {
  // create a dropdown selection
  var select = d3.select(el)
      .selectAll("div")
      .filter(function(d) { return d == index; } )
      .append("select")
      .on("change", function(z) {
	updateCircles(el, x, index, opts);
	updateText(el, x, index, opts);
	updateArrows(el, x, index, opts);
      });

  var options = select
      .selectAll("option")
      .data(selectVars)
      .enter()
      .append("option")
      .text(function(d) { return d; });
}

var getInput = function(el, x, index, inputIx) {
  // get the selected options
  var group = d3.select(el)
      .selectAll("div")
      .filter(function(d) { return d == index; })
  var select = group.selectAll("select")[0][inputIx]
  var options = group.selectAll("select").selectAll("option")[inputIx]
  var selectedIndex = select.selectedIndex
  var curOption = options[selectedIndex].__data__;
  return {"selectedIndex": selectedIndex,
	  "curOption": curOption};
}

var getSizeInfo = function(el, x, index) {
  var sizeInfo = getInput(el, x, index, 1);
  var sizeDomain = uniqueValues(x, sizeInfo.curOption).map(parseFloat)
  var sizeScale = d3.scale.linear()
      .domain([d3.min(sizeDomain), d3.max(sizeDomain)])
      .range([4, 15])
  return {"curSize": sizeInfo.curOption, "sizeScale": sizeScale};
}

var getColorInfo = function(el, x, index, opts) {
  // get the selected color scale
  var colInfo = getInput(el, x, index, 0);
  var colorDomain = uniqueValues(x, colInfo.curOption)
  if(colorDomain[0] == "no_color") {
    return {"curCol": colInfo.curOption,
	    "colorScale": d3.scale.ordinal()
	    .domain(colorDomain)
	    .range(["#424242"])}
  }

  var colorScale;
  if(isNumeric(colorDomain[0])) {
    colorDomain = colorDomain.map(parseFloat)
    colorScale = d3.scale
      .quantize()
      .domain([d3.min(colorDomain), d3.max(colorDomain)])
      .range(opts["continuous_palette"][0])
  } else {
    if(colorDomain.length < 3) {
      colorDomain = colorDomain.concat(["dummyColor1", "dummyColor2"])
    }
    var max_cols = d3.min([colorDomain.length, opts["ordinal_palette"][0].length]);
    colorScale = d3.scale.ordinal()
      .domain(colorDomain)
      .range(opts["ordinal_palette"][0].slice(0, max_cols))
  }
  return {"curCol": colInfo.curOption, "colorScale": colorScale};
}
