
var drawSetup = function(el, x, index) {
    var group = d3.select(el)
	.selectAll("div")
	.filter(function(d) { return d == index; });
    var svg = group.select("svg")
    var scales = getScales(getDomain(x), svg.attr("width"), svg.attr("height"));
    return {"group": group, "svg": svg, "scales": scales};
}

var drawScatter = function(el, x, index, type) {
    var colInfo = getColorInfo(el, x, index);
    var sizeInfo = getSizeInfo(el, x, index);
    switch(type) {
      case "point":
  	drawCircles(el, x, index, colInfo, sizeInfo);
	break;
      case "text":
	drawText(el, x, index, colInfo);
	break;
    case "arrow":
	drawArrow(el, x, index, colInfo, sizeInfo);
	break;
    }
}
