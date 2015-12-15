
var drawScatter = function(x, index, type) {
    var colInfo = getColorInfo(x, index);
    switch(type) {
      case "point":
  	drawCircles(x, index, colInfo);
	break;
      case "text":
	drawText(x, index, colInfo);
	break;
    case "arrow":
	drawArrow(x, index, colInfo);
	break;
    }
}
