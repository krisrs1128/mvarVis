HTMLWidgets.widget({

  name: 'plot_mvar_d3',
  type: 'output',

  initialize: function(el, width, height) {
    return {"width": width, "height": height};
  },

  renderValue: function(el, x, instance) {
    // clear current canvas
    d3.select(el)
      .selectAll("*")
      .remove();

    // variables used to calculate layout
    var elemWidth = x[0]["opts"]["width"]
    var svgWidth = elemWidth * (1 - x[0]["opts"]["prop_input"]);
    var elemHeight = x[0]["opts"]["height"] / x["length"]

    setupElems(el, x["length"], elemHeight, elemWidth,
	       x[0]["opts"]["prop_input"]);
    for(var i in d3.range(x["length"])) {
      cur_x = HTMLWidgets.dataframeToD3(x[i].data);
      createAllInputs(el, cur_x, i, x[i]["opts"]);
      setupSVG(el, cur_x, svgWidth, elemHeight, i, x["length"]);
      drawScatter(el, cur_x, i, x[i]["opts"]);
      makeTable(el, elemWidth, cur_x, i);
    }
  }
});
