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
      .remove()

    setupElems(el, x.length, instance.width);
    for(var i in d3.range(x.length)) {
      cur_x = HTMLWidgets.dataframeToD3(x[i].data);
      createAllInputs(el, cur_x, i, x[i]["opts"]);
      setupSVG(el, cur_x, x[i]["opts"]["width"],
	       x[i]["opts"]["height"], i, x["length"]);
      drawScatter(el, cur_x, i, x[i]["opts"]);
      makeTable(el, x[i]["opts"]["width"], cur_x, i);
    }
  }
});

