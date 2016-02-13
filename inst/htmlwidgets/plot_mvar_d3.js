HTMLWidgets.widget({

  name: 'plot_mvar_d3',
  type: 'output',

  initialize: function(el, width, height) {
    return {"width": width, "height": height};
  },

  renderValue: function(el, x, instance) {
    setupElems(el, x.length, instance.width);
    for(var i in d3.range(x.length)) {
      cur_x = HTMLWidgets.dataframeToD3(x[i].data);
      createInput(el, cur_x, i);
      createQuantiInput(el, cur_x, i);
      setupSVG(el, cur_x, instance["width"], instance["height"], i, x["length"]);
      drawScatter(el, cur_x, i, x[i]["type"]);
      makeTable(el, instance["width"], cur_x, i);
    }
  }
});

