HTMLWidgets.widget({

    name: 'plot_layer',
    type: 'output',

    initialize: function(el, width, height) {
	setupSVG(el, width, height);
    },

    renderValue: function(el, x, instance) {
  	//
  	// Args:
  	//   el: The HTML element containing the visualization
  	//   x: The problem data input by R
  	//   instance: The instance data, from initialization or the last
  	//     resizing
	x = HTMLWidgets.dataframeToD3(x);
	createInput(el, x);
	drawScatter(x);
	makeTable(el, x);
    }
});
