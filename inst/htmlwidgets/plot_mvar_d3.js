HTMLWidgets.widget({

    name: 'plot_mvar_d3',
    type: 'output',

    initialize: function(el, width, height) {
	return {"width": width, "height": height};
    },

    renderValue: function(el, x, instance) {
	setupElems(el, x.length, instance.width);
	for(i in d3.range(x.length)) {
	    cur_x = HTMLWidgets.dataframeToD3(x[i]);
	    createInput(el, cur_x, i);
	    setupSVG(el, instance.width, instance.height, i, x.length);
	    drawScatter(cur_x, i);
	    makeTable(el, cur_x, i);
	}
    }
});
