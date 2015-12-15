HTMLWidgets.widget({

    name: 'plot_mvar_d3',
    type: 'output',

    initialize: function(el, width, height) {
	return {"width": width, "height": height};
    },

    renderValue: function(el, x, instance) {
	console.log(x.length)

	setupElems(el, x.length, instance.width);
	for(i in d3.range(x.length)) {
	    console.log(i);
	    cur_x = HTMLWidgets.dataframeToD3(x[i]);
	    console.log("Creating input")
	    createInput(el, cur_x, i);
	    setupSVG(el, instance.width, instance.height, i, x.length);
	    console.log("drawing scatter")
	    drawScatter(cur_x, i);
	    console.log("making table")
	    makeTable(el, cur_x, i);
	}
    }
});
