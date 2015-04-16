HTMLWidgets.widget({

    name: 'plot_mvar_d3',
    type: 'output',

    initialize: function(el, width, height) {
    },

    renderValue: function(el, x, instance) {
	//
	// Args:
	//   el: The HTML element containing the visualization
	//   x: The problem data input by R
	//   instance: The instance data, from initialization or the last
	//     resizing

    x = x.x;

    var height = 800; // will change to user specified, but test with fixed
    var width = 800;

    var svg;
    function createSvg(){
    	svg = d3.select("#htmlwidget_container").append("svg")
    			.attr("class", "axis")
    			.attr("width", width)
    			.attr("height", height);
    }
    createSvg();

    function renderAxis(scale, orient) {

		var axis = d3.svg.axis()
					.scale(scale)
					.orient(orient)
					.ticks(5);

		svg.append("g")
			.attr("transform", function(){
				if(["top", "bottom"].indexOf(orient) >= 0)
					return "translate(0," + (height / 2) + ")";
				else
					return "translate(" + (width / 2) + ", 0)";
				}
			)
			.call(axis);
	}

	var all_coord_x = [];
	var all_coord_y = [];
	for(i = 0; i < x.length; i++) {
		all_coord_x = all_coord_x.concat(x[i].axis1);
	 	all_coord_y = all_coord_x.concat(x[i].axis2);
	}

	min_x = Math.min.apply(null, all_coord_x);
	min_y = Math.min.apply(null, all_coord_y);
	max_x = Math.max.apply(null, all_coord_x);
	max_y = Math.max.apply(null, all_coord_y);

	var xScale = d3.scale.linear()
					.domain([min_x, max_x])
					.range([0, width])

	var yScale = d3.scale.linear()
		 			.domain([min_y, max_y])
		 			.range([0, height])

	// x and y axes
	renderAxis(d3.scale.linear()
				.domain([min_x, max_x])
				.range([0, height]), "left");
	renderAxis(d3.scale.linear()
				.domain([min_y, max_y])
				.range([0, width]), "bottom");


	// Render each layer of projections
	function render_layer(mvar_proj, feature) {

		var arrayUnique = function(a) {
			return a.reduce(function(p, c) {
			if (p.indexOf(c) < 0) p.push(c);
				return p;
			}, []);
		};

		var colorScale = d3.scale.category20()
						.domain(arrayUnique(mvar_proj.label))

		coord = []
		for(i = 0; i < mvar_proj.axis1.length; i++) {
			coord[i] = [mvar_proj.axis1[i], mvar_proj.axis2[i], mvar_proj.label[i]];
		}

		svg.selectAll("circle")
			.data(coord)
			.enter()
			.append("circle")
			.attr("cx", function(d) { return (xScale(d[0]));})
			.attr("cy", function(d) { return (yScale(d[1]));})
			.style("fill", function(d) {return(colorScale(d[2]));})
			.attr("r", 5);
	}

    render_layer(x[0]);
    render_layer(x[1]);
    }
});
