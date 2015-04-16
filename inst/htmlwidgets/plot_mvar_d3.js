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
    for(i = 0; i < x.length; i++) {
        x[i] = HTMLWidgets.dataframeToD3(x[i]);
    }
    x = x[0].concat(x[1]);


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

    var min_x = 1e15;
        min_y = 1e15;
        max_x = -1e15;
        max_y = -1e15;

    for(j = 0; j < x.length; j++) {
        if(x[j].axis1 < min_x) {
            min_x = x[j].axis1;
        }
        if(x[j].axis2 < min_y) {
            min_y = x[j].axis2;
        }
        if(x[j].axis1 > max_x) {
            max_x = x[j].axis1;
        }
        if(x[j].axis2 > max_y) {
            max_y = x[j].axis2;
        }
    }

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
    function render(x) {

      attr_function = function(circles) {
          var new_circle = circles.attr("cx", function(d) { return (xScale(d.axis1));})
          .attr("cy", function(d) { return (yScale(d.axis2));})
          .attr("layer", x.layer)
          .attr("fill", function(d) {
            if(d.display) {
                return ("black");
            } else {
                return ("transparent");
            }
          })
          .attr("r", 5);

          return (new_circle);
      }

        var circles = svg.selectAll("circle")
            .data(x)
            .call(attr_function);

        circles
          .enter()
          .append("circle")
          .call(attr_function)
    }

    unique_layers = [];
    for(i = 0; i < x.length; i++) {
        if(unique_layers.indexOf(x[i].layer_ix) == -1) {
            unique_layers = unique_layers.concat(x[i].layer_ix);
        }
    }

    // add button for determining layers
    for(l = 0; l < unique_layers.length; l++) {
        d3.select("#htmlwidget_container")
            .append("label")
            .text("layer " + l)
            .attr("for", "layer_" + l)
            .append("input")
            .attr("class", "filter_button")
            .attr("type", "checkbox")
            .attr("ix", l)
            .attr("id", "layer_" + l);
    }

    d3.selectAll("label .filter_button")
        .on("change", function() {

            var ix = d3.select(this).attr("ix")
            for(i = 0; i < x.length; i++) {
                if(x[i].layer_ix - 1 == ix) {
                    if(this.checked) {
                        x[i].display = true;
                    } else {
                        x[i].display = false;
                    }
                }
            }
            render(x);
        });


	}
});
