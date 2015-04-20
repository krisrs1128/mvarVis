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

    get_unique_attributes = function(x) {
        unique_layers = [];
        unique_attributes = [];
        for(i = 0; i < x.length; i++) {
            if(unique_layers.indexOf(x[i].layer_ix) == -1) {
                unique_layers = unique_layers.concat(x[i].layer_ix);
                unique_attributes.push(Object.keys(x[i]));
            }
        }
        return (unique_attributes)
    }

    add_buttons = function(unique_attributes) {
        // add button for determining layers
        for(l = 0; l < unique_attributes.length; l++) {
            d3.select(el)
                .append("label")
                .text("layer " + l)
                .attr("for", "layer_" + l)
                .append("input")
                .attr("class", "filter_button")
                .attr("type", "checkbox")
                .attr("checked", true)
                .attr("ix", l)
                .attr("id", "layer_" + l);
        }
    }

    add_options = function(type, unique_attributes) {
        for(cur_attr = 0; cur_attr < unique_attributes.length; cur_attr++) {
            var cur_option = d3.select(el)
                .append("label")
                .text(type + " layer " + cur_attr)
                .append("select")
                .attr("class", type + "_select")
                .attr("layer", cur_attr);

            for(cur_value_ix = 0; cur_value_ix < unique_attributes[cur_attr].length; cur_value_ix++) {
                cur_value = unique_attributes[cur_attr][cur_value_ix];

                cur_option
                    .append("option")
                    .attr("value", cur_value)
                    .text(cur_value);
            }
        }
    }

    createSvg = function() {
        svg = d3.select(el).append("svg")
                .attr("class", "axis")
                .attr("width", width)
                .attr("height", height);
    }

    get_selected_options = function() {
        var color_select = d3.selectAll("label select.color_select")[0]
        selected_options = []
        for(i = 0; i < color_select.length; i++) {
            selected_option_ix = color_select[i].selectedIndex;
            selected_options.push(color_select[i].options[selected_option_ix].text);
        }
        return (selected_options);
    }

    create_color_scale = function(x) {
        selected_options = get_selected_options();

        var color_domain = [];
        for(i = 0; i < x.length; i++) {
            for(j = 0; j < selected_options.length; j++) {
                var cur_keys = Object.keys(x[i]);
                if(cur_keys.indexOf(selected_options[j]) != -1) {
                    cur_value = x[i][selected_options[j]];
                    if(color_domain.indexOf(cur_value) == -1) {
                        color_domain.push(cur_value);
                    }
                }
            }
        }

        color_scale = d3.scale.category20c()
                        .domain(color_domain);
        return (color_scale);
    }

    renderAxis = function(scale, orient) {
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

    layer_scale = function(x) {
        var all_layers = []
        for(i = 0; i < x.length; i++) {
            if(x.indexOf(x[i].layer_ix) == -1) {
                all_layers.push(x[i].layer_ix);
            }
        }
        var color_scale_layer = d3.scale.category10()
                .domain(all_layers);
        return (color_scale_layer);
    }

    attr_function = function(x, circles, tip) {

        console.log("this");
        console.log(tip);

      // extract color scale info
      var selected_opts = get_selected_options();
      var color_scale = create_color_scale(x);
      var color_scale_layer = layer_scale(x);

      var new_circle = circles.attr("cx", function(d) { return (xScale(d.axis1));})
          .attr("cy", function(d) { return (yScale(d.axis2));})
          .attr("layer", x.layer)
          .attr("fill", function(d) {
            if(d.display) {
                var selected_var = selected_opts[d.layer_ix]
                return(color_scale(d[selected_var]));
            } else {
                return ("transparent");
            }
          })
          .attr("stroke", function(d) {
            if(d.display) {
                return(color_scale_layer(d.layer_ix));
            } else {
                return ("transparent");
            }
           })
          .attr("stroke-width", 3)
          .attr("r", 8)
          .on('mouseover', tip.show)
          .on('mouseout', tip.hide);
      return (new_circle);
    }

    // Render each layer of projections
    function render(x, tip) {
        var circles = svg.selectAll("circle")
            .data(x)

        attr_function(x, circles, tip);

        circles.enter()
          .append("circle");

        attr_function(x, circles, tip);
    }

    get_min_max_over_objects = function(object, feature) {
      var min = 1e15;
      var max = -1e15
      for(i = 0; i < object.length; i++) {
        if(object[i][feature] < min) {
          min = object[i][feature];
        }
        if(object[i][feature] > max) {
          max = object[i][feature];
        }
      }
      return ([min, max]);
    }

    create_tip = function(selected_options) {
        var tip = d3.tip()
        .attr('class', 'd3-tip')
        .offset([-10, 0])
        .html(function(d) {
            if(d.display) {
                return d[selected_options[d.layer_ix]];
            }
        })
        return (tip);
    }

    ///////////////////////////////////////////////////////////////////////////////
    // Create the plot using the functions above
    ///////////////////////////////////////////////////////////////////////////////

    for(i = 0; i < x.length; i++) {
        x[i] = dataframeToD3(x[i]);
    }
    x = x[0].concat(x[1]);

    unique_attributes = get_unique_attributes(x);
    add_buttons(unique_attributes);
    add_options("color", unique_attributes)
    add_options("text", unique_attributes)

    var height = 800; // will change to user specified, but test with fixed
    var width = 800;

    var svg;
    createSvg();

    selected_options = get_selected_options();
    tip = create_tip(selected_options);
    svg.call(tip);

    var x_min_max = get_min_max_over_objects(x, "axis1");
    var y_min_max = get_min_max_over_objects(x, "axis2");

    var xScale = d3.scale.linear()
                    .domain(x_min_max)
                    .range([0, width])

    var yScale = d3.scale.linear()
                    .domain(y_min_max)
                    .range([0, height])

    // x and y axes
    renderAxis(d3.scale.linear()
                .domain(x_min_max)
                .range([0, height]), "left");
    renderAxis(d3.scale.linear()
                .domain(y_min_max)
                .range([0, width]), "bottom");

    render(x, tip);

    d3.selectAll("label .filter_button")
        .on("change", function() {
            var ix = d3.select(this).attr("ix")
            for(i = 0; i < x.length; i++) {
                if(x[i].layer_ix == ix) {
                    if(this.checked) {
                        x[i].display = true;
                    } else {
                        x[i].display = false;
                    }
                }
            }
            render(x, tip);
        });

    d3.selectAll("label .color_select")
        .on("change", function()  {
            selected_options = get_selected_options();
            tip = create_tip(selected_options);
            svg.call(tip);
            render(x, tip);
        });
});
