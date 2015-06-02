#' @title Construct Layers List
#'
#' @description Here are some defaults for layers options that are commonly
#' used. The pre-set options currently are
#'    points: We plot points for each layer. If no color aesthetic associated
#'      with row or column annotation is provided, each layer of points will
#'      be a different color. This is the default plot.
#'    text: This plots text for every layer.
#'    point-text: This plots points for the first layer and text for the second
#'      layer. Only applies to tables with two layers.
#'    text-point: This plots text for the first layer and points for the second.
#'    points-and-text: This shows both points and slightly offset points for
#'      every layer.
#'    point-text-arrow: This plots points for the first layer, and both text
#'      and arrows for the second layer.
#'
#' @param n_tables How many tables are in the mvarTable object to plot?
#' @param layers_list A string specifying the type of layers to include.
#'
#' @return layers_list A list of lists, whose i^th element is a list describing
#'  the ggplot layers to include for the i^th table.
#'
#' @export
build_layers_list <- function(n_tables, layers_list = "point") {
  if(is.character(layers_list)) {
    # Case that user has specified a preset option, rather than a full character list
    layers_list <- switch(layers_list,
                          "point" = rep(list(list(point = TRUE)), n_tables),
                          "text" = rep(list(list(point = FALSE, text = TRUE)), n_tables),
                          "point-text" = list(list(point = TRUE), list(text = TRUE, point = FALSE)),
                          "text-point" = list(list(point = FALSE, text = TRUE), list(point = TRUE)),
                          "points-and-text" = rep(list(list(point = TRUE, text = TRUE)), n_tables),
                          "point-text-arrow" = list(list(points = TRUE), list(points = FALSE, text = TRUE, arrow = TRUE)))
  }
  return (layers_list)
}
