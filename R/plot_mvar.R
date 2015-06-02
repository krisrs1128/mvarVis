#' @title Plot an mvar object from annotation names
#'
#' @param mvar_object An object of class \code{mvarAxis} that we want to
#'  visualize.
#' @param layers_list Either a list of lists containing detailed plotting
#'  layer options, or a character string matching one of a few defaults.
#'  If a list of lists is provided, we expect the i^th component to be a list
#'  whose names are any of "point", "text", and "arrow", and whose corresponding
#'  values are TRUE if that layer is to be added to the plot and FALSE otherwise.
#'  If a character is provided, we expect one of the following presets:
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
#' @param x The name of the column in the coord data to use as the x-axis in
#'  the desired plot. This will default to the first axis in the ordination.
#' @param y The name of the column in the coord data to use as the y-axis in
#'  the desired plot. This will default to the second axis in the ordination.
#' @param col The color to use for points or text in the plot. This can either
#'  be a column in one or more of the annotation objects, in which case the
#'  values from that annotation will be used for coloring, or a string specifying
#'  the actual color to use.
#' @param shape The points to use for points in the plot. This can either
#'  be a column in one or more of the annotation objects, in which case the
#'  values from that annotation will be used for coloring, or a string specifying
#'  the actual color to use.
#' @param size The size of points in the plot. This can either be a column in
#'  one or more of the annotation objects, in which case the values from that
#'  annotation will be used for coloring, or a string specifying the actual color
#'  to use.
#' @param label The label to use for text in the plot. This can either be a column in
#'  one or more of the annotation objects, in which case the values from that
#'  annotation will be used for coloring, or a string specifying the actual color
#'  to use.
#' @return p A ggplot object mapping the layers specified in the arguments.
#' @export
plot_mvar <- function(mvar_object, layers_list = "point", x = "axis_1",
                      y = "axis_2", col = NULL, shape = NULL, size = NULL,
                      label = NULL, facet_vector = NULL) {
  layers_list <- build_layers_list(length(mvar_object@table), layers_list)
  full_lists <- build_aes_and_non_aes_lists(mvar_object, x, y, col, shape, size, label)
  opts <- build_opts(mvar_object, layers_list, full_lists$aes_list,
                     full_lists$non_aes_list, facet_vector)
  p <- plot_mvar_from_opts(mvar_object, opts)
  return (p)
}
