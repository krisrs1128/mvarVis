#' @title Construct Aesthetics List
#'
#' @description While the plot\_mvar\_from\_opts is very flexible, it does not
#' provide an easy to use interface for the most common plotting procedures. This
#' provides some of the default plotting options for aes lists (those whose names
#' are columns in the mvar object) and non-aes lists (those that are not related
#' to the data, for example, calling geom_text() with col = "red".)
#'
#' @param mvar_object The mvar_object that we would like to plot.
#' @param x The column name specifying the x-axis in the ordination. Defaults
#'  to axis_1 for each coord object in the mvar.
#' @param y The column name specifying the y-axis in the ordination. Defaults
#'  to axis_2 for each coord object in the mvar.
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
#'  @return Two lists containing the aes and non aes options. Both lists have
#'  length given by the number layers in the mvar object. The i^th component
#'  contains directions for plotting the i^th layer. The aes list contains
#'  options that are column names in the corresponding annotation, the non-aes
#'  components are not in the data annotation.
#'
#' @export
build_aes_and_non_aes_lists <- function(mvar_object, x = "axis_1", y = "axis_2",
                                        col = NULL, shape = NULL, size = NULL,
                                        label = NULL) {
  n_tables <- length(mvar_object@table)
  aes_list <- rep(list(list()), n_tables)
  non_aes_list <- rep(list(list()), n_tables)
  for(cur_table in 1:n_tables) {

    # if the data does not have any color annotation already, set color to be
    # the index of the desired layer.
    if(is.null(col)){
      cur_col  <- cur_table
    } else {
      cur_col <- col
    }
    cur_colnames <- colnames(mvar_object@table[[cur_table]]@annotation)
    all_colnames <- unlist(lapply(mvar_object@table, function(x) colnames(x@annotation)))
    full_aes_list <- list(x = x, y = y, col = cur_col, shape = shape,
                          size = size, label = label)
    cur_ix <- which(full_aes_list %in% cur_colnames)
    any_ix <- which(full_aes_list %in% all_colnames)
    aes_list[[cur_table]] <- full_aes_list[cur_ix]
    non_aes_list[[cur_table]] <- full_aes_list[setdiff(1:length(full_aes_list), any_ix)]
    non_aes_list[[cur_table]][c("x", "y")] <- NULL # x and y can never have non-data-driven aesthetics
  }
  return (list(aes_list = aes_list, non_aes_list = non_aes_list))
}
