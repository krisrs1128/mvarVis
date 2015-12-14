
################################################################################
# These are 'opts-mergers', which allow the user to supply partially specified
# lists, and fill in the remaining automatically.
################################################################################

# table-opts -------------------------------------------------------------------
#' @title Build Opts lists
#' @param mvar_object The mvar_object that we would like to plot.
#' @param layers_list A list containing which layer attributes to plot for
#'  every layer. See the function \code{build_layers_list()} for more
#'  details.
#' @param aes_list A list containing the aesthetics attributes for every
#'  layer. See \code{build_aes_and_non_aes_lits()} for more details.
#' @param non_aes_list The list containing the non-data-driven aesthetics
#'  attributes for every layer. See \code{build_aes_and_non_aes_lits()} for
#'  more details.
#' @param facet_vector A vector containing column names in the annotation data
#'  to use for faceting.
#' @return opts A list that can be input into \code{plot_mvar_from_opts()}.
build_opts <- function(mvar_object, layers_list, aes_list, non_aes_list,
                       facet_vector = NULL) {
  n_tables <- length(mvar_object@table)
  opts <- rep(list(list()), n_tables)

  for(cur_table in 1:n_tables) {
    opts[[cur_table]]$facet_vector <- facet_vector
    opts[[cur_table]]$layers_list <- layers_list[[cur_table]]
    opts[[cur_table]]$aes_list <- aes_list[[cur_table]]
    opts[[cur_table]]$non_aes_list <- non_aes_list[[cur_table]]
  }
  return (opts)
}

#' @title Get the tables to put into an mvarTable object
#' @description For ade4, we need to specify the row and columns scores to
#' output, depending on the method.
#' @param method The name of the ordination method from which we want to
#'  extract scores.
#' @return If the method is implemented in ade4, the names of the tables that
#'  give the row and column scores.
default_table_names <- function(method) {
  table_names <- switch(method,
                        "pca" = c("li", "co"),
                        "acm" = c("li", "co"),
                        "coa" = c("li", "co"),
                        "fca" = c("li", "co"),
                        "fpca" = c("li", "co"),
                        "pco" = c("li", "co"),
                        "hillsmith" = c("li", "co"),
                        "mix" = c("li", "co"),
                        "nsc" = c("li", "co"),
                        "dpcoa" = c("l1", "l2"),
                        "decorana" = c(), # right now we're using scores() from vegan, instead of extracting any tables
                        "metaMDS"  = c(),
                        "isomap" = c(),
                        "isoMDS" = c(),
                        "vegan_cca" = c(),
                        "ade4_cca" = c("li", "co"),
                        "rda" = c(),
                        "CCorA" = c(),
                        "procuste" = c("scor1", "scor2"),
                        "coinertia" = c("li", "co"))
  return (table_names)
}

# plot-opts --------------------------------------------------------------------
#' @title Construct Layers List
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
#' @param n_tables How many tables are in the mvarTable object to plot?
#' @param layers_list A string specifying the type of layers to include.
#' @return layers_list A list of lists, whose i^th element is a list describing
#'  the ggplot layers to include for the i^th table.
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

#' @title Supply Default Table Plotting Options
#' @param opts A list of plotting options, for which we need to merge
#'    unspecified elements.
#' @return opts An updated list of plotting options.
#' @export
merge_table_plot_opts <- function(opts = list()) {
  if(is.null(opts$aes_list)) opts$aes_list <- list()
  if(is.null(opts$layers_list)) opts$layers_list <- list()
  if(is.null(opts$non_aes_list)) opts$non_aes_list <- list()

  default_aes_list <- list(x = "axis_1", y = "axis_2", col = NULL,
                           shape = NULL, size = NULL, label = "label")
  opts$aes_list <- modifyList(default_aes_list, opts$aes_list)
  default_layers_list <- list(point = TRUE, text = FALSE, arrow = FALSE)
  opts$layers_list <- modifyList(default_layers_list, opts$layers_list)
  default_non_aes_list <- list()
  opts$non_aes_list <- modifyList(default_non_aes_list, opts$non_aes_list)
  return (opts)
}

#' @title Construct Aesthetics List
#' @description While the plot\_mvar\_from\_opts is very flexible, it does not
#' provide an easy to use interface for the most common plotting procedures. This
#' provides some of the default plotting options for aes lists (those whose names
#' are columns in the mvar object) and non-aes lists (those that are not related
#' to the data, for example, calling geom_text() with col = "red".)
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
