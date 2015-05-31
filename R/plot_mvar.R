
#' @title Supply Default Table Plotting Options
#'
#' @param opts A list of plotting options, for which we need to merge
#'    unspecified elements.
#' @return opts An updated list of plotting options.
#'
#' @export
merge_table_plot_opts <- function(opts = list()) {
  if(is.null(opts$aes_list)) opts$aes_list <- list()
  if(is.null(opts$layers_list)) opts$layers_list <- list()

  default_aes_list <- list(x = "axis_1", y = "axis_2", col = NULL,
                           shape = NULL, size = NULL, label = "label")
  opts$aes_list <- modifyList(default_aes_list, opts$aes_list)
  default_layers_list <- list(point = TRUE, text = FALSE, arrow = FALSE)
  opts$layers_list <- modifyList(default_layers_list, opts$layers_list)
  return (opts)
}

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
#'    point-text-arrow: This plots points for the first layer, and both text
#'      and arrows for the second layer.
#'
#' @param n_tables How many tables are in the mvarTable object to plot?
#' @param layers_list A string specifying the type of layers to include.
#'
#' @return layers_list A list of lists, whose i^th element is a list describing
#'  the ggplot layers to include for the i^th table.
build_layers_list <- function(n_tables, layers_list = "point") {
  if(is.character(layers_list)) {
    # Case that user has specified a preset option, rather than a full character list
    layers_list <- switch(layers_list,
           "point" = rep(list(list(point = TRUE)), n_tables),
           "text" = rep(list(list(point = FALSE, text = TRUE)), n_tables),
           "point-text" = list(list(point = TRUE), list(text = TRUE, point = FALSE)),
           "text-point" = list(list(point = FALSE, text = TRUE), list(point = TRUE)),
           "point-text-arrow" = list(list(points = TRUE), list(points = FALSE, text = TRUE, arrow = TRUE)))
  }
  return (layers_list)
}

#' @title Construct Aesthetics List
#'
#'
build_opts <- function(mvar_object, layers_list, x = "axis_1",
                       y = "axis_2", col = NULL, shape = NULL, size = NULL,
                       label = NULL, facet_vector = NULL) {
  n_tables <- length(mvar_object@table)
  opts <- rep(list(list()), n_tables)

  for(cur_table in 1:n_tables) {
    cur_colnames <- colnames(mvar_object@table[[cur_table]]@annotation)
    cur_aes_list <- list(col = col, shape = shape, size = size, label = label)
    cur_ix <- which(cur_aes_list %in% cur_colnames)
    opts[[cur_table]]$facet_vector <- facet_vector
    opts[[cur_table]]$layers_list <- layers_list[[cur_table]]
    opts[[cur_table]]$aes_list <- cur_aes_list[cur_ix]
  }
  return (opts)
}

#' @title Plot an element of class mvarAxis
#'
#' @param table_slot An object of class mvarAxis
#' @param opts A list of lists, giving directions for the aesthetic mapping.
#'    We expect three component lists, though they will be filled in with
#'    default values, so they only need to be partially specified.
#'      $layers_list: A list whose names are any of "point", "text", and "arrow",
#'        and whose corresponding values are TRUE if that layer is to be added
#'        to the plot and FALSE otherwise.
#'      $aes_list: A list whose names are any of "col", "shape", "size", or
#'        "text", and whose values are the names of the coordinates or
#'        annotation columns to be used in defining aesthetics.
#'      $facet_vector: A vector of annotation column names to be used in
#'        in faceting.
#'
#' @return p A ggplot object mapping the layers specified in the arguments.
#'
#' @importFrom grid arrow unit
#' @importFrom ggplot2 ggplot geom_point geom_segment geom_text aes_string
#'    facet_grid
#'
#' @export
plot_table <- function(table_slot, opts = list(), p = ggplot(), table_ix = 1) {
  opts <- merge_table_plot_opts(opts)
  data <- cbind(table_slot@annotation, table_slot@coord)

  opts$aes_list <- opts$aes_list[!sapply(opts$aes_list, is.null)]
  table_aes <- do.call(aes_string, opts$aes_list)
  if(opts$layers_list$point) {
    if(!is.null(table_aes$col)) {
      p  <- p + geom_point(data = data, table_aes)
    } else {
      # If there is no specified color aesthetic, use it to label the different
      # layers in the biplot
      p <- p + geom_point(data = data, table_aes, col = table_ix)
    }
  }
  if(opts$layers_list$arrow) {
    table_aes_copy <- table_aes
    table_aes_copy$xend  <- table_aes$x
    table_aes_copy$yend <- table_aes$y
    table_aes_copy$x  <- 0
    table_aes_copy$y  <- 0
    p <- p + geom_segment(data = data, table_aes_copy, arrow = arrow(length = unit(0.5, "cm")))
  }
  if(opts$layers_list$text) {
    if(!is.null(table_aes$col)) {
      p <- p + geom_text(data = data, table_aes)
    } else {
      # If there is no specified color aesthetic, use it to label the different
      # layers in the biplot
      p <- p + geom_text(data = data, table_aes, col = table_ix)
    }
  }
  if(!is.null(opts$facet_vector)) {
    if(length(opts$facet_vector) == 1) {
      facet_fmla_string <- paste0(opts$facet_vector, "~ .")
    } else {
      facet_fmla_string <- paste0(opts$facet_vector, collapse = "~")
    }
    p <- p + facet_grid(formula(facet_fmla_string))
  }
  return (p)
}

#' @title Plot an mvar object from options list
#'
#' @description \code{plot_mvar} helps plot multiple projection layers
#' associated with an mvar object, using \code{ggplot2} with various options to
#' customize the plot's appearance.
#'
#' @param mvar_object An object of class \code{mvarAxis} that we want to
#'    visualize.
#' @param opts A list whose i^th component contains the opts argument for the
#'    i^th table in the \code{mvar_object}. See \code{plot_table()} for a
#'    description of what these need to contain.
#'
#' @return p A ggplot object mapping the layers specified in the arguments.
#'
#' @export
plot_mvar_from_opts <- function(mvar_object, opts = NULL) {
  if(is.null(opts)) {
    opts <- rep(list(list()), length(mvar_object@table))
  }
  p <- ggplot()
  for(cur_table in 1:length(mvar_object@table)) {
    p <- plot_table(mvar_object@table[[cur_table]], opts[[cur_table]], p, cur_table)
  }

  # add eigenvalue labels for the axis specified by the first element in the aes list
  if(!is.na(mvar_object@eig[1])) {
    merged_aes <- merge_table_plot_opts(opts)$aes_list
    x_axis  <- as.numeric(gsub("axis_", "", merged_aes$x))
    y_axis <- as.numeric(gsub("axis_", "", merged_aes$y))
    eigs_prop <- mvar_object@eig[c(x_axis, y_axis)] / sum(mvar_object@eig)
    x_label <- sprintf("%s (%g variance)", merged_aes$x, round(eigs_prop[1], 3))
    y_label <- sprintf("%s (%g variance)", merged_aes$y, round(eigs_prop[2], 3))
    p <- p + scale_x_continuous(x_label) +
      scale_y_continuous(y_label) +
      coord_fixed(ratio = eigs_prop[2] / eigs_prop[1])
  }
  return (p)
}

#' @title Plot an mvar object from annotation names
#'
#'
#' @export
plot_mvar <- function(mvar_object, layers_list = "point", x = "axis_1",
                      y = "axis_2", col = NULL, shape = NULL, size = NULL,
                      label = NULL, facet_vector = NULL) {
  layers_list <- build_layers_list(length(mvar_object@table), layers_list)
  opts <- build_opts(mvar_object, layers_list, x, y, col, shape, size, label,
                     facet_vector)
  p <- plot_mvar_from_opts(mvar_object, opts)
  return (p)
}
