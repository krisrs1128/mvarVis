
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
                           shape = NULL, size = NULL, label = NULL)
  opts$aes_list <- modifyList(default_aes_list, opts$aes_list)
  default_layers_list <- list(point = TRUE, text = FALSE, arrow = FALSE)
  opts$layers_list <- modifyList(default_layers_list, opts$layers_list)
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
#'
#' @export
plot_table <- function(table_slot, opts = list(), p = ggplot()) {
  opts <- merge_table_plot_opts(opts)
  data <- cbind(table_slot@annotation, table_slot@coord)

  opts$aes_list <- opts$aes_list[!sapply(opts$aes_list, is.null)]
  table_aes <- do.call(aes_string, opts$aes_list)
  if(opts$layers_list$point) {
    p  <- p + geom_point(data = data, table_aes)
  }
  if(opts$layers_list$text) {
    p <- p + geom_text(data = data, table_aes)
  }
  if(opts$layers_list$arrow) {
    table_aes$xend  <- table_aes$x
    table_aes$yend <- table_aes$y
    table_aes$x  <- 0
    table_aes$y  <- 0
    p <- p + geom_segment(data = data, table_aes, arrow = arrow(length = unit(0.5, "cm")))
  }
  if(!is.null(opts$facet_vector)) {
    facet_fmla <- formula(paste0(opts$facet_vector, collapse = "~"))
    p <- p + facet_grid(facet_fmla)
  }
  return (p)
}

#' @title Plot an mvar object
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
#' @importFrom ggplot2 geom_point aes_string geom_segment geom_text ggtitle ggplot
#'
#' @export
plot_mvar <- function(mvar_object, opts) {
  for(cur_table in 1:length(mvar_object@table)) {
    if(cur_table == 1) {
      p <- plot_table(mvar_object@table[[cur_table]], opts[[cur_table]])
    } else {
      p <- plot_table(mvar_object@table[[cur_table]], opts[[cur_table]], p)
    }
  }
  return (p)
}
