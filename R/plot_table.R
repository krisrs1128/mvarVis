#' @title Plot an element of class mvarLayer
#'
#' @param table_slot An object of class mvarLayer
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
#' @param p A ggplot object on which to build on top of. Defaults to an empty
#'  object.
#' @param table_ix The layer number of the current table, which is used to
#'  ensure different layers have different colors (in the case that color
#'  is not an aesthetic determined from data).
#' @return p A ggplot object mapping the layers specified in the arguments.
#' @importFrom grid arrow unit
#' @importFrom ggplot2 ggplot geom_point geom_segment geom_text aes_string
#'    facet_grid stat_density_2d
#' @export
plot_table <- function(table_slot, opts = list(), p = ggplot(), table_ix = 1) {
  opts <- merge_table_plot_opts(opts)
  data <- cbind(table_slot@annotation, table_slot@coord)

  opts$aes_list <- opts$aes_list[!sapply(opts$aes_list, is.null)]
  table_aes <- do.call(aes_string, opts$aes_list)
  non_aes <- opts$non_aes_list

  # add the points layer
  if(opts$layers_list$point) {
    p <- p + do.call(geom_point, c(list(data = data, mapping = table_aes), non_aes))
  }

  # add the arrows layer
  if(opts$layers_list$arrow) {
    table_aes_copy <- table_aes
    table_aes_copy$xend  <- table_aes$x
    table_aes_copy$yend <- table_aes$y
    table_aes_copy$x  <- 0
    table_aes_copy$y  <- 0
    p <- p + geom_segment(data = data, table_aes_copy, arrow = arrow(length = unit(0.5, "cm")))
  }

  # add the text layer
  if(opts$layers_list$text) {
    p <- p + do.call(geom_text, c(list(data = data, mapping = table_aes), non_aes))
  }

  # add the contour layer
  if(opts$layers_list$contour) {
    aes_list <- opts$aes_list
    aes_list$group <- "label"
    aes_list$fill <- aes_list$col
    table_aes_copy <- do.call(aes_string, aes_list)

    p <- p + do.call(stat_density_2d, c(list(data = data, mapping = table_aes_copy),
                                        non_aes))
  }

  # add the density layer
  if(opts$layers_list$density) {
    aes_list <- opts$aes_list
    aes_list$group <- "label"
    aes_list$alpha <- "..level.."
    table_aes_copy <- do.call(aes_string, aes_list)

    non_aes_copy <- non_aes
    non_aes_copy$geom <- "polygon"
    non_aes_copy$lty <- "blank"

    p <- p + do.call(stat_density_2d, c(list(data = data, mapping = table_aes_copy),
                                        non_aes_copy))
  }

  # add faceting
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
