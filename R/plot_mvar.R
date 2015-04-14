#' @title Plot an mvar object
#'
#' @description \code{plot_mvar} helps plot multiple projection layers
#' associated with an mvar object, using \code{ggplot2} with various options to
#' customize the plot's appearance.
#'
#' @param mvar_object An object of class \code{mvarTable} that we want to
#'    visualize. Each \code{mvarAxis} component can be visualized as a
#'    different \code{ggplot2} layer; the \code{coord} slot is needed to provide
#'    the point positions while the \code{annotation} slot is used to map
#'    aesthetics to these points.
#' @param point_ix A vector giving the indices of the axes in the
#'    \code{mvar_object}'s \code{tables} component that we want to plot as
#'    points. Specifically, for each element $i$ of \code{point_ix}, we look up
#'    the i^th element in the \code{mvar_object} \code{table} slot, extract the
#'     \code{coord} slot, and plot each row as a separate point
#' @param text_ix A vector analogous to \code{point_ix}, execpt that it gives
#'    the elements that we plot as text labels. The actual text that appears
#'    will default to the \code{$label} element in the \code{annotation} slot,
#'    but this can be customized using the \code{text_vars} component.
#' @param arrow_ix A vector analogous to \code{point_ix} and \code{text_ix},
#'    giving the elements for which we plot arrows connecting the origin to
#'    the \code{coord} position.
#' @param rescaling_ix When making a biplot, the scales of the two sets of
#'    points aren't always comparable -- only the *directions* of the
#'    projections of feature variables are meaningful. This vector indexes the
#'    tables whose coordinate values we will rescale to match those of a
#'    reference projection's values, see \code{rescaling_ref}.
#' @param rescaling_ref The index within the \code{mvar_object} \code{table}
#'    list of the projection to use as a reference when rescaling other
#'    projection values. For example, in a biplot including samples and
#'    features, we could plot the actual values for the samples, and scale the
#'    feature coordinates to be about the same magnitude as the samples.
#' @param text_vars A list the same length as the \code{mvar_object}
#'    \code{table} list whose i^th element either (1) gives the column name in
#'    the associated annotation table to which map to text, or (2) is NULL,
#'    meaning that table does not have a text label.
#' @param proj_axes Which columns in each \code{coord} object should we use to
#'    do the projections? Defaults to 1-2, the two axes explaining the most
#'    variability.
#' @param color_vars A list with the same format as \code{text_vars}, but for
#'    mapping color aesthetics of each layer.
#' @param point_size_vars A list with the same format as \code{text_vars}, but
#'    for mapping the point size aesthetics of each layer.
#' @param text_size_vars A list with the same format as \code{text_vars}, but
#'    for mapping the text size aesthetics of each layer.
#' @param shape_vars A list with the same format as \code{text_vars}, but for
#'    mapping the shape aesthetic of each layer.
#'
#' @return p A ggplot object mapping the layers specified in the arguments.
#'
#' @importFrom ggplot2 geom_point aes_string geom_segment geom_text ggtitle ggplot
#' @importFrom grid arrow unit
#'
#' @export
plot_mvar <- function(mvar_object,
                      point_ix = 1:length(mvar_object@table),
                      text_ix = numeric(0),
                      arrow_ix = 2,
                      rescaling_ix = 2,
                      rescaling_ref = 1,
                      text_vars = rep(list(NULL), length(mvar_object@table)),
                      proj_axes = 1:2,
                      color_vars = rep(list(NULL), length(mvar_object@table)),
                      point_size_vars = rep(2, length(mvar_object@table)),
                      text_size_vars = rep(list(NULL), length(mvar_object@table)),
                      shape_vars = rep(list(NULL), length(mvar_object@table))) {
  p <- ggplot()
  proj_axes <- paste0("axis", proj_axes)

  # Loop over every projection in mvar_object@table, and add the relevant
  # aesthetic layers
  for(table_ix in length(mvar_object@table):1) {
    cur_coord <- mvar_object@table[[table_ix]]@coord
    colnames(cur_coord) <- paste0("axis", 1:ncol(cur_coord))
    cur_ann <- mvar_object@table[[table_ix]]@annotation

    # Rescale coordinates to a reference, if desired
    if(table_ix %in% rescaling_ix) {
      ref_coord <- mvar_object@table[[rescaling_ref]]@coord
      cur_coord <- scale_to_max_radius(ref_coord, cur_coord)
    }

    # Combined df of both coordinates and annoation
    cur_df <- cbind(cur_coord, cur_ann)

    # Add a point layer for current data, if desired
    if(table_ix %in% point_ix) {

      # Point size is either a number meaning the literal point size, or a
      # string specifying the column in the current annotation table to which
      # we map point sizes
      if(!is.numeric(point_size_vars[[table_ix]])) {
        point_map <- geom_point(data = cur_df,
                                aes_string(x = proj_axes[1],
                                           y = proj_axes[2],
                                           col = color_vars[[table_ix]],
                                           shape = shape_vars[[table_ix]],
                                           size = point_size_vars[[table_ix]]))
      } else {
        point_map <- geom_point(data = cur_df,
                                aes_string(x = proj_axes[1],
                                           y = proj_axes[2],
                                           col = color_vars[[table_ix]],
                                           shape = shape_vars[[table_ix]]),
                                size = point_size_vars[[table_ix]])
      }
    } else {
        point_map <- NULL
    }

    # Add a text layer for current data, if desired
    if(table_ix %in% text_ix) {
      if(!is.numeric(text_size_vars[[table_ix]])) {
        text_map <- geom_text(data = cur_df,
                              aes_string(x = proj_axes[1],
                                         y = proj_axes[2],
                                         label = text_vars[[table_ix]],
                                         col = color_vars[[table_ix]],
                                         shape = shape_vars[[table_ix]],
                                         size = text_size_vars[[table_ix]]))
      } else {
        text_map <- geom_text(data = cur_df,
                              aes_string(x = proj_axes[1],
                                         y = proj_axes[2],
                                         label = text_vars[[table_ix]],
                                         col = color_vars[[table_ix]],
                                         shape = shape_vars[[table_ix]]),
                                size = text_size_vars[[table_ix]])
      }
    } else {
      text_map <- NULL
    }

    # Add an arrow layer for the current data, if desired
    if(table_ix %in% arrow_ix) {
      arrow_map <- geom_segment(data = cur_df,
                                aes_string(x = 0, y = 0,
                                           xend = paste0(".9 * ", proj_axes[1]),
                                           yend = paste0(".9 * ", proj_axes[2]),
                                           col = color_vars[[table_ix]],
                                           shape = shape_vars[[table_ix]]),
                                arrow = arrow(length = unit(0.5, "cm")))
    } else {
      arrow_map <- NULL
    }

    # Append the layers for the current projection
    p <- p + text_map + arrow_map + point_map
  }
  return (p)
}
