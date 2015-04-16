#' Interactive multivariate analysis plots
#'
#' @import htmlwidgets
#'
#' @export
plot_mvar_d3 <- function(mvar_object, rescaling_ix = 2, rescaling_ref = 1,
                         width = 800, height = 800) {

  x <- list()
  for(table_ix in length(mvar_object@table):1) {
    cur_coord <- mvar_object@table[[table_ix]]@coord
    colnames(cur_coord) <- paste0("axis", 1:ncol(cur_coord))
    cur_ann <- mvar_object@table[[table_ix]]@annotation

    # Rescale coordinates to a reference, if desired
    if(table_ix %in% rescaling_ix) {
      ref_coord <- mvar_object@table[[rescaling_ref]]@coord
      cur_coord <- scale_to_max_radius(ref_coord, cur_coord)
    }

    vis_info <- data.frame(display = TRUE, layer_ix = table_ix - 1)

    # Combined df of both coordinates and annoation
    x[[table_ix]] <- cbind(cur_coord, cur_ann, vis_info)

  }

  htmlwidgets::createWidget(
    name = "plot_mvar_d3",
    x,
    width = width,
    height = height,
    package = "mvarVis"
  )
}
