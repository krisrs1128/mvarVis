#' Interactive multivariate analysis plots
#' @importFrom htmlwidgets createWidget
#' @export
plot_mvar_d3 <- function(mvar_object, types = NULL, width = NULL, height = 400) {

  if(is.null(width)) {
    width <- height * length(mvar_object@table)
  }
  if(is.null(types)) {
    types <- rep("point", length(mvar_object@table))
    if(length(types) > 2) {
      types[2] <- "text"
    }
  }

  x <- list()
  for(table_ix in seq_along(mvar_object@table)) {
    cur_coord <- mvar_object@table[[table_ix]]@coord
    colnames(cur_coord) <- paste0("axis", seq_len(ncol(cur_coord)))
    cur_ann <- mvar_object@table[[table_ix]]@annotation
    x[[table_ix]] <- list(data = data.frame(cur_coord, cur_ann),
                          type = types[table_ix])
  }

  createWidget(name = "plot_mvar_d3", x = x, width = width, height = height,
               package = "mvarVis")

}

#' @importFrom htmlwidgets createWidget
#' @export
plot_layer <- function(mvar_layer, width = 400, height = 400) {
  x <- data.frame(mvar_layer@coord, mvar_layer@annotation)
  createWidget(name = "plot_layer", x = x, width = width, height = height,
               package = "mvarVis")
}
