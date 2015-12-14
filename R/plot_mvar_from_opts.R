#' @title Plot an mvar object from options list
#'
#' @description \code{plot_mvar} helps plot multiple projection layers
#' associated with an mvar object, using \code{ggplot2} with various options to
#' customize the plot's appearance.
#' @param mvar_object An object of class \code{mvarLayer} that we want to
#'    visualize.
#' @param opts A list whose i^th component contains the opts argument for the
#'    i^th table in the \code{mvar_object}. See \code{plot_table()} for a
#'    description of what these need to contain.
#' @return p A ggplot object mapping the layers specified in the arguments.
#' @export
plot_mvar_from_opts <- function(mvar_object, opts = NULL) {
  if(is.null(opts)) {
    opts <- rep(list(list()), length(mvar_object@table))
  }
  p <- ggplot()
  for(cur_table in 1:length(mvar_object@table)) {
    p <- plot_table(mvar_object@table[[cur_table]], opts[[cur_table]], p, cur_table)
  }
  if(!is.na(mvar_object@eig[1])) {
    p <- add_eigenvalue_info(mvar_object@eig, p, opts)
  }
  return (p)
}
