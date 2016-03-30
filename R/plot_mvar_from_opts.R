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
#' @importFrom ggplot2 scale_shape_manual guides guide_legend
#' @export
plot_mvar_from_opts <- function(mvar_object, opts = NULL, opts_center = NULL) {
  if(is.null(opts)) {
    opts <- rep(list(list()), length(mvar_object@table))
  }
  p <- ggplot()
  if (class(mvar_object) == "mvarTable") {
    for(cur_table in seq_along(mvar_object@table)) {
      p <- plot_table(mvar_object@table[[cur_table]], opts[[cur_table]], p, cur_table)
    }
    if(!is.na(mvar_object@eig[1])) {
      p <- add_eigenvalue_info(mvar_object@eig, p, opts)
    }
  } else if (class(mvar_object) == "mvarBootTable") {
    mvar_center <- mvar_object@center
    mvar_boot <- mvar_boot_to_table(mvar_object)
    center_opts <- opts$center
    boot_opts <- opts$boot

    for(cur_table in seq_along(mvar_center@table)) {
      p <- plot_table(mvar_boot@table[[cur_table]], boot_opts[[cur_table]], p, cur_table)
      if (!("shape" %in% names(center_opts[[cur_table]]$aes_list)))
        center_opts[[cur_table]]$non_aes_list$shape <- 21
      p <- plot_table(mvar_center@table[[cur_table]], center_opts[[cur_table]], p, cur_table) +
        scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
        guides(fill = guide_legend(override.aes = list(shape = 21)))
    }
    if(!is.na(mvar_boot@eig[1])) {
      p <- add_eigenvalue_info(mvar_boot@eig, p, opts)
    }
  } else {
    stop("Input object must be of class mvarTable or mvarBootTable")
  }
  return (p)
}
