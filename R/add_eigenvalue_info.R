#' @title Add eigenvalue information
#'
#' @description Add eigenvalue labels and rescale axes according to proportion
#'  of variation explained
#'
#' @param p The plot to add eigenvalue information to.
#' @param opts The list containing aesthetic and layers information. This is
#'  needed to determine the x and y axis labels.
#' @return p The plot with eigenvalue information added, and with the aspect
#'  ratio fixed by the ratio of eigenvalues.
add_eigenvalue_info <- function(p, opts = list()) {
  # add eigenvalue labels for the axis specified by the first element in the aes list
    merged_aes <- merge_table_plot_opts(opts)$aes_list
    x_axis  <- as.numeric(gsub("axis_", "", merged_aes$x))
    y_axis <- as.numeric(gsub("axis_", "", merged_aes$y))
    eigs_prop <- mvar_object@eig[c(x_axis, y_axis)] / sum(mvar_object@eig)
    x_label <- sprintf("%s [%g%%]", merged_aes$x, 10 * round(eigs_prop[1], 3))
    y_label <- sprintf("%s [%g%%]", merged_aes$y, 10 * round(eigs_prop[2], 3))
    p <- p + scale_x_continuous(x_label) +
      scale_y_continuous(y_label) +
      coord_fixed(ratio = eigs_prop[2] / eigs_prop[1])
  }
  return (p)
}
