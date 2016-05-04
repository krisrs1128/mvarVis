#' @title Add eigenvalue information
#'
#' @description Add eigenvalue labels and rescale axes according to proportion
#'  of variation explained
#'
#' @param eigs A vector of the eigenvalues to use in the rescaling.
#' @param p The plot to add eigenvalue information to
#' @param opts The list containing aesthetic and layers information. This is
#'  needed to determine the x and y axis labels.
#' @return p The plot with eigenvalue information added, and with the aspect
#'  ratio fixed by the ratio of eigenvalues.
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous coord_fixed
add_eigenvalue_info <- function(eigs, p, coordfixed = TRUE, opts = list()) {
  # add eigenvalue labels for the axis specified by the first element in the aes list
  merged_aes <- merge_table_plot_opts(opts)$aes_list
  x_axis  <- as.numeric(gsub("axis_", "", merged_aes$x))
  y_axis <- as.numeric(gsub("axis_", "", merged_aes$y))
  eigs_prop <- eigs[c(x_axis, y_axis)] / sum(eigs)
  x_label <- sprintf("%s [%g%%]", merged_aes$x, 100 * round(eigs_prop[1], 3))
  y_label <- sprintf("%s [%g%%]", merged_aes$y, 100 * round(eigs_prop[2], 3))
  p <- p + scale_x_continuous(x_label) +
    scale_y_continuous(y_label) 
  if (coordfixed) {
    p <- p + coord_fixed(ratio = eigs_prop[2] / eigs_prop[1])
  }
  return(p)
}
