#' @title Build Opts lists
#'
#' @param mvar_object The mvar_object that we would like to plot.
#' @param layers_list A list containing which layer attributes to plot for
#'  every layer. See the function \code{build_layers_list()} for more
#'  details.
#' @param aes_list A list containing the aesthetics attributes for every
#'  layer. See \code{build_aes_and_non_aes_lits()} for more details.
#' @param non_aes_list The list containing the non-data-driven aesthetics
#'  attributes for every layer. See \code{build_aes_and_non_aes_lits()} for
#'  more details.
#' @param facet_vector A vector containing column names in the annotation data
#'  to use for faceting.
#'
#' @return opts A list that can be input into \code{plot_mvar_from_opts()}.
build_opts <- function(mvar_object, layers_list, aes_list, non_aes_list,
                       facet_vector = NULL) {
  n_tables <- length(mvar_object@table)
  opts <- rep(list(list()), n_tables)

  for(cur_table in 1:n_tables) {
    opts[[cur_table]]$facet_vector <- facet_vector
    opts[[cur_table]]$layers_list <- layers_list[[cur_table]]
    opts[[cur_table]]$aes_list <- aes_list[[cur_table]]
    opts[[cur_table]]$non_aes_list <- non_aes_list[[cur_table]]
  }
  return (opts)
}
