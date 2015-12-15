
################################################################################
# Conversion from vegan / ade4 / FactoMineR -> mvarTable
################################################################################

#' @title Convert a FactoMineR object to class mvarTable
#' @param ade4_object The output of a call to a function in FactoMineR.
#' @param tables_to_include A character vector specifying which elements in
#' the \code{FactoMineR} object to store in the mVarTable. We expect that
#' ade4_object[[cur_table]]$coord be nonnull, for every cur_table in
#' tables_to_include.
#' @examples
#' library("FactoMineR")
#' example(ca)
#' factominer_to_mvar(res.ca, c("row", "col"))
#' example(PCA)
#' factominer_to_mvar(res.pca, c("ind", "var"))
#' example(MFA)
#' factominer_to_mvar(res, c("ind$coord", "quanti.var$coord"))
#' @export
factominer_to_mvar <- function(factominer_object, tables_to_include) {
  mvar_layer_list <- list()

  # Build an mvarLayer object for each
  for(cur_table in tables_to_include) {
    factominer_subset <- factominer_object[[cur_table]]$coord
    factominer_subset <- as.matrix(factominer_subset)
    colnames(factominer_subset) <- paste0("layer_", 1:ncol(factominer_subset))
    cur_annotation <- data.frame(label = rownames(factominer_subset))
    mvar_layer_list[[cur_table]] <- new("mvarLayer", coord = factominer_subset,
                                        annotation = cur_annotation)
  }

  # Get eigenvalues
  if(!is.null(factominer_object$eig)) {
    eig  <-  factominer_object$eig$eigenvalue
  } else {
    eig <- NA
  }

  # Combined tables mvarTable object
  new("mvarTable", table = mvar_layer_list, eig = eig)
}
