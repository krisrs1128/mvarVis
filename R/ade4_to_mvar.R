#' @title Convert an ade4 object into class mVarTable
#'
#' @param ade4_object The output of a call to dudi.* from package \code{ade4}.
#'    This is generally a named list containing tables of multiple projections
#'    of interest. Calling \code{class(ade4_object)} should show
#'    \code{ordination_method dudi}.
#' @param tables_to_include A character vector specifying which projections in
#'    the \code{ade4} object to store in the mVarTable object. This vector can
#'    have arbitrary length, the only requirement is that calling
#'    \code{ade4_object[tables_to_include[i]]} should be a data frame. Defaults
#'    to \code{c("li", "co")}, the row and column projections in a call to
#'    \code{dudi.pca()}.
#'
#' @return mvar_table An object of class mvarTable, storing the tables specified
#'    in \code{tables_to_include} and the eigenvalues from the \code{ade4}
#'    decomposition. The annotation slots of each mVarAxis are the row names of
#'    the projected coordinates.
#'
#' @examples
#'  library("ade4")
#'  data(USArrests)
#'  arrests_pca <- dudi.pca(USArrests, scannf = FALSE, nf = 2)
#'  ade4_to_mvar(arrests_pca)
#'
#'  arrests_pco <- dudi.pco(dist(USArrests), scannf = FALSE, nf = 2)
#'  ade4_to_mvar(arrests_pco)
#'
#'  # Example taken from coinertia() in ade4 package
#'  data(doubs)
#'  dudi1 <- dudi.pca(doubs$env, scale = TRUE, scan = FALSE, nf = 3)
#'  dudi2 <- dudi.pca(doubs$fish, scale = FALSE, scan = FALSE, nf = 2)
#'  coin1 <- coinertia(dudi1,dudi2, scan = FALSE, nf = 2)
#'  ade4_to_mvar(coin1, tables_to_include = c("li", "co", "aX", "aY"))
#'
#'  @export
ade4_to_mvar <- function(ade4_object, tables_to_include=c("li", "co")) {
  mvar_axis_list <- list()

  # Build an mvarAxis object for each
  for(cur_table in tables_to_include) {

    # Convert coordinates into a matrix
    ade4_subset <- ade4_object[[cur_table]]
    ade4_subset_mat <- as.matrix(ade4_subset)
    dimnames(ade4_subset_mat) <- list(NULL, paste0("axis_", 1:ncol(ade4_subset_mat)))

    # Annotation defaults to projection matrix rownames
    cur_annotation <- data.frame(label = rownames(ade4_subset))

    mvar_axis_list[[cur_table]] <- new("mvarAxis", coord = ade4_subset_mat,
                                       annotation = cur_annotation)
  }

  # Combined tables mvarTable object
  mvar_table <- new("mvarTable", table = mvar_axis_list, eig = ade4_object$eig)
  return (mvar_table)
}
