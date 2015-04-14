#' @title Convert a vegan object into class mVarTable
#'
#' @param An object resulting from a call to a \code{vegan} ordination method.
#'
#' @return mvar_table An object of class \code{mvarTable}, storing the site
#'    and species tables in the \code{table} slot and the eigenvalues in the
#'    \code{eig} slot. The annotation slots of each mVarAxis are the row names
#'    of the projected coordinates.
#'
#' @examples
#'
#' @importFrom vegan scores
#'
#'  @export
vegan_to_mvar <- function(vegan_object) {

  mvar_axis_list <- list()
  scores_list <- vegan::scores(vegan_object)

  # In case of one table, we need to convert to a list to be consistent with
  # multitable case below
  if(is.matrix(scores_list)) {
    scores_list <- list(scores_list)
  }

  # Build an mvarAxis object for each
  for(cur_table in scores_list) {

    # Convert coordinates into a matrix
    vegan_subset <- cur_table
    vegan_subset_mat <- as.matrix(ade4_subset)
    dimnames(vegan_subset_mat) <- NULL

    # Annotation defaults to projection matrix rownames
    cur_annotation <- data.frame(label = rownames(vegan_subset))

    mvar_axis_list[[cur_table]] <- new("mvarAxis", coord = vegan_subset_mat,
                                       annotation = cur_annotation)
  }

  # Add eigenvalues, if the current vegan call computed this
  if(!is.null(vegan_object$CA$eig)) {
    cur_eig <- vegan_object$CA$eig
  } else {
    cur_eig <- NULL
  }

  # Combined tables mvarTable object
  mvar_table <- new("mvarTable", table = mvar_axis_list, eig = cur_eig)
  return (mvar_table)
}
