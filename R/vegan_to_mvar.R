#' @title Convert a vegan object into class mVarTable
#'
#' @param An object resulting from a call to a \code{vegan} ordination method.
#'
#' @return mvar_table An object of class \code{mvarTable}, storing the site
#'    and species tables in the \code{table} slot and the eigenvalues in the
#'    \code{eig} slot. The annotation slots of each mVarAxis are the row names
#'    of the projected coordinates.
#'
#' @importFrom vegan scores
#'
#'  @export
vegan_to_mvar <- function(vegan_object) {
  mvar_axis_list <- list()
  scores_list <- list()

  # site or species scores may or may not be available
  for(cur_display in c("site", "species")) {
    cur_scores <- try(scores(vegan_object, display = cur_display), silent = TRUE)
    if(class(cur_scores) != "try-error") {
    scores_list[[cur_display]] <- cur_scores
    }
  }

  # canonical correlations analysis has a different structure
  if(class(vegan_object) == "CCorA") {
    for(cur_display in c("corr.X.Cx", "corr.X.Cy", "corr.Y.Cx", "corr.Y.Cy")) {
      scores_list[[cur_display]] <- vegan_object[[cur_display]]
    }
  }

  # Build an mvarAxis object for each
  for(cur_table in names(scores_list)) {

    # Convert coordinates into a matrix
    vegan_subset <- scores_list[[cur_table]]
    vegan_subset_mat <- as.matrix(vegan_subset)
    dimnames(vegan_subset_mat) <- list(NULL, paste0("axis_", 1:ncol(vegan_subset_mat)))

    # Annotation defaults to projection matrix rownames
    cur_annotation <- data.frame(label = rownames(vegan_subset))

    mvar_axis_list[[cur_table]] <- new("mvarAxis", coord = vegan_subset_mat,
                                       annotation = cur_annotation)
  }

  # Add eigenvalues, if the current vegan call computed this
  if(!is.null(vegan_object$CA$eig)) {
    cur_eig <- vegan_object$CA$eig
  } else if(!is.null(vegan_object$Eigenvalues)) {
    cur_eig <- vegan_object$Eigenvalues
  } else {
    cur_eig <- as.numeric(NA)
  }

  # Combined tables mvarTable object
  mvar_table <- new("mvarTable", table = mvar_axis_list, eig = cur_eig)
  return (mvar_table)
}
