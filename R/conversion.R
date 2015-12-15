
################################################################################
# Function to convert factominer / ade4 / vegan -> mvarTable
################################################################################

#' @title Convert a FactoMineR object to class mvarTable
#' @param ade4_object The output of a call to a function in FactoMineR.
#' @param tables_to_include A character vector specifying which elements in
#' the \code{FactoMineR} object to store in the mVarTable. We expect that
#' ade4_object[[cur_table]]$coord be nonnull, for every cur_table in
#' tables_to_include.
#' @example
#' library("FactoMineR")
#' example(CA)
#' factominer_to_mvar(res.ca, c("row", "col"))
#' example(MFA)
#' factominer_to_mvar(res, c("ind", "quanti.var"))
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

#' @title Convert an ade4 object into class mVarTable
#' @param ade4_object The output of a call to dudi.* from package \code{ade4}.
#' This is generally a named list containing tables of multiple projections
#' of interest. Calling \code{class(ade4_object)} should show
#' \code{ordination_method dudi}.
#' @param tables_to_include A character vector specifying which projections in
#' the \code{ade4} object to store in the mVarTable object. This vector can
#' have arbitrary length, the only requirement is that calling
#' \code{ade4_object[tables_to_include[i]]} should be a data frame.
#' @return mvar_table An object of class mvarTable, storing the tables specified
#' in \code{tables_to_include} and the eigenvalues from the \code{ade4}
#' decomposition. The annotation slots of each mVarLayer are the row names of
#' the projected coordinates.
#' @examples
#'  library("ade4")
#'  data(USArrests)
#'  arrests_pca <- dudi.pca(USArrests, scannf = FALSE, nf = 2)
#'  ade4_to_mvar(arrests_pca, c("li", "co"))
#'
#'  arrests_pco <- dudi.pco(dist(USArrests), scannf = FALSE, nf = 2)
#'  ade4_to_mvar(arrests_pco, c("li", "co"))
#'
#'  # Example taken from coinertia() in ade4 package
#'  data(doubs)
#'  dudi1 <- dudi.pca(doubs$env, scale = TRUE, scan = FALSE, nf = 3)
#'  dudi2 <- dudi.pca(doubs$fish, scale = FALSE, scan = FALSE, nf = 2)
#'  coin1 <- coinertia(dudi1,dudi2, scan = FALSE, nf = 2)
#'  ade4_to_mvar(coin1, tables_to_include = c("li", "co", "aX", "aY"))
#'
#'  @export
ade4_to_mvar <- function(ade4_object, tables_to_include) {
  mvar_layer_list <- list()

  # Build an mvarLayer object for each
  for(cur_table in tables_to_include) {

    # Convert coordinates into a matrix
    ade4_subset <- ade4_object[[cur_table]]
    ade4_subset_mat <- as.matrix(ade4_subset)
    dimnames(ade4_subset_mat) <- list(NULL, paste0("layer_", 1:ncol(ade4_subset_mat)))

    # Annotation defaults to projection matrix rownames
    cur_annotation <- data.frame(label = rownames(ade4_subset))

    mvar_layer_list[[cur_table]] <- new("mvarLayer", coord = ade4_subset_mat,
                                       annotation = cur_annotation)
  }

  # Get eigenvalues
  if(!is.null(ade4_object$eig)) {
    eig  <-  ade4_object$eig
  } else if(!is.null(ade4_object$d)) {
    eig  <-  ade4_object$d
  } else {
    eig <- NA
  }

  new("mvarTable", table = mvar_layer_list, eig = eig)
}

#' @title Convert a vegan object into class mVarTable
#' @param An object resulting from a call to a \code{vegan} ordination method.
#' @return mvar_table An object of class \code{mvarTable}, storing the site
#'    and species tables in the \code{table} slot and the eigenvalues in the
#'    \code{eig} slot. The annotation slots of each mVarLayer are the row names
#'    of the projected coordinates.
#' @importFrom vegan scores
#'  @export
vegan_to_mvar <- function(vegan_object) {
  mvar_layer_list <- list()
  scores_list <- list()

  # site or species scores may or may not be available
  for(cur_display in c("site", "species")) {
    cur_scores <- try(scores(vegan_object, display = cur_display), silent = TRUE)
    if(class(cur_scores) != "try-error") {
    scores_list[[cur_display]] <- cur_scores
    }
  }

  # canonical correlations analysis has a different structure
  if("CCorA" %in% class(vegan_object)) {
    for(cur_display in c("corr.X.Cx", "corr.X.Cy", "corr.Y.Cx", "corr.Y.Cy")) {
      scores_list[[cur_display]] <- vegan_object[[cur_display]]
    }
  }

  # Build an mvarLayer object for each
  for(cur_table in names(scores_list)) {

    # Convert coordinates into a matrix
    vegan_subset <- scores_list[[cur_table]]
    vegan_subset_mat <- as.matrix(vegan_subset)
    dimnames(vegan_subset_mat) <- list(NULL, paste0("layer_", 1:ncol(vegan_subset_mat)))

    # Annotation defaults to projection matrix rownames
    cur_annotation <- data.frame(label = rownames(vegan_subset))

    mvar_layer_list[[cur_table]] <- new("mvarLayer", coord = vegan_subset_mat,
                                       annotation = cur_annotation)
  }

  # Add eigenvalues, if the current vegan call computed this
  if(!is.null(vegan_object$CCA$eig)) {
    cur_eig <- vegan_object$CCA$eig
  } else if(!is.null(vegan_object$Eigenvalues)) {
    cur_eig <- vegan_object$Eigenvalues
  } else {
    cur_eig <- as.numeric(NA)
  }

  # Combined tables mvarTable object
  mvar_table <- new("mvarTable", table = mvar_layer_list, eig = cur_eig)
  return (mvar_table)
}
