#' @title Wrapper for ade4 and vegan ordi functions
#'
#' @description Perform an ordination using an ade4 or vegan function, and
#'  return an mvarVis object with the annotation completed.
#'
#' @param X Either a data frame, a distance object, a list of both a data frame
#'  and a distance, a formula and a data frame, or a list of data frames,
#'  depending on the desired method. \code{pcm}, \code{acm}, \code{coa},
#'  \code{fca}, \code{fpca}, \code{hillsmith}, \code{mix},
#'  and \code{nsc} require the raw data frame. \code{pco}, \code{isomap}, and
#'  \code{metaMDS} can take either a \code{data.frame} or a \code{dist}.
#'  vegan_cca and rda require a list whose first element
#'  is a formula and whose second element is a data set containing the
#'  constraining variables, as in the usual \code{cca} function in \code{vegan}.
#'  \code{dpcoa} requires a list whose first element is a data frame and whose
#'  second element is a distance between the rows on that data frame.
#'  \code{CCorA}, \code{ade4_cca}, and \code{procuste} all require a list of
#'  data frames.
#' @param rows_annot For one table methods, a data.frame whose j^th row
#'  describes the j^th sample in the coordinates matrices corresponding to
#'  row scores. For procustes and CCorA, we require a list of dataframes, one
#'  for each of the directions.
#' @param cols_annot The analogous object for column scores.
#' @param method The method among those listed above that will perform the
#'  required ordination.
#' @param dist_method If a distance matrix is used by the specified method and
#'  \code{X} is not a distance object, we will call \code{vegdist} on the
#'  \code{X} using this string as the distance.#' @param rows_annot
#' @param table_names A vector of strings specifying which tables to extract, for
#' @export
ordi <- function(X, method = "ade4_pca", rows_annot = NULL, cols_annot = NULL,
                 dist_method = "euclidean", table_names = NULL, ...) {
  implemented_methods <- c("ade4_pca", "acm", "coa", "fca", "fpca", "pco",
                           "hillsmith","mix","nsc", "dpcoa", "decorana",
                           "metaMDS", "isomap", "isoMDS", "vegan_cca",
                           "ade4_cca", "rda", "CCorA", "procuste", "coinertia",
                           "factominer_pca", "CA", "MFA", "DMFA", "FAMD",
                           "HMFA", "MCA", "spMCA")
  method <- match.arg(method, implemented_methods)

  # Perform ordination
  X_ord <- ordi_wrapper(X, method, dist_method, ...)

  if(is.null(table_names)) {
    table_names <- default_table_names(method)
  }

  # Convert to an mvar object
  X_mvar <- convert_to_mvar(X_ord, table_names)

  # give appropriate annotation
  annotation_list <- get_annotation_list(X_mvar, rows_annot, cols_annot)
  mvar_annotate(X_mvar, annotation_list)
}
