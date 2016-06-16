#' @title Wrapper for vegan / ade4 / factominer  ordination methods
#' @description Given data X and a string describing a method in \code{ade4}, or
#'  \code{factominer}, or \code{vegan}, return the corresponding ordination
#' object from those packages.
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
#'  @param method The method among those listed above that will perform the
#'  required ordination.
#'  @param dist_method If a distance matrix is used by the specified method and
#'  \code{X} is not a distance object, we will call \code{vegdist} on the
#'  \code{X} using this string as the distance.
#' @return The result of a call to the specified method, in the original
#' \code{ade4}, \code{factominer}, or \code{vegan} classes.
#' @importFrom vegan vegdist cca decorana metaMDS isomap rda CCorA
#' @importFrom ade4 dudi.pca dudi.acm dudi.coa dudi.fca dudi.fpca dudi.hillsmith
#'    dudi.mix dudi.nsc dudi.pco dpcoa procuste cca
#' @importFrom FactoMineR PCA CA DMFA FAMD HMFA MCA 
#' @export
ordi_wrapper <- function(X, method = "ade4_pca", dist_method = "euclidean", ...) {
  ordi_method <- match_ordi_method(method)
  direct_methods <- c("ade4_pca", "acm", "coa", "fca", "fpca", "hillsmith", "mix",
                      "nsc", "decorana", "factominer_pca", "CA", "MFA", "DMFA",
                      "FAMD", "HMFA", "MCA")
  dist_methods <- c("pco", "isomap", "dpcoa", "metaMDS")
  formula_methods <- c("vegan_cca", "rda")
  df_list_methods <- c("CCorA", "ade4_cca", "procuste")

  if(method %in% direct_methods) {
    # Methods that can be called on a single data frame X
    X_ord <- ordi_method(X, ...)
  } else if(method %in% dist_methods) {
    # Methods that require a distance matrix
    if(method %in% c("pco", "isomap")) {
      # Methods that are called on just a distance matrix
      if(class(X) != "dist") {
        X <- vegdist(X, method = dist_method)
      }
      X_ord <- ordi_method(X, ...)
    } else if(method  %in% "metaMDS") {
      # This is a distance method which requires some automatic preprocessing
      X_ord <- ordi_method(X, distance = dist_method, ...)
    } else if(method %in% c("dpcoa")) {
      # Methods called on both  data frame and a distance matrix
      if(class(X[[2]]) != "dist") {
        X[[2]]  <- vegdist(X, method = dist_method)
      }
      X_ord <- ordi_method(data.frame(X[[1]]), X[[2]], ...)
    }
  } else if(method %in% formula_methods) {
    # Methods that require a formula + data
    X_ord <- ordi_method(formula = X$fmla, data = X$data)
  } else if(method %in% df_list_methods) {
    # Methods that require a list of data frames
    X_ord <- ordi_method(X[[1]], X[[2]], ...)
  } else if(method %in% c("coinertia")) {
    # methods that operate on the result of a pca
    dudi1 <- dudi.pca(X[[1]], scannf = F)
    dudi2 <- dudi.pca(X[[2]], scannf = F)
    X_ord <- ordi_method(dudi1, dudi2, ...)
  }
  return (X_ord)
}
