# convert-class -----------------------------------------------------------

#' @title Convert vegan and ade4 objects to class mvar
#'
#' @description Convert an \code{ade4} or \code{vegan} object to class
#' \code{mvar}.
#'
#' @param X_ord The result of a call to a \code{ade4} or \code{vegan} ordination
#' method.
#' @param table_names A vector of strings specifying which tables to extract, for
#' \code{ade4} objects. Each of these tables will be an element in the resulting
#' mvar object. Defaults to c("li", "co").
#'
#' @return An mvar object with the scores and eigenvalues of \code{X_ord}.
#'
#' @export
convert_to_mvar <- function(X_ord, table_names = c("li", "co")) {
  # convert to mvar class
  cur_class <- class(X_ord)
  vegan_classes <- c("rda", "cca", "isomap", "decorana", "CCorA", "metaMDS", "monoMDS")
  ade4_classes <- c("dpcoa", "procuste", "dudi")

  if(any(ade4_classes %in% cur_class)) {
    # ade4 classes
    available_tables <- intersect(names(X_ord), table_names)
    if(length(available_tables) != length(table_names)) {
      warning(cat("The following tables are not returned by the specified ordi method: ",
                  setdiff(table_names, names(X_ord))))
      if(length(available_tables) > 0) {
        stop("None of the requested tables are output by the specified ordination method")
      }
    }
    X_mvar <- ade4_to_mvar(X_ord, table_names)
  } else if(any(vegan_classes %in% cur_class)) {
    X_mvar <- vegan_to_mvar(X_ord)
  }
  return (X_mvar)
}
