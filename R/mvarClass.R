#' @title mvarLayer-class
#'
#' @description A class for storing a single projection of a data set.
#'
#' @rdname mvarLayer-class
#'
#' @importFrom methods setClass
#' @export
setClass("mvarLayer",
         representation(
           coord = "matrix",
           annotation = "data.frame"
           )
         )

#' @title mvarTable-class
#'
#' @description A class for storing multiple projections of a data set.
#'
#' @rdname mvarTable-class
#'
#' @importFrom methods setClass
#' @export
setClass("mvarTable",
         representation(
           table = "list",
           eig = "numeric"
           )
         )

check_table <- function(object) {
  # Check that table is a list of mvarLayer objects
  class_is_mvar_layer <- lapply(object, function(x) class(x)=="mvarLayer")
  if(!all(class_is_mvaraxis)) {
    errors <- paste0("Elements ", which(!class_is_mvaraxis), " are not valid mvarLayer objects.")
  }
  errors
}
