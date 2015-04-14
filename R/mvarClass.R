#' @title mvarAxis-class
#'
#' @description A class for storing a single projection of a data set.
#'
#' @rdname mvarAxis-class
#'
#' @importFrom methods setClass
#' @export
setClass("mvarAxis",
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
  # Check that table is a list of mvarAxis objects
  class_is_mvaraxis <- lapply(object, function(x) class(x)=="mvarAxis")
  if(!all(class_is_mvaraxis)) {
    errors <- paste0("Elements ", which(!class_is_mvaraxis), " are not valid mvarAxis objects.")
  }
  return (errors)
}
