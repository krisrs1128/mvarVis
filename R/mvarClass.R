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
################################################################################
################################################################################
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
################################################################################
################################################################################
#' @title mvarBootTable-class
#'
#' @description A class for storing multiple projections of a data set and
#' of its boostrap samples.
#'
#' @rdname  mvarBootTable-class
#'
#' @importFrom methods setClass
#' @export
setClass("mvarBootTable",
         representation(
           center = "list"
           bootTables = "list",
           centerEig = "numeric"
           bootEigs = "list"
           )
         )
################################################################################
################################################################################
#' @title Check if valid table format
#'
#' @description A function checking if the object has a valid format
#' for the table slot of mvarTable.
#'
check_table <- function(object) {
  errors <- TRUE
  # Check that table is a list of mvarLayer objects
  class_is_mvar_layer <- sapply(object, function(x) class(x)=="mvarLayer")
  if(!all(class_is_mvar_layer)) {
    errors <- paste0("Elements ", which(!class_is_mvar_layer), " are not valid mvarLayer objects.")
  }
  return(TRUE)
}