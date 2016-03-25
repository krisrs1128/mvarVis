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
#' @description A class for storing multiple projections of the data set and
#' of its boostrap samples.
#'
#' @rdname  mvarBootTable-class
#'
#' @importFrom methods setClass
#' @export
setClass("mvarBootTable",
         representation(
           center = "mvarTable",
           boot = "list"
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
    errors <- paste0("Elements ", which(!class_is_mvar_layer), 
                     " are not valid mvarLayer objects.")
  }
  return(TRUE)
}
################################################################################
################################################################################
#' @title Convert mvarBootTable to mvarTable
#' 
#' @description Converts mvarBootTable object into 2 mvarTable objects
#' one for center and one for bootstrap samples.
mvar_boot2Table <- function(mvarBootTab) {
  if (class(object) != "mvarBootTable") 
    stop("Input object must of of class mvarBootTable")
  nBoots <- length(mvarBootTab@boot)
  bootEigs <- lapply(1:nBoots, function(i) mvarBootTab@boot[[i]]@eig)
  min_n_eig <- min(sapply(bootEigs, function(x) length(x)))
  bootEigs <- lapply(bootEigs, function(x) x[1:min_n_eig])
  meanBootEigs <- colMeans(do.call(rbind, bootEigs))
  return(new("mvarTable", 
        table = merge_tables(mvarBootTab@boot), 
        eig = meanBootEigs))
}
################################################################################
################################################################################
#' @title Merge tables 
#' 
#' @description Merge tables from the list of mvarTable objects
merge_tables <- function(mvarTableList){
  firstTab <- mvarTableList[[1]]@table
  for (tabIDX in 1:length(firstTab)) {
    curAnnot <- firstTab[[tabIDX]]@annotation
    firstTab[[tabIDX]]@annotation <- cbind(curAnnot, 
                                           bootIDX =rep(1, nrow(curAnnot)))
  }
  superTable <- firstTab
  for (i in 2:length(mvarTableList)) {
    newTable <- mvarTableList[[i]]@table
    bootIDXName <- data.frame(bootIDX = i)
    for (tabIDX in 1:length(newTable)) {
      curAnnot <- newTable[[tabIDX]]@annotation
      newAnnot <- cbind(curAnnot, bootIDX = rep(i, nrow(curAnnot)))
      superTable[[tabIDX]]@coord <- rbind(superTable[[tabIDX]]@coord, 
                                          newTable[[tabIDX]]@coord)
      superTable[[tabIDX]]@annotation <- rbind(superTable[[tabIDX]]@annotation, 
                                               newAnnot)
    }
  }
  return(superTable)
}
