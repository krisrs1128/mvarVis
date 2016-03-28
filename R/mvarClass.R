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
mvar_boot_to_table <- function(mvar_boot_tab) {
  stopifnot(class(mvar_boot_tab) == "mvarBootTable")

  n_boot <- length(mvar_boot_tab@boot)
  boot_eigs <- lapply(1:n_boot, function(i) mvar_boot_tab@boot[[i]]@eig)
  min_n_eig <- min(sapply(boot_eigs, length))
  boot_eigs <- lapply(boot_eigs, function(x) x[1:min_n_eig])
  mean_eigs <- colMeans(do.call(rbind, boot_eigs))
  new("mvarTable", table = merge_tables(mvar_boot_tab@boot), eig = mean_eigs)
}
################################################################################
################################################################################
#' @title Merge tables
#'
#' @description Merge tables from the list of mvarTable objects
merge_tables <- function(mvar_tables){
  n_tables <- sapply(mvar_tables, function(x) length(x@table))
  stopifnot(all(n_tables == n_tables[1]))
  n_tables <- n_tables[1]

  # initialize results
  annot <- vector(mode = "list", length = n_tables)
  coords <- vector(mode = "list", length = n_tables)

  # merge over tables and bootstrap reps
  list_length <- length(mvar_tables)
  for(j in seq_len(n_tables)) {
    annot[[j]] <- vector(mode = "list", length = list_length)
    coords[[j]] <- vector(mode = "list", length = list_length)
    for(i in seq_along(mvar_tables)) {
      annot[[j]][[i]] <- data.frame(mvar_tables[[i]]@table[[j]]@annotation, bootIDX = i)
      coords[[j]][[i]] <- mvar_tables[[i]]@table[[j]]@coord
    }
    annot[[j]] <- do.call(rbind, annot[[j]])
    coords[[j]] <- do.call(rbind, coords[[j]])
  }

  lapply(1:n_tables, function(j) {
    new("mvarLayer", coord = coords[[j]], annotation = annot[[j]])
  })
}
