#' @title Search Vegan and ade4 for matching method
#'
#' @importFrom ade4 dudi.pca dudi.pco dudi.acm dudi.coa dudi.fpca dudi.hillsmith
#'    dudi.mix dudi.nsc dpcoa cca
#' @importFrom vegan cca
match_ordi_method <- function(method) {
  # determine the function to be called
  paste_dudi <- c("pca", "pco", "acm", "coa", "fpca","hillsmith", "mix", "nsc")
  if(method %in% paste_dudi) {
    # Methods that need to be called as dudi."name"
    ordi_method <- eval(parse(text=paste0("dudi.", method)))
  } else if(method %in% "vegan_cca") {
    ordi_method <- eval(parse(text = "vegan::cca"))
  } else if(method %in% "ade4_cca") {
    ordi_method <- eval(parse(text = "ade4::cca"))
  } else {
    # Methods that can be called directly
    ordi_method <- eval(parse(text=method))
  }
  return (ordi_method)
}

# ordi-without-annotation ---------------------------------------------

#' @title Wrapper for vegan and ade4 ordi methods
#'
#' @importFrom vegan vegdist cca decorana metaMDS isomap rda CCorA
#' @importFrom ade4 dudi.pca dudi.acm dudi.coa dudi.fca dudi.fpca dudi.hillsmith
#'    dudi.mix dudi.nsc dudi.pco dpcoa procuste cca
#' @export
ordi_wrapper <- function(X, method = "pca", dist_method = "euclidean", ...) {
  ordi_method <- match_ordi_method(method)
  direct_methods <- c("pca", "acm", "coa", "fca", "fpca", "hillsmith", "mix", "nsc")
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

# convert-class -----------------------------------------------------------

#' @title Convert vegan and ade4 objects to class mvar
#'
#' @export
convert_to_mvar <- function(X_ord, table_names) {
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

# get-annotation ----------------------------------------------------------

#' @title Get Annotation to combine with mvarVis Object
#'
#' @export
get_annotation_list <- function(X_mvar, rows_annot = NULL, cols_annot = NULL) {
  annotation_list <- list()
  for(cur_table in names(X_mvar@table)) {
    if(cur_table %in% c("li", "l1", "site") & !is.null(rows_annot)) {
      annotation_list[[cur_table]] <- rows_annot
    } else if(cur_table %in% c("co", "l2", "species") & !is.null(cols_annot)) {
      annotation_list[[cur_table]] <- cols_annot

    # For multitable methods, need column and row annotation lists, not just data frames
    } else if(cur_table %in% c("corr.X.Cx", "corr.X.Cy", "load1") & !is.null(cols_annot)) {
      annotation_list[[cur_table]] <- cols_annot[[1]]
    } else if(cur_table %in% c("corr.Y.Cx", "corr.Y.Cy", "load2") & !is.null(cols_annot)) {
      annotation_list[[cur_table]] <- cols_annot[[2]]
    } else if(cur_table %in% c("scor1") & !is.null(rows_annot)) {
      annotation_list[[cur_table]] <- rows_annot[[1]]
    } else if (cur_table %in% c("scor2") & !is.null(rows_annot)) {
      annotation_list[[cur_table]] <- rows_annot[[2]]
    } else {
      n_points <- nrow(X_mvar@table[[cur_table]]@annotation)
      annotation_list[[cur_table]] <- data.frame(ix = 1:n_points)
    }
  }
  return(annotation_list)
}

#' Get the tables to put into an mvarTable object
#'
#' @param method The name of the ordination method from which we want to
#'  extract scores.
#' @return If the method is implemented in ade4, the names of the tables that
#'  give the row and column scores.
default_table_names <- function(method) {
  table_names <- switch(method,
         "pca" = c("li", "co"),
         "acm" = c("li", "co"),
         "coa" = c("li", "co"),
         "fca" = c("li", "co"),
         "fpca" = c("li", "co"),
         "pco" = c("li", "co"),
         "hillsmith" = c("li", "co"),
         "mix" = c("li", "co"),
         "nsc" = c("li", "co"),
         "dpcoa" = c("l1", "l2"),
         "decorana" = c(), # right now we're using scores() from vegan, instead of extracting any tables
         "metaMDS"  = c(),
         "isomap" = c(),
         "isoMDS" = c(),
         "vegan_cca" = c(),
         "ade4_cca" = c("li", "co"),
         "rda" = c(),
         "CCorA" = c(),
         "procuste" = c("scor1", "scor2"),
         "coinertia" = c("li", "co"))
  return (table_names)
}

# ordi-wrapper ------------------------------------------------------

#' @title Wrapper for ade4 and vegan ordi functions
#'
#' @importFrom ade4 dudi.pca
#' @export
ordi <- function(X, rows_annot = NULL, cols_annot = NULL, method = "pca",
                 dist_method = "euclidean", table_names = NULL, ...) {
  implemented_methods <- c("pca", "acm", "coa", "fca", "fpca", "pco",
                           "hillsmith","mix","nsc", "dpcoa", "decorana",
                           "metaMDS", "isomap", "isoMDS", "vegan_cca",
                           "ade4_cca", "rda", "CCorA", "procuste", "coinertia")
  method <- match.arg(method, implemented_methods)
  X_ord <- ordi_wrapper(X, method, dist_method, ...)
  if(is.null(table_names)) {
    table_names <- default_table_names(method)
  }
  X_mvar <- convert_to_mvar(X_ord, table_names)
  annotation_list <- get_annotation_list(X_mvar, rows_annot, cols_annot)
  # give appropriate annotation
  X_mvar <- mvar_annotate(X_mvar, annotation_list)
  return (X_mvar)
}
