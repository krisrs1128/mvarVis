
# ordi-without-annotation ---------------------------------------------

#' @title Wrapper for vegan and ade4 ordi methods
#'
#' @export
ordi_wrapper <- function(X, method = "pca", dist_method = "euclidean", ...) {
  # perform ordi
  if(method %in% c("pca", "pco", "acm", "coa", "fpca",
                   "hillsmith", "mix", "nsc")) {
    # Methods that need to be called as dudi."name"
    ordi_method <- eval(parse(text=paste0("dudi.", method)))
  } else {
    # Methods that can be called directly
    ordi_method <- eval(parse(text=method))
  }

  if(method %in% c("pca", "acm", "coa", "fca",
                   "fpca", "hillsmith", "mix", "nsc")) {
    # Methods that can be called on a single data frame X
    X_ord <- ordi_method(X, ...)
  } else if(method %in% c("pco", "dpcoa")) {
    if(method %in% c("pco")) {
      # Methods that are called on just a distance matrix
      if(!is.dist(X)) {
        X <- vegdist(X, method = dist_method)
      }
      X_ord <- ordi_method(X, ...)
    } else if(method %in% c("dpcoa")) {
      # Methods called on both  data frame and a distance matrix
      if(!is.dist(X[[2]])) {
        X[[2]]  <- vegdist(X, method = dist_method)
      }
      X_ord <- ordi_method(data.frame(X[[1]]), X[[2]], ...)
    }
  }
  return (X_ord)
}

# convert-class -----------------------------------------------------------

#' @title Convert vegan and ade4 objects to class mvar
#'
#' @export
convert_to_mvar <- function(X_ord, table_names = c("li", "co")) {
  # convert to mvar class
  cur_class <- class(X_ord)
  if(any(c("rda", "cca", "decorana") %in% cur_class)) {
    # Vegan classes
    X_mvar <- vegan_to_mvar(X_ord)
  } else if(any(c("dpcoa", "dudi") %in% cur_class)) {
    # ade4 classes

    available_tables <- intersect(names(X_ord), table_names)
    if(length(available_tables) != length(table_names)) {
      warning(cat("The following tables are not returned by the specified ordi method: ",
                  setdiff(table_names, names(X_ord))))
    }
    X_mvar <- ade4_to_mvar(X_ord, table_names)
  }

  return (X_mvar)
}

# get-annotation ----------------------------------------------------------

#' @title Get Annotation to combine with mvarVis Object
#'
#' @export
get_annotation_list <- function(X_mvar, rows_annot = NULL,
                                cols_annot = NULL) {
  annotation_list <- list()
  for(cur_table in names(X_mvar@table)) {
    if(cur_table %in% c("li", "l1", "site") & !is.null(rows_annot)) {
      annotation_list[[cur_table]] <- rows_annot
    } else if(cur_table %in% c("co", "l2", "species") & !is.null(cols_annot)) {
      annotation_list[[cur_table]] <- cols_annot
    } else {
      n_points <- nrow(X_mvar@table[[cur_table]]@annotation)
      annotation_list[[cur_table]] <- data.frame(ix = 1:n_points)
    }
  }
  return(annotation_list)
}

# ordi-wrapper ------------------------------------------------------

#' @title Wrapper for ade4 and vegan ordi functions
#'
#' @importFrom ade4 dudi.pca
#' @export
ordi <- function(X, rows_annot = NULL,
                     cols_annot = NULL, method = "pca",
                     dist_method = "euclidean", table_names = list("li", "co"), ...) {
  implemented_methods <- c("pca", "acm", "coa", "fca", "fpca", "pco",
                           "hillsmith","mix","nsc", "pca", "pco",
                           "dpcoa", "decorana", "metaMDS", "isomap",
                           "isoMDS")
  method <- match.arg(method, implemented_methods)
  X_ord <- ordi_wrapper(X, method, dist_method, ...)
  X_mvar <- convert_to_mvar(X_ord, table_names)
  annotation_list <- get_annotation_list(X_mvar, rows_annot,
                                         cols_annot)
  # give appropriate annotation
  X_mvar <- mvar_annotate(X_mvar, annotation_list)
  return (X_mvar)
}
