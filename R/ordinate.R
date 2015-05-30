
# ordinate-without-annotation ---------------------------------------------

ordinate_wrapper <- function(X = NULL, D = NULL, method = "pca",
                             dist_method = "euclidean", ...) {
  # perform ordination
  if(method %in% c("pca", "pco", "acm", "coa", "fpca",
                   "hillsmith", "mix", "nsc")) {
    # Methods that need to be called as dudi."name"
    ordination_method <- eval(parse(text=paste0("dudi.", method)))
  } else {
    # Methods that can be called directly
    ordination_method <- eval(parse(text=method))
  }

  if(method %in% c("pca", "acm", "coa", "fca",
                   "fpca", "hillsmith", "mix", "nsc")) {
    # Methods that can be called on a single data frame X
    X_ord <- ordination_method(X, ...)
  } else if(method %in% c("pco", "dpcoa")) {
    # Methods that require a distance matrix (and possibly X)
    if(is.null(D)) {
      D <- vegdist(X, method = dist_method)
    }
    if(method %in% c("pco")) {
      # Methods that are called on just D
      X_ord <- ordination_method(D, ...)
    } else if(method %in% c("dpcoa")) {
      # Methods called on both D and X
      X_ord <- ordination_method(data.frame(X), D, ...)
    }
  }
  return (X_ord)
}

# convert-class -----------------------------------------------------------

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
      warning(cat("The following tables are not returned by the specified ordination method: ",
                  setdiff(table_names, names(X_ord))))
    }
    X_mvar <- ade4_to_mvar(X_ord, table_names)
  }

  return (X_mvar)
}

# get-annotation ----------------------------------------------------------

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

# ordination-wrapper ------------------------------------------------------

#' @title Wrapper for ade4 and vegan ordination functions
#'
#' @importFrom ade4 dudi.pca
ordinate <- function(X = NULL, D = NULL, rows_annot = NULL,
                     cols_annot = NULL, method = "pca",
                     dist_method = "euclidean", table_names = list("li", "co"), ...) {
  stopifnot(!(is.null(X) & is.null(D)))
  implemented_methods <- c("pca", "acm", "coa", "fca", "fpca", "pco",
                           "hillsmith","mix","nsc", "pca", "pco",
                           "dpcoa", "decorana", "metaMDS", "isomap",
                           "isoMDS")
  method <- match.arg(method, implemented_methods)
  X_ord <- ordinate_wrapper(X, D, method, dist_method, ...)
  X_mvar <- convert_to_mvar(X_ord, table_names)
  annotation_list <- get_annotation_list(X_mvar, rows_annot,
                                         cols_annot)
  # give appropriate annotation
  X_mvar <- mvar_annotate(X_mvar, annotation_list)
  return (X_mvar)
}
