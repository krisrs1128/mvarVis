#' Sample from Dirichlet Distribution
#' 
#' @description Generate a sample from a dirichlet distribution with parameter
#' \code{alpha}
#'
#' @param n The number of Dirichlet samples to draw.
#' @param alpha (Optional) A vector of parameters for Dirichlet variable.
#' Defaults to 1.
#' @return A vector of a random sample from Dirichlet distribution with
#' parameter \code{alpha}
#' @examples
#' rdirichlet(5, sample(1:10))
rdirichlet <- function(n = 1, alpha = 1) {
  stopifnot(all(alpha >= 0))
  alpha_rep <- rep(alpha, each = n)
  k <- length(alpha)
  X <- matrix(rgamma(n * k, alpha_rep, 1), k, n, byrow = T)
  sweep(X, 2, colSums(X), "/") # normalize
}

#' Boostrap a count vector (Internal)
#'
#' @param x (Required) A vector of counts.
#' @param k (Optional) The size of the bootstrap sample. Defaults to sum(x).
#' @param n (Optional) Default 1. An integer indicating the number of
#'  boostrap count vectors to return.
#' @return A vector or a matrix of \code{n} vectors boostrapped from \code{x}.
#' @examples
#' x <- sample(1:1000)
#' boot_count_vector_(x, n = 5)
#' boot_count_vector_(x, k = 100, n = 5)
boot_count_vector_ <- function(x, k = NULL, n = 1){
  if (any(x < 0)) {
    stop("No negative counts allowed")
  }
  if(is.null(k)) {
    k <- sum(x)
  }

  rmultinom(n, k, prob = x / sum(x))
}

#' Boostrap a count vector
#'
#'\code{boot_count_vector} returns \code{n} boostrap vectors of
#' counts. This is basically a multinomial sample with \code{sum(x)} trials
#' with weights equal \code{x}.
#'
#' @param x (Required) A vector of counts.
#' @param n (Optional) Default 1. An integer indicating the number of
#'  boostrap count vectors to return.
#' @param depth (Optional) Should the count vectors be normalized to a given
#' depth?
#' @param replace_zero (Optional) A logical specifying whether to replace
#' zeros in x.
#' @param replace_value (Optional) The value to replace zeros with, when
#' replace_zero is TRUE; i.e. we add a small positive weight. Default value is
#' 1.
#' @return A vector or a matrix of \code{n} vectors boostrapped from \code{x}.
#' @examples
#' boot_count_vector(sample(1:1000, 5))
#'
#' x <- sample(1:1000, 10)
#' x[sample(1:length(x), 4)] <- 0
#' boot_count_vector(x, replace_zero = 0.5)
#'
#' boot_count_vector(x, depth = 100)
boot_count_vector <- function(x, n = 1, depth = NULL, replace_zero = FALSE,
                              replace_value = 1) {
  # replace zeros
  if(replace_zero) {
    x[x == 0] <- replace_value
  }

  # if depth is not specified, draw same number of elements as in x
  if(is.null(depth)) {
    depth <- sum(x)
  }

  boot_count_vector_(x, depth, n)
}

#' Boostrap a vector of proportions
#'
#' \code{boot_prop_vector} returns \code{n} samples of boostraped vector of 
#' proportions. It draws a Dirichlet sample with parameter \code{x}. If
#' \code{depth} is supplied, the samples are scaled to have sum equal to
#' \code{depth}.
#'
#' @param x (Required). A vector of counts.
#' @param n (Optional). Default 1. An integer indicating the number of
#'  boostrap count vectors to return.
#' @param replace_zero (Optional) A logical specifying whether to replace
#' zeros in x.
#' @param replace_value (Optional) The value to replace zeros with, when
#' replace_zero is TRUE; i.e. we add a small positive weight. Default value is
#' 1.
#' @return A vector or a matrix of \code{n} vectors boostrapped from \code{x}.
#' @examples
#' y <- runif(10)
#' y <- y / sum(y)
#' boot_prop_vector(y, n = 4, replace_zero = 0.05)
boot_prop_vector <- function(x, n = 1, depth = NULL, replace_zeros = FALSE,
                             replace_value = 1) {
  if(replace_zeros) {
    x[x == 0] <- replace_value
  }

  if(is.null(depth)) {
    depth <- sum(x)
  }
  
  depth * rdirichlet(n, x)
}

################################################################################
################################################################################
#' Bootstrap table.
#'
#' \code{boot_table} returns samples boostrapped from table \code{tab} 
#' supplied. Each column of the returned table is a boostrap trial of
#' the corresponding column of the \code{tab} input table.
#' If the supplied table, \code{T}, is a matrix of raw counts (integers),
#' the columns of the output table are multinomial samples with weights 
#' proportional to corresponding columns of \code{tab}, otherwise the output
#' columns are samples from Dirichlet distribution with parameters equal
#' to corresponding columns of \code{tab}.
#'
#' @param tab (Required). A matrix or data.frame of numeric counts/weights.
#' @param n (Optional). Default 1. An integer indicating the number of
#'  boostrap count tables to return.
#' @param common_depth (Optional). Default \code{TRUE}. A logical or numeric
#'  scalar. If numeric or \code{TRUE} the boostrap counts are normalized so
#'  that the column sums are even and equal to \code{common_depth} or
#'  \code{median} of the \code{tab} column sums respectively.
#' @param replace_zero (Optional). Default FALSE. A logical or numeric
#'  scalar/vector replacing the zero probability of having a read for an OTU.
#'  If numeric or TRUE zeros in \code{x} will be replaced
#'  by \code{replace_zero} or 1 respectively, i.e. we add a small positive
#'  weight in place of zeros. If \code{replace_zero} is a vector then each
#'  component corresponds to respective columns of \code{tab}.
#' @param round (Optional). Default FALSE. A logical scalar. Should the
#'  boostrap counts be rounded to the nearest integer?
#' @return \code{n} x dim(\code{tab})[1] x dim(\code{tab})[2] 3D array of 
#' samples boostrapped from \code{tab}.
#' @export
#' @examples
#' x <- matrix(sample(1:1000, 30, replace = TRUE), 5, 6)
#' x[1, 4] <- 0; x[2, 5] <- 0; x[4, 6] <- 0; x[1, 5] <- 0
#' x
#' boot_table(x, n = 10)
#' boot_table(x, common_depth = 100)
#' boot_table(x, common_depth = 100, n = 10, round = TRUE)
#' boot_table(x, common_depth = 100, n = 10, replace_zero = 0.5)
boot_table <- function(tab, n = 1, common_depth = FALSE,
                       replace_zero = FALSE, round = FALSE) {
  if (common_depth == TRUE)
    common_depth <- median(colSums(tab))
  if (length(replace_zero) == 1)
    replace_zero = rep(replace_zero, ncol(tab))
  if (length(replace_zero) != ncol(tab))
    stop("`replace_zero` length > 1 and does not match the number of 
         columns of `tab`.")
  bootMats <- array(0, dim=c(n, nrow(tab), ncol(tab)))
  dimnames(bootMats)[[2]] <- rownames(tab)
  dimnames(bootMats)[[3]] <- colnames(tab)
  if(n > 1) dimnames(bootMats)[[1]] <- paste("trial", 1:n, sep = "_")
  
  for(i in 1:ncol(tab)){
    mat <- boot_count_vector(tab[, i], n, common_depth, replace_zero[i])
    if(round)
      mat <- round(mat)
    if (!all(dim(mat) == dim(bootMats[, , i]))) mat <- t(mat)
    bootMats[, , i] <- mat
  }
  return(bootMats)
}
################################################################################
################################################################################
#' Boostrap ordination.
#'
#' \code{boot_ordination} computes the ordination for samples boostrapped
#' from data in \code{D}. Ordination is performed using \code{mvarVis::ordi}
#' function with specified \code{method}. 
#' 
#' @param D (Required). A data frame of raw counts/weights. Raw data 
#'  is required, and distance matrix/objects are not acceptable.
#' @param n (Optional). Default 50. An integer indicating the number of
#'  boostrap samples generated.
#' @param method (Required). The method among those listed above that will 
#'  perform the required ordination.
#' @param dist_method (Optional). If a distance matrix is used by the specified 
#'  method. We will call \code{vegdist} on the \code{D} using this string as 
#'  the distance.
#' @return A list ordination objects generated by calling 
#' \code{mvarVis::ordi} on each of the boostrap samples, and then
#' rotating the coordinates using procustes method to fit them to the 
#' coordinates of ordination of the original data table.
#' @export
#' @examples
#' D <-  matrix(runif(100, max = 100), nrow = 25)
#' bootOrd <- boot_ordination(D, n = 50, method = "ade4_pca", 
#'                            dist_method = "euclidean", scannf = F, nf = 2)
#'                            
boot_ordination <- function(D, n = 50, method = "ade4_pca", dist_method = "euclidean", 
                            rows_annot = NULL, cols_annot = NULL, table_names = NULL,
                            common_depth = FALSE, replace_zero = FALSE, round = FALSE, ...) {
  if(is.null(table_names) & method == "pco") {
    table_names <- c("li")
  }
  boot_data <- boot_table(D, n, common_depth, replace_zero, round)
  if (class(dist_method) == "function") {
    origDist <- dist_method(D)
    orig_ord <- ordi(origDist, method =  method, rows_annot = rows_annot, 
                     cols_annot = cols_annot, table_names = table_names, ...)
    boot_ord <- lapply(1:dim(boot_data)[1], function(i) {
      ibootDist <- dist_method(boot_data[i, , ])
      ordi(ibootDist, method = method, rows_annot = rows_annot, 
           cols_annot = cols_annot, table_names = table_names, ...)
    })
  } else {
    orig_ord <- ordi(D, method = method, dist_method = dist_method, 
                     rows_annot = rows_annot, cols_annot = cols_annot, 
                     table_names = table_names,...)
    boot_ord <- lapply(1:dim(boot_data)[1], function(i) {
      ordi(boot_data[i, , ], method = method, dist_method = dist_method, 
           rows_annot = rows_annot, cols_annot = cols_annot, 
           table_names = table_names, ...)
    })
  }
  for (bootIDX in 1:length(boot_ord)) {
    mvar <- boot_ord[[bootIDX]]
    for (tabIDX in 1:length(mvar@table)){
      centerCoord <- orig_ord@table[[tabIDX]]@coord
      ibootCoord <- mvar@table[[tabIDX]]@coord
      # Procustes rotation to fit the original ordination configuration
      ibootCoord <- vegan::procrustes(centerCoord, ibootCoord)$Yrot
      colnames(ibootCoord) <- colnames(centerCoord)
      rownames(ibootCoord) <- rownames(centerCoord)
      boot_ord[[bootIDX]]@table[[tabIDX]]@coord <- ibootCoord
    }
  }
  mvar_boot_table <- new("mvarBootTable", 
                         center = orig_ord,
                         boot = boot_ord)
  return(mvar_boot_table)
}
