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

#' Wrapper for boostrap a count or Dirichlet vectors
#'
#' \code{boot_vector} returns \code{n} samples of boostraped vector of
#' raw counts or proportions. If raw counts (integers) are supplied in \code{x},
#' a multinomial sample is drawn with \code{sum(x)} trials with weights
#' equal \code{x}. If \code{x} does not contain integers, a Dirichlet sample
#' with parameter \code{x}. If \code{depth} is supplied, the samples
#' are scaled to have sum equal to \code{depth}.
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
#' boot_vector(sample(1:1000, 5))
#'
#' x <- sample(1:1000, 10)
#' x[sample(1:length(x), 4)] <- 0
#' boot_vector(x, n = 3, replace_zero = TRUE)
#'
#' y <- runif(10)
#' y <- y/sum(y)
#' boot_vector(y, n = 4, replace_zero = 0.05)
boot_vector <-  function(x, n = 1, depth = NULL, replace_zeros = FALSE,
                         replace_value = 1) {
  if(all(x %% 1 == 0)) {
    res <- boot_count_vector(x, n, depth, replace_zeros, replace_value)
  } else {
    res <- boot_prop_vector(x, n, depth, replace_zeros, replace_value)
  }
  res
}

#' Bootstrap table.
#'
#' \code{boot_table} returns samples boostrapped from table \code{tab}
#' supplied. Each column of the returned table is a boostrap trial of
#' the corresponding column of the \code{tab} input table.
#' The columns of the output table are multinomial samples with weights
#' proportional to corresponding columns of \code{tab}.
#'
#' @param tab (Required). A matrix or data.frame of numeric counts/weights.
#' @param n (Optional). Default 1. An integer indicating the number of
#'  boostrap count tables to return.
#' @param common_depth (Optional). Default \code{FALSE}. The value to which to
#' normalize the boostrapped column sums. If not provided, do not normalize to
#' a common depth.
#' @param common_value (Optional) The depth to use when common_depth is TRUE.
#' Defaults to the median of the column sums in tab.
#' @param replace_zero (Optional) A logical specifying whether to replace
#' zeros in x.
#' @param replace_value (Optional) The value to replace zeros with, when
#' replace_zero is TRUE; i.e. we add a small positive weight. Default value is
#' 1.
#' @param round_values (Optional). Default FALSE. A logical scalar. Should the
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
boot_table <- function(tab, n = 1, common_depth = FALSE, common_value = NULL,
                       replace_zero = FALSE, replace_value = 1,
                       round_values = FALSE) {
  if(common_depth & is.null(common_value)) {
    common_value <- median(colSums(tab))
  }
  if(!common_depth) {
    common_value <- NULL
  }

  # initialize result
  res_names <- list(paste("trial", 1:n, sep = "_"), rownames(tab), colnames(tab))
  boot_mat <- array(0, dim=c(n, nrow(tab), ncol(tab)), dimnames = res_names)

  # bootstrap each column
  for(j in 1:ncol(tab)){
    boot_mat[,, j] <- t(boot_vector(tab[, j], n, common_value, replace_zero,
                                   replace_value))
  }
  
  if(round_values) {
    boot_mat <- round(boot_mat)
  }

  boot_mat
}

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
#' @param method (Required). The method that will perform the required
#' ordination. See ordi()for options.
#' @param dist_method (Optional). If a distance matrix is used by the specified
#'  method. We will call \code{vegdist} on the \code{D} using this string as
#'  the distance. Defaults to euclidean distance.
#' @param common_depth (Optional). Default \code{FALSE}. The value to which to
#' normalize the boostrapped column sums. If not provided, do not normalize to
#' a common depth.
#' @param common_value (Optional) The depth to use when common_depth is TRUE.
#' Defaults to the median of the column sums in tab.
#' @param replace_zero (Optional) A logical specifying whether to replace
#' zeros in x.
#' @param replace_value (Optional) The value to replace zeros with, when
#' replace_zero is TRUE; i.e. we add a small positive weight. Default value is
#' 1.
#' @param round_values (Optional). Default FALSE. A logical scalar. Should the
#'  boostrap counts be rounded to the nearest integer?
#' @importFrom plyr alply
#' @importFrom vegan procrustes
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
boot_ordination <- function(D, n = 50, method = "pco", taxa_are_rows = TRUE,
                            dist_method = "euclidean", rows_annot = NULL,
                            cols_annot = NULL, table_names = NULL,
                            common_depth = FALSE, common_value = NULL,
                            replace_zero = FALSE, replace_value = 1,
                            round_values = FALSE, ...) {
  
  # transpose D to make boot_table compatible
  if (!taxa_are_rows) D <- t(D)
  
  # generate bootstrap samples
  boot_data <- boot_table(D, n, common_depth, common_value, replace_zero,
                          replace_value, round_values)
  
  # if common_depth then normalize the original counts 
  if (common_depth) {
    D <- floor(D)
    if (is.null(common_value)) common_value <- median(colSums(D))
    D <- common_value * apply(D, 2, function(x) x/sum(x)) 
  }
  
  if (round_values) D <- round(D)
  
  # need to convert to distances matrices if dist_method is not a string
  if(class(dist_method) == "function") {
    D <- dist_method(D)
    boot_data <- alply(boot_data, 1, dist_method)
  } else {
    boot_data <- alply(boot_data, 1) # convert from array to list
  }
  
  # wrapper for ordi() using supplied options
  ordi_ <- function(x) {
    if (class(x) != "dist") {
      row_idx <- (rowSums(x) > 0)
      col_idx <- (colSums(x) > 0)
      x <- x[row_idx, ]; x <- x[, col_idx]
      if (!is.null(rows_annot)) rows_annot <- rows_annot[row_idx, ]
      if (!is.null(cols_annot)) cols_annot <- cols_annot[col_idx, ]
      if (method %in%  c("pco", "isomap", "dpcoa")) x <- t(x)
    }
    ordi(x, method = method, dist_method = dist_method,
         rows_annot = rows_annot, cols_annot = cols_annot,
         table_names = table_names, ...)
  }

  orig_ord <- ordi_(D)
  boot_ord <- lapply(boot_data, ordi_)

  for(boot_ix in seq_along(boot_ord)) {
    mvar <- boot_ord[[boot_ix]]
    for(tab_ix in seq_along(mvar@table)) {
      center_coord <- orig_ord@table[[tab_ix]]@coord
      boot_coord <- mvar@table[[tab_ix]]@coord

      # Procustes rotation to fit the original ordination configuration
      boot_coord <- procrustes(center_coord, 
                               boot_coord, scale = FALSE)$Yrot
      dimnames(boot_coord) <- dimnames(center_coord)
      boot_ord[[boot_ix]]@table[[tab_ix]]@coord <- boot_coord
    }
  }

  new("mvarBootTable", center = orig_ord, boot = boot_ord)
}


# boot_procrustes <- function(D, boot_dist, method, dist_method,
#                             rows_annot, cols_annot,
#                             table_names) {
#   
#   # wrapper for ordi() using supplied options
#   ordi_ <- function(x) {
#     ordi(x, method = method, dist_method = dist_method,
#          rows_annot = rows_annot, cols_annot = cols_annot,
#          table_names = table_names, ...)
#   }
#   
#   orig_ord <- ordi_(D)
#   boot_ord <- lapply(boot_dist, ordi_)
#   
#   for(boot_ix in seq_along(boot_ord)) {
#     mvar <- boot_ord[[boot_ix]]
#     for(tab_ix in seq_along(mvar@table)) {
#       center_coord <- orig_ord@table[[tab_ix]]@coord
#       boot_coord <- mvar@table[[tab_ix]]@coord
#       
#       # Procustes rotation to fit the original ordination configuration
#       boot_coord <- procrustes(center_coord, 
#                                boot_coord, scale = FALSE)$Yrot
#       dimnames(boot_coord) <- dimnames(center_coord)
#       boot_ord[[boot_ix]]@table[[tab_ix]]@coord <- boot_coord
#     }
#   }
#   return(new("mvarBootTable", center = orig_ord, boot = boot_ord))
# }
# 
# 
# boot_distatis <- function(boot_dist, rows_anno, cols_annot) {
#   
#   if(!requireNamespace("DistatisR", quietly = TRUE)){
#     stop("DistatisR package needed for this function to work.
#          Please install it.", call. = FALSE)
#   }
#   
#   boot_dist <- lapply(boot_dist, as.matrix)
#   distCube  <- array(unlist(boot_dist), 
#                     dim = c(nrow(boot_dist[[1]]), ncol(boot_dist[[1]]), 
#                             length(boot_dist)))
#   distatisOrdination <- DistatisR::distatis(distCube)
#   
#   # Coordinates of the compromise
#   compromise <- data.frame(distatisOrdination$res4Splus$F)
#   colnames(compromise) <- paste("Axis.", 1:ncol(compromise), sep = "")
#   compromise$sampleID <- rownames(sample_data(physeq))
#   compromise <- cbind(compromise, sample_data(physeq))
#   rownames(compromise) <- rownames(sample_data(physeq))
#   
#   bootDistatis <- list()
#   bootDistatis$compromise <- compromise
#   bootDistatis$coords <- lapply(1:length(distList), function(x)
#     distatisOrdination$res4Splus$PartialF[, , x])
#   bootDistatis$values <- eigen(distatisOrdination$res4Splus$Splus)$values
#   return(new("mvarBootTable", center = orig_ord, boot = boot_ord))
# }