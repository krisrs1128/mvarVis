#' Sample from Dirichlet Distribution
#' 
#' Generate a sample from a dirichlet distribution with parameter \code{alpha}
#' 
#' @param alpha (Required). A vector of parameters for Dirichlet variable
#' @return A vector of a random sample from Dirichlet distribution with
#' parameter \code{alpha}
#' @examples
#' rdirichlet(sample(1:10, 5))
rdirichlet <- function(alpha) {
  y <- rgamma(length(alpha), alpha, 1)
  return(y / sum(y))
}
################################################################################
################################################################################
#' Boostrap a count vector
#'
#' \code{boot_count_vector} returns \code{n} boostrap vectors of counts.
#' This is basically a multinomial sample with \code{sum(x)} trials
#' with weights equal \code{x}.
#'
#' @param x (Required). A vector of counts.
#' @param n (Optional). Default 1. An integer indicating the number of
#'  boostrap count vectors to return.
#' @param replace_zero (Optional). A logical or numeric scalar 
#' (FALSE by default). If TRUE or numeric zeros in \code{x} will be replaced 
#' by 1 or \code{replace_zero} respectively, i.e. we add a small positive weight
#' in place of zeros.
#' @return A vector or a matrix of \code{n} vectors boostrapped from \code{x}.
#' @examples
#' boot_count_vector(sample(1:1000, 5))
#'
#' x <- sample(1:1000, 10)
#' x[sample(1:length(x), 4)] <- 0
#' boot_count_vector(x, replace_zero = TRUE)
#' boot_count_vector(x, replace_zero = 0.5)
boot_count_vector <- function(x, n = 1, depth = FALSE, replace_zero = FALSE){
  if (any(x < 0))
    stop("No negative counts allowed")
  bootVecs <- array(0, dim=c(length(x), n))
  if (sum(x) == 0)
    return(bootVec) # if empty vector return immediately
  if (replace_zero) {
    replace_zero <- ifelse(replace_zero == TRUE, 1, replace_zero)
    x[which(x == 0)] <- replace_zero
  }
  bootSample <- replicate(n, sample.int(length(x), sum(x), replace = TRUE,
                                        prob=x))
  for(i in 1:n) {
    bootTab <- table(bootSample[, i])
    if (is.numeric(depth))
      bootTab <- depth * bootTab / sum(bootTab)
    # Assign the tabulated random boostrap sample values to the species vector
    bootVecs[as(names(bootTab), "integer"), i] <- bootTab
  }
  # Return abundance vector.
  return(bootVecs)
}
################################################################################
################################################################################
#' Boostrap a vector
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
#' @param replace_zero (Optional). A logical or numeric scalar 
#' (FALSE by default). If TRUE or numeric zeros in \code{x} will be replaced 
#' by 1 or \code{replace_zero} respectively, i.e. we add a small positive weight
#' in place of zeros.
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
boot_vector <- function(x, n = 1, depth = FALSE, replace_zero = FALSE){
  if (any(x < 0))
    stop("No negative counts allowed")
  if (all(x %% 1 == 0)) return(boot_count_vector(x, n, depth, replace_zero))
  
  if (replace_zero) {
    replace_zero <- ifelse(replace_zero == TRUE, 1, replace_zero)
    x[which(x == 0)] <- replace_zero
  }
  if (!is.numeric(depth)) depth <- sum(x)
  return(replicate(n, rdirichlet(x))*depth)
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
    mat <- boot_vector(tab[, i], n, common_depth, replace_zero[i])
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
#' @importFrom vegan vegdist cca decorana metaMDS isomap rda CCorA
#' @importFrom ade4 dudi.pca dudi.acm dudi.coa dudi.fca dudi.fpca dudi.hillsmith
#'    dudi.mix dudi.nsc dudi.pco dpcoa procuste cca
#' @importFrom FactoMineR PCA CA DMFA FAMD HMFA MCA spMCA
#' @export
#' 
#' @examples
#' D <-  matrix(runif(100, max = 100), nrow = 25)
#' bootOrd <- boot_ordination(D, n = 50, method = "ade4_pca", 
#'                            dist_method = "euclidean")
#'                            
boot_ordination <- function(D, n = 50, method = "ade4_pca", 
                            dist_method = "euclidean", common_depth = FALSE, 
                            replace_zero = FALSE, round = FALSE, ...) {
  boot_data <- boot_table(D, n, common_depth, replace_zero, round)
  if (class(dist_method) == "function") {
    origDist <- dist_method(D)
    orig_ord <- ordi(origDist, method, ...)
    boot_ord <- lapply(1:dim(boot_data)[1], function(i) {
      ibootDist <- dist_method(boot_data[i, , ])
      ordi(ibootDist, method = method, ...)
    })
  } else {
    orig_ord <- ordi(D, method, dist_method, ...)
    boot_ord <- lapply(1:dim(boot_data)[1], function(i) {
      ordi(boot_data[i, , ], method = method, dist_method = dist_method, ...)
    })
  }
  for (bootIDX in 1:length(boot_ord)) {
    mvar <- boot_ord[[bootIDX]]
    for (tabIDX in 1:length(mvar@table)){
      centerCoord <- orig_ord@table[[tabIDX]]@coord
      ibootCoord <- mvar@table[[tabIDX]]@coord
      # Procustes rotation to fit the original ordination configuration
      ibootCoord <- vegan::procrustes(centerCoord, ibootCoord)$Yrot
      colnames(ibootCoord) <- colnames(origCoord)
      rownames(ibootCoord) <- rownames(origCoord)
      boot_ord[[bootIDX]]@table[[tabIDX]]@coord <- ibootCoord
    }
  }
  boot_tables <- lapply(boot_ord, function(x) x@table)
  boot_eigs <- lapply(boot_ord, function(x) x@eig)
  mvar_boot_table <- new("mvarBootTable", 
                         center = orig_ord@table, 
                         centerEig = orig_ord@eig,
                         bootTables = boot_tables,
                         bootEigs = boot_eigs)
  return(mvar_boot_table)
}