#' @title Scale one table to the median radius of another
#'
#' @description When making a biplot, the scales of the two sets of points
#'    aren't always comparable -- only the *directions* of the projections of
#'    feature variables are meaningful. This helper rescales the points in the
#'    matrix \code{x2} to have radius comparable to the points in x1.
#'    Specifically, we rescale \code{x2} so it's median radius is equal to the
#'    median radius of the \code{x1} points.
#' @param x1 The reference matrix used to rescale \code{x2}.
#' @param x2 The matrix to rescale.
#' @return x2_scaled The matrix \code{x2} rescaled to have the same median
#'    radius as \code{x1}.
scale_to_median_radius <- function(x1, x2) {
  x1_radius <- sqrt(rowSums(x1 ^ 2))
  x2_radius <-sqrt(rowSums(x2 ^ 2))
  x1_med_radius <- median(x1_radius)
  x2_scaled <- x2 * x1_med_radius / median(x2_radius)
  return (x2_scaled)
}

#' @title Scale one table to the max radius of another
#'
#' @param x1 The reference matrix used to rescale \code{x2}.
#' @param x2 The matrix to rescale.
#' @param q We rescale to this fraction of the maximum radius in x1.
#'
#' @return x2_scaled The rescaled matrix \code{x2}.
scale_to_max_radius <- function(x1, x2, q = 0.9) {
  x1_radius <- sqrt(rowSums(x1 ^ 2))
  x2_radius <-sqrt(rowSums(x2 ^ 2))
  x1_max_radius <- max(x1_radius)
  x2_scaled <- q * x2 * x1_max_radius / max(x2_radius)
  return (x2_scaled)
}

#' @title Add annotation to an \code{mvarTable}
#'
#' @description Given an \code{mvarTable}, we may want to enrich the annotation
#'    using separate data frames. This function takes a list of data frames of
#'    the same length as the number of tables in the \code{mvarTable}, and
#'    \code{cbind}'s these data frames with the existing annotation slots.
#'
#' @param mvar_object The \code{mvarTable} whose annotation slots we want to
#'    expand.
#' @param annotations_list A list of data frames that will be \code{cbind}ed
#'    into the annotation slots in the \code{mvarTable}.
#'
#' @return mvar_object The original mvar_object, with annotation extended using
#'    the annotations_list data frames.
#'
#' @export
mvar_annotate <- function(mvar_object, annotations_list) {
  stopifnot(length(annotations_list) == length(mvar_object@table))
  for(ann_ix in 1:length(annotations_list)) {
    mvar_object@table[[ann_ix]]@annotation <- cbind(
      mvar_object@table[[ann_ix]]@annotation,
      annotations_list[[ann_ix]]
    )
  }
  return (mvar_object)
}

#' @title Get squared cosines from a set of scores
#' @param coord The coordinates from which to compute contributions. We assume
#' coordinates for all dimensions are present.
#' @return The squared cosines between each observation and each component.
#' @references Abdi and Williams "Principle Component Analysis", equation (11).
#' @export
cosines <- function(coord) {
  res <- coord ^ 2 / rowSums(coord ^ 2)
  colnames(res) <- paste0("cosine_", colnames(res))
  res
}

#' @title Get running sum of squared cosines from a set of scores
#' @param coord The coordinates from which to compute contributions. We assume
#' coordinates for all dimensions are present.
#' @return The sum of squared cosines up to each dimension, for each
#' observation.
#' @references Abdi and Williams "Principle Component Analysis", equation (11).
#' @examples
#' data(wine)
#' wine_mfa <- ordi(wine[, -c(1:2)], "MFA", wine[, 1:2], group = c(5,3,10,9,2), type = rep("s",5), graph = F)
#' coord <- slot(slot(wine_mfa, "table")$ind, "coord")
#' running_cosines(coord)
#' @export
running_cosines <- function(coord) {
  res <- t(apply(cosines(coord), 1, cumsum))
  colnames(res) <- paste0("total_", colnames(res))
  res
}
