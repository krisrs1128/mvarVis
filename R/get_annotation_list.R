#' @title Get Annotation to combine with mvarVis Object
#'
#' @param X_mvar An object of class mvar, whose annotation elements we want to
#' fill out.
#' @param rows_annot For one table methods, a data.frame whose j^th row
#' describes the j^th sample in the coordinates matrices corresponding to
#' row scores. For procustes and CCorA, we require a list of dataframes, one
#' for each of the directions.
#' @param cols_annot The analogous object for column scores.
#'
#' @return A list copying the different annotation data frames so that if the
#' i^th table in \code{X_mvar} corresponds to row scores, the i^th element in
#' the list gives the rows annotation, and analogously for column annotation.
#'
#' @export
get_annotation_list <- function(X_mvar, rows_annot = NULL, cols_annot = NULL) {
  annotation_list <- list()
  for(cur_table in names(X_mvar@table)) {
    if(cur_table %in% c("li", "l1", "site", "ind", "row") & !is.null(rows_annot)) {
      annotation_list[[cur_table]] <- rows_annot
    } else if(cur_table %in% c("co", "l2", "species", "var", "col") & !is.null(cols_annot)) {
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
