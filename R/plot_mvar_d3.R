#' Interactive multivariate analysis plots
#' @examples
#' library("FactoMineR")
#' data(wine)
#' wine_mfa <- ordi(wine[, -c(1:2)], "MFA", wine[, 1:2], group = c(5,3,10,9,2),
#'                  type = rep("s",5), graph = F)
#' plot_mvar_d3(wine_mfa)
#' plot_mvar_d3(wine_mfa, c("text", "arrow"), height = 500)
#' data(hobbies)
#' hobbies_mca <- ordi(hobbies[, 1:18], method = "MCA",
#'                     rows_annot = hobbies[, 19:23], graph = F)
#' plot_mvar_d3(hobbies_mca, height = 500)
#' @importFrom htmlwidgets createWidget
#' @export
plot_mvar_d3 <- function(mvar_object, types = NULL, width = NULL, asp = NULL,
                         height = 200) {
  if(is.null(asp)) {
    asp <- ifelse(length(mvar_object@eig) > 2,
                  mvar_object@eig[1] / mvar_object@eig[2], 1)
  }
  if(is.null(width)) {
    width <- asp * height * length(mvar_object@table)
  }
  if(is.null(types)) {
    types <- rep("point", length(mvar_object@table))
    if(length(types) >= 2) {
      types[2] <- "text"
    }
  }

  x <- list()
  for(table_ix in seq_along(mvar_object@table)) {
    cur_coord <- mvar_object@table[[table_ix]]@coord
    colnames(cur_coord) <- paste0("axis", seq_len(ncol(cur_coord)))
    cur_coord <- cbind(cur_coord, running_cosines(cur_coord))
    cur_ann <- mvar_object@table[[table_ix]]@annotation
    x[[table_ix]] <- list(data = data.frame(cur_coord, cur_ann),
                          type = types[table_ix])
  }

  createWidget(name = "plot_mvar_d3", x = x, width = width, height = height,
               package = "mvarVis")

}

#' @importFrom htmlwidgets createWidget
#' @export
plot_layer <- function(mvar_layer, width = 400, height = 400) {
  x <- data.frame(mvar_layer@coord, mvar_layer@annotation)
  createWidget(name = "plot_layer", x = x, width = width, height = height,
               package = "mvarVis")
}
