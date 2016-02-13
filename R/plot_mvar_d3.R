
#' @title Merge default opts for mvar htmlwidgets plots
#' @param opts A potentially partially specified list, to fill in with defaults.
#' Currently supported options are, \cr
#'   $height How tall should each individual table plot be? \cr
#'   $width How wide should each individual table plot be? \cr
#'   $asp If one of height or width is ommitted, what is the aspect ratio to
#'   use to fill that in? \cr
#'   $types What types of figure should be shown, for each table? Defaults to
#'   points for all tables.
#' @return A version of opts with defaults filled in.
#' @export
merge_mvar_d3_defaults <- function(opts = list(), mvar_object) {
  default_opts <- list()

  # aspect ratio (height / width) defaults to ratio of top two eigenvalues
  if(is.null(opts$asp) & length(mvar_object@eig) >= 2) {
    default_opts$asp <- mvar_object@eig[2] / mvar_object@eig[1]
  } else {
    default_opts$asp <- 1
  }

  ref_asp <- ifelse(is.null(opts$asp), default_opts$asp, opts$asp)
  default_opts$width <- ifelse(is.null(opts$width), 500, opts$width)
  default_opts$height <- ifelse(is.null(opts$height),
                                ref_asp * default_opts$width,
                                opts$height)

  # plot points for all the tables
  default_opts$types <- rep("point", length(mvar_object@table))
  opts <- opts[!sapply(opts, is.null)]
  modifyList(default_opts, opts)
}

#' @title Interactive multivariate analysis plots
#' @examples
#' library("FactoMineR")
#' data(wine)
#' wine_mfa <- ordi(wine[, -c(1:2)], "MFA", wine[, 1:2], group = c(5,3,10,9,2),
#'                  type = rep("s",5), graph = F)
#' plot_mvar_d3(wine_mfa)
#' plot_mvar_d3(wine_mfa, c("text", "arrow"), width = 500)
#' data(hobbies)
#' hobbies_mca <- ordi(hobbies[, 1:18], method = "MCA",
#'                     rows_annot = hobbies[, 19:23], graph = F)
#' plot_mvar_d3(hobbies_mca, width = 500, asp = 1)
#' @importFrom htmlwidgets createWidget
#' @importFrom magrittr %>%
#' @export
plot_mvar_d3 <- function(mvar_object, types = NULL, height = NULL, asp = NULL,
                         width = NULL) {
  opts <- list(types = types, width = width, asp = asp, height = height) %>%
    merge_mvar_d3_defaults(mvar_object)
  print(opts)

  x <- list()
  for(table_ix in seq_along(mvar_object@table)) {
    cur_coord <- mvar_object@table[[table_ix]]@coord
    colnames(cur_coord) <- paste0("axis", seq_len(ncol(cur_coord)))
    cur_coord <- cbind(cur_coord, running_cosines(cur_coord))
    cur_ann <- mvar_object@table[[table_ix]]@annotation
    x[[table_ix]] <- list(data = data.frame(cur_coord, cur_ann),
                          type = opts$types[table_ix])
  }

  createWidget(name = "plot_mvar_d3", x = x, width = opts$width,
               height = opts$height, package = "mvarVis")

}

#' @importFrom htmlwidgets createWidget
#' @export
plot_layer <- function(mvar_layer, width = 400, height = 400) {
  x <- data.frame(mvar_layer@coord, mvar_layer@annotation)
  createWidget(name = "plot_layer", x = x, width = width, height = height,
               package = "mvarVis")
}
