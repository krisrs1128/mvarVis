
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

  # how much of each panel should be input selections?
  default_opts$prop_input <- 3/12

  # default aspect ratio is NOT the ratio of eigenvalues
  k_tables <- length(mvar_object@table)
  default_opts$width <- ifelse(is.null(opts$width), 750, opts$width)
  default_opts$height <- ifelse(is.null(opts$height),
                                .5 * k_tables * default_opts$width, opts$height)

  # plot points for all the tables
  if(is.null(opts$types)) {
    default_opts$types <- rep(list(list("point")), k_tables)
  } else {
    default_opts$types <- opts$types
  }

  # define range of point sizes
  default_opts$rMin <- 1
  default_opts$rMax <- 20

  # default color palettes
  default_opts$ordinal_palettes <- replicate(k_tables,
                                             c("#66c2a5","#fc8d62","#8da0cb","#e78ac3","#a6d854","#ffd92f","#e5c494","#b3b3b3"),
                                             simplify = F)
  default_opts$continuous_palettes <- replicate(k_tables,
                                                c('#000080','#2c3a8d','#37699a','#3199a5','#54bdb4','#95d1c7','#cbe6dc','#fffaf0','#f5cdc3','#e8a098','#d7736f','#c2514f','#ac3a35','#96221c','#800000'),
                                                simplify = F)

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
plot_mvar_d3 <- function(mvar_object, types = NULL, height = NULL,
                         width = NULL, rMin = NULL, rMax = NULL,
                         ordinal_palettes = NULL, continuous_palettes = NULL,
                         prop_input = NULL) {
  opts <- list(types = types, width = width, height = height,
               prop_input = prop_input, rMin = rMin, rMax = rMax,
               ordinal_palettes = ordinal_palettes,
               continuous_palettes = continuous_palettes) %>%
                 merge_mvar_d3_defaults(mvar_object)

  x <- list()
  for(table_ix in seq_along(mvar_object@table)) {
    cur_coord <- mvar_object@table[[table_ix]]@coord
    colnames(cur_coord) <- paste0("axis", seq_len(ncol(cur_coord)))
    cur_coord <- cbind(cur_coord, running_cosines(cur_coord))
    cur_ann <- mvar_object@table[[table_ix]]@annotation
    table_opts <- list(type = opts$types[[table_ix]], # always want an array in js
                       continuous_palette = opts$continuous_palettes[table_ix],
                       ordinal_palette = opts$ordinal_palettes[table_ix],
                       width = opts$width, height = opts$height,
                       prop_input = opts$prop_input,
                       rMin = opts$rMin, rMax = opts$rMax)
    x[[table_ix]] <- list(data = data.frame(cur_coord, cur_ann),
                          opts = table_opts)
  }
  k_tables <- length(mvar_object@table)
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
