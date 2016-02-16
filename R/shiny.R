
################################################################################
#
################################################################################

#' @export
mvarVisOutput <- function(outputId, width = "300px", height = "500px") {
  shinyWidgetOutput(outputId, "plot_mvar_d3", width, height, package = "mvarVis")
}

#' @export
renderMvarVis <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, mvarVisOutput, env, quoted = TRUE)
}
