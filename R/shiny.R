
################################################################################
#
################################################################################

#' @export
mvarVisOutput <- function(outputId, width = "100%", height = "500px") {
  print(width)
  res <- shinyWidgetOutput(outputId, "plot_mvar_d3", width, height, package = "mvarVis")
  print(res)
  res
}

#' @export
renderMvarVis <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, mvarVisOutput, env, quoted = TRUE)
}
