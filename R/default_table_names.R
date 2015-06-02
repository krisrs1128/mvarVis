#' @title Get the tables to put into an mvarTable object
#'
#' @description For ade4, we need to specify the row and columns scores to
#' output, depending on the method.
#'
#' @param method The name of the ordination method from which we want to
#'  extract scores.
#' @return If the method is implemented in ade4, the names of the tables that
#'  give the row and column scores.
default_table_names <- function(method) {
  table_names <- switch(method,
                        "pca" = c("li", "co"),
                        "acm" = c("li", "co"),
                        "coa" = c("li", "co"),
                        "fca" = c("li", "co"),
                        "fpca" = c("li", "co"),
                        "pco" = c("li", "co"),
                        "hillsmith" = c("li", "co"),
                        "mix" = c("li", "co"),
                        "nsc" = c("li", "co"),
                        "dpcoa" = c("l1", "l2"),
                        "decorana" = c(), # right now we're using scores() from vegan, instead of extracting any tables
                        "metaMDS"  = c(),
                        "isomap" = c(),
                        "isoMDS" = c(),
                        "vegan_cca" = c(),
                        "ade4_cca" = c("li", "co"),
                        "rda" = c(),
                        "CCorA" = c(),
                        "procuste" = c("scor1", "scor2"),
                        "coinertia" = c("li", "co"))
  return (table_names)
}
