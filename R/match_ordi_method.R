#' @title Search Vegan and ade4 for matching method
#'
#' @description Given a character string describing an ordination method
#'  from ade4 or vegan, return the actual function for that method.
#'
#' @param A string describing the function to return. The currently accepted
#'  strings, along with the functions they return, are
#'    pca: dudi.pca
#'    pco: dudi.pco
#'    acm: dudi.acm
#'    coa: dudi.coa
#'    fpca: dudi.fpca
#'    hillsmith: dudi.hillsmith
#'    mix: dudi.mix
#'    nsc: dudi.nsc
#'    vegan_cca: cca, from vegan
#'    ade4_cca: cca, from ade4,
#'  and any other string that exactly matches a function in ade4 or vegan.
#'
#' @return ordi_method The function from either ade4 or vegan giving the
#'  desired function.
#'
#' @importFrom ade4 dudi.pca dudi.pco dudi.acm dudi.coa dudi.fpca dudi.hillsmith
#'    dudi.mix dudi.nsc dpcoa cca
#' @importFrom vegan cca
match_ordi_method <- function(method) {
  # determine the function to be called
  paste_dudi <- c("pca", "pco", "acm", "coa", "fpca","hillsmith", "mix", "nsc")
  if(method %in% paste_dudi) {
    # Methods that need to be called as dudi."name"
    ordi_method <- eval(parse(text=paste0("dudi.", method)))
  } else if(method %in% "vegan_cca") {
    ordi_method <- eval(parse(text = "vegan::cca"))
  } else if(method %in% "ade4_cca") {
    ordi_method <- eval(parse(text = "ade4::cca"))
  } else {
    # Methods that can be called directly
    ordi_method <- eval(parse(text=method))
  }
  return (ordi_method)
}
