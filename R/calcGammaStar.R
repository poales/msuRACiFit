#' Provide the details on your plants rubisco kinetics to calculate gamma star.
#'
#' @param c Rubisco constant
#' @param dHa Rubisco constant
#' @param tleaf Leaf temperature in celsius. Default 25
#' @param oxygen Oxygen percent. Default 21.
#' @name calcGammaStar
#' @export


calcGammaStar <- function(c,dHa,tleaf=25,oxygen=21){
  return(exp(c - (dHa/(0.008314*(273.15+tleaf))))*oxygen/21)
}
