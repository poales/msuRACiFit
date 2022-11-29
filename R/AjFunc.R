#'AjFunc
#'
#'Generates the J-limited data
#'@param Cc CO2 concentration at the site of carboxylation
#'@param aG The proportion of glycerate carbon that exits photorespiration as glycine
#'@param aS The proportion of glycerate carbon that exits photorespiration as serine
#'@param Rd Day respiration
#'@param j Maximum rate of electron transport given current conditions
#'@param gammastar Cc compensation point
#'@param coef passed from CoefFunc
#'@name AjFunc

AjFunc <- function(Cc, aG, aS, Rd, j, gammastar, coef){
  coef * j/(4+(4+8*aG+4*aS)*2*((1-aG)*gammastar)/Cc)-Rd
}