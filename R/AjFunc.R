#'AjFunc
#'
#'Generates the J-limited data
#'@param Cc CO2 concentration at the site of carboxylation
#'@param aG The proportion of glycerate carbon that exits photorespiration as glycine
#'@param aS The proportion of glycerate carbon that exits photorespiration as serine
#'@param Rd Day respiration
#'@param Vcmax Michaelis-Menten VMax for carboxylation
#'@param j Maximum rate of electron transport given current conditions
#'@param TPU Rate of triose phosphate usage
#'@param gm Mesophyll conductance to carbon
#'@name AjFunc

AjFunc <- function(Cc, aG, aS, Rd, Vcmax, j, TPU, gm){
  coef = CoefFunc( aG , gammastar, Cc )
  coef * j/(4+(4+8*aG+4*aS)*2*((1-aG)*gammastar)/Cc)-Rd # fit a
}