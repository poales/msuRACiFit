#'ApFunc
#'
#'Generates the P-limited data
#'@param Cc CO2 concentration at the site of carboxylation
#'@param aG The proportion of glycerate carbon that exits photorespiration as glycine
#'@param aS The proportion of glycerate carbon that exits photorespiration as serine
#'@param Rd Day respiration
#'@param Vcmax Michaelis-Menten VMax for carboxylation
#'@param j Maximum rate of electron transport given current conditions
#'@param TPU Rate of triose phosphate usage
#'@param gm Mesophyll conductance to carbon
#'@param gammastar Cc compensation point
#'@name ApFunc

ApFunc <- function(Cc, aG, aS, Rd, Vcmax, j, TPU, gm, gammastar){
  coef = CoefFunc( aG , gammastar, Cc )
  result = coef * (3*TPU/(1-0.5*(1+3*aG+4*aS)*2*(1-aG)*gammastar/Cc))-Rd
  # insert infinite y-values where the x-value (Cc) is low
  result[Cc < 10] = 1000
  return(result)
}