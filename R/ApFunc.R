#'ApFunc
#'
#'Generates the P-limited data
#'@param Cc CO2 concentration at the site of carboxylation
#'@param aG The proportion of glycerate carbon that exits photorespiration as glycine
#'@param aS The proportion of glycerate carbon that exits photorespiration as serine
#'@param Rd Day respiration
#'@param TPU Rate of triose phosphate usage
#'@param gammastar Cc compensation point
#'@param coef passed from CoefFunc
#'@name ApFunc

ApFunc <- function(Cc, aG, aS, Rd, TPU, gammastar, coef){
  result = coef * (3*TPU/(1-0.5*(1+3*aG+4*aS)*2*(1-aG)*gammastar/Cc))-Rd
  # insert infinite y-values where the x-value (Cc) is low
  result[Cc < 20] = 1000
  result[result<0] = 1000
  return(result)
}