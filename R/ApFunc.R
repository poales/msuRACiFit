#'ApFunc
#'
#'Generates the P-limited data
#'@param Cc
#'@param aG
#'@param aS
#'@param Rd
#'@param Vcmax
#'@param j
#'@param TPU
#'@param gm
#'@name ApFunc

ApFunc <- function(Cc, aG, aS, Rd, Vcmax, j, TPU, gm){
  coef = CoefFunc( aG , gammastar, Cc )
  result = coef * (3*TPU/(1-0.5*(1+3*aG+4*aS)*2*(1-aG)*gammastar/Cc))-Rd
  # insert infinite y-values where the x-value (Cc) is low
  result[Cc < 20] = 100000
  return(result)
}