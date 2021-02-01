#'AjFunc
#'
#'Generates the J-limited data
#'@param Cc
#'@param aG
#'@param aS
#'@param Rd
#'@param Vcmax
#'@param j
#'@param TPU
#'@param gm
#'@name AjFunc

AjFunc <- function(Cc, aG, aS, Rd, Vcmax, j, TPU, gm){
  coef = CoefFunc( aG , gammastar, Cc )
  coef * j/(4+(4+8*aG+4*aS)*2*((1-aG)*gammastar)/Cc)-Rd # fit a
}