#'AcFunc
#'
#'Generates the C-limited data
#'@param Cc
#'@param aG
#'@param aS
#'@param Rd
#'@param Vcmax
#'@param j
#'@param TPU
#'@param gm
#'@param Kc
#'@param Ko
#'@name AcFunc

AcFunc <- function(Cc, aG, aS, Rd, Vcmax, j, TPU, gm,Kc,Ko,O2){
  coef = CoefFunc( aG , gammastar, Cc )
  coef * (Vcmax *Cc)/((Cc +Kc * (1 + O2/Ko)))-Rd
}