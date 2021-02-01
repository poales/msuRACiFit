#'AFunc
#'
#'Generates the limited A data
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
#'@name AFunc

Afunc <- function(Cc, aG, aS, Rd, Vcmax, j, TPU, gm,Kc,Ko,O2){
  ac = AcFunc(Cc, aG, aS, Rd, Vcmax, j, TPU, gm,Kc,Ko,O2)
  aj = AjFunc(Cc, aG, aS, Rd, Vcmax, j, TPU, gm)
  ap = ApFunc(Cc, aG, aS, Rd, Vcmax, j, TPU, gm)
  pmin(ac,aj,ap)
}