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
#'@name AFunc

Afunc <- function(Cc, aG, aS, Rd, Vcmax, j, TPU, gm){
  ac = AcFunc(Cc, aG, aS, Rd, Vcmax, j, TPU, gm)
  aj = AjFunc(Cc, aG, aS, Rd, Vcmax, j, TPU, gm)
  ap = ApFunc(Cc, aG, aS, Rd, Vcmax, j, TPU, gm)
  p.min(c(ac,aj,ap))
}