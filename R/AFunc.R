#'AFunc
#'
#'Generates the limited A data
#'@param Cc CO2 concentration at the site of carboxylation
#'@param aG The proportion of glycerate carbon that exits photorespiration as glycine
#'@param aS The proportion of glycerate carbon that exits photorespiration as serine
#'@param Rd Day respiration
#'@param Vcmax Michaelis-Menten VMax for carboxylation
#'@param j Maximum rate of electron transport given current conditions
#'@param TPU Rate of triose phosphate usage
#'@param gm Mesophyll conductance to carbon
#'@param Kc Michaelis-Menten parameter, binding coeff for carbon
#'@param Ko Michaelis-Menten parameter, binding coeff for oxygen
#'@param O2 Oxygen concentration
#'@name AFunc

Afunc <- function(Cc, aG, aS, Rd, Vcmax, j, TPU, gm,Kc,Ko,O2){
  ac = AcFunc(Cc, aG, aS, Rd, Vcmax, j, TPU, gm,Kc,Ko,O2)
  aj = AjFunc(Cc, aG, aS, Rd, Vcmax, j, TPU, gm)
  ap = ApFunc(Cc, aG, aS, Rd, Vcmax, j, TPU, gm)
  pmin(ac,aj,ap)
}