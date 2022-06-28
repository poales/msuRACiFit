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
#'@param gammastar Cc compensation point
#'@name AFunc


AFunc <- function(Cc, aG, aS, Rd, Vcmax, j, TPU, gm,Kc,Ko,O2,gammastar){
  ac = AcFunc(Cc, aG, aS, Rd, Vcmax, j, TPU, gm,Kc,Ko,O2,gammastar)
  aj = AjFunc(Cc, aG, aS, Rd, Vcmax, j, TPU, gm,gammastar)
  ap = ApFunc(Cc, aG, aS, Rd, Vcmax, j, TPU, gm,gammastar)
  tot <- tibble::tibble(ac,aj,ap,.name_repair = "minimal")
  l <- apply(tot,1,function(x)which.min(abs(x+Rd)))
  res <- 1:length(l)
  for(i in 1:length(l)){
    res[i] <- tot[[i,l[[i]]]]
  }
  return(res)
}

