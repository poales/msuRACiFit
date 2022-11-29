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

# AFunc <- function(Cc, aG, aS, Rd, Vcmax, j, TPU, gm,Kc,Ko,O2,gammastar){
#   ac = AcFunc(Cc, aG, aS, Rd, Vcmax, j, TPU, gm,Kc,Ko,O2,gammastar)
#   aj = AjFunc(Cc, aG, aS, Rd, Vcmax, j, TPU, gm,gammastar)
#   ap = ApFunc(Cc, aG, aS, Rd, Vcmax, j, TPU, gm,gammastar)
# 
#   pmin(ac,aj,ap)
# }

AFunc <- function(Cc, aG, aS, Rd, Vcmax, j, TPU, gm,Kc,Ko,O2,gammastar){
  coef <- CoefFunc( aG , gammastar, Cc )
  ac = AcFunc(Cc, Rd, Vcmax, Kc,Ko,O2, coef)
  aj = AjFunc(Cc, aG, aS, Rd, j, gammastar, coef)
  ap = ApFunc(Cc, aG, aS, Rd, TPU, gammastar, coef)
  tot <- tibble::tibble(ac,aj,ap,.name_repair = "minimal") #put them in a table to help pick which one is smallest
  l <- apply(tot,1,function(x)which.min(abs(x+Rd))) #pick which one is closest to Rd - this is the pivot point when you go below 0, not absolute 0
  res <- 1:length(l) #create output vector
  for(i in 1:length(l)){
    res[i] <- tot[[i,l[[i]]]] #input the selected lowest value into the output vector
  }
  return(res)
}

