#'AFuncMinusTPU
#'
#'Generates the limited A data, ignoring TPU
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
#'@name AFuncMinusTPU

AFuncMinusTPU <- function(Cc, aG, aS, Rd, Vcmax, j, TPU, gm,Kc,Ko,O2,gammastar){
  coef <- CoefFunc( aG , gammastar, Cc )
  ac = AcFunc(Cc, Rd, Vcmax, Kc,Ko,O2, coef)
  aj = AjFunc(Cc, aG, aS, Rd, j, gammastar, coef)
  out <- c(1:length(ac)) #create the output vector right away
  for(i in 1:length(out)){ #no table, which saves a lot of time. work on the vectors.
    out[i] <- switch( #pick which one is the closest to Rd, then immediately add it to the outputs
      which.min( #we can't just use min because it has to pivot around rd, so we need abs and +rd there
        abs(
          c(
            ac[i],
            aj[i]
          ) + Rd
        )
      ),ac[i],aj[i]
    )
    
  }
  return(out)
}

