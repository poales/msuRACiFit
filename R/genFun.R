#' Create the function from which to fit
#'
#' This function accepts some of the parameters required to fit an A/Ci curve.
#' It also accepts a vector of forced data to fit with and generates a function that remembers which parameters are fixed
#' @param forceValues A vector of forced values in order: VcMax, J, TPU, gm, Rd, aG, aS
#' @param gammastar The Cc compensation point. Default: Tobacco.
#' @param O2 The oxygen concentration in parts per hundred.
#' @param pCi The internal pressure of CO2
#' @param assimilationData The measured assimilation data
#' @param tleaf Leaf temperature in celsius
#' @param ignoreTPU Whether to fit TPU or not. Leave false if you don't know what you're doing!
#' @name genFun



genFun <- function(forceValues = c(NA,NA,NA,NA,NA,NA,NA),gammastar=3.52,O2=21,pCi,assimilationData,tleaf=25,ignoreTPU=F,
                   Kc=exp(35.9774-(80.99 / (0.008314*(273.15 + tleaf)))),
                   Ko=exp(12.3772-(23.72 / (0.008314*(273.15 + tleaf))))){
  names <- c("VcMax","J","TPU","gm","rL","aG","aS")
  names(forceValues) <- names
  forceValues <- forceValues[!is.na(forceValues)]
  param_names <- names[!names %in% names(forceValues)]
  forceValues <- split(forceValues,names(forceValues))
  gammastar <- gammastar
  O <- O2
  y <- assimilationData
  Kc <- exp(35.9774-(80.99 / (0.008314*(273.15 + tleaf))))
  Ko <- exp(12.3772-(23.72 / (0.008314*(273.15 + tleaf))))
  if(ignoreTPU){
    fn <- function(params) {
      params <- as.list(params)
      names(params) <- param_names
      vals <- c(params,forceValues)
      Cc <- pCi - y/vals$gm
      y.out <- AFuncMinusTPU(Cc, vals$aG, vals$aS, vals$rL, vals$VcMax, vals$J, vals$TPU, vals$gm,Kc,Ko,O2,gammastar)
      return(unlist(y-y.out))
    }
  } else{
    fn <- function(params) {
      params <- as.list(params)
      names(params) <- param_names
      vals <- c(params,forceValues)
      Cc <- pCi - y/vals$gm
      y.out <- AFunc(Cc, vals$aG, vals$aS, vals$rL, vals$VcMax, vals$J, vals$TPU, vals$gm,Kc,Ko,O2,gammastar)
      return(unlist(y-y.out))
    }
  }
  
  return(fn)
}
