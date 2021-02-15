#' Create the function from which to fit
#'
#' This function accepts some of the parameters required to fit an A/Ci curve.
#' It also accepts a vector of forced data to fit with and generates a function that remembers which parameters are fixed
#' @param forceValues A vector of forced values in order: VcMax, J, TPU, gm, Rd, aG, aS
#' @param gammastar The Cc compensation point. Default: Tobacco.
#' @param O2 The oxygen concentration in parts per hundred.
#' @param pCi The internal pressure of CO2
#' @param assimilationData The measured assimilation data
#' @param ignoreTPU Whether to fit TPU or not. Leave false if you don't know what you're doing!
#' @name genFun



genFun <- function(forceValues = c(NA,NA,NA,NA,NA,NA,NA),gammastar=3.52,O2=21,pCi,assimilationData,tleaf=25,ignoreTPU=F){
  if(!is.na(forceValues[1])){
    Vcmax <- forceValues[1]
    vc.is.forced <- T
  }else{
    vc.is.forced <- F
  }
  if(!is.na(forceValues[2])){
    j <- forceValues[2]
    j.is.forced <- T
  }else{
    j.is.forced <- F
  }
  if(!is.na(forceValues[3])){
    TPU <- forceValues[3]
    tpu.is.forced <- T
  }else{
    tpu.is.forced <- F
  }
  if(!is.na(forceValues[4])){
    gm <- forceValues[4]
    gm.is.forced <- T
  }else{
    gm.is.forced <- F
  }
  if(!is.na(forceValues[5])){
    Rd <- forceValues[5]
    rd.is.forced <- T
  }else{
    rd.is.forced <- F
  }
  if(!is.na(forceValues[6])){
    aG <- forceValues[6]
    ag.is.forced <- T
  }else{
    ag.is.forced <- F
  }
  if(!is.na(forceValues[7])){
    aS <- forceValues[7]
    as.is.forced <- T
  }else{
    as.is.forced <- F
  }
  gammastar <- gammastar
  O <- O2
  y <- assimilationData
  Kc <- exp(35.9774-(80.99 / (0.008314*(273.15 + tleaf))))
  Ko <- exp(12.3772-(23.72 / (0.008314*(273.15 + tleaf))))
  fn <- function(params) {
    
    i <- 1
    if(!vc.is.forced){
      Vcmax <- params[i]
      i <- i+1
    }
    if(!j.is.forced){
      j <- params[i]
      i <- i+1
    }
    if(!tpu.is.forced){
      TPU <- params[i]
      i <- i+1
    }
    if(!gm.is.forced){
      gm <- params[i]
      i <- i+1
    }
    if(!rd.is.forced){
      Rd <- params[i]
      i <- i+1
    }
    if(!ag.is.forced){
      aG <- params[i]
      i <- i+1
    }
    if(!as.is.forced){
      aS <- params[i]
      i <- i+1
    }
    
    Cc <- pCi - y/gm
    if(ignoreTPU){
      #print("ignoring TPU!")
      y.out <- AFuncMinusTPU(Cc, aG, aS, Rd, Vcmax, j, TPU, gm,Kc,Ko,O2,gammastar) 
    }else{
      y.out <- AFunc(Cc, aG, aS, Rd, Vcmax, j, TPU, gm,Kc,Ko,O2,gammastar)   
    } 
    return((unlist(y-y.out))^2)
  }
  return(fn)
}
