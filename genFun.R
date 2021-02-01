#' Create the function from which to fit
#'
#' This function accepts some of the parameters required to fit an A/Ci curve.
#' It also accepts a vector of forced data to fit with and generates a function that remembers which parameters are fixed
#' @param forceValues A vector of forced values in order: VcMax, J, TPU, gm, Rd, aG, aS
#' @param gammastar The Cc compensation point. Default: Tobacco.
#' @param O2 The oxygen concentration in parts per hundred.
#' @param pCi The internal pressure of CO2
#' @name genFun
#' @export



genFun <- function(forceValues = c(NA,NA,NA,NA,NA,NA,NA),gammastar=3.52,O2=21,pCi){
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
  gammastar <- 3.52
  O <- O2
  fn <- function(params, y) {
    
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
      rd <- params[i]
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
    
    # Vcmax <- params[1]
    # j <- params[2]
    # TPU <- params[3]
    # gm <- params[4]
    # Rd <- params[5]
    # aG <- params[6]
    # aS <- params[7]
    
    Kc <- 35.9774-
      (80.99 / (0.008314*(273.15 + 25)))
    Ko <- 12.3772-
      (23.72 / (0.008314*(273.15 + 25)))
    Cc <- pCi - y/gm
    #
    CoefFunc <- function( aG , gammastar, Cc ){
      1-((1-aG)*gammastar/Cc)
    }
    #
    AcFunc <- function(Cc, aG, aS, Rd, Vcmax, j, TPU, gm){
      coef = CoefFunc( aG , gammastar, Cc )
      coef * (Vcmax *Cc)/((Cc +Kc * (1 + O/Ko)))-Rd
    }
    #
    AjFunc <- function(Cc, aG, aS, Rd, Vcmax, j, TPU, gm){
      coef = CoefFunc( aG , gammastar, Cc )
      coef * j/(4+(4+8*aG+4*aS)*2*((1-aG)*gammastar)/Cc)-Rd # fit a
    }
    # Assimilation assuming TPU limitation
    ApFunc <- function(Cc, aG, aS, Rd, Vcmax, j, TPU, gm){
      coef = CoefFunc( aG , gammastar, Cc )
      result = coef * (3*TPU/(1-0.5*(1+3*aG+4*aS)*2*(1-aG)*gammastar/Cc))-Rd
      # insert infinite y-values where the x-value (Cc) is low
      result[Cc < 20] = Inf
      return(result)
    }
    Afunc <- function(Cc, aG, aS, Rd, Vcmax, j, TPU, gm){
      ac = AcFunc(Cc, aG, aS, Rd, Vcmax, j, TPU, gm)
      aj = AjFunc(Cc, aG, aS, Rd, Vcmax, j, TPU, gm)
      ap = ApFunc(Cc, aG, aS, Rd, Vcmax, j, TPU, gm)
      p.min(c(ac,aj,ap))
    }
    y.out <- Afunc(Cc, aG, aS, Rd, Vcmax, j, TPU, gm)
    return(y-y.out)
  }
  return(gn)
}