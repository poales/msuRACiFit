#' fitAci
#'
#' Fit A/Ci curves using forced values, and bounds
#' @param data The A/Ci data to be used
#' @param forceValues A vector of forced values in order: VcMax, J, TPU, gm, Rd, aG, aS
#' @param gammastar The Cc compensation point. Default: Tobacco.
#' @param O2 The oxygen concentration in parts per hundred.
#' @param initialGuess Initial guesses for fitting. Will generate ok guesses automatically if not provided
#' @param bound_l The minimum values to be considered for each parameter
#' @param bound_h the maximum value to be considered for each parameter
#' @param name_assimilation The name given to the assimilation column in "data"
#' @param name_ci Name given to the Ci column in "data"

ficAci <- function(data,forceValues = c(NA,NA,NA,NA,NA,NA,NA),gammastar=3.52,O2=21,initialGuess=NA,bound_l=c(1,1,1,.001,.001,0,0),
                   bound_h=c(1000,1000,1000,30,30,1,.75),name_assimilation ="A",name_ci=c("pCi","Ci")){
  locs <- match(tolower(name_ci),tolower(colnames(data)))
  loc <- min(na.omit(locs))
  pCi <- data[,loc]
  AData <- data[name_assimilation]
  myFun <- genFun(forceValues = forceValues,gammastar=gammastar,O2=O2,pCi)
  if(is.na(initialGuess)){
    initialGuess <- genGuess(AData)
  }
  
}