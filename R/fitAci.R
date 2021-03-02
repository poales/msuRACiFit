#' fitAci
#'
#' Fit A/Ci curves using forced values, and bounds
#' @param data The A/Ci data to be used
#' @param name_assimilation The name given to the assimilation column in "data"
#' @param name_ci Name given to the Ci column in "data"
#' @param gammastar The Cc compensation point. Default: Tobacco.
#' @param O2 The oxygen concentration in parts per hundred.
#' @param pressure Atmospheric pressure in kPa
#' @param tleaf The leaf temperature, in celsius
#' @param initialGuess Initial guesses for fitting. Will generate ok guesses automatically if not provided
#' @param forceValues A vector of forced values in order: VcMax, J, TPU, gm, Rd, aG, aS. Include NAs where not forced, if you modify this!
#' @param bound_l The minimum values to be considered for each parameter
#' @param bound_h the maximum value to be considered for each parameter
#' @param ignoreTPU Whether to fit TPU or not. Leave false if you don't know what you're doing!
#' @param maxiter How many iterations to allow for the fitting algorithm. If you have a lot of points, large values for maxiter will take a long time!
#' @name fitACi
#' @export

fitACi <- function(data,name_assimilation ="A",name_ci=c("pCi","Ci"),gammastar=3.52,O2=21,pressure = 101,tleaf=25,initialGuess=NA,
                   forceValues = c(NA,NA,NA,NA,NA,NA,NA),bound_l=c(1,1,1,.001,.001,0,0),
                   bound_h=c(1000,1000,1000,30,30,1,.75),ignoreTPU=F,maxiter=250){
  locs <- match(tolower(name_ci),tolower(colnames(data)))
  loc <- min(na.omit(locs))
  pCi <- data[,loc]
  
  if(!grepl(pattern="p",tolower(colnames(pCi)))){
    pCi <- pCi /1000000*1000*pressure
  }
  #pCi <- pCi[[1]]
  AData <- data[name_assimilation]
  myFun <- genFun(forceValues = forceValues,gammastar=gammastar,O2=O2*pressure/101,pCi=pCi,assimilationData=AData,tleaf=tleaf,ignoreTPU=ignoreTPU)
  guessFlag <- F
  if(length(initialGuess)!=7){
    initialGuess <- genGuess(AData)
  }
  if(length(forceValues)!=7){
    print("forceValues length is not correct, defaulting to NA")
    forceValues <- rep(NA,7)
  }
  
  #Process the guesses, and the bounds, for which values have been forced
  initialGuess <- initialGuess[is.na(forceValues)]
  bound_l <- bound_l[is.na(forceValues)]
  bound_h <- bound_h[is.na(forceValues)]
  #print(maxiter)
  #print(initialGuess)
  myfit <- minpack.lm::nls.lm(par=initialGuess,lower=bound_l,upper = bound_h,fn = myFun,control = minpack.lm::nls.lm.control(maxiter=maxiter,maxfev = 1250))
  print(myfit)
  return(myfit)
}
