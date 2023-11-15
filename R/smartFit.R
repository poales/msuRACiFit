#' smartFit
#'
#' Fit A/Ci curves iteratively, sampling several different gm values. Fitting is done in two steps for
#' each gm in the range. Fitting is done initially with forced gm to get the shape right and then refit 
#' with the previous fit as the starting condition to optimize gm. This should help the fitting program reach 
#' the most accurate fit with the least amount of finagling.
#' @param data_assimilation A vector of assimilation values
#' @param data_ci A vector of the Ci values.
#' @param ci_as_pressure Whether Ci should be treated as pressure (T) or ppm (F).
#' @param gm_min Lowest gm value sampled.
#' @param gm_max Highest gm value sampled.
#' @param gm_samples How many gm values are sampled
#' @param gammastar The Cc compensation point. Default: Tobacco.
#' @param O2 The oxygen concentration in parts per hundred.
#' @param pressure Atmospheric pressure in kPa
#' @param tleaf The leaf temperature, in celsius
#' @param forceValues A vector of forced values in order: VcMax, J, TPU, gm, Rd, aG, aS. Include NAs where not forced, if you modify this!
#' @param bound_l The minimum values to be considered for each parameter
#' @param bound_h the maximum value to be considered for each parameter
#' @param ignoreTPU Whether to fit TPU or not. Leave false if you don't know what you're doing!
#' @param maxiter How many iterations to allow for the fitting algorithm. If you have a lot of points, large values for maxiter will take a long time!
#' @name smartFit
#' @export

#notes relative to fitACi
#we are going to pass in vectors of assimilation and ci
#this makes more sense really...
#
#because we are using iterative, we don't need to pass in an initialGuess, we will create that for each iteration separately
#
#we want to accept a forceValues but we will be overriding the gm forcevalue as part of the process

smartFit <- function(data_assimilation,data_ci,ci_as_pressure=T,gm_min = 0.5, gm_max = 20, gm_samples = 10,gammastar=3.52,O2=21,pressure = 101,tleaf=25,
                   forceValues = c(NA,NA,NA,NA,NA,NA,NA),bound_l=c(1,1,1,.001,.001,0,0),
                   bound_h=c(1000,1000,1000,30,30,1,.75),ignoreTPU=F,maxiter=250){

  if(gm_samples <2){
    stop("No point using smartFit when you don't want to sample more than one gm")
  }
  if(!ci_as_pressure){
    data_ci <- data_ci /1000000*1000*pressure
  }
  if(length(forceValues)!=7){
    print("forceValues length is not correct, defaulting to NA")
    forceValues <- rep(NA,7)
  }
  
  fixed_gm_value <- 1.5
  gms_in <- seq(from=gm_min,to=gm_max,length.out=gm_samples)
  ssr_out <- rep(0,gm_samples)
  fits_out <- vector("list",gm_samples)
  params_out <- tibble::tibble(Vcmax = rep(0,gm_samples),
                       J = rep(0,gm_samples),
                       TPU = rep(0,gm_samples),
                       gm = rep(0,gm_samples),
                       rL = rep(0,gm_samples),
                       ag = rep(0,gm_samples),
                       as = rep(0,gm_samples))
  #loop starts here####
  for(iter_count in 1:gm_samples){
    
    forceValues_iter <- forceValues
    forceValues_iter[4] <- gms_in[iter_count]
    
    #begin first iteration
    myFun <- genFun(forceValues = forceValues_iter,gammastar=gammastar,O2=O2*pressure/101,pCi=data_ci,assimilationData=data_assimilation,tleaf=tleaf,ignoreTPU=ignoreTPU)
    initialGuess <- genGuess(data_assimilation)
    
    #Process the guesses, and the bounds, for which values have been forced
    initialGuess <- initialGuess[is.na(forceValues_iter)]
    bound_l_iter <- bound_l[is.na(forceValues_iter)]
    bound_h_iter <- bound_h[is.na(forceValues_iter)]

    
    myfit <- minpack.lm::nls.lm(par=initialGuess,lower=bound_l_iter,upper = bound_h_iter,fn = myFun,control = minpack.lm::nls.lm.control(maxiter=maxiter,maxfev = 1250,ptol=0,ftol=0))

    my_params <- c(0,0,0,0,0,0,0)
    j <- 1
    for(i in 1:7){
      if(!is.na(forceValues_iter[i])){
        my_params[i] <- forceValues_iter[i]
      } else{
        my_params[i] <- myfit$par[j]
        j <- j+1
      }
    }
    
    #now second iteration - fit again, with using the params from before as an initial guess
    forceValues_iter[4] <- NA #unlock gm
    
    myFun <- genFun(forceValues = forceValues_iter,gammastar=gammastar,O2=O2*pressure/101,pCi=data_ci,assimilationData=data_assimilation,tleaf=tleaf,ignoreTPU=ignoreTPU)
    initialGuess <- my_params

    
    #Process the guesses, and the bounds, for which values have been forced
    initialGuess <- initialGuess[is.na(forceValues_iter)]
    bound_l_iter <- bound_l[is.na(forceValues_iter)]
    bound_h_iter <- bound_h[is.na(forceValues_iter)]

    myfit_2 <- minpack.lm::nls.lm(par=initialGuess,lower=bound_l_iter,upper = bound_h_iter,fn = myFun,control = minpack.lm::nls.lm.control(maxiter=maxiter,maxfev = 1250,ptol=0,ftol=0))

    my_params_2 <- c(0,0,0,0,0,0,0)
    j <- 1
    for(i in 1:7){
      if(!is.na(forceValues_iter[i])){
        my_params_2[i] <- forceValues_iter[i]
      } else{
        my_params_2[i] <- myfit_2$par[j]
        j <- j+1
      }
    }
    ssr_out[iter_count] <- sum(stats::residuals(myfit_2)^2)
    params_out[iter_count,] <- as.list(my_params_2)
    fits_out[[iter_count]] <- myfit_2
  }
  #pick the iteration which had the lowest ssr####
  chosen <- which.min(ssr_out)
  final_params <- params_out[chosen,]
  final_fit <- fits_out[[chosen]]
  
  
  #outside of the loop here, we don't want to include this in the loop as it will prevent the second iteration from working properly####
  if(ignoreTPU){
    final_params[3] <- NA
  }
  return(list(final_params,final_fit))
}
