#' fitComplete
#'
#' Fit A/Ci curves using forced values and dictated bounds. Returns a list with the fit, the graph, and the table.
#' @param data The A/Ci data to be used
#' @param name_assimilation The name given to the assimilation column in "data"
#' @param name_ci Name given to the Ci column in "data"
#' @param gammastar The Cc compensation point. Default: Tobacco.
#' @param O2 The oxygen concentration in parts per hundred.
#' @param pressure Atmospheric pressure in kPa
#' @param tleaf The leaf temperature, in celsius
#' @param initialGuess Initial guesses for fitting. Will generate ok guesses automatically if not provided
#' @param forceValues A vector of forced values in order: VcMax, J, TPU, gm, Rd, aG, aS
#' @param bound_l The minimum values to be considered for each parameter
#' @param bound_h the maximum value to be considered for each parameter
#' @param ignoreTPU Whether to fit TPU or not. Leave false if you don't know what you're doing!
#' @param maxiter Maximum number of iterations for fitting. Can run very slowly if you have a large dataset and high maxiter. Default 250
#' @name fitComplete
#' @export


fitComplete <- function(data,name_assimilation ="A",name_ci=c("pCi","Ci"),gammastar=3.52,O2=21,pressure = 101,tleaf=25,initialGuess=NA,
                        forceValues = c(NA,NA,NA,NA,NA,NA,NA),bound_l=c(1,1,1,.001,.001,0,0),
                        bound_h=c(1000,1000,1000,30,30,1,.75),ignoreTPU=F,maxiter=250){
  aciFit <- fitACi(data=data, gammastar=gammastar, O2=O2, initialGuess = initialGuess, forceValues = forceValues, bound_l=bound_l,
                   bound_h=bound_h,name_assimilation = name_assimilation,name_ci = name_ci,pressure=pressure, tleaf=tleaf,ignoreTPU=ignoreTPU,maxiter=maxiter)
  my_params <- aciFit[[1]]
  aciFit <- aciFit[[2]]
  myTable <- reconstituteTable(data=data,fitParams=my_params,tleaf=tleaf,name_assimilation=name_assimilation,name_ci=name_ci, pressure=pressure,
                               gammastar=gammastar,O2 = O2,ignoreTPU = ignoreTPU)
  myGraph <- reconstituteGraph(data=data, fitParams = my_params,tleaf = tleaf,name_assimilation = name_assimilation,name_ci=name_ci,pressure=pressure,
                               gammastar=gammastar,O2=O2,ignoreTPU=ignoreTPU)
  return(list(my_params,aciFit,myTable,myGraph))
  
}