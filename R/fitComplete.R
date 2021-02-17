#' fitComplete
#'
#' Fit A/Ci curves using forced values and dictated bounds. Returns a list with the fit, the graph, and the table.
#' @param data The A/Ci data to be used
#' @param forceValues A vector of forced values in order: VcMax, J, TPU, gm, Rd, aG, aS
#' @param gammastar The Cc compensation point. Default: Tobacco.
#' @param O2 The oxygen concentration in parts per hundred.
#' @param initialGuess Initial guesses for fitting. Will generate ok guesses automatically if not provided
#' @param bound_l The minimum values to be considered for each parameter
#' @param bound_h the maximum value to be considered for each parameter
#' @param name_assimilation The name given to the assimilation column in "data"
#' @param name_ci Name given to the Ci column in "data"
#' @param pressure Atmospheric pressure in kPa
#' @param ignoreTPU Whether to fit TPU or not. Leave false if you don't know what you're doing!
#' @name fitComplete
#' @export


fitComplete <- function(data,gammastar=3.52,O2=21,initialGuess=NA,forceValues = c(NA,NA,NA,NA,NA,NA,NA),bound_l=c(1,1,1,.001,.001,0,0),
                        bound_h=c(1000,1000,1000,30,30,1,.75),name_assimilation ="A",name_ci=c("pCi","Ci"),pressure = 101,tleaf=25,ignoreTPU=F){
  aciFit <- fitACi(data=data, gammastar=gammastar, O2=O2, initialGuess = initialGuess, forceValues = forceValues, bound_l=bound_l,
                   bound_h=bound_h,name_assimilation = name_assimilation,name_ci = name_ci,pressure=pressure, tleaf=tleaf,ignoreTPU=ignoreTPU)
  myTable <- reconstituteTable(data=data,fitParams=aciFit$par,tleaf=tleaf,name_assimilation=name_assimilation,name_ci=name_ci, pressure=pressure,
                               gammastar=gammastar,O2 = O2,ignoreTPU = ignoreTPU)
  myGraph <- reconstituteGraph(data=data, fitParams = aciFit$par,tleaf = tleaf,name_assimilation = name_assimilation,name_ci=name_ci,pressure=pressure,
                               gammastar=gammastar,O2=O2,ignoreTPU=ignoreTPU)
  return(list(aciFit,myTable,myGraph))
  
}