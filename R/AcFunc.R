#'AcFunc
#'
#'Generates the C-limited data
#'@param Cc CO2 concentration at the site of carboxylation
#'@param Rd Day respiration
#'@param Vcmax Michaelis-Menten VMax for carboxylation
#'@param Kc Michaelis-Menten parameter, binding coeff for carbon
#'@param Ko Michaelis-Menten parameter, binding coeff for oxygen
#'@param O2 Oxygen concentration
#'@param coef Passed from CoefFunc
#'@name AcFunc

AcFunc <- function(Cc, Rd, Vcmax, Kc,Ko,O2, coef){
  coef * (Vcmax *Cc)/((Cc +Kc * (1 + O2/Ko)))-Rd
}