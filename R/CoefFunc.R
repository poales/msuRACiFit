#'CoefFunc
#'
#'Generates a little data which makes life a lot easier
#'@param aG Proportion of glycerate carbon released from photorespiration as glycine
#'@param gammastar The Cc compensation point for rubisco
#'@param Cc Concentration of carbon dioxide at the site of carboxylation
#'@name CoefFunc

CoefFunc <- function(aG, gammastar, Cc ){
  1-((1-aG)*gammastar/Cc)
}