#'CoefFunc
#'
#'Generates a little data which makes life a lot easier
#'@param aG
#'@param gammastar
#'@param Cc
#'@name CoefFunc

CoefFunc <- function( aG , gammastar, Cc ){
  1-((1-aG)*gammastar/Cc)
}