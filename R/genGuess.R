#' genGuess
#' 
#' Generate initial guesses for VcMax, j, TPU
#' @param A_Data Assimilation data. We don't need Ci for this actually
#' @name genGuess

genGuess <- function(A_Data){
  guessTPU <- max(A_Data)/3 #if we just guess that the top point is TPU limited
  guessJ <- max(A_Data)*5 #A good guess is max assimilation times five... lower if lower oxygen...
  guessC <- guessJ #not typically the same as J, but tends to be at least in the same order of magnitude
  guessRd <- guessTPU /30 #about 10% of assimilation is a decent guess
  #other good guesses: gm of 3, alphaG and alphaS are low
  return(c(guessC,guessJ,guessTPU,3,guessRd,.1,.1))
}
