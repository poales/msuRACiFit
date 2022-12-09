#'nameParams
#'
#'Names relevant parameters and returns a tibble
#'@param params The fitting parameters
#'@name nameParams


nameParams <- function(params){
  tibble::tibble("VcMax" = params[1], "J"=params[2],"TPU" = params[3],"gm" = params[4],"rL"=params[5],
                 "ag"=params[6],"as"=params[7])
}