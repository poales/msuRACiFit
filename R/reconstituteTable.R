#' Reconstitute data and pick limitations
#' 
#' Take fit data and send it back through to build a table with limiting curves
#' @param data The original A/Ci data
#' @param fitParams The returned params data from the fitting function
#' @param tleaf The leaf temperature
#' @param name_assimilation The name given to assimilation column in "data"
#' @param name_ci The name given to the internal CO2 concentration column in "data"
#' @param pressure The atmospheric pressure in kPa
#' @name reconstituteTable
#' @export


reconstituteTable <- function(data,fitParams,tleaf=25,name_assimilation="A", name_ci=c("pCi","Ci"),pressure=101,gammastar=3.52,O2=21){
  locs <- match(tolower(name_ci),tolower(colnames(data)))
  loc <- min(na.omit(locs))
  pCi <- data[,loc]
  if(!grepl(pattern="p",tolower(colnames(pCi)))){
    pCi <- pCi /1000000*1000*pressure
  }
  AData <- data[name_assimilation]
  Kc <- exp(35.9774-(80.99 / (0.008314*(273.15 + tleaf))))
  Ko <- exp(12.3772-(23.72 / (0.008314*(273.15 + tleaf))))
  vcmax <- fitParams[1]
  j <- fitParams[2]
  tpu <- fitParams[3]
  gm <- fitParams[4]
  rd <- fitParams[5]
  ag <- fitParams[6]
  as <- fitParams[7]
  data2 <- tibble::tibble(A = data$A, "pCi" = pCi,"Cc"=unlist(pCi) - unlist(data$A)/gm)
  cdat <- tibble::tibble(A=AcFunc(data2$Cc,ag,as,rd,vcmax,j,tpu,gm,Kc,Ko,O2,gammastar),Cc = data2$Cc)
  jdat <- tibble::tibble(A=AjFunc(data2$Cc,ag,as,rd,vcmax,j,tpu,gm,gammastar),Cc=data2$Cc)
  pdat <- tibble::tibble(A=ApFunc(data2$Cc,ag,as,rd,vcmax,j,tpu,gm,gammastar),Cc=data2$Cc)
  #remap data rq
  #now I have to pick limitations and create a column with that data
  data2 <- tibble::add_column(data2,"Rubisco Limited" = cdat$A,"ET Limited" = jdat$A, "TPU Limited" = pdat$A)
  data2 <- tibble::add_column(data2, "Limiting process" = apply(data2[,4:6],1,FUN = which.min))
  #now we have to show individual residuals
  limiting_values <-c()
  for(i in 1:nrow(data2)){
    limiting_values[i] <- unname(data2[i,data2$`Limiting process`[i]+3])
  }
  limiting_values <- dplyr::bind_cols(unlist(limiting_values))
  data2 <- tibble::add_column(data2,residual = data2$A-limiting_values$...1)
  data2 <- tibble::add_column(data2,`res^2`=data2$residual^2)
  
  return(data2)
}


