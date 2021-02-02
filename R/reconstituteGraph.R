#' Reconstitute and Graph fit data
#' 
#' Take fit data and send it back through to make a graph
#' @param data The original A/Ci data
#' @param fitParams The returned params data from the fitting function
#' @param tleaf The leaf temperature
#' @param name_assimilation The name given to assimilation column in "data"
#' @param name_ci The name given to the internal CO2 concentration column in "data"
#' @param pressure The atmospheric pressure in kPa
#' @name reconstituteGraph
#' @export


reconstituteGraph <- function(data,fitParams,tleaf,name_assimilation="A", name_ci=c("pCi","Ci"),pressure=101){
  locs <- match(tolower(name_ci),tolower(colnames(data)))
  loc <- min(na.omit(locs))
  pCi <- data[,loc]
  if(!grepl(pattern="p",tolower(colnames(pCi)))){
    pCi <- pCi /1000000*1000*pressure
  }
  AData <- data[name_assimilation]
  Kc <- exp(35.9774-(80.99 / (0.008314*(273.15 + tleaf))))
  Ko <- exp(12.3772-(23.72 / (0.008314*(273.15 + tleaf))))
  gammastar <- 3.52
  O2 <- 21
  vcmax <- fitParams[1]
  j <- fitParams[2]
  tpu <- fitParams[3]
  gm <- fitParams[4]
  rd <- fitParams[5]
  ag <- fitParams[6]
  as <- fitParams[7]
  Ccs <- seq(from=0,to=max(pCi)*1.2,length.out=200)
  cdat <- tibble(A=AcFunc(Ccs,ag,as,rd,vcmax,j,tpu,gm,Kc,Ko,O2,gammastar),Cc = Ccs)
  jdat <- tibble(A=AjFunc(Ccs,ag,as,rd,vcmax,j,tpu,gm,gammastar),Cc=Ccs)
  pdat <- tibble(A=ApFunc(Ccs,ag,as,rd,vcmax,j,tpu,gm,gammastar),Cc=Ccs)
  #remap data rq
  data2 <- tibble(A = data$A,"Cc"=unlist(pCi) - unlist(data$A)/gm)
  ggplot()+
    geom_point(data2,mapping=aes(x=Cc,y=A),size=3)+
    geom_point(cdat,mapping=aes(x=Cc,y=A,color="C"),size=1)+
    geom_point(jdat,mapping=aes(x=Cc,y=A,color="J"),size=1)+
    geom_point(pdat,mapping=aes(x=Cc,y=A,color="P"),size=1)+
    scale_color_manual(values=c("red","blue","orange"))+
    ylim(0,max(data2$A)*1.5)+
    theme_classic() %>% return()
}