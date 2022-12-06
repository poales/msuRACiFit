#' Reconstitute and Graph fit data
#' 
#' Take fit data and send it back through to make a graph
#' @param data The original A/Ci data
#' @param fitParams The returned params data from the fitting function
#' @param name_assimilation The name given to assimilation column in "data"
#' @param name_ci The name given to the internal CO2 concentration column in "data"
#' @param gammastar The Cc compensation point. Default: Tobacco.
#' @param O2 The oxygen concentration in parts per hundred.
#' @param pressure Atmospheric pressure in kPa
#' @param tleaf The leaf temperature, in celsius
#' @param ignoreTPU Whether to fit TPU or not. Leave false if you don't know what you're doing!
#' @name reconstituteGraph
#' @export


reconstituteGraph <- function(data,fitParams,name_assimilation="A", name_ci=c("pCi","Ci"),gammastar=3.52,O2=21,pressure=101,tleaf=25,ignoreTPU=F){
  if(!tibble::is_tibble(data)){
    data <- tibble::tibble(data)
  }
  locs <- match(tolower(name_ci),tolower(colnames(data)))
  loc <- min(stats::na.omit(locs))
  pCi <- data[,loc]
  if(!grepl(pattern="p",tolower(colnames(pCi)))){
    pCi <- pCi /1000000*1000*pressure
  }
  AData <- data[name_assimilation]
  Kc <- exp(35.9774-(80.99 / (0.008314*(273.15 + tleaf))))
  Ko <- exp(12.3772-(23.72 / (0.008314*(273.15 + tleaf))))
  # vcmax <- fitParams[1]
  # j <- fitParams[2]
  # tpu <- fitParams[3]
  # gm <- fitParams[4]
  # rd <- fitParams[5]
  # ag <- fitParams[6]
  # as <- fitParams[7]
  Ccs <- seq(from=0,to=max(pCi)*1.2,length.out=200)
  # cdat <- tibble::tibble(A=AcFunc(Ccs,ag,as,rd,vcmax,j,tpu,gm,Kc,Ko,O2,gammastar),Cc = Ccs)
  # jdat <- tibble::tibble(A=AjFunc(Ccs,ag,as,rd,vcmax,j,tpu,gm,gammastar),Cc=Ccs)
  # pdat <- tibble::tibble(A=ApFunc(Ccs,ag,as,rd,vcmax,j,tpu,gm,gammastar),Cc=Ccs)
  coef <- with(fitParams,{
    CoefFunc( aG , gammastar, Ccs)
  })
  data2 <- with(fitParams,{
    tibble::tibble(A = unlist(AData), "pCi" = pCi,"Cc"=unlist(pCi) - unlist(AData)/gm)
  })
  cdat <- with(fitParams,{
    tibble::tibble(A=AcFunc(data2$Cc,rL,VcMax,Kc,Ko,O2,coef),Cc = Ccs)
  })
  jdat <- with(fitParams,{
    tibble::tibble(A=AjFunc(data2$Cc,ag,as,rL,J,gammastar,coef),Cc=Ccs)
  })
  pdat <- with(fitParams,{
    tibble::tibble(A=ApFunc(data2$Cc,ag,as,rL,TPU,gammastar,coef),Cc=Ccs)
  })
  #remap data rq
  # data2 <- tibble::tibble(A = unlist(AData),"Cc"=unlist(pCi) - unlist(AData)/gm)
  if(ignoreTPU){
    myPlot <- ggplot2::ggplot()+
      ggplot2::geom_point(data2,mapping=ggplot2::aes(x=Cc,y=A),size=3)+
      ggplot2::geom_point(cdat,mapping=ggplot2::aes(x=Cc,y=A,color="Rubisco"),size=1)+
      ggplot2::geom_point(jdat,mapping=ggplot2::aes(x=Cc,y=A,color="RuBP-Regen"),size=1)+
      #ggplot2::geom_point(pdat,mapping=ggplot2::aes(x=Cc,y=A,color="P"),size=1)+
      ggplot2::scale_color_manual(values=c("red","blue","orange"))+
      ggplot2::ylim(0,max(data2$A)*1.5)+
      ggplot2::theme_classic()+
      ggplot2::ylab(rlang::expr(italic("A")~"(\u03BCmol m"^-2*"s"^-1*")"))+
      ggplot2::xlab(rlang::expr(italic("Cc")~"(Pa)"))
  }else{
    myPlot <- ggplot2::ggplot()+
      ggplot2::geom_point(data2,mapping=ggplot2::aes(x=Cc,y=A),size=3)+
      ggplot2::geom_point(cdat,mapping=ggplot2::aes(x=Cc,y=A,color="Rubisco"),size=1)+
      ggplot2::geom_point(jdat,mapping=ggplot2::aes(x=Cc,y=A,color="RuBP-Regen"),size=1)+
      ggplot2::geom_point(pdat,mapping=ggplot2::aes(x=Cc,y=A,color="TPU"),size=1)+
      ggplot2::scale_color_manual(values=c("red","blue","orange"))+
      ggplot2::ylim(0,max(data2$A)*1.5)+
      ggplot2::theme_classic()+
      ggplot2::ylab(rlang::expr(italic("A")~"(\u03BCmol m"^-2*"s"^-1*")"))+
      ggplot2::xlab(rlang::expr(italic("Cc")~"(Pa)"))
      #ggplot2::xlab(expr("italic(Cc)"))
  }
  
  return(myPlot)
}
