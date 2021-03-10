#'generateServer
#'
#'Generates the server side of the shiny application.
#'@name generateServer



generateServer <- function(){
  # Define server logic to summarize and view selected dataset ----
  server <- function(input, output, session) {
    df <- NULL
    tleaf=25
    params <- c(0,0,0,0,0,0,0)
    lbounds <- c(0,0,0,0,0,0,0)
    ubounds <- c(0,0,0,0,0,0,0)
    fitdat <- NA
    
    output$sumres <- shiny::renderText({
      sumres
    })
    sumres <- 0
    names <- c("vcmax","j","tpu","gm","rd","ag","as")
    #writing data
    output$write <- shiny::downloadHandler(
      filename=function(){
        paste(gsub(pattern = "(.*)\\..*",replacement="\\1",input$myFile$name),"output.csv")
      },
      content= function(file){
        readr::write_csv(tibble::tibble("VcMax" = input$vcmax, "J"=input$j,"TPU" = ifelse(input$ignoreTPU,NA,input$tpu),"gm" = input$gm,"rL"=input$rd,
                                        "ag"=input$ag,"as"=input$as,"ssr" = sumres(),"points" = ifelse(!is.null(df()),nrow(mytable()),0),"TLeaf" = input$tleaf,"O2" = input$oxygen,
                                        "patm" = input$patm,"gammastar" = input$gammastar),file = file)
      }
    )
    output$writetable <- shiny::downloadHandler(
      filename=function(){
        paste(gsub(pattern = "(.*)\\..*",replacement="\\1",input$myFile$name),"fitting table.csv")
      },
      content= function(file){
        readr::write_csv(mytable(),file = file)
      }
    )
    shiny::observeEvent(eventExpr=input$genGuess,{
      if(!is.null(df())){
        locks2 <- c(NA,NA,NA,NA,NA,NA,NA)
        for(i in 1:length(locks)){
          if(locks()[i]){
            locks2[i] <- params[i]
          }
        }
        
        AData <- df()[input$yax]
        guessed <- genGuess(AData)
        
        i <- 1 #track location on page
        j <- 1 #track location in fitdat$par
        for(i in 1:7){
          if(!locks()[i]){
            shinyWidgets::updateAutonumericInput(session,names[i],value = guessed[i])
          }
        }
      }
    })
    shiny::observeEvent(eventExpr = input$fit,{
      params <- as.numeric(c(input$vcmax,input$j,input$tpu,input$gm,input$rd,input$ag,input$as))
      # print("Params:")
      # print(params)
      # print(typeof(params))
      lbounds <- as.numeric(c(input$vcmaxlbound,input$jlbound,input$tpulbound,input$gmlbound,input$rdlbound,input$aglbound,input$aslbound))
      # print("lbounds:")
      # print(lbounds)
      # print(typeof(lbounds))
      ubounds <- as.numeric(c(input$vcmaxubound,input$jubound,input$tpuubound,input$gmubound,input$rdubound,input$agubound,input$asubound))

      #locks <- c(input$vcmaxlock,input$jlock,input$tpulock,input$gmlock,input$rdlock,input$aglock,input$aslock)
      locks2 <- c(NA,NA,NA,NA,NA,NA,NA)
      for(i in 1:length(locks())){
        if(locks()[i]){
          locks2[i] <- params[i]
        }
      }
      if(!is.null(df())){


        fitdat <- fitACi(data=tibble::tibble(df()),input$gammastar,O2 = input$oxygen,initialGuess = params,forceValues = locks2,bound_l = lbounds,
                         bound_h = ubounds,name_assimilation = input$yax,name_ci = input$xax,pressure=input$patm,tleaf=input$tleaf,ignoreTPU=input$ignoreTPU,
                         maxiter=input$maxiter)[[2]]

        i <- 1 #track location on page
        j <- 1 #track location in fitdat$par
        for(i in 1:7){
          if(!locks()[i]){
            shinyWidgets::updateAutonumericInput(session,names[i],value = fitdat$par[j])
            j <- j+1
          }
        }
        
        
      }
        
    })
    output$xax <- renderUI({
      cn <- colnames(df())
      if(!is.null(df())){
        nm <- cn[dplyr::first(grep("ci",ignore.case = T,x = cn))]
        print(nm)
        shiny::selectInput(inputId = "xax",
                           label = "Ci Variable:",
                           choices = cn,selected = nm)
      } else{
        shiny::selectInput(inputId = "xax",
                           label = "Ci Variable:",
                           choices = cn)
      }

    })
    output$yax <- renderUI({
      cn <- colnames(df())
      if(!is.null(df())){
        if("A" %in% cn){
          nm2 <- "A"
        } else{
          nm2 <- cn[dplyr::first(grep("Pho",ignore.case = T,x = cn))]
        }
        print(nm2)
        shiny::selectInput(inputId = "yax",
                           label = "Assimilation Var:",
                           choices = cn,selected = nm2)
      } else{
        shiny::selectInput(inputId = "yax",
                           label = "Assimilation Var:",
                           choices = cn)
      }
      
    })
    df <- shiny::reactive({
      if(is.null(input$myFile)){
        NULL
      }else{
        interpFile(input$myFile$datapath)
      }
    })
    locks <- shiny::reactive({
      c(input$vcmaxlock,input$jlock,input$tpulock,input$gmlock,input$rdlock,input$aglock,input$aslock)
    })
    # to renderPlot to indicate that:
    #
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs (input$bins) change
    # 2. Its output type is a plot
    output$distPlot <- plotly::renderPlotly({
      #inFile <- input$myFile
      params <- c(input$vcmax,input$j,input$tpu,input$gm,input$rd,input$ag,input$as)
      if(is.null(df())){
        a <- ggplot2::ggplot(df(),mapping=ggplot2::aes(x="Cc",y="A"))+
          ggplot2::theme_classic()
      }else{
        
        a <- reconstituteGraph(df(),params,
                               tleaf=input$tleaf,name_assimilation=input$yax, name_ci=input$xax,pressure=input$patm,gammastar=input$gammastar,O2=input$oxygen,ignoreTPU=input$ignoreTPU)
        
      }
      plotly::config(plotly::layout(plotly::ggplotly(a,source="A"),
        yaxis=list(
          title=plotly::TeX("A~(\\mu mol m^{-2}s^{-1})") 
        ),
        xaxis=list(
          title=plotly::TeX("Cc~(Pa)")
        ),
        legend=list(
          orientation='h',
          x=0,y=1.01
        )
      ),mathjax="cdn")
        
      
    })
    mytable <- shiny::reactive({
      params <- c(input$vcmax,input$j,input$tpu,input$gm,input$rd,input$ag,input$as)
      if(is.null(df()))
        NULL
      else{
        reconstituteTable(df(),params,
              tleaf=input$tleaf,name_assimilation=input$yax, name_ci=input$xax,pressure=input$patm,gammastar=input$gammastar,O2=input$oxygen,ignoreTPU=input$ignoreTPU)
        
        
      }
    })
    sumres <- shiny::reactive({
      if(!is.null(mytable())){
        sum(mytable()$`res^2`)
      } else
        NULL
    })
    output$sumres <- shiny::renderText({
      sumres()
    })
    output$chosen <- shiny::renderTable({
      #inFile <- input$myFile
      mytable()
      

    },server=FALSE,spacing="xs")
    


    #output$click <- renderPrint({
    #  d <- event_data("plotly_click")
    #  if (is.null(d)) "Click events appear here (double-click to clear)" else d$pointNumber + 1
    #})




    shiny::observeEvent(input$stop, {
      shiny::stopApp(message("App stopped"))
    })
    #output$x11 = DT::renderDataTable(iris[,1:3], server = FALSE, selection = 'single')


  }
}
