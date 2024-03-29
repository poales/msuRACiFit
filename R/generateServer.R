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
    cutting <- 1
    cutChoices <- shiny::reactiveVal(1)
    enabled <- shiny::reactiveVal(TRUE)
    presets <- msuRACiFit::gsPresets
    
    output$sumres <- shiny::renderText({
      sumres
    })
    sumres <- 0
    names <- c("vcmax","j","tpu","gm","rd","ag","as")
    #writing data ####
    output$write <- shiny::downloadHandler(
      filename=function(){
        if(input$cutEnable)
          paste(gsub(pattern = "(.*)\\..*",replacement="\\1",input$myFile$name)," curve ",as.numeric(input$chosenCut),"output.csv")
        else
          paste(gsub(pattern = "(.*)\\..*",replacement="\\1",input$myFile$name),"output.csv")
      },
      content= function(file){
        readr::write_csv(tibble::tibble("VcMax" = input$vcmax, "J"=input$j,"TPU" = ifelse(input$ignoreTPU,NA,input$tpu),"gm" = input$gm,"rL"=input$rd,
                                        "ag"=input$ag,"as"=input$as,"ssr" = sumres(),"points" = ifelse(!is.null(df()),nrow(mytable()),0),"TLeaf" = input$tleaf,"O2" = input$oxygen,
                                        "patm" = input$patm,"gammastar" = input$gammastar),file = file)
      }
    )
    #writeTable ####
    output$writetable <- shiny::downloadHandler(
      filename=function(){
        paste(gsub(pattern = "(.*)\\..*",replacement="\\1",input$myFile$name),"fitting table.csv")
      },
      content= function(file){
        readr::write_csv(mytable(),file = file)
      }
    )
    #disableToggle####
    shiny::observeEvent(input$disableToggle,{
      x <- enabled()
      x[input$chosen_rows_selected] <- !x[input$chosen_rows_selected]
      enabled(x)
    })
    #generate guess####
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
    #fit ####
    shiny::observeEvent(eventExpr = input$fit,{
      #read in data from numeric entry boxes
      params <- as.numeric(c(input$vcmax,input$j,input$tpu,input$gm,input$rd,input$ag,input$as))
      lbounds <- as.numeric(c(input$vcmaxlbound,input$jlbound,input$tpulbound,input$gmlbound,input$rdlbound,input$aglbound,input$aslbound))
      ubounds <- as.numeric(c(input$vcmaxubound,input$jubound,input$tpuubound,input$gmubound,input$rdubound,input$agubound,input$asubound))

      #look for locked data and pass that as fixedValues
      locks2 <- c(NA,NA,NA,NA,NA,NA,NA)
      for(i in 1:length(locks())){
        if(locks()[i]){
          locks2[i] <- params[i]
        }
      }
      if(!is.null(df())){
        df_disableApplied <- df()[enabled(),]
        

        fitdat <- fitACi(data=tibble::tibble(df_disableApplied),input$gammastar,O2 = input$oxygen,initialGuess = params,forceValues = locks2,bound_l = lbounds,
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
    #smartfit ####
    shiny::observeEvent(eventExpr = input$smartFit,{
      #read in data from numeric entry boxes
      shiny::req(df())
      params <- as.numeric(c(input$vcmax,input$j,input$tpu,input$gm,input$rd,input$ag,input$as))
      lbounds <- as.numeric(c(input$vcmaxlbound,input$jlbound,input$tpulbound,input$gmlbound,input$rdlbound,input$aglbound,input$aslbound))
      ubounds <- as.numeric(c(input$vcmaxubound,input$jubound,input$tpuubound,input$gmubound,input$rdubound,input$agubound,input$asubound))
      
      #look for locked data and pass that as fixedValues
      locks2 <- c(NA,NA,NA,NA,NA,NA,NA)
      for(i in 1:length(locks())){
        if(locks()[i]){
          locks2[i] <- params[i]
        }
      }
      if(!is.null(df())){
        df_disableApplied <- df()[enabled(),]
        # print(paste0("Type of df_disableApplied ",tibble::is_tibble(df_disableApplied)))
        # print(paste0("yax: ",input$yax))
        # print(df_disableApplied[[input$yax]])
        # print(paste0("xax: ",input$xax))
        # print(df_disableApplied[[input$xax]])
        flag_press <- T
        if(!grepl(pattern="p",tolower(input$xax))){
          flag_press <- F
        }
        fitdat <- smartFit(data_assimilation = df_disableApplied[[input$yax]], data_ci = df_disableApplied[[input$xax]], ci_as_pressure = flag_press, gm_min = input$minsmartgm,gm_max = input$maxsmartgm,
                         gm_samples = input$maxsmartiter,gammastar = input$gammastar,O2 = input$oxygen,forceValues = locks2,bound_l = lbounds,
                         bound_h = ubounds,pressure=input$patm,tleaf=input$tleaf,ignoreTPU=input$ignoreTPU,
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
    #firstIn <- shiny::bindEvent(shiny::reactive({
    #firstIn ####
    firstIn <- shiny::bindEvent(shiny::reactive({
      #this stores the full data loaded in. we will manipulate the data later. 
      #There's a possibility that we try to cut up the data etc which means we will want to be able to refer back to this at a later time, so we have to store it in.
      #It should only react to the input file changing.
      if(is.null(input$myFile)){
        NULL
      }else{
        interpFile(input$myFile$datapath)
      }
    #}),input$myFile)
    },label="firstin"),input$myFile,ignoreNULL=F)
    #df####
    df <- shiny::reactive({
      shiny::req(firstIn())
      #if nothing else, give it firstIn
      #if something else, modify firstIn
      if(input$cutEnable & input$cutColChosen != ""){
        if(input$cutColChosen != ""){
          x2 <- ss_runsetter(firstIn(),aslist=T,threshold=input$cutLength,column=input$cutColChosen)
          cutChoices(1:length(x2))
          enabled(rep(TRUE,nrow(x2[[as.numeric(input$chosenCut)]])))
          x2[[as.numeric(input$chosenCut)]]
        } else{
          cutChoices(1)
          if(!is.null(firstIn())){
            enabled(rep(TRUE,nrow(firstIn())))
          }
          
          firstIn()
        }
        
      }else{
        cutChoices(1)
        if(!is.null(firstIn())){
          enabled(rep(TRUE,nrow(firstIn())))
        }
        
        firstIn()
        
      }
    })
    #create the drop down box for selecting the x axis variable, and initialize it
    #axes defining ####
    xax_reactive <- shiny::bindEvent(shiny::reactive({
      cn <- NULL
      if(!is.null(df())){
        cn <- colnames(df())
        #find the items with ci in them
        ci_find <- grep("ci",ignore.case = T,x = cn)
        
        if(length(ci_find)==0){ #nothing found...
          nm <- cn[1]
        }else{ #use pci if you can find it
          pci_find <- grep(pattern="p",x=cn[ci_find],ignore.case = T)
          if(length(pci_find)>0){
            nm <- cn[ci_find[dplyr::first(pci_find)]]
          } else{
            nm <- cn[dplyr::first(ci_find)]
          }
        }
        
        #initialize the xaxis variable, preferably to pci
        shiny::selectInput(inputId = "xax",
                           label = "Ci Variable:",
                           choices = cn,selected = nm)
      } else{
        warning("Determined df was null")
        #placeholder only
        shiny::selectInput(inputId = "xax",
                           label = "Ci Variable:",
                           choices = "Ci")
      }
    }),df())
    output$xax <- shiny::renderUI({
      shiny::req(xax_reactive())
      xax_reactive()
    })
    
    #create the drop down menu for the y axis variable and initialize it, preferably to "A"
    output$yax <- shiny::renderUI({
      cn <- colnames(df())
      nm2 <- NULL
      if(!is.null(df())){
        if("A" %in% cn){
          nm2 <- "A"
        } else{
          nm2 <- cn[1]
        }
        shiny::selectInput(inputId = "yax",
                           label = "Assimilation Var:",
                           choices = cn,selected = nm2)
      }else{
        shiny::selectInput(inputId = "yax",
                           label = "Assimilation Var:",
                           choices = "A")
      }
    })
    #data cutting ####
    #allow the user to pick a variable by which the data will be divided
    #used for splitting multiple curves apart from one data file
    #preferentially, initialize it to the "elapsed" column, most useful from licor data files
    output$cutColChoices <- shiny::bindEvent(shiny::renderUI({
      cn <- colnames(firstIn())
      nm2 <- NULL
      if(!is.null(firstIn())){
        if("elapsed" %in% cn){
          nm2 <- "elapsed"
        } else if(!is.null(input$xax)){
          
        }else{
          nm2 <- cn[1]
        }
        
      }
      shiny::selectInput(inputId = "cutColChosen",
                         label = "Splitting var:",
                         choices = cn,selected = nm2)
    }),firstIn(),input$cutEnable)
    #if you've split up the data, you'll need to pick which slice to eat
    output$cutopts <- shiny::renderUI({
      shiny::selectInput(inputId="chosenCut",
                         label="Which curve?",
                         choices = cutChoices(),
                         selected = 1)
    })
    #render gammastar calculation based on the selected plant and or input data
    gs <- shiny::reactive({
      gs1 <- calcGammaStar(input$c,input$dHa,input$tleaf,input$oxygen)
      if(!is.null(input$chosenPreset) & input$gammastarCalc){
        if(input$chosenPreset != "Nicotiana tabacum" & input$chosenPreset != "Arabidopsis thaliana")
          gs1 <- gs1 * 1000 / 1000000 * input$patm
      }
      gs1
    })
    gd <- shiny::observe({
      if(input$gammastarCalc){
        shinyWidgets::updateAutonumericInput(session,"gammastar",value = gs())
      }
    })
    #here's the drop down menu for preset gammastar calculations
    output$presetOpts <- shiny::renderUI({
      shiny::selectInput(inputId="chosenPreset",
                         label="Preset plant",
                         choices = unname(unlist(presets[,1])),
                         selected = "Nicotiana tabacum")
    })
    #box for calculating your own gammastar values
    gx <- shiny::observe({
      if(!is.null(input$chosenPreset)){
        presetRow <- presets[presets$plant==input$chosenPreset,]
        shinyWidgets::updateAutonumericInput(session,"c",value = unname(unlist(presetRow$c)))
        shinyWidgets::updateAutonumericInput(session,"dHa",value = unname(unlist(presetRow$dHa)))
      }
      
      
    })
    
    locks <- shiny::reactive({
      c(input$vcmaxlock,input$jlock,input$tpulock,input$gmlock,input$rdlock,input$aglock,input$aslock)
    })
    
    #display the plot
    output$distPlot <- plotly::renderPlotly({
      #inFile <- input$myFile
      shiny::req(input$xax,input$yax)
      params <- c(input$vcmax,input$j,input$tpu,input$gm,input$rd,input$ag,input$as)
      if(is.null(df())){
        a <- ggplot2::ggplot(df(),mapping=ggplot2::aes(x="Cc",y="A"))+
          ggplot2::theme_classic()
      }else{
        df_disableApplied <- df()[enabled(),]
        a <- reconstituteGraph(df_disableApplied, nameParams(params),
                               tleaf=input$tleaf,name_assimilation=input$yax, name_ci=input$xax,pressure=input$patm,gammastar=input$gammastar,O2=input$oxygen,ignoreTPU=input$ignoreTPU)
      }
      plotly::config(plotly::layout(plotly::ggplotly(a+ggplot2::labs(x="x",y="y"),source="A"),
        yaxis=list(
          title=plotly::TeX("A~(\\mu mol~m^{-2}s^{-1})")
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
    mytable_pre <- shiny::reactive({
      shiny::req(df(),input$yax,input$xax)
      params <- c(input$vcmax,input$j,input$tpu,input$gm,input$rd,input$ag,input$as)
      if(is.null(df()))
        NULL
      else{
        reconstituteTable(df(),nameParams(params),
              tleaf=input$tleaf,name_assimilation=input$yax, name_ci=input$xax,pressure=input$patm,gammastar=input$gammastar,O2=input$oxygen,ignoreTPU=input$ignoreTPU)
        
        
      }
    })
    mytable <- shiny::reactive({
      
      if(is.null(mytable_pre()))
        NULL
      else{

        table_pre <- mytable_pre()
        table_pre$`residual`[!enabled()] <- 0
        table_pre$`res^2`[!enabled()] <- 0
        table_pre$`Limiting process`[!enabled()] <- 0

        table_pre
        
        
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
    #output$chosen <- shiny::renderTable({
    output$chosen <- DT::renderDataTable({
      if(is.null(mytable()))
        DT::datatable(mytable())
      else{
        if(input$ignoreTPU){ #have to round differently if you ignore TPU
          DT::formatRound(DT::datatable(mytable(),
                                        options=list(
                                          autoWidth=TRUE,
                                          columnDefs = list(list(width = '100px',targets="_all")),
                                          pageLength=50
                                        )
          ),c(1:5,7,8),3)
        }else{
          DT::formatRound(DT::datatable(mytable(),
                                        options=list(
                                          autoWidth=TRUE,
                                          columnDefs = list(list(width = '100px',targets="_all")),
                                          pageLength=50
                                        )
          ),c(1:6,8,9),3)
        }
        
      }
      #inFile <- input$myFile
      
      #},server=FALSE,spacing="xs")
    }) 



    #output$click <- renderPrint({
    #  d <- event_data("plotly_click")
    #  if (is.null(d)) "Click events appear here (double-click to clear)" else d$pointNumber + 1
    #})




    shiny::observeEvent(input$stop, {
      shiny::stopApp(message("App stopped"))
    })


  }
}
