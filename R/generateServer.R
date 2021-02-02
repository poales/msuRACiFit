#'generateServer
#'
#'Generates the server side of the shiny application.
#'@param myEnv A parameter containing an environment with import data and export data fields.
#'@name generateServer



generateServer <- function(myEnv){
  # Define server logic to summarize and view selected dataset ----
  server <- function(input, output) {
    df <- myEnv$input
    chosenDat <- tibble::tibble()

    output$xax <- renderUI({
      selectInput(inputId = "xax",
                  label = "x Axis Var:",
                  choices = colnames(df))
    })
    output$yax <- renderUI({
      selectInput(inputId = "yax",
                  label = "y Axis Var:",
                  choices = colnames(df))
    })


    sel <- reactiveValues(table =tibble::tibble(),
                          chosen=tibble::tibble(),
                          xaxis = colnames(df)[1],
                          yaxis=colnames(df)[2])

    sel$xaxis <- colnames(df)[1]
    sel$yaxis <- colnames(df)[2]

    observeEvent(eventExpr = input$xax,{
      sel$xaxis <- input$xax
    })
    observeEvent(eventExpr = input$yax,{
      sel$yaxis <- input$yax
    })
    # to renderPlot to indicate that:
    #
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs (input$bins) change
    # 2. Its output type is a plot
    output$distPlot <- plotly::renderPlotly({
      a <- ggplot2::ggplot(df,mapping=ggplot2::aes_string(x=sel$xaxis,y=sel$yaxis))+
        ggplot2::geom_point()+
        ggplot2::theme_minimal()
      plotly::ggplotly(a,source="A")


    })


    #output$click <- renderPrint({
    #  d <- event_data("plotly_click")
    #  if (is.null(d)) "Click events appear here (double-click to clear)" else d$pointNumber + 1
    #})
    output$brush <- shiny::renderTable({
      d <- plotly::event_data("plotly_selected")
      if (is.null(d)){
        return(df)
      }  else {
        y <- t(colMeans(
          df[d$pointNumber+1,purrr::map_lgl(df,is.numeric)],
          na.rm=T
        ))
        z <- df[d$pointNumber[1]+1,purrr::map_lgl(df,is.character)]
        y <- bind_cols(data.frame(y),z)
        sel$table <- y
        return(y)
      }
      #chosenDat$sel
    })

    observeEvent(input$chosen_selected_id, {
      str(input$chosen_selected_id)
    })

    shiny::observeEvent(input$add,{
      sel$chosen <- dplyr::bind_rows(isolate(sel$chosen), isolate(sel$table))
    })
    shiny::observeEvent(input$remove,{
      sel$chosen <- sel$chosen[-input$chosen_rows_selected,]
    })

    #observeEvent()

    shiny::observeEvent(input$stop, {
      shiny::stopApp(message("App stopped"))
    })

    output$chosen <- DT::renderDataTable({
      #message("entered chosen renderTable")
      if(nrow(sel$chosen)!=0){
        q <- dplyr::mutate_if(sel$chosen,is.numeric,round,4)
        myEnv$output <<- q
        tibble::rownames_to_column(q)[,-1]
      } else {
        sel$chosen
      }

    },selection="multiple",server=FALSE)
    #output$x11 = DT::renderDataTable(iris[,1:3], server = FALSE, selection = 'single')


  }
}
