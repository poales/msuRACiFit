#'genApp
#'
#'Takes a data table input and opens a shiny app for manual processing.
#'@param input The input data table to be displayed and worked up
#'@name genApp
#'@export



genApp <- function(input){
  myenv <- new.env()
  myenv$input <- input
  ui <- generateUi()
  serv <- generateServer(myenv)
  shiny::runApp(shiny::shinyApp(ui,serv))
  return(myenv$output)

}
