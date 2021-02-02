#'genApp
#'
#'Opens the graphic user interface for the fitAci subroutines
#'@name genApp
#'@export



genApp <- function(){
  ui <- generateUi()
  serv <- generateServer()
  shiny::runApp(shiny::shinyApp(ui,serv))
}
