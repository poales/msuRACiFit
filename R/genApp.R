#'genApp
#'
#'Opens the graphic user interface for the fitAci subroutines
#'@name genApp
#'@export



genApp <- function(){
  ui <- generateUi()
  serv <- generateServer()
  if(interactive()){
    shiny::runApp(shiny::shinyApp(ui,serv))
  } else{
    shiny::runApp(shiny::shinyApp(ui,serv),launch.browser = T)
  }
  
  
}
