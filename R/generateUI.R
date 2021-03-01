#'generateUI
#'
#'Generates the UI side of the shiny application.
#'@name generateUI


generateUi <- function(){

  # Define UI for dataset viewer app ----
  ui <- shiny::fixedPage(
    shiny::tags$style(shiny::HTML("
        input[type=number] {
              -moz-appearance:textfield;
        }
        input[type=number]::{
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;
        }
    ")),

    # App title ----
    shiny::titlePanel("A/Ci Fitting"),

    shiny::fluidRow( #top row: graphs, params, etc
      shiny::column(width=6,
             # shiny::actionButton("add", "Add Value"),
             # shiny::actionButton("remove","Remove Value"),
             # shiny::uiOutput("xax"),
             # shiny::uiOutput("yax"),
             # shiny::verbatimTextOutput("y11"),
             shiny::fluidRow(
                shiny::actionButton("stop", "Stop", class = "btn-danger", onclick = "setTimeout(function(){window.close();}, 100);"),
                shiny::actionButton("genGuess","Generate guesses!"),
                shiny::actionButton("fit","Fit!"),
                shiny::downloadButton("write","Write!")
             ),
             shiny::fluidRow(),
             shiny::fluidRow( #top row: load/write data
               shiny::fileInput('myFile','Load a data file:')
             ),
             
             shiny::fluidRow( #second row: parameters, need 5 columns
               #label row...
               shiny::fluidRow(
                 shiny::column(width=2, #label
                               shiny::mainPanel(
                                 shiny::p("Param")
                               )
                 ),
                 shiny::column(width=2, #value
                               shiny::mainPanel(
                                 shiny::p("Value")
                               )
                 ),
                 shiny::column(width=2, #lockbox
                               shiny::mainPanel(
                                 shiny::p("Lock")
                               )
                 ),
                 shiny::column(width=3, #lbound
                               shiny::mainPanel(
                                 shiny::p("LowBound")
                               )
                 ),
                 shiny::column(width=3,#ubound
                               shiny::mainPanel(
                                 shiny::p("HiBound")
                               )
                 )
               ),

               #now make a series of rows for each variable, with 5 columns each
               shiny::fluidRow(
                 shiny::column(width=2, #label
                               shiny::mainPanel(
                                 shiny::p("VcMax")
                               )
                ),
                shiny::column(width=3, #value
                              shiny::numericInput("vcmax",label=NULL, value = 100)
                ),
                shiny::column(width=1, #lockbox
                              shiny::checkboxInput("vcmaxlock",label=NULL)
                ),
                shiny::column(width=3, #lbound
                              shiny::numericInput("vcmaxlbound",label=NULL, value = 0)
                ),
                shiny::column(width=3,#ubound
                              shiny::numericInput("vcmaxubound",label=NULL,value=1000)
                )
               ),
               shiny::fluidRow(
                 shiny::column(width=2, #label
                               shiny::mainPanel(
                                 shiny::p("J")
                               )
                 ),
                 shiny::column(width=3, #value
                               shiny::numericInput("j",label=NULL, value = 100)
                 ),
                 shiny::column(width=1, #lockbox
                               shiny::checkboxInput("jlock",label=NULL)
                 ),
                 shiny::column(width=3, #lbound
                               shiny::numericInput("jlbound",label=NULL, value = 0)
                 ),
                 shiny::column(width=3,#ubound
                               shiny::numericInput("jubound",label=NULL,value=1000)
                 )
               ),
               shiny::fluidRow(
                 shiny::column(width=2, #label
                               shiny::mainPanel(
                                 shiny::p("TPU")
                               )
                 ),
                 shiny::column(width=3, #value
                               shiny::numericInput("tpu",label=NULL, value = 10)
                 ),
                 shiny::column(width=1, #lockbox
                               shiny::checkboxInput("tpulock",label=NULL)
                 ),
                 shiny::column(width=3, #lbound
                               shiny::numericInput("tpulbound",label=NULL, value = 0)
                 ),
                 shiny::column(width=3,#ubound
                               shiny::numericInput("tpuubound",label=NULL,value=1000)
                 )
               ),
               shiny::fluidRow(
                 shiny::column(width=2, #label
                               shiny::mainPanel(
                                 shiny::p("gm")
                               )
                 ),
                 shiny::column(width=3, #value
                               shiny::numericInput("gm",label=NULL, value = 3)
                 ),
                 shiny::column(width=1, #lockbox
                               shiny::checkboxInput("gmlock",label=NULL)
                 ),
                 shiny::column(width=3, #lbound
                               shiny::numericInput("gmlbound",label=NULL, value = .001)
                 ),
                 shiny::column(width=3,#ubound
                               shiny::numericInput("gmubound",label=NULL,value=20)
                 )
               ),
               shiny::fluidRow(
                 shiny::column(width=2, #label
                               shiny::mainPanel(
                                 shiny::p("rL")
                               )
                 ),
                 shiny::column(width=3, #value
                               shiny::numericInput("rd",label=NULL, value = 2)
                 ),
                 shiny::column(width=1, #lockbox
                               shiny::checkboxInput("rdlock",label=NULL)
                 ),
                 shiny::column(width=3, #lbound
                               shiny::numericInput("rdlbound",label=NULL, value = 0.001)
                 ),
                 shiny::column(width=3,#ubound
                               shiny::numericInput("rdubound",label=NULL,value=10)
                 )
               ),
               shiny::fluidRow(
                 shiny::column(width=2, #label
                               shiny::mainPanel(
                                 shiny::p("ag")
                               )
                 ),
                 shiny::column(width=3, #value
                               shiny::numericInput("ag",label=NULL, value = 0)
                 ),
                 shiny::column(width=1, #lockbox
                               shiny::checkboxInput("aglock",label=NULL)
                 ),
                 shiny::column(width=3, #lbound
                               shiny::numericInput("aglbound",label=NULL, value = 0)
                 ),
                 shiny::column(width=3,#ubound
                               shiny::numericInput("agubound",label=NULL,value=1)
                 )
               ),
               shiny::fluidRow(
                 shiny::column(width=2, #label
                               shiny::mainPanel(
                                 shiny::p("as")
                               )
                 ),
                 shiny::column(width=3, #value
                               shiny::numericInput("as",label=NULL, value = 0)
                 ),
                 shiny::column(width=1, #lockbox
                               shiny::checkboxInput("aslock",label=NULL)
                 ),
                 shiny::column(width=3, #lbound
                               shiny::numericInput("aslbound",label=NULL, value = 0)
                 ),
                 shiny::column(width=3,#ubound
                               shiny::numericInput("asubound",label=NULL,value=0.75)
                 )
               )

             ) #end of parameters block
      ),

      shiny::column(width=6, plotly::plotlyOutput("distPlot",height = "500px")
             #DT::dataTableOutput('x11')

      )
      #),
      # Main panel for displaying outputs ----
      #mainPanel(
    ),
    shiny::fluidRow(
      shiny::column(width=8,
                    shiny::tableOutput("chosen")
      ),
      shiny::column(width=4,
            shiny::fluidRow(
              shiny::mainPanel(width=3,
                               shiny::p("Sum of Squares Residual: ")
              ),
              shiny::verbatimTextOutput(outputId = "sumres")
            ),
            shiny::fluidRow(
              shiny::headerPanel("")
            ),
            shiny::fluidRow(
              shiny::numericInput("tleaf",label="Leaf Temp",value=25),
              shiny::numericInput("patm",label="Pressure (kPa)",value=101),
              shiny::numericInput("gammastar",label="Gamma* (kPa)",value=3.52),
              shiny::numericInput("oxygen",label="Oxygen%",value=21),
              shiny::checkboxInput("ignoreTPU",label="Ignore TPU",value = F),
              shiny::numericInput("maxiter",label="Maximum Fitting Iterations",value=250)
            )
            
        
      )
    )
  )
}
