#'generateUI
#'
#'Generates the UI side of the shiny application.
#'@name generateUI


generateUi <- function(){

  # Define UI for dataset viewer app ----
  ui <- shiny::fluidPage(

    # App title ----
    shiny::titlePanel("ACI FITTING"),

    shiny::fluidRow( #top row: graphs, params, etc
      shiny::column(width=6,
             # shiny::actionButton("add", "Add Value"),
             # shiny::actionButton("remove","Remove Value"),
             # shiny::uiOutput("xax"),
             # shiny::uiOutput("yax"),
             # shiny::verbatimTextOutput("y11"),
             # shiny::actionButton("stop", "Stop", class = "btn-danger", onclick = "setTimeout(function(){window.close();}, 100);")),
             shiny::fluidRow( #top row: load/write data
               fileInput('myFile','Pick CSV file!',accept='.csv')
             ),
             shiny::fluidRow( #second row: parameters, need 5 columns
               #label row...
               shiny::fluidRow(
                 shiny::column(width=2, #label
                               mainPanel(
                                 p("Param")
                               )
                 ),
                 shiny::column(width=2, #value
                               mainPanel(
                                 p("Value")
                               )
                 ),
                 shiny::column(width=2, #lockbox
                               mainPanel(
                                 p("Lock")
                               )
                 ),
                 shiny::column(width=3, #lbound
                               mainPanel(
                                 p("LowBound")
                               )
                 ),
                 shiny::column(width=3,#ubound
                               mainPanel(
                                 p("HiBound")
                               )
                 )
               ),

               #now make a series of rows for each variable, with 5 columns each
               shiny::fluidRow(
                 shiny::column(width=2, #label
                               mainPanel(
                                 p("VcMax")
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
                               mainPanel(
                                 p("J")
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
                               mainPanel(
                                 p("TPU")
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
                               mainPanel(
                                 p("gm")
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
                               mainPanel(
                                 p("rd")
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
                               mainPanel(
                                 p("ag")
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
                               mainPanel(
                                 p("as")
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

             )
      ),

      shiny::column(width=5, plotly::plotlyOutput("distPlot")
             #DT::dataTableOutput('x11')

      )
      #),
      # Main panel for displaying outputs ----
      #mainPanel(
    ),
    shiny::fluidRow(
      shiny::column(width=6,
             DT::dataTableOutput("chosen")
      ),
      shiny::column(width=4,
             shiny::tableOutput("brush")
      )
    )
  )
}
