library(shiny)

source('mod_plot.R')
source('mod_ggplot2.R')
source('mod_plotly.R')
source('mod_rgl.R')


shinyUI = fixedPage(
  #theme = "flatly.min.css",

  tags$head(
    tags$link(rel="stylesheet", href = "style.css"),
    tags$script(src = "main.js")
  ),

  h1("Shiny plots"),

  tabsetPanel(
    plotUITab(),
    ggplot2UITab(),
    plotlyUITab(),
    rglUITab()
  )
)


shinyServer = function(input, output, session) {
  plotServer(input, output, session)
  ggplot2Server(input, output, session)
  plotlyServer(input, output, session)
  rglServer(input, output, session)
}

shinyApp(ui = shinyUI, server = shinyServer)
