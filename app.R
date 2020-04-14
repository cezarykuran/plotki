library(shiny)

source('R/mod_plot.R')
source('R/mod_ggplot2.R')
source('R/mod_plotly.R')
source('R/mod_rgl.R')


shinyUI = fixedPage(
  #theme = "flatly.min.css",

  tags$head(
    tags$link(rel="stylesheet", href = "style.css"),
    tags$script(src = "main.js")
  ),

  h1("Shiny plots"),

  tabsetPanel(
    tabPanel('About', includeMarkdown('README.md')),
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
