library(plotly)
library(jsonlite)

plotlyUITab <- function() {
  shiny::tabPanel("plotly",
    shiny::p(class="h4", "Library plotly"),
    plotly::plotlyOutput('plotly', height = '50vh'),
    shiny::sliderInput("plotlyRange", label = NULL, min = 1995, max = 2018, value = c(1995, 2018))
  )
}

plotlyServer <- function(input, output, session) {
  jsonUrlPrefix <- 'https://bdl.stat.gov.pl/api/v1/data/by-variable/'

  d <- reactive({
    c(
      jsonlite::fromJSON(paste0(jsonUrlPrefix, '58?format=json&unit-level=0'))$results$values,
      jsonlite::fromJSON(paste0(jsonUrlPrefix, '35039?format=json&unit-level=0'))$results$values
    )
  })

  observeEvent(input$plotlyRange, {
    dput(input$plotlyRange)
  })

  dt <- reactive({
    input$plotlyRange
    d()
  }) 

  output$plotly <- renderPlotly({
    p <- plotly::plot_ly(xaxis = 'years')
    p <- plotly::add_trace(p, x = d()[1][[1]]$year, y = d()[1][[1]]$val, type = 'scatter', mode = 'lines', name = 'malzenstwa')
    p <- plotly::add_trace(p, x = d()[2][[1]]$year, y = d()[2][[1]]$val, type = 'scatter', mode = 'lines', name = 'rozwody')
    p
  })
}
