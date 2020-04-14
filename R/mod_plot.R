plotUITab <- function() {
  tabPanel("plot",
    p(class="h4", "Generic X-Y Plotting"),
    p("Function plot() with animation - to zoom select area on a plot."),
    div(
      class= "plotContainer",
      plotOutput("plot", width=NULL, click = "plotClick", brush = "plotBrush"),
      plotOutput("plotDetails", width=NULL)
    ),
    div(class = 'well',
      div(class='row',
        div(class='col-sm-10', sliderInput('plotSize', NULL, value = 500, min = 10, max = 1000)),
        div(class='col-sm-2', tags$button(type="button", id="plotReload", class="btn btn-sm btn-primary pull-right action-button shiny-bound-input", "Reload data"))
      )
    )
  )
}

plotServer <- function(input, output, session) {
  x <- reactive({
    input$plotReload
    cbind(matrix(seq(1, input$plotSize, by=1),ncol=1), round(runif(input$plotSize,0,1), 2))
  })
  
  details <- FALSE
  detailsArea <- NULL
  
  detailsDraw <- function() {
    detailsArea <<- c(
      xmin = input$plotBrush$xmin,
      xmax = input$plotBrush$xmax,
      ymin = input$plotBrush$ymin,
      ymax = input$plotBrush$ymax
    )
    
    xmin <- round(input$plotBrush$xmin)
    xmin <- ifelse(xmin>0, xmin, 1)
    xmax <- round(input$plotBrush$xmax)
    xmax <- ifelse(xmax<nrow(x()), xmax, nrow(x()))
    if(xmin >= xmax) xmin <- (xmax-1)
    
    xdetails <- x()[xmin:xmax,]
    xdetails <- xdetails[(xdetails[,2]>input$plotBrush$ymin)&(xdetails[,2]<input$plotBrush$ymax),]
    if(is.vector(xdetails))
      xdetails <- matrix(xdetails, ncol = 2)

    if(!is.null(xdetails) && nrow(xdetails)>0) {
      if(!details) {
        session$sendCustomMessage("details", 'show')
        details <<- TRUE
      }
      output$plotDetails <- renderPlot(plot(x = xdetails[,1], y = xdetails[,2], xlab="", ylab=""))
    }
    else if(details) {
      session$sendCustomMessage("details", 'hide')
      details <<- FALSE
    }
  }

  output$plot <-renderPlot(plot(x(), xlab="", ylab=""))

  observeEvent(input$plotBrush, {
    detailsDraw()
  })
  
  observeEvent(input$plotReload, {
    if(details) detailsDraw()
  })

  observeEvent(input$plotClick, {
    if(details
       && (input$plotClick$x + 20 < detailsArea['xmin'] || input$plotClick$x - 20 > detailsArea['xmax']
           || input$plotClick$y + 20 < detailsArea['ymin'] || input$plotClick$y - 20 > detailsArea['ymax'])
    ) {
      session$sendCustomMessage("details", 'hide')
      details <<- FALSE
    }
  })
}