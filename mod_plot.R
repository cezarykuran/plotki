plotUITab <- function() {
  tabPanel("plot",
           p(class="h4", "Generic X-Y Plotting"),
           p("Function plot() with animation - select area on plot to zoom."),
           div(
             class= "plotContainer",
             plotOutput("plot", width=NULL, click = "plotClick", brush = "plotBrush"),
             plotOutput("plotDetails", width=NULL)
           )
  )
}

plotServer <- function(input, output, session) {
  df <- read.csv("example.csv")
  colnames(df) <- c("y", "x")
  
  x <- cbind(matrix(seq(1, 500, by=1),ncol=1), round(runif(500,0,1), 2))

  details <- FALSE
  detailsArea <- NULL
  
  #output$plot <-renderPlot(plot(df$x, df$y, xlab="", ylab=""))
  output$plot <-renderPlot(plot(x, xlab="", ylab=""))

  observeEvent(input$plotBrush, {
    
    detailsArea <<- c(
      xmin = input$plotBrush$xmin,
      xmax = input$plotBrush$xmax,
      ymin = input$plotBrush$ymin,
      ymax = input$plotBrush$ymax
    )

    dfdetails <- subset(
      df,
      x > round(input$plotBrush$xmin)
      & x < round(input$plotBrush$xmax)
      & y > round(input$plotBrush$ymin)
      & y < round(input$plotBrush$ymax)
    )
    xmin <- round(input$plotBrush$xmin)
    xmax <- round(input$plotBrush$xmax)

    xdetails <- x[ifelse(xmin>0, xmin, 1):ifelse(xmax<nrow(x), xmax, nrow(x)),]
    xdetails <- xdetails[(xdetails[,2]>input$plotBrush$ymin)&(xdetails[,2]<input$plotBrush$ymax),]

    #if(nrow(dfdetails) > 0) {
    if(nrow(xdetails) > 0) {
      if(!details) {
        session$sendCustomMessage("details", 'show')
        details <<- TRUE
      }
      #output$plotDetails <- renderPlot(plot(dfdetails$x, dfdetails$y, xlab="", ylab=""))
      output$plotDetails <- renderPlot(plot(x = xdetails[,1], y = xdetails[,2], xlab="", ylab=""))
    }
  })
  
  observeEvent(input$plotClick, {
    if(details
       && (input$plotClick$x < detailsArea['xmin'] || input$plotClick$x > detailsArea['xmax']
           || input$plotClick$y < detailsArea['ymin'] || input$plotClick$y > detailsArea['ymax'])
    ) {
      session$sendCustomMessage("details", 'hide')
      details <<- FALSE
    }
  })
}