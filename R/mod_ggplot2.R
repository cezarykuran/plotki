library(ggplot2)
library(colourpicker)
library(stringr)

ggplot2fl <- function() {
  list.files(path = "./upload", pattern = "*.csv") 
}

ggplot2UITab <- function() {
  tabPanel("ggplot2",
    p(class="h4", "Library ggplot2"),
    div(class='ggplot2Container',
        plotOutput("ggplot2", height = '50vh', click = "ggplot2Click", brush = "ggplot2Brush"),
        shiny::actionButton('ggplot2Reset', NULL, icon = icon('arrows-alt'))
    ),
    div(class='well',
      div(class='row',
        div(class='col-md-3 col-sm-6', selectInput('ggplot2theme', 'Theme', c('classic', 'bw', 'minimal', 'light', 'dark'))),
        div(class='col-md-3 col-sm-6', selectInput('ggplot2type', 'Type', c('point', 'line'))),
        div(class='col-md-3 col-sm-6', colourInput('ggplot2colour', 'Colour', value="#334455")),
        div(class='col-md-3 col-sm-6', sliderInput('ggplot2width', 'Width', value = 1, min = 0.5, max = 10, step = 0.5))
      ),
      div(class='row',
        div(class='col-md-3 col-sm-6', selectInput('ggplot2csv', 'Source', ggplot2fl())),
        div(class='col-md-3 col-sm-6', fileInput('ggplot2upload', accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), 'Upload csv'))
      )
    )
  )
}

ggplot2Server <- function(input, output, session) {
  enlarged <- NULL
  reset <- 0

  df <- reactive({
    enlarged <<- NULL

    f <- paste0('upload/', input$ggplot2csv)
    if(!file.exists(f)) return(NULL)
    
    read.csv(f, header = TRUE)
  })
  
  dfsubset <- reactive({
    d <- df()
    
    if(!is.null(input$ggplot2Brush)) {
      enlarged <<- c(
        'xmin' = input$ggplot2Brush$xmin,
        'xmax' = input$ggplot2Brush$xmax,
        'ymin' = input$ggplot2Brush$ymin,
        'ymax' = input$ggplot2Brush$ymax
      )
      session$resetBrush("ggplot2Brush")
    }

    if(input$ggplot2Reset != reset) {
      reset <<- input$ggplot2Reset
      enlarged <<- NULL
    }
    
    if(!is.null(enlarged)) {
      d <- subset(d,
        d[,1] > enlarged['xmin'] & d[,1] < enlarged['xmax']
        & d[,2] > enlarged['ymin'] & d[,2] < enlarged['ymax']
      )
    }
    d
  })

  output$ggplot2 <- renderPlot({
    d <- dfsubset()

    if(is.null(d))
      NULL
    else {
      p <- ggplot(d) + aes(x = d[,1], y = d[,2]) + labs(x=colnames(d)[1], y=colnames(d)[2])
      
      if(input$ggplot2type == 'point')
        p <- p + geom_point(colour = input$ggplot2colour, size=input$ggplot2width)
      else if(input$ggplot2type == 'line')
        p <- p + geom_line(colour = input$ggplot2colour, size=input$ggplot2width)
      
      if(input$ggplot2theme == 'bw')
        p <- p + theme_bw()
      else if(input$ggplot2theme == 'minimal')
        p <- p + theme_minimal()
      else if(input$ggplot2theme == 'classic')
        p <- p + theme_classic()
      else if(input$ggplot2theme == 'light')
        p <- p + theme_light()
      else if(input$ggplot2theme == 'dark')
        p <- p + theme_dark()
      else
        p <- p + theme_gray()
      p
    }
  })
  
  upload <- reactive({
    input$ggplot2upload
  })
  
  observe({
    if (is.null(upload()))
      return()

    name <- str_replace_all(upload()$name, '[^a-zA-Z1-9.]', '_')
    file.copy(upload()$datapath, paste0('./upload/', name))
    dput(input$ggplot2Csv)
    updateSelectInput(session, 'ggplot2Csv',
        choices = ggplot2fl(),
        selected = ifelse(nchar(input$ggplot2Csv) == 0, name, input$ggplot2Csv)
    )
  })
}
