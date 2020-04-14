options(rgl.useNULL = TRUE)
library(rgl)
#library(rglwidget)

rglUITab <- function() {
  tabPanel("rgl",
    p(class="h4", "Library RGL (WebGL)"),
           
    rglwidgetOutput("rglplot"),
    div(class="well",
        div(class='row',
            div(class='col-sm-10', sliderInput('rglplotRandomMax', NULL, value = 20, min = 10, max = 50)),
            div(class='col-sm-2', tags$button(type="button", id="rglplotRandom", class="btn btn-sm btn-primary pull-right action-button shiny-bound-input", "Add random"))
        ),

        hr(),

        div(class="row",
            div(class="col-md-2 col-sm-3",
                div(class="form-group",
                    div(class="input-group input-group-sm",
                        div(class="input-group-addon", "x"),
                        tags$input(type="number", id="rglplotX", class="form-control shiny-bound-input")
                    )
                )
            ),
            div(class="col-md-2 col-sm-3",
                div(class="form-group",
                    div(class="input-group",
                        div(class="input-group-addon", "y"),
                        tags$input(type="number", id="rglplotY", class="form-control shiny-bound-input")
                    )
                )
            ),
            div(class="col-md-2 col-sm-3",
                div(class="form-group",
                    div(class="input-group",
                        div(class="input-group-addon", "z"),
                        tags$input(type="number", id="rglplotZ", class="form-control shiny-bound-input")
                    )
                )
            ),
            div(class="col-md-2 col-sm-3",
                div(class="form-group",
                    div(class="input-group",
                        div(class="input-group-addon", "r"),
                        tags$input(type="number", id="rglplotR", class="form-control shiny-bound-input")
                    )
                )
            ),
            div(class="col-md-2 col-sm-6",
                div(class="form-group",
                    selectInput("rglplotColour", NULL, colours(), selected="gray")
                )
            ),
            div(class="col-md-2 col-sm-6",
                div(class="form-group",
                    tags$button(type="button", id="rglplotAdd", class="btn btn-sm btn-primary action-button", "Add")
                )
            )
        ),
        
        div(id="rglplotError",'')
    )
  )
}

rglServer <- function(input, output, session) {
  rglData <- matrix(, nrow = 0, ncol = 5);

  output$rglplot <- renderRglwidget({
    try(rgl.close(), silent = TRUE)
    text3d(0, text='insert points')
    rglwidget()
  })
  
  rglDataRedraw <- function() {
    output$rglplot <- renderRglwidget({
      try(rgl.close(), silent = TRUE)
      
      for(i in 1:nrow(rglData)) {
        spheres3d(rglData[i,1], rglData[i,2], rglData[i,3], radius = rglData[i,4], color = rglData[i,5])
      }
      axes3d()
      rglwidget()
    })
  }
  
  observeEvent(input$rglplotRandom, {
    rglData <<- rbind(rglData, c(
                    runif(1,-input$rglplotRandomMax,input$rglplotRandomMax),
                    runif(1,-input$rglplotRandomMax,input$rglplotRandomMax),
                    runif(1,-input$rglplotRandomMax,input$rglplotRandomMax),
                    runif(1,1,round(input$rglplotRandomMax/2)),
                    colours()[runif(1,1,length(colours()))]
                  ))
    rglDataRedraw()
  })
  
  observeEvent(input$rglplotAdd, {
    e <- c()
    if(!is.numeric(input$rglplotX))
      e <- append(e, "Wrong x value")
    if(!is.numeric(input$rglplotY))
      e <- append(e, "Wrong y value")
    if(!is.numeric(input$rglplotZ))
      e <- append(e, "Wrong z value")
    if(!is.numeric(input$rglplotR) || input$rglplotR < 0)
      e <- append(e, "Wrong r value")
    
    if(length(e) > 0) {
      session$sendCustomMessage("rglplotError", e)
      return()
    }
    session$sendCustomMessage("rglplotError", FALSE)
    
    rglData <<- rbind(rglData, c(input$rglplotX, input$rglplotY, input$rglplotZ, input$rglplotR, input$rglplotColour))
    rglDataRedraw()
  })
}
