library(ggplot2)

ggplot2UITab <- function() {
  tabPanel("ggplot2",
           p("Details with animation - select space on plot"),
           plotOutput("ggplot2", click = "ggplot2Click", brush = "ggplot2Brush")
  )
}

ggplot2Server <- function(input, output, session) {
  df <- read.csv("example.csv")
  colnames(df) <- c("y", "x")
  
  output$ggplot2 <- renderPlot({
    ggplot(df, aes(x=x, y=y)) + labs(x="x", y="y") + geom_point()
  })
}