#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
shinyServer(function(input, output) {
  model <- reactive({
    brushed_data <- brushedPoints(cars, input$brush1,
                                  xvar = "dist", yvar = "speed")
    if(nrow(brushed_data) < 2){
      return(NULL)
    }
    lm(dist ~ speed, data = brushed_data)
  })
  output$slopeOut <- renderText({
    if(is.null(model())){
      "No model found"
    } else {
      model()[[1]][2]
    }
  })
  output$intOut <- renderText({
    if(is.null(model())){
      "No model found"
    } else {
      model()[[1]][1]
    }
  })
  output$plot1 <- renderPlot({
    plot(cars$dist, cars$speed, xlab = "Speed",
         ylab = "Distance", main = "Car speed",
         cex = 1.5, pch = 16, bty = "n")
    if(!is.null(model())){
      abline(model(), col = "blue", lwd = 2)
    }
  })
})

