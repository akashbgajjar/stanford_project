library(shiny)
library(ggplot2)

#Directory where the Shiny scripts are located
#setwd("college-scorecard/Code/Shiny")

source("data.R")

shinyServer(function(input, output) {
  
  output$airPlot <- reactivePlot(function() {
    # body of the function
     # any correlation between graduation rate and cost?
    ggplot(MyData,aes_string(input$X_axis,input$Y_axis))+
      geom_point(size=4,color="purple")+geom_smooth(se=FALSE,method="lm")
        
    # print(plot)
  })
})

