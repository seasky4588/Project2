#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(GGally)

# data set only read in once
Data <- read_csv("insurance2.csv")
Data$sex <- as.factor(Data$sex)
Data$region <- as.factor(Data$region)
Data$smoker <- as.factor(Data$smoker)
Data$insuranceclaim <- as.factor(Data$insuranceclaim)

Data <- Data%>%mutate(lnCharges=log(charges + 1))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # create plot
  g <- ggplot(Data, aes(x=bmi, y=lnCharges))
  
  g + geom_point(size=3, aes(col=steps))
   

   ggpairs(Data)
   hist(log(Data$charges))
   
   fit <- lm(lnCharges ~ age*bmi*smoker*steps, data=Data); summary(fit)
   
   
   
   
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
})
