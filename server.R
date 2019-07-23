
library(shiny)
library(tidyverse)
library(GGally)


# Read Data
Data <- read_csv("insurance.csv")
Data$sex <- as.factor(Data$sex)
Data$smoker <- as.factor(Data$smoker)
Data$region <- as.factor(Data$region)

# Add ln(charges) 
Data <- Data%>%mutate(lnCharges=log(charges + 1))



# ShinyServer
shinyServer(function(input, output, session) {
  
  getData <- reactive({
    
    newData <- Data %>% filter(sex == input$sex, region == input$region)
 
     })
  
  
  #create plot
  output$Plot <- renderPlot({
    
    #get filtered data
    newData <- getData()
    
    #create plot
    g <- ggplot(newData, aes(x=age, y=lnCharges))
    
    if(input$smoke&input$steps){
      g + geom_point(size=3, aes(col=steps))+facet_wrap(~smoker)
    } else if(input$smoke) {
      g + geom_point(size=3)+facet_wrap(~smoker, labeller = label_both)
    } else {
      g + geom_point(size = input$size)
    }
  })
  
  
  
  # update the sliderInput
  observe({
    if(input$steps){
      updateSliderInput(session, "size", min=3)
    } else {
      updateSliderInput(session, "size", min=1)
    }
  })
  
  
  #create text info
  output$info <- renderText({
    #get filtered data
    newData <- getData()
    
    paste("The average individual medical costs billed by health insurance for", input$sex, " ", "in", " ", input$region," ", "is", round(mean(newData$charges, na.rm = TRUE), 2), sep = " ")
  })
  
  #create output of observations    
  output$table <- renderTable({
    getData()

  })
  
  # update the title using renderUI() and uiOutput()
  output$title <- renderUI({
    h1(paste0("Analysis", " ",  "of Insurance Cost Data", " ", "for", " ", toupper(substring(input$sex,1,1)),substring(input$sex,2)," ", "in"," ", toupper(substring(input$region,1,1)),substring(input$region,2)))
  })
  
})