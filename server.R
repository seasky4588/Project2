
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
  
  
  
  output$boxPlot <- renderPlot({
    
    #create plot
    
    if(input$x_col=="sex"){
      g1 <- ggplot(Data, aes(x=sex, y=charges))
      g1 +geom_boxplot(aes(fill=sex))
    } else if(input$x_col=="smoker") {
      g1 <- ggplot(Data, aes(x=smoker, y=charges))
      g1 +geom_boxplot(aes(fill=smoker))
    } else {
      g1 <- ggplot(Data, aes(x=region, y=charges))
      g1 +geom_boxplot(aes(fill=region))
    }
  })
    
  
  
  output$Plot <- renderPlot({
    
    #get filtered data
    newData <- getData()
    
    #create plot
    g2 <- ggplot(newData, aes(x=age, y=charges))
    
    if(input$smoke&input$steps){
      g2 + geom_point(size=3, aes(col=steps))+facet_wrap(~smoker)
    } else if(input$smoke) {
      g2 + geom_point(size=3)+facet_wrap(~smoker, labeller = label_both)
    } else {
      g2 + geom_point(size = input$size)
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
  
  
  #create numeric summaries
  output$info <- renderText({
    #get filtered data
    newData <- getData()
    
    if(input$x_col=="sex"){
      paste("The average individual medical costs billed by health insurance for female is ", round(Data%>%filter(sex=="female")%>%summarise(mean(charges, na.rm=TRUE)), 2), 
            "and for male is ",round(Data%>%filter(sex=="male")%>%summarise(mean(charges, na.rm=TRUE)), 2), sep = " ")
    }else if(input$x_col=="smoker"){paste("The average individual medical costs billed by health insurance for non-smoker is ", round(Data%>%filter(smoker=="no")%>%summarise(mean(charges, na.rm=TRUE)), 2), 
                     "and for smoker is ",round(Data%>%filter(smoker=="yes")%>%summarise(mean(charges, na.rm=TRUE)), 2), sep = " ")
    }else{ 
      paste("The average individual medical costs billed by health insurance for northeast is ", 
             round(Data%>%filter(region=="northeast")%>%summarise(mean(charges, na.rm=TRUE)), 2), 
            ", for northwest is ",round(Data%>%filter(region=="northwest")%>%summarise(mean(charges, na.rm=TRUE)), 2), 
            ", for southeast is ",round(Data%>%filter(region=="southeast")%>%summarise(mean(charges, na.rm=TRUE)), 2),
            "and for southwest is ",round(Data%>%filter(region=="southwest")%>%summarise(mean(charges, na.rm=TRUE)), 2),
            sep = " ")
    }
    
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