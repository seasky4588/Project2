
library(shiny)
library(tidyverse)
library(GGally)
library(DT)


# Read Data
Data <- read_csv("insurance.csv")
Data$sex <- as.factor(Data$sex)
Data$smoker <- as.factor(Data$smoker)
Data$region <- as.factor(Data$region)



# ShinyServer
shinyServer(function(input, output, session) {
  
  
  ## Tab1
  
  # create boxplot 
  
  output$boxPlot <- renderPlot({
    
    if(input$x_col=="sex"){
      g1 <- ggplot(Data, aes(x=sex, y=charges))
      g1 +geom_boxplot(aes(fill=sex)) + labs(title ="Medical charges by sex")
    } else if(input$x_col=="smoker") {
      g1 <- ggplot(Data, aes(x=smoker, y=charges))
      g1 +geom_boxplot(aes(fill=smoker)) +labs(title ="Medical charges by smoking status")
    } else {
      g1 <- ggplot(Data, aes(x=region, y=charges))
      g1 +geom_boxplot(aes(fill=region)) + labs(title ="Medical charges by region")
    }
  })
    
  
  # Allow save the plot 
  
  plotInput <- reactive({
    
    if(input$x_col=="sex"){
      g1 <- ggplot(Data, aes(x=sex, y=charges))
      g1 +geom_boxplot(aes(fill=sex)) + labs(title ="Medical charges by sex")
    } else if(input$x_col=="smoker") {
      g1 <- ggplot(Data, aes(x=smoker, y=charges))
      g1 +geom_boxplot(aes(fill=smoker)) +labs(title ="Medical charges by smoking status")
    } else {
      g1 <- ggplot(Data, aes(x=region, y=charges))
      g1 +geom_boxplot(aes(fill=region)) + labs(title ="Medical charges by region")
    }
  })
  
  
  output$downPlot <- downloadHandler(
    filename = function() { 
      paste("boxplot", ".png", sep="") 
      },
    content = function(file) {
      png(file) # open the png device
      print(plotInput())
      dev.off() # turn the device off
    })
  
  
  
  # filter Data
  
  getData <- reactive({
    newData <- Data %>% filter(sex == input$sex, region == input$region)
  })
  
  
  # create scatter plot
  output$Plot <- renderPlot({
    
    #get filtered data
    newData <- getData()
    
    #create plot
    g2 <- ggplot(newData, aes(x=age, y=charges))+ labs(title ="Medical charges by age")
    
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
    
  
  
  ## Tab2
  
  # Create the simple linear Regression
  
  
  
  ## Tab3
  
  # filter Data
  
  getData2 <- reactive({
    newData2 <- Data %>% filter(between(charges, input$charge[[1]],input$charge[[2]]))
  })
  
  
  
  output$slr <- renderPlot({
    
    #create plot
    
    #get filtered data
    newData2 <- getData2()
    
    if(input$x1=="age"){
      g3 <- ggplot(newData2, aes(x=age, y=charges))
      g3 + geom_point() + geom_smooth(method="lm")
    } else if(input$x1=="bmi") {
      g3 <- ggplot(newData2, aes(x=bmi, y=charges))
      g3 + geom_point() + geom_smooth(method="lm")
    } else if(input$x1=="steps")  {
      g3 <- ggplot(newData2, aes(x=steps, y=charges))
      g3 + geom_point() + geom_smooth(method="lm")
    } else{
      g3 <- ggplot(newData2, aes(x=children, y=charges))
      g3 + geom_point() + geom_smooth(method="lm")  
    }
   
       
  })
  
  output$slrPred <- renderPrint({
    
    #get filtered data
    newData2 <- getData2()
    
    
    slr <- lm(charges ~ age, newData2)
    summary(slr)
    predict(slr, data.frame(age=input$param1))
    
  })
  
  
  
  ## Tab4
  
  #create output of observations    
  output$table <- renderDT({
    datatable(Data)

  })
  
  # download the filtered data
  output$downData <- downloadHandler(
         filename = function() {
           paste("data-", Sys.Date(), ".csv", sep="")
          },
        content = function(file) {
        write.csv(Data, file)
  })
  
  

  
})