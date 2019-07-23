
library(shiny)
library(ggplot2)
library(shinydashboard)


dashboardPage(skin="yellow",
  # add title
  dashboardHeader(title ="Project3", titleWidth = 750),
  
  # define sidebar items
  dashboardSidebar(sidebarMenu(
    menuItem("About", tabName ="about", icon= icon("archive")),
    menuItem("Application", tabName="app", icon= icon("laptop"))
  )),
  
  # define the body of the app
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName= "about",
        fluidRow(
        )),
      tabItem(tabName= "app",
        fluidRow(
          column(3,
                box(width=12, title="Select sex and region of policyholder",
                    selectizeInput("sex", "Sex", selected = "female", choices = levels(as.factor(Data$sex))),
                    selectizeInput("region", "Residential area of policyholder", 
                                   selected = "southeast", choices = levels(as.factor(Data$region)))
                    ),
                sliderInput("size", "Size of Points on Graph",
                            min = 1, max = 10, value = 5, step = 1),
                br(),
                checkboxInput("smoke", h4("Seperate Smoking status", style = "color:blue;")), 
                
                # conditionalPanel 
                conditionalPanel(condition="input.smoke",
                                 checkboxInput("steps", h4("Color Code averaging walking steps per day", 
                                                           style = "color:red;")))
                  
                ),
           column(9, 
                  tabsetPanel(
                    tabPanel("data exploration",
                        fluidRow(
                            column(12,
                                  plotOutput("Plot"),
                                  br(),
                                  textOutput("info"))
                             )), # end tab panel
                    tabPanel("clustering", 
                        fluidRow()), # end tab panel
                    tabPanel("Modeling",
                        fluidRow()), # end tab panel
                    tabPanel("Data",
                        fluidRow(
                           column(12,
                                  tableOutput("table"))
                        )) # end tab panel
                    
                  ) # end tabset Panel
                ) # end column
              ) # end fluidRow
            ) # end tabItem    
          ) # end tabItems
      
      ) # end DashboardBody
    ) # end Dashborad Page
  