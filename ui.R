
library(shiny)
library(ggplot2)
library(shinydashboard)


dashboardPage(skin="blue",
  # add title
  dashboardHeader(title ="Predictive Model for Medical Charges", titleWidth = 750),
  
  # define sidebar items
  dashboardSidebar(sidebarMenu(
    menuItem("About", tabName ="about", icon= icon("comment-dots")),
    menuItem("Application", tabName="app", icon= icon("chart-bar"))
  )),
  
  # define the body of the app
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName= "about",
        fluidRow(
          # add in latex functionality if needed
          withMathJax(),
          
          # two columns for each of the two items
          column(6,
                h1("What does this app do?"),
                # box to contain description
                box(background = "purple", width=12,
                    h4("This application visulalizes some analyses with health dataset"),
                    h4("There are two types of statistical methods, unsupervised and supervised method."),
                    h4("Unsupervised model is ~~"),
                    h4("Supervised moedel is ~~")
                    ) # end box
                 ), # end column
           column(6,
                 h1("How to use the app?"),
                 # box to contain description
                 box(background = "yellow", width=12,
                     h4("This application visulalizes some analyses with health dataset"),
                     h4("There are two types of statistical methods, unsupervised and supervised method."),
                     h4("Unsupervised model is ~~"),
                     h4("Supervised moedel is ~~")
                     ) # end box
                 ) # end column
          
         ) # end fluid Row 
        ), # end tabItem
      tabItem(tabName= "app",
        fluidRow(
          column(3,
                br(), 
                box(width=12, title="Select indenpent value(x) for Box Plot",
                    selectizeInput("x_col", "x_value", selected = "smoker", choices = c("sex","smoker","region"))
                 ), 
                br(),
                box(width=12, title="Select sex and region of policyholder for Scatter Plot and Data",
                    selectizeInput("sex", "Sex", selected = "female", choices = levels(as.factor(Data$sex)),multiple = TRUE),
                    selectizeInput("region", "Residential area", 
                                   selected = "southeast", choices = levels(as.factor(Data$region)), multiple = TRUE),
                    br(),
                    sliderInput("size", "Size of Points on Scatter Plot",
                                min = 1, max = 10, value = 5, step = 1),
                    br(),
                    checkboxInput("smoke", h4("Seperate Smoking status", style = "color:blue;")), 
                    
                    # conditionalPanel 
                    conditionalPanel(condition="input.smoke",
                                     checkboxInput("steps", h4("Color Code averaging walking steps per day", 
                                                               style = "color:red;")))
                    )
                  
                ),
           column(9, 
                  tabsetPanel(
                    tabPanel("data exploration",
                        fluidRow(
                            column(12,
                                  plotOutput("boxPlot"),
                                  br(),
                                  textOutput("info"),
                                  br(),
                                  plotOutput("Plot")
                             ))), # end tab panel
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
  