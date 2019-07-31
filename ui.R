

library(shiny)
library(ggplot2)
library(shinydashboard)


# Read Data
Data <- read_csv("insurance.csv")
Data$sex <- as.factor(Data$sex)
Data$smoker <- as.factor(Data$smoker)
Data$region <- as.factor(Data$region)


dashboardPage(skin="blue",
  # add title
  dashboardHeader(title ="Predictive Model for Medical Charges", titleWidth = 750),
  
  # define sidebar items
  dashboardSidebar(sidebarMenu(
    menuItem("About", tabName ="about", icon= icon("comment-dots")),
    menuItem("Data Exploration", tabName="Start", icon= icon("laptop")),
    menuItem("Unsupervised Learning", tabName="PCA", icon= icon("brain")),
    menuItem("Modeling", tabName="Modeling", icon= icon("chart-bar")),
    menuItem("Data", tabName="Data", icon= icon("book"))
  )),
  
  # define the body of the app
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName= "about",
        fluidRow(
          # two columns for each of the two items
          column(6,
                h1("About the Data"),
                # box to contain description
                box(background = "purple", width=12,
                    h4("This data is a sample dataset of Personal Medical Cost(1,338 Observations) in US."),
                    h4("It includes nine informations which are age, sex, bmi, steps, childeren, smoker, region and charges,"),
                    h4("The intersted value is charges(reponse, y), individual medical costs billed by health insurance."),
                    h4("The purpose is making a prediction model with related variables."),
                    br(),
                    h4("[ Variables information ]"),
                    h5("- age : age of policyholder"),
                    h5("- sex: gender of policyholder"), 
                    h5("- bmi: body mass index (kg / m^2)"),
                    h5("- steps: average walking steps per day of policyholder"), 
                    h5("- children: number of children/dependents of policyholder"),
                    h5("- smoker: smoking state of policyholder (non-smoker=no, smoker=yes)"), 
                    h5("- region: the residential area of policyholder in the US"),
                    h5("- charges: individual medical costs billed by health insurance"),
                    a(strong("This data set was found from Kaggle (click here) "), href="https://www.kaggle.com/datasets")
                        
                    ) # end box
                 ), # end column
           column(6,
                 h1("How to use the app?"),
                 # box to contain description
                 box(background = "yellow", width=12,
                     h4("This application consists of four pages."),
                     h4("The first page is data exloration. In this page, the user can create simple numerical and graphical summaries.
                         The boxplot tab shows medical charges distribution by the variable which the user select for x-value.
                         And scatterplot tab shows the relationship between age and charges with smoking and steps status if user selects."),
                     h4("The second page is unsupervised learning, there is no outcome measure, and the goal is to describe the associations 
                         and patterns among a set of input measure. In this page, the user can do the principal components analysis(PCA)."),
                     h4("The third page is Modeling, the goal is to predict the value of an outcome measure based on a number of 
                         input measures. In first tab, the user can make a simple linear regression(SLR) to predict the medical cost.
                         The second tab, user can try multiple linear regression(MLR) by adding categorical parameter."),
                     h4("The last page is Data, user can scroll through the data and download."),
                     a(strong("If you want to see R code, visit the github page (click here)"), href="https://github.com/seasky4588/Project3")
                     ) # end box
                 ) # end column
                ) # end fluid Row 
              ), # end tabItem
      
      tabItem(tabName= "Start",
        fluidRow(
          column(3,
                br(), 
                box(background = "yellow", width=12, title=strong("Select independent value(x) for Box Plot"),
                    selectizeInput("x_col", "x_value", selected = "smoker", choices = c("sex","smoker","region"))
                 ), 
                br(),
                box(background = "yellow", width=12, title=strong("Select sex and region of policyholder for Scatter Plot"),
                    selectizeInput("sex", "Sex", selected = "female", choices = levels(as.factor(Data$sex)),multiple = TRUE),
                    selectizeInput("region", "Residential area", 
                                   selected = "southeast", choices = levels(as.factor(Data$region)), multiple = TRUE),
                    sliderInput("size", "Size of Points on Scatter Plot",
                                min = 1, max = 10, value = 5, step = 1),
                    checkboxInput("smoke", h4("Seperate Smoking status", style = "color:blue;")), 
                    
                    # conditionalPanel 
                    conditionalPanel(condition="input.smoke",
                                     checkboxInput("steps", h4("Color Code averaging walking steps per day", 
                                                               style = "color:red;")))
                    )
                  
                  ), #end column
           column(9, 
                  tabsetPanel(
                    tabPanel(strong("BoxPlot"),
                        fluidRow(
                            column(12,
                                  h4("1-1. The Box Plot:"),
                                  p(class = 'text-right', downloadButton('downPlot ', 'Download Plot')),
                                  plotOutput("boxPlot"),
                                  br(),
                                  h4("1-2. Numerical Summary:"),
                                  textOutput("info"))
                             )), # end tab panel
                    tabPanel(strong("Scatter Plot"), 
                        fluidRow(
                            column(12,
                                  h4("2-1. The Scatter Plot :"), 
                                  plotOutput("Plot", click = "plot_click"),
                                  br(),
                                  h4("2-2. The region values where you click on the plot: "),
                                  verbatimTextOutput("infoPlot"))
                            )) # end tab panel
                          ) # end tabset Panel
                 ) # end column
                ) # end fluidRow  
              ), # end tabItem 
      
      tabItem(tabName= "PCA",
              fluidRow(
                column(4,
                      br(),
                      box(background = "purple", width=12, title=strong("Principle Components Analysis (PCA)"),
                          h4("PCA is a dimension reduction technique."),
                          h4("If you have p variables, they contain some joint variability/correlation."),
                          h4("PCA looks for linear combinations of those p variables that account for most the variability."), 
                          h4("Usually can represent variability of the p variables by m < p PCs.")
                          ),
                      box(background = "yellow", width=12, title=strong("Select independent values for PCA "),
                          selectizeInput("PCs", "Variables", selected = "bmi", choices = c("bmi", "steps", "children"), multiple=TRUE))
                      ),
                column(8,
                       tabsetPanel(
                         tabPanel(strong("BiPlot"),
                                  fluidRow(
                                    column(12,
                                           h4("1. PCA Biplot :"),
                                           plotOutput("Biplot")), 
                                           verbatimTextOutput("infoPCs")
                                  )), # end tab panel
                         tabPanel(strong("ScreePlot"), 
                                  fluidRow(
                                    column(12,
                                           h4("2. PCA ScreePlot :"), 
                                           plotOutput("Screeplot"))
                                )) # end tab panel
                             ) # end tabset Panel
                     ) # end column
                   ) # end fluidRow
              ), # end tabItem
      
      tabItem(tabName= "Modeling",
              fluidRow(
                column(3,
                       br(),
                       box(background = "yellow", width=12, title=strong("Select the range of Charges"),
                           sliderInput("charge", "y",
                                   min = round(min(Data$charges),0), max = round(max(Data$charges),0), value = c(min, max))),
                       box(background = "yellow", width=12, title=strong("Select the first parameter value"),
                           selectizeInput("x1", "x1", selected = "age", choices = c("age","bmi"))
                       ),
                       box(background = "yellow", width=12, title=strong("Select the second parameter value for MLR"),
                           selectizeInput("x2", "x2", selected = "smoker", choices = c("region", "smoker", "sex"))
                       ),
                       box(background = "purple", width=12, title=strong("Put new parameter values for Prediction"),
                           numericInput("new_age", "new_age", value=30),
                           numericInput("new_bmi", "new_bmi", value=20),
                           selectizeInput("new_region", "new_region", selected="southeast", choice=levels(as.factor(Data$region))),
                           selectizeInput("new_smoker", "new_smoker", selected="no", choice=c("no", "yes")),
                           selectizeInput("new_sex", "new_sex", selected="female", choice=c("female", "male"))
                       )
                      ), # end column
                column(9,
                       tabsetPanel(
                         tabPanel(strong("SLR"),
                                  fluidRow(
                                    column(12,
                                           h4("1-1. Simple Linear Regression Model:"),
                                           withMathJax(), helpText(h3('$$y = \\beta_0 + \\beta_1*x_1 + e$$')), ## add in latex functionality
                                           br(),
                                           plotOutput("slr"),
                                           br(),
                                           h4("1-2. Predict the medical charges with above model:"),
                                           textOutput("infoSlr"),
                                           verbatimTextOutput("slrPred"))
                                  )), # end tab panel
                         
                         
                         tabPanel(strong("MLR"), 
                                  fluidRow(
                                    column(12,
                                           h4("2-1. Multiple Linear Regression Model:"),
                                           helpText(h3('$$y = \\beta_0 + \\beta_1*x_1 + \\beta_2*x_2 + \\beta_3*x_1*x_2 + e$$')),
                                           br(),
                                           plotOutput("mlr"),
                                           br(),
                                           h4("2-2. Predict the medical charges with above model:"),
                                           textOutput("infoMlr"),
                                           verbatimTextOutput("mlrPred"))
                                  )) # end tab panel
                               ) # end tabset Panel
                    ) # end column
                   ) # end fluidRow
              ), # end tabItem
      
      tabItem(tabName= "Data",
              fluidRow(
                column(12,
                       h4("1. Data Table: "),
                       p(class = 'text-right', downloadButton('downData', 'Download Data')),
                       DT::dataTableOutput("table")
                       ) # end column
                      ) # end fluidRow
               ) # end tabItem
      
          ) # end tabItems
      ) # end DashboardBody
    ) # end Dashborad Page
  