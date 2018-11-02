#User Interface
#UI

## Required packages
library(shiny)
library(DT)
library(koboloadeR)
library(devtools)
library(shinythemes)
install_github("mrdwab/koboloadeR")

## Create the user interface
## One row across the top for logging in and retrieving the datasets
##   available via those login credentials.
## A sidebar panel that lets you select a dataset to load.
## A main panel that has two tabs, one that shows the ID and names of
##   the datasets available, one that will show the contents of the
##   requested dataset.

x <- TRUE

shinyUI(fluidPage(theme = shinytheme("united"),
                  headerPanel("CVAIII Data Viewer"),
                  fluidRow(
                    column(3, textInput("username", label = "Username", value = "reach_initiative_ukraine")),
                    column(3, passwordInput("password", label = "Password", value = "")),
                    column(4, textInput("api", label = "API", value = "kobohr")),
                    column(2, actionButton("listDatasets", "Access Datasets"))),
                  hr(),
                  conditionalPanel(
                    condition = "input.listDatasets == 0",
                    fluidRow(
                      column(6, 
                             h3("REACH CVA3"),
                             h3("Usage"),
                             p("Enter your", em("username"), ", ", em("password"), ", ", 
                               "and the ", em("API"), " that you want to use and click",
                               code("Access Datasets"), ". This will load the most recent data available."),
                             p("To down load the dataset, scroll to the bottom and select the format (CSV, PDF, etc.) ", 
                               code("OR"), " filter the data by date using the filter at the top of the table,
                               It may briefly display an error while the dataset is 
                               downloading.")
                             )
                             )
                      ),
                  conditionalPanel(
                    condition = "input.listDatasets != 0",
                    fluidRow(
                      
                      column(10,
                             tabsetPanel(
                               tabPanel( "Instructions", fluidRow(
                                 column(6, 
                                        h3("REACH CVA3"),
                                        h3("Usage"),
                                        p("Enter your", em("username"), ", ", em("password"), ", ", 
                                          "and the ", em("API"), " that you want to use and click",
                                          code("Access Datasets"), ". This will load the most recent data available."),
                                        p("To down load the dataset, scroll to the bottom and select the format (CSV, PDF, etc.) ", 
                                          code("OR"), " filter the data by date using the filter at the top of the table,
                                          It may briefly display an error while the dataset is 
                                          downloading.")
                                        )
                                        )),
                               #HH
                               
                               tabPanel(
                                 "HH", h3("CVA3 HouseHold Survey"),
                                 h4("Household location check"),
                                 fluidRow(
                                   column(4, wellPanel(
                                     dateInput('date1',
                                               label = 'Date input: yyyy-mm-dd',
                                               value = Sys.Date()
                                     ))),
                                   #column(12, leafletOutput("HHmap"), align="center"),
                                   column(12, leafletOutput("HHmap"))
                                 ),
                                 h4("Household Grouped Data"),
                                 dataTableOutput("HHgt"),
                                 h4("Household Data by Record for selected date"),
                                 dataTableOutput("HHt")

                                 ),
                               
                               #COMM
                               tabPanel(
                                 "Comm", h3("CVA3 Community Survey"),
                                 h4("Community location check"),
                                 fluidRow(
                                   column(4, wellPanel(
                                     dateInput('date2',
                                               label = 'Date input: yyyy-mm-dd',
                                               value = Sys.Date()
                                     ))),
                                   #column(12, leafletOutput("HHmap"), align="center"),
                                   column(12, leafletOutput("Commmap"))
                                 ),
                                 h4("Community Grouped Data"),
                                 dataTableOutput("Commgt"),
                                 h4("Community Data by Record for selected date"),
                                 dataTableOutput("Commt")

                               ),
                               #HEALTH
                               tabPanel(
                                 "Health", h3("CVA3 Health Survey"),
                                 h4("Health location check"),
                                 fluidRow(
                                   column(4, wellPanel(
                                     dateInput('date3',
                                               label = 'Date input: yyyy-mm-dd',
                                               value = Sys.Date()
                                     ))),
                                   #column(12, leafletOutput("HHmap"), align="center"),
                                   column(12, leafletOutput("Healthmap"))
                                 ),
                                 h4("Community Grouped Data"),
                                 dataTableOutput("Healthgt"),
                                 h4("Community Data by Record for selected date"),
                                 dataTableOutput("Healtht")

                               ),
                               #ADMIN
                               tabPanel(
                                 "Admin", h3("CVA3 Admin Survey"),
                                 h4("Admin location check"),
                                 fluidRow(
                                   column(4, wellPanel(
                                     dateInput('date4',
                                               label = 'Date input: yyyy-mm-dd',
                                               value = Sys.Date()
                                     ))),
                                   #column(12, leafletOutput("HHmap"), align="center"),
                                   column(12, leafletOutput("Adminmap"))
                                 ),
                                 h4("Admin Grouped Data"),
                                 dataTableOutput("Admingt"),
                                 h4("Admin Data by Record for selected date"),
                                 dataTableOutput("Admint")

                                 
                               ),
                               #SS
                               tabPanel(
                                 "SocialS", h3("CVA3 Social Security Survey"),
                                 h4("Admin location check"),
                                 fluidRow(
                                   column(4, wellPanel(
                                     dateInput('date5',
                                               label = 'Date input: yyyy-mm-dd',
                                               value = Sys.Date()
                                     ))),
                                   #column(12, leafletOutput("HHmap"), align="center"),
                                   column(12, leafletOutput("SSmap"))
                                 ),
                                 h4("Social Security Grouped Data"),
                                 dataTableOutput("SSgt"),
                                 h4("Social Security Data by Record for selected date"),
                                 dataTableOutput("SSt")

                                 
                               ),
                               #EDUCATION
                               tabPanel(
                                 "Education", h3("CVA3 Education Survey"),
                                 h4("Education location check"),
                                 fluidRow(
                                   column(4, wellPanel(
                                     dateInput('date6',
                                               label = 'Date input: yyyy-mm-dd',
                                               value = Sys.Date()
                                     ))),
                                   #column(12, leafletOutput("HHmap"), align="center"),
                                   column(12, leafletOutput("Edumap"))
                                 ),
                                 h4("Education Grouped Data"),
                                 dataTableOutput("Edugt"),
                                 h4("Education Data by Record for selected date"),
                                 dataTableOutput("Edut")

                                 
                               ),
                               #Admin Stats
                               tabPanel(
                                 "AdminStat", h3("CVA3 AdminStat Survey"),
                                 h4("AdminStat Data by Record"),
                                 dataTableOutput("AdminSt")
                                 
                               )
                             )
                      )
                    )
                  ),
                  ## This is to fix the alignment of the "listDatasets" button with
                  ##   the rest of the login details.
                  tags$style(type='text/css', 
                             "#listDatasets { width:100%; margin-top: 25px;}")
                  )
        )