
library(shiny)
library(tidyverse)
library(shinydashboard)
#1-A Application title

Header <- dashboardHeader(title = "Rental Analysis")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Davidson County Rents", tabName = "mnu_1_tab"),
    menuItem("Mean Rent by year", tabName = "mnu_2_tab"),
    menuItem("Increase in Rent by Zip",tabName = "widgets", badgeColor = "green"),
    menuItem("Compare Annual Rent Growth", tabName = "mnu_4_tab"),
    menuItem("Analyze Rent Changes", tabName = "mnu_5_tab")
  )
)

body <- dashboardBody(
  tabItems(
    #TAB1-BEGIN
    tabItem(tabName = "mnu_1_tab",
            #ROW1-BEGIN
            fluidRow(
              box(
                  title = "Davidson County Rents - Search", width = 6, solidHeader = TRUE, status = "primary",collapsible = TRUE,
                  selectInput("ddlBedrooms", label = "Select Bedrooms", width = "30%",
                              choices = list("1 Bedroom" = 1, "2 Bedroom" = 2, "3 Bedroom" = 3), 
                              selected = 3),
                  
                  selectInput("ddlZip", "Select Zip",width = "30%",
                              choices = c( dfRentalZip %>% 
                                             pull(ZipCode) %>% 
                                             unique() %>% 
                                             sort(),
                              selected = 37215
                              ),
                              multiple = TRUE
                  ),
                  selectInput("ddlYear", label = "Select Year",width = "30%",
                              choices=c(dfRentalYears),
                              selected = 2022)
              )

                     
              
            ),
            
            
            #ROW1-END
            
            #ROW2-BEGIN
            fluidRow(
              box(width=12, title = "Box Plot", solidHeader = TRUE,status = "primary",collapsible = TRUE,
                    plotOutput(outputId = "zipBoxPlot")
                  )
            ),
     
            #ROW2-END
          
            #ROW3-BEGIN
            fluidRow(
              box(width=12, title = "Search Results", solidHeader = TRUE,status = "primary",collapsible = TRUE,
                  br(),
                  dataTableOutput("DisplaydfRental")
              )
              
            ),
            #ROW3-END
            
    ),
    #TAB1 - END
    #TAB2-BEGIN
    tabItem(tabName = "mnu_2_tab",
            
    #T2- ROW1-BEGIN
    #ROW1-BEGIN
    
    fluidRow(
      box(
        title = "Mean Rent - Search", width = 4, solidHeader = TRUE, status = "primary", collapsible = TRUE,
        selectInput("m2_ddlBedrooms", label = "Select Bedrooms", 
                    choices = list("1 Bedroom" = 1, "2 Bedroom" = 2, "3 Bedroom" = 3), 
                    selected = 3),
        selectInput("m2_ddlYear", label = "Select Year",
                    choices=c(dfRentalYears),
                    selected = 2022)
      )
    ),
    #ROW1-END,
    #T2- ROW1-END
    
    #T2- ROW2-BEGIN
    fluidRow(
     
      box(width=12, title = "Search Results", solidHeader = TRUE,status = "primary",
        br(),
        dataTableOutput("DisplaydfMeanRental")
      )
    )
    #T2- ROW2-END
            
            
            
    ),
    #TAB2 - END
    
    #TAB3 - BEGIN
    tabItem(tabName = "widgets",
            fluidRow( 
              		   
              box(width=4, title = "Search - Change in Rent by Zip(3 Bedroom Homes)", solidHeader = TRUE,status = "primary",collapsible = TRUE,
                  #2 Input: Dropdownlist
                  selectInput("ddlYear_PctIncrease", label = "Select Year",
                              choices=c(dfRentalYears),
                              selected = 2022)
              )           

            ), 
            fluidRow(
              box(width=12, title = "Percentage Increase in Rents", solidHeader = TRUE,status = "primary",collapsible = TRUE,
                  plotOutput(outputId = "BarPlotPctIncreaseT3")                  
              )
            ),
            
            fluidRow(
              box(width=12, title = "Davidson County Areas", solidHeader = TRUE,status = "primary", collapsible = TRUE,
                  dataTableOutput("DisplaydfZipCodeCityArea")
              )
            ),
            fluidRow(
              box(width=12, title = "Mean Rents", solidHeader = TRUE,status = "primary", collapsible = TRUE,
                  plotOutput(outputId = "BarPlotMeanRentT3")
              )
            ),
            fluidRow(
              box(width=12, title = "Increase in rent by Zip - 2022", solidHeader = TRUE,status = "primary", collapsible = TRUE,
                  dataTableOutput("DisplaydfRentalPctIncrease2022")
              )
            ),
            # fluidRow(
            #   box(width=12, title = "Search Results", solidHeader = TRUE,status = "primary", collapsible = TRUE,
            #     dataTableOutput("DisplaydfRentalPctIncrease")
            #   )
            # )
    ),
    #TAB3 - END
    
    #TAB4 - BEGIN
    tabItem(tabName = "mnu_4_tab",
            fluidRow( 
              box(width=11, title = "3 Bedrooms Single Family Homes - Over All Rent change Over the Years", solidHeader = TRUE,status = "primary",
                  br(),
                  plotOutput(outputId = "ptOutOverAllRentComp", width = "70%", height = "400px",)
              )
            ),
            fluidRow(
              box(width=11, title = "Source table", solidHeader = TRUE,status = "primary",collapsible = TRUE,
                  br(),
                  dataTableOutput("DisplaydfOverAllRentCompT4_global")
              )
            )
    ),
    #TAB4 - END
    #TAB5 - BEGIN
    tabItem(tabName = "mnu_5_tab",
            
            #ROW0 - BEGIN
            fluidRow(
              box(width=12, title = "% Mean rent Changed in 5 years", solidHeader = TRUE,status = "primary", collapsible = TRUE,
                  plotOutput(outputId = "BarPlotPctChangeOverYears_5Yr")
              )
            ),
            #ROW0 - END
            
            # Create a new Row in the UI for selectInputs
            #ROW1 - BEGIN
            fluidRow(
              box(width=12, title = "% Mean rent Changed in 5 years", solidHeader = TRUE,status = "primary", collapsible = TRUE,
                  dataTableOutput("PctChangeOverYears_5YrCityArea_global_RDT") 
              )
            ),
            #ROW1 - END
            fluidRow(
              box(width=6, title = "Analyze Rents - Search ", solidHeader = TRUE,status = "primary", collapsible = TRUE,
                  selectInput("ddlZip_T5",width = "30%",
                              "Select Zip:",
                              c("All",
                                unique(as.character(dfRentalZip$ZipCode)))),
                  
                  selectInput("ddlYear_T5",width = "30%",
                              "Select Year:",
                              c("All",
                                unique(as.character(dfRentalYears$ListDateYear)))),
                  
                  selectInput("ddlArea_T5",width = "30%",
                              "Area:",
                              c("All",
                                unique(as.character(dfRentalArea$Area))))									   
              )
            ),
            #ROW2-BEGIN

            #ROW2-END
            fluidRow(
              box(width=12, title = "Rent by Year/s", solidHeader = TRUE,status = "primary", collapsible = TRUE,
                  dataTableOutput("DisplaydfOverAllRentCompT5_global") 
              )
            )
    )
    #TAB5 - END
    

  )
)

# Define UI for application 
shinyUI(
  dashboardPage(skin = "green",
                Header,
                sidebar,
                body
  )
)