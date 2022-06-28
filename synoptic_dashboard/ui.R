#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = "COMPASS Synoptic"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("compass")),
      menuItem("Sapflow", tabName = "sapflow", icon = icon("tree")),
      menuItem("TEROS", tabName = "teros", icon = icon("temperature-high")),
      menuItem("AquaTroll", tabName = "troll", icon = icon("water")),
      menuItem("Battery", tabName = "battery", icon = icon("car-battery")),
      menuItem("Alerts", tabName = "alerts", icon = icon("comment-dots"))
      )
    ),
  dashboardBody(
    tabItem(tabName = "sapflow", 
            plotOutput("distPlot"))
  ), 
  skin = "purple"
  
  )
