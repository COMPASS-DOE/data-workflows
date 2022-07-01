#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(googledrive)
library(janitor)
library(purrr)
library(shiny)
library(tidyverse)

theme_set(theme_bw())

## Call global to read in all the functions
source("global.R")

# Define server logic
shinyServer(function(input, output) {
  
  ## Create reactive aquatroll dataframe
  reactive_df <- reactive({
    
    aquatroll <- process_the_troll()
    
    aquatroll
  })
  
  output$troll_table <- renderDataTable(reactive_df() %>% 
                                          tail(n = 10))
})
