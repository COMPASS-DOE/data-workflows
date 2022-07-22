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

     sapflow <- withProgress(process_sapflow(), message = "Updating sapflow...") %>%
         fitler(Timestamp > two_weeks_ago)
     teros <- withProgress(process_teros(), message = "Updating TEROS...") %>%
         filter(TIMESTAMP > two_weeks_ago)
     aquatroll <- withProgress(process_the_troll(), message = "Updating AquaTroll...") %>%
         filter(datetime > two_weeks_ago)
    #aquatroll <- read_csv("./test_data/aquatroll.csv")
    #teros <- read_csv("./test_data/teros.csv")
    #sapflow <- read_csv("./test_data/sapflow.csv")


    #browser()
    #x <<- aquatroll
    list(aquatroll = aquatroll,
         teros = teros,
         sapflow = sapflow)
  })

  output$sf_table <- renderDataTable(reactive_df()$sapflow %>%
                                          tail(n = 10))

  output$troll_table <- renderDataTable(reactive_df()$aquatroll %>%
                                          tail(n = 10))

  output$teros_table <- renderDataTable(reactive_df()$teros %>%
                                          tail(n = 10))

  output$troll_ts <- renderPlotly({

    b <- reactive_df()$aquatroll %>%
      ggplot(aes_string(x = "datetime", y = input$select, color = "location")) +
      geom_line() +
      facet_wrap(~site, ncol = 1, scales = "free") +
      labs(x = "")

    ggplotly(b)
    }
  )

  output$sapflow_ts <- renderPlotly({

      s <- reactive_df$sapflow() %>%
          ggplot(aes_string(x = "Timestamp", y = input$selectsf, color = "Location")) +
          geom_line() +
          facet_wrap(~Site, ncol = 1, scales = "free")

      ggplotly(s)
  })

})
