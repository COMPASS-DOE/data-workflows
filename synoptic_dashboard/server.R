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

<<<<<<< Updated upstream
    #aquatroll <- read_csv("./test_data/aquatroll.csv")
    aquatroll <- process_the_troll()
    teros <- process_teros()
    sapflow <- process_sapflow()
=======
    # sapflow <- withProgress(process_sapflow(), message = "Updating sapflow...") %>%
     #    filter(Timestamp > two_weeks_ago)
     sapflow <- withProgress(process_sapflow(), message = "Updating sapflow...") %>%
         filter(Timestamp > "2022-07-29")
     #teros <- withProgress(process_teros(), message = "Updating TEROS...") %>%
     #    filter(TIMESTAMP > two_weeks_ago)
     teros <- withProgress(process_teros(), message = "Updating TEROS...") %>%
         filter(TIMESTAMP > "2022-07-29")
     #aquatroll <- withProgress(process_the_troll(), message = "Updating AquaTroll...") %>%
     #    filter(datetime > two_weeks_ago)
     aquatroll <- withProgress(process_the_troll(), message = "Updating AquaTroll...") %>%
         filter(datetime > "2022-07-29")
    # aquatroll <- readRDS("./test_data/aquatroll.rds")
    # teros <- readRDS("./test_data/teros.rds")
    # sapflow <- readRDS("./test_data/sapflow.rds")
>>>>>>> Stashed changes

    #x <<- teros
   #browser()
    #x <<- aquatroll
    #list(aquatroll = aquatroll,
    #     teros = teros,
    #     sapflow = sapflow)
  })

<<<<<<< Updated upstream
  output$troll_table <- renderDataTable(reactive_df()$aquatroll %>%
                                          tail(n = 10))
=======
  # output$sf_table <- renderDataTable(reactive_df()$sapflow %>%
  #                                         tail(n = 10))
  #
  # output$troll_table <- renderDataTable(reactive_df()$aquatroll %>%
  #                                         tail(n = 10))
  #
  # output$teros_table <- renderDataTable(reactive_df()$teros %>%
  #                                         tail(n = 10))
>>>>>>> Stashed changes

  output$troll_ts <- renderPlotly({

    b <- reactive_df()$aquatroll %>% filter(datetime > two_weeks_ago) %>%
      ggplot(aes(x = datetime, y = input$select, color = location)) +
      geom_line() +
      facet_wrap(~site, ncol = 1, scales = "free") +
      labs(x = "")

    ggplotly(b)
    }
  )
<<<<<<< Updated upstream
=======

  # output$teros_ts <- renderPlotly({
  #
  #     t <- reactive_df()$teros %>%
  #         filter(!is.na(variable)) %>%
  #         filter(Site == input$selectteros) %>%
  #         ggplot(aes(x = TIMESTAMP, y = value, color = as.factor(Port)), group = Location) +
  #         geom_line() +
  #         facet_wrap(~variable, scales = "free", ncol = 1)
  #
  #     ggplotly(t)
  # })


  output$sapflow_ts <- renderPlotly({

      s <- reactive_df()$sapflow %>%
          filter(Site == input$selectsf) %>%
          group_by(Site, Location, Port) %>%
          drop_na(Value) %>%
          ggplot(aes(x = Timestamp, y = Value, color = as.factor(Port)), group = Location) +
          geom_line() +
          facet_wrap(~Location) +
          labs(color = "Port", ncol = 1)

      ggplotly(s)
  })

>>>>>>> Stashed changes
})
