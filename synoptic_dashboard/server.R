#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

theme_set(theme_bw())

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = 6)

        # This lets you stop the code and see what's in the environment
        #browser()
        
        # draw the histogram with the specified number of bins
        
        ggplot(faithful, aes(eruptions)) + 
          geom_histogram()

    })

})
