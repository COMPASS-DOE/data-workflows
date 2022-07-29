#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
<<<<<<< Updated upstream
    #tabItem(tabName = "sapflow", 
     #       dataTableOutput("sf_table")), 
    tabItem(tabName = "aquatroll", 
            selectInput("select", label = h3("Select box"), 
                        choices = list("WL (m below surface)" = "wl_below_surface_m", 
                                       "Temperature" = "temperature", 
                                       "DO (mg/L)" = "rdo_concen"), 
                        selected = "wl_below_surface_m"),
            plotlyOutput("troll_ts"),
             dataTableOutput("troll_table"))
  ), 
=======
    tabItems(

      tabItem(tabName = "dashboard",
              h2("Hi!")),

      ## Loading MSM data for no good reason
     tabItem(tabName = "sapflow",
            selectInput("selectsf", label = h3("Site:"),
                         choices = list("Old Woman Creek" = "OWC",
                                        "Portage River" = "PTR",
                                        "Crane Creek" = "CRC",
                                        "Moneystump Marsh" = "MSM",
                                        "Goodwin Island" = "GWI"),
                         selected = "GWI"),
            #dataTableOutput("sf_table"),
            plotlyOutput("sapflow_ts")),

     tabItem(tabName = "sapflow",
             selectInput("selectteros", label = h3("Site:"),
                         choices = list("Old Woman Creek" = "OWC",
                                        "Portage River" = "PTR",
                                        "Crane Creek" = "CRC",
                                        "Moneystump Marsh" = "MSM",
                                        "Goodwin Island" = "GWI"),
                         selected = "GWI"),
             #dataTableOutput("sf_table"),
             plotlyOutput("teros_ts")),

      tabItem(tabName = "troll",
              selectInput("select", label = h3("Select box"),
                          choices = list("Water Level (m below surface)" = "wl_below_surface_m",
                                         "Temperature" = "temperature",
                                         "DO (mg/L)" = "rdo_concen"),
                          selected = "wl_below_surface_m"),
              plotlyOutput("troll_ts"),
              dataTableOutput("troll_table"))
    )
  ),
>>>>>>> Stashed changes
  skin = "purple"
  
  )
