library(shiny)
library(shinydashboard)
library(tidyverse)
library(glue)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(plotly)
library(wordcloud2)
library(tidytext)
library(tm)
library(tools)
library(broom)
library(highcharter)
library(leaflet)
library(leaflet.extras)

# library(ggrepel)
# library(highcharter)

# Tab with dreams, priorities, barriers visuals
super_sat_tab <- tabItem(
  "sat_tab", 
  h3("Estimated Enrollment for:"), 
  h3(textOutput("lea_text")),
  h4(textOutput("cities_text")),
  fluidRow(
    column(
      # box(
      # actionButton("count", "Count"),
      valueBoxOutput("enrollment_value", width = 12),
      # width = 6 # Width of Value box
      # ),
      
      tabBox(
        # title = "plots", 
        tabPanel(
          "Population-Enrollment Relationship", 
          highchartOutput("regression_plot"), 
          h4("Use the graph above to evaluate how well city populations correlate to enrollment")
        ),
        tabPanel(
          "Enrollment Yr to Yr", 
          highchartOutput("enr_time"), 
          h4("The graph above to shows enrollment trends by year") 
        ), 
        width = 12
        
        # box(
        #   highchartOutput("regression_plot"),
        #   width = 12 # Width of plot
        # ),
        # box(
        #   highchartOutput("enr_time"),
        #   width = 12 # Width of plot
      ),
      width = 6 # Column width
      
      
      
    ), 
    column(
      box(
        leafletOutput("map"), 
        fileInput("geocode_file", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        width = 12
      ),
      width = 6
    )
  ) # End of top row
  # End of bottom row
)

# Tab for Listening Tours
listening_tab <- tabItem("listen_tab")
# Tab for AFDEA Survey
sbg_survey_tab <- tabItem("sbg_tab")
# Tab for Academic Summit
acad_summit_tab <- tabItem(
  "summit_tab", 
  fluidRow()
)
# Tab for All Data Sources
all_sources_tab <- tabItem(
  "all_data_tab", 
  fluidRow(
    # box(dataTableOutput("ml_data_dt"), width = 12)
    box(dataTableOutput("all_sources_dt"), width = 12)
  )
)

pop_yr_tab <- tabItem(
  "pop_yr_tab", 
  fluidRow(
    box(dataTableOutput("table_of_data"), width = 12)
  )
)

chart_tab <- tabItem(
  "chart_tab",
  fluidRow(
    # box(highchartOutput("chart1"), width = 12)
    # box(leafletOutput("map"), width = 12)
  )
)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  # Header----  
  header = dashboardHeader(title = "Enrollment Estimator"), # end of header
  
  # Sidebar----- 
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebar", 
      menuItem("Super Saturday", tabName = "sat_tab"), 
      # menuItem("SBG Survey", tabName = "sbg_tab"),
      # menuItem("All Data Sources", tabName = "all_data_tab"), 
      menuItem("Full Data Sources", tabName = "all_data_tab"),
      menuItem("Pop and Year", tabName = "pop_yr_tab"),
      menuItem("Chart Example", tabName = "chart_tab"),
      conditionalPanel(
        'input.sidebar == "sat_tab"', 
        # checkboxGroupInput(
        #   inputId = "topics", 
        #   label = "Select Topics:", 
        #   choices = unique(super_saturday_feedback$topic_description)[!is.na(super_saturday_feedback$topic_description)],
        #   selected = unique(super_saturday_feedback$topic_description)[!is.na(super_saturday_feedback$topic_description)]
        # )
        uiOutput("city_choices"), 
        uiOutput("lea_choices")
      )
    )
  ), # end of sidebar
  
  # Body----
  body = dashboardBody(
    tabItems(
      super_sat_tab, 
      # listening_tab, 
      # sbg_survey_tab, 
      # acad_summit_tab, 
      all_sources_tab, 
      chart_tab,
      pop_yr_tab 
      
    )
  ), # end of body
  
  skin = "black"
  
) # end of dashboardPage
