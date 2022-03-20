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

# library(ggrepel)
# library(highcharter)

# Tab with dreams, priorities, barriers visuals
super_sat_tab <- tabItem(
  "sat_tab", 
  fluidRow(
    box() 
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
all_sources_tab <- tabItem("all_data_tab")

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
      menuItem("Full Data Sources", tabName = "summit_tab"),
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
      listening_tab, 
      sbg_survey_tab, 
      acad_summit_tab
    )
  ), # end of body
  
  skin = "black"
  
) # end of dashboardPage
