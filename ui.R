# LIBRARIES

library(dplyr)
library(tibble)
library(ggplot2)
library(shinyjs)
library(shinyWidgets)
library(ggiraph)
library(tidyr)
library(stringr)

# UI
ui <- fluidPage(
  
  useShinyjs(),
  chooseSliderSkin("Flat"),
  setSliderColor(rep("LightSlateGray", 10), seq(1, 10, 1) ),
  
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="style.css")
  ),
  
  titlePanel(title = "Science Based Target Tool",
             windowTitle = "Science Based Target Tool"),
  
  sidebarLayout(position = "left",
                
                # SIDEBAR PANEL
                
                sidebarPanel(width = 2,
                  selectInput(inputId = "target_setting_method",
                              label = "Target setting method",
                              choices = c("Absolute Contraction Approach", "Sectoral Decarbonization Approach"),
                              selected = "Absolute Contraction Approach",
                              selectize = T),
                  selectInput(inputId = "sda_scenario",
                              label = "SDA scenario",
                              choices = c("ETP B2DS", "SBTi 1.5C"),
                              selected = "ETP B2DS",
                              selectize = T),
                  selectInput(inputId = "sda_sector",
                              label = "SDA sector",
                              choices = c("Power", "Iron and steel", "Cement", "Aluminium", "Pulp and paper", "Services - Buildings"),
                              selected = "",
                              selectize = T),
                  selectInput(inputId = "base_year",
                              label = "Base year",
                              choices = c(seq(2014, 2021)),
                              selected = "",
                              selectize = T),
                  sliderInput(inputId = "target_year",
                              label = "Target year",
                              min = 2023,
                              max = 2050,
                              value = 2050,
                              step = 1,
                              sep = ""),
                  selectInput(inputId = "projected_output_measure",
                              label = "Projected output measure",
                              choices = c("Growth rate", "Fixed market share", "Target year output"),
                              selected = "",
                              selectize = T),
                  numericInput(inputId = "base_year_output",
                               label = "Base year output",
                               value = "",
                               min = 0),
                  numericInput(inputId = "growth",
                               label = "Forecasted growth",
                               value = "",
                               min = 0),
                  numericInput(inputId = "scope_1_emissions",
                               label = "Scope 1 emissions",
                               value = "",
                               min = 0),
                  numericInput(inputId = "scope_2_emissions",
                               label = "Scope 2 emissions",
                               value = "",
                               min = 0)
                ),
                
                # MAIN PANEL
                
                mainPanel(
                  
                  girafeOutput("main_plot", height = "100%"),
                  
                  fluidRow(
                    column(7,
                  
                      dataTableOutput("results_table")
                      
                    )
                    
                  )
                  
                  
                )
  )
  
)