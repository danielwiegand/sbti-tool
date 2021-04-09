"This file provides the UI of the app"

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
ui <- navbarPage("Science Based Targets Tool",
                 
                 tabPanel("Quick guide",
                          
                          fluidRow(
                            column(3),
                            column(6,
                                   
                                   HTML("<br /><b>DISCLAIMER:  This app is not officially supported by the SBTi and still in an experimental state!</b><br /><br />"),
                                   tags$br(),
                                   h2("Using the SBT tool"),
                                   HTML("<ol><li>Select a target-setting method<br />
                     <!--<tab>- Methods available in the SBT tool are Sectoral Decarbonization Approach and Absolute Contraction Approach<br />
                     <tab>- Methods available in the scope 3 tool are Absolute Contraction Approach, Economic intensity and Physical intensity-->
                     <li>Enter emissions and activity data, as required by the method selected</li>
                     <li>Review the summary of results available from the selected SBT method</li>
                     <li>You can review the full data set of target modelling results</li></ol>
                     "),
                                   tags$br(),
                                   h2("Online resources"),
                                   HTML("<ul><li><a href = 'https://sciencebasedtargets.org/'>Science Based Targets initiative website</a></li>
                          <li>SBTi Step by step guide</li>
                          <li>SBTi Call to action guidelines</li>
                          <li>SBTi Criteria and recommendations</li>
                          <li>Target setting methods</li>
                          <li>Target Validation Protocol</li>
                          <li>Foundations of Science-based Target Setting</li>
                          <li>SBT resources for transport</li>
                          <li>SBTi Quick Start Guide for Electric Utilities</li></ul>
                          ")
                                   
                            ),
                            column(3)
                            
                          )
                          
                          
                 ),
                 
                 tabPanel("SBT Tool",
                          
                          useShinyjs(),
                          chooseSliderSkin("Flat"),
                          setSliderColor(rep("LightSlateGray", 10), seq(1, 10, 1) ),
                          
                          tags$head(
                            tags$link(rel="stylesheet", type="text/css", href="style.css")
                          ),
                          
                          # titlePanel(title = "Science Based Target Tool",
                          #            windowTitle = "Science Based Target Tool"),
                          
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
                                                                 selected = "Iron and steel",
                                                                 selectize = T),
                                                     selectInput(inputId = "base_year",
                                                                 label = "Base year",
                                                                 choices = c(seq(2014, 2021)),
                                                                 selected = 2020,
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
                                                                 choices = c("Fixed market share", "Growth rate", "Target year output"),
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
                                                                  label = "Scope 1 emissions (t CO2e)",
                                                                  value = "",
                                                                  min = 0),
                                                     numericInput(inputId = "scope_2_emissions",
                                                                  label = "Scope 2 emissions (t CO2e)",
                                                                  value = "",
                                                                  min = 0)
                                        ),
                                        
                                        # MAIN PANEL
                                        
                                        mainPanel(width = 9,
                                          
                                          girafeOutput("main_plot", height = "100%"),
                                          
                                          fluidRow(
                                            
                                            column(7,
                                                   
                                                   girafeOutput("intensity_plot", height = "100%")
                                                   
                                            ),
                                            
                                            column(5,
                                                   
                                                   uiOutput("results_table_heading"),
                                                   
                                                   dataTableOutput("results_table")
                                                   
                                            )
                                            
                                          ),
                                          
                                          fluidRow(
                                            column(12,
                                                   
                                                   uiOutput("full_results_table_heading"),
                                                   
                                                   dataTableOutput("full_results_table")
                                                   
                                                   )
                                          )
                                          
                                          
                                        )
                          )
                          
                 ),
                 
                 tabPanel("Scope 3 Tool",
                          
                          "... to come!"
                          
                 ),
                 
                 tabPanel("Readme",
                          
                          fluidRow(
                            column(3),
                            column(6,
                                   h2("Terms of use"),
                                   HTML("This Tool is intended to enable companies to develop appropriate science-based emissions reductions targets, as well as to assist companies and interested third parties in assessing and evaluating companies' targets.<br />
                                    These terms of use govern all access to and use of the Tool. Please read these terms carefully before accessing or using the Tool and any associated materials. By accepting these terms, you indicate that you have read and understood them and that you agree to abide by them. If you do not agree to these terms, you will not be able to use the Tool.<br />
                                    The Science Based Targets initiative (SBTi) and its 'Partner Organizations' (CDP, the United Nations Global Compact (UNGC), World Resources Institute (WRI), and the World Wide Fund for Nature (WWF)) reserve the right, at their discretion, to withdraw or amend the Tool without notice, and will not be liable if for any reason the Tool is unavailable at any time or for any period. Further, the Partner Organizations reserve the right to modify or replace any part of these terms of use. It is your responsibility to check these terms periodically for changes. Your continued use of the Tool following the posting of any changes to these terms constitutes acceptance of those changes.
                                    "),
                                   
                                   h2("Disclaimer")
                                   # HTML("The Tool and associated materials have been prepared by the SBTi with a high degree of expertise and professionalism, and reflect current best practice in science-based target setting. However, the Partner Organizations, collectively and individually, do not warrant the Tool for any purpose, nor do they make any representations regarding its accuracy, completeness, timeliness or fitness for any use or purpose whatsoever. Information generated by the tool is not intended to amount to advice on which reliance should be placed.<br />
                                   # Developing science-based targets is a multi-step process and appropriate company-wide science-based targets can only be developed after careful consideration of the necessary input data on company emissions and activity. Only then should emissions targets be developed. The SBTi does not examine, verify or hold any such input data provided by users of the Tool.<br />
                                   # The Tool relies on data obtained from a variety of third-party sources. This data was obtained from sources believed by the SBTi be reliable, but there can be no assurance as to the accuracy or completeness of this data. Further, scenarios and assumptions included in the Tool are inherently uncertain due to events or combinations of events that cannot reasonably be foreseen, including, without limitation, the actions of governments, organizations and individuals. The Partner Organizations, collectively and individually, make no warranties or representations about the accuracy or completeness of the data or assumptions contained in this Tool.<br />
                                   # You therefore understand that you use the Tool at your own discretion and risk. You agree that you are not entitled to rely on any information generated by the Tool. You further agree to hold the Partner Organizations, collectively and individually, harmless for loss you might suffer arising out of any inaccuracies in information generated by the Tool. Under no circumstances shall the Partner Organizations, collectively and individually, be liable for direct, indirect or consequential loss or damage arising from the use of the Tool or an inability to use it.
                                   # ")
                                   ),
                            column(3)
                          )
                          
                          
                 ) # Close tabPanel
) # Close navbarPage