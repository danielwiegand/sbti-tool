"This file contains all observers which interactively change the behaviour of UI elements"

observe({
  "Create red borders for empty input fields"
  shinyjs::toggleCssClass(id = "base_year_output", class = "red_border", condition = is.na(input$base_year_output))
  shinyjs::toggleCssClass(id = "growth", class = "red_border", condition = is.na(input$growth))
  shinyjs::toggleCssClass(id = "scope_1_emissions", class = "red_border", condition = is.na(input$scope_1_emissions))
  shinyjs::toggleCssClass(id = "scope_2_emissions", class = "red_border", condition = is.na(input$scope_2_emissions))
})

hide_elements <- function(output_names){
  "Function to hide UI elements by means of `shinyjs`"
  lapply(output_names, function(output_name){
    shinyjs::hide(output_name)
  })
}

show_elements <- function(output_names){
  "Function to show UI elements by means of `shinyjs`"
  lapply(output_names, function(output_name){
    shinyjs::show(output_name)
  })
}

observeEvent(input$target_setting_method, {
  "Depending on the target setting method, set the SDA scenario and hide some UI elements"
  if(input$target_setting_method == "Absolute Contraction Approach") {
    updateSelectInput(session, inputId = "sda_scenario", selected = "ETP B2DS")
    hide_elements(c("sda_scenario", "sda_sector", "projected_output_measure", "base_year_output", "growth"))
  } else if(input$target_setting_method == "Sectoral Decarbonization Approach") {
    show_elements(c("sda_scenario", "sda_sector", "projected_output_measure", "base_year_output", "growth"))
  }
})

observeEvent({input$sda_sector
              input$target_setting_method}, {
  "Depending on selected SDA sector, show / hide the SDA scenario element"
  if(input$target_setting_method == "Sectoral Decarbonization Approach" & input$sda_sector == "Power") {
    show_elements("sda_scenario")
  } else {
    updateSelectInput(session, inputId = "sda_scenario", selected = "ETP B2DS")
    hide_elements("sda_scenario")
  }
})

observeEvent(input$sda_sector, {
  "Depending on SDA sector, change the displayed activity unit"
  updateSelectInput(session, "base_year_output", label = paste0("Base year output in ", activity_unit()))
})

observeEvent({input$projected_output_measure
  input$sda_sector
  input$target_setting_method}, {
    "Show/hide the `growth` UI element and update its label interactively"
    if(input$projected_output_measure == "Growth rate") {
      show_elements("growth")
      updateSelectInput(session, "growth", label = "Annual activity growth rate (%)")
    } else if(input$projected_output_measure == "Target year output") {
      show_elements("growth")
      updateSelectInput(session, "growth", label = paste0("Target year output in ", activity_unit()))
    } else if(input$projected_output_measure == "Fixed market share") {
      hide_elements("growth")
    }
  })