# CONSTANTS

ANNUAL_REDUCTION_WB2C = 0.025 # Absolute contraction: annual percental reduction WB2C
ANNUAL_REDUCTION_1.5C = 0.042 # Absolute contraction: annual percental reduction 1.5C

server <- function(input, output) {
  
  # SOURCE
  # source("pathway.R", local = T)
  
  base_dataframe = reactive({
    data.frame(
      year = seq(input$base_year, input$target_year),
      scope1 = input$scope_1_emissions,
      scope2 = input$scope_2_emissions
    )
  })
  
  # ABSOLUTE CONTRACTION
  
  calculate_absolute_contraction = function(emissions, scenario){
    annual_reduction = ifelse(scenario == "WB2C", ANNUAL_REDUCTION_WB2C, ANNUAL_REDUCTION_1.5C)
    reduction = emissions * annual_reduction
    # Take the cumsum and reduce reduction amount by reduction[1], because we want to start reducing in the 
    # year following the base year (no reductions should apply to base year itself):
    reduction = cumsum(reduction) - reduction[1]
    out = emissions - reduction
    return(out)
  }
  
  prepare_abs_contr_dataframe <- function(scenario) {
    out <- base_dataframe() %>%
      mutate(scope1 = calculate_absolute_contraction(scope1, scenario),
             scope2 = calculate_absolute_contraction(scope2, scenario)) %>%
      pivot_longer(!year, names_to = "scope", values_to = "emissions") %>%
      mutate(scenario = scenario,
             # Keep only values >= 0
             emissions = ifelse(emissions >= 0, emissions, 0))
    return(out)
  }
  
  result_absolute_contraction = reactive({
    wb2c <- prepare_abs_contr_dataframe("WB2C")
    one.5C <- prepare_abs_contr_dataframe("1.5C")
    out <- rbind(wb2c, one.5C) %>%
      rownames_to_column("data_id")
    return(out)
  })
  
  plot_absolute_contraction <- function(data, selected_scenario) {
    data <- data %>%
      filter(scenario == selected_scenario)
    out <- ggplot(data, aes(x = year, y = emissions, col = scope)) +
      geom_point_interactive(aes(tooltip = paste0(scope, " (", year, "): ", emissions), data_id = data_id)) +
      geom_line_interactive(aes(tooltip = scope)) +
      scale_x_continuous(breaks = seq(2015, 2050, by = 5)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(title = paste0("Scenario ", selected_scenario),  x = "Year", y = "Emissions") +
      theme_minimal()
    return(out)
    }

  output$test <- renderGirafe(
    girafe(ggobj = plot_absolute_contraction(result_absolute_contraction(), "WB2C"), width_svg = 10, height_svg = 3.5) %>%
      girafe_options(opts_hover(css = "stroke:grey; stroke-width:2px; fill:none; fill-opacity:0"))
  )
  
  output$test2 <- renderGirafe(
    girafe(ggobj = plot_absolute_contraction(result_absolute_contraction(), "1.5C"), width_svg = 10, height_svg = 3.5) %>%
      girafe_options(opts_hover(css = "stroke:grey; stroke-width:2px; fill:none; fill-opacity:0"))
  )
  
}