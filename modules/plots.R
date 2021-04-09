"This file contains plots and tables"

# MAIN PLOT ####

plot_main_plot <- function(data) {
  "Function to prepare the main plot (emission paths).
  Input:
    data: data.frame containing the emission paths per scope"
  
  out <- ggplot(data, aes(x = year, y = emissions, col = scope, linetype = scope)) +
    geom_hline(yintercept = 0, colour = "#d3d3d3") +
    geom_line(alpha = 1, size = 0.7, show.legend = T) +
    geom_point_interactive(aes(tooltip = paste0(scope, " (", year, "): ", emissions, " t CO2e"), 
                               data_id = data_id), size = 4, alpha = 0.1, col = "white") +
    scale_x_continuous(breaks = seq(2015, 2050, by = 5)) +
    scale_y_continuous(limits = c(0, NA)) +
    facet_wrap(~scenario, ncol = 2) +
    scale_color_manual(values = c("#2e86c1", "#ec7063")) +
    scale_linetype_manual(values = c("solid", "solid"), guide = "none") +
    labs(title = "Emission path", x = "Year", y = "Emissions (t CO2e)", color = "Scope") +
    theme_minimal() +
    theme(legend.position = c(0.85, 0.8), legend.title = element_blank())
  return(out)
}

output$main_plot <- renderGirafe({
  "Render the main plot"
  girafe(ggobj = plot_main_plot(emission_path()), width_svg = 10, height_svg = 3) %>%
    girafe_options(opts_hover(css = "stroke:grey; stroke-width:2px; fill:black; fill-opacity:0.3"))
})


# INTENSITY PLOT ####

emission_intensity_path <- reactive({
  "The development of the company's emission intensities (emissions divided by activity per year)"
  if(input$projected_output_measure == "Fixed market share") {
    req(input$scope_1_emissions, input$scope_2_emissions, input$base_year_output)
  } else {
    req(input$scope_1_emissions, input$scope_2_emissions, input$base_year_output, input$growth)
  }
  company_activity() %>%
    rownames_to_column("year") %>%
    mutate(year = as.numeric(year)) %>%
    rename("activity" = "value") %>%
    left_join(emission_path()) %>%
    mutate(intensity = emissions / activity)
})

create_sector_intensity_dataframe <- function(scenarios) {
  "Function to create the sector intensity paths for Scope 1 and Scope 2. If two scenarios are involved
    (Absolute Contraction Approach), two identical sector dataframes are stacked for display in the plot facets.
    Inputs:
      scenarios (list of strings): The scenarios involved
    Outputs:
      out (data.frame): Sector intensity paths, identical per scenario"
  
  out <- data.frame()
  for(scenario in scenarios) {
    sector_data <- cbind(sector_scope1_emission_intensity(), sector_scope2_emission_intensity()) %>%
      rownames_to_column("year") %>%
      pivot_longer(!year, names_to = "scope", values_to = "intensity") %>%
      mutate(scenario = scenario)
    out <- rbind(out, sector_data)
  }
  return(out)
}

result_intensity_plot <- reactive({
  "The resulting intensity paths for company and sector"
  company_data <- emission_intensity_path() %>%
    mutate(scope = paste0(scope, " (company)")) %>%
    select(year, scope, scenario, intensity)
  scenarios <- unique(company_data$scenario)
  sector_data <- create_sector_intensity_dataframe(scenarios)
  out <- rbind(company_data, sector_data) %>% # bind company and sector data
    rownames_to_column("data_id") %>%
    mutate(year = as.numeric(year),
           intensity = round(intensity, 2))
})

plot_intensity_plot <- function(data) {
  "Function to prepare the emission intensity plot
  Input:
    data: data.frame containing the emission intensity paths per scope, including the sector's path"
  
  out <- ggplot(data, aes(x = year, y = intensity)) +
    geom_hline(yintercept = 0, colour = "#d3d3d3") +
    geom_line(aes(linetype = scope, colour = scope), alpha = 1, size = 0.7, show.legend = FALSE) +
    # Display company data points
    geom_point_interactive(data = data[str_detect(data$scope, "company"),],
                           aes(tooltip = paste0(scope, " (", year, "): ", intensity, " tCO2e / t"), 
                               data_id = data_id), size = 3, alpha = 0.1, colour = "white") +
    # Display sector data points
    geom_point_interactive(data = data[str_detect(data$scope, "sector"),], 
                           aes(tooltip = paste0(scope, " (", year, "): ", intensity, " tCO2e / t"), 
                               data_id = data_id, colour = scope), size = 0.8, alpha = 1) +
    scale_x_continuous(breaks = seq(2015, 2050, by = 5)) +
    scale_y_continuous(limits = c(0, NA)) +
    scale_color_manual(values = c("#2e86c1", "#a9cce3", "#ec7063", "#f1948a")) +
    scale_linetype_manual(values = c("solid", "blank", "solid", "blank")) +
    labs(title = "Emission intensity path", x = "Year", y = "Intensity (t CO2e / t)", color = "Scope") +
    facet_wrap(~scenario, ncol = 2) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank())
  return(out)
}

output$intensity_plot <- renderGirafe({
  "Render the emission intensity plot"
  girafe(ggobj = plot_intensity_plot(result_intensity_plot()), width_svg = 6.5, height_svg = 3.5) %>%
    girafe_options(opts_hover(css = "stroke:grey; stroke-width:2px; fill:black; fill-opacity:0.7"))
})

# RESULTS TABLE ####

results_table <- reactive({
  out <- emission_path() %>%
    filter(year %in% c(input$base_year, input$target_year)) %>%
    group_by(year, scope, scenario) %>%
    summarize(emissions = sum(emissions)) %>%
    pivot_wider(names_from = "year", values_from = "emissions") %>%
    arrange(scenario, scope) %>%
    ungroup() %>%
    drop_na() %>%
    rename(`Base year` = !!names(.[3]), `Target year` = !!names(.[4]), Scenario = scenario, Scope = scope) %>%
    mutate(Scope = recode_for_plot(Scope)) %>%
    mutate(Reduction = paste0(round((1 - `Target year` / `Base year`) * 100), "%"),
           `Base year` = paste0(round(`Base year`, 2), " t CO2e"),
           `Target year` = paste0(round(`Target year`, 2), " t CO2e"))
})

output$results_table_heading <- renderUI({
  req(emission_path())
  h3("Summary of results")
})

output$results_table <- renderDataTable(
  expr = results_table(),
  options = list(
    paging = FALSE,
    searching = FALSE,
    info = FALSE
  )
)

output$full_results_table_heading <- renderUI({
  req(emission_path())
  h3("Full results")
})

full_results_table <- reactive({
  emission_path() %>%
    select(-data_id) %>%
    mutate(emissions = format(emissions, nsmall = 2)) %>%
    group_by(scenario, scope) %>%
    pivot_wider(names_from = year, values_from = emissions)
})

output$full_results_table <- renderDataTable(
  expr = full_results_table(),
  options = list(scrollX = TRUE, 
                 scrollCollapse = TRUE,
                 paging = FALSE,
                 searching = FALSE,
                 info = FALSE
                 )
  )