# CONSTANTS

ANNUAL_REDUCTION_WB2C = 0.025 # Absolute contraction: annual percental reduction WB2C
ANNUAL_REDUCTION_1.5C = 0.042 # Absolute contraction: annual percental reduction 1.5C

ETP_DATA <- read.csv("./data/etp.csv", stringsAsFactors = F) %>%
  pivot_longer(cols = starts_with("x"), names_to = "year", values_to = "value") %>%
  mutate(year = str_replace(year, "X", ""),
         year = as.numeric(year))


server <- function(input, output) {
  
  # SOURCE
  # source("pathway.R", local = T)
  
  # BASE DATA ####
  
  filter_etp_data <- function(filter_by) {
    ETP_DATA %>%
      filter(Sector.ETP == input$sda_sector, Flow.1 == filter_by, Region == "World", 
             year >= input$base_year, year <= input$target_year) %>%
      column_to_rownames("year") %>%
      select(value)
  }
  
  get_etp_2050_value <- function(filter_by) {
    ETP_DATA %>%
      filter(Sector.ETP == input$sda_sector, Flow.1 == filter_by, Region == "World", 
             year == 2050) %>%
      column_to_rownames("year") %>%
      select(value)
  }
  
  sector_activity <- reactive({
    filter_etp_data("Output")
  })
  
  sector_emissions <- reactive({
    filter_etp_data("Emissions")
  })
  
  sector_emission_intensity <- reactive({
    sector_emissions() / sector_activity()
  })
  
  sector_electricity <- reactive({
    filter_etp_data("Electricity")
  })
  
  normalized_company_activity <- reactive({
    data.frame(
      year = seq(input$base_year, input$target_year),
      value = input$growth / 100 + 1
    ) %>%
      mutate(value = case_when(year == input$base_year ~ 1, TRUE ~ value),
             value = cumprod(value)) %>%
      column_to_rownames("year") %>%
      select(value)
  })
  
  company_activity <- reactive({
    normalized_company_activity() * input$base_year_output
  })
  
  base_dataframe = reactive({
    data.frame(
      year = seq(input$base_year, input$target_year),
      scope1 = input$scope_1_emissions,
      scope2 = input$scope_2_emissions
    )
  })
  
  # SCIENCE-BASED TARGET CALCULATION ####
  
  # 1. Absolute contraction
  calculate_absolute_contraction <- function(emissions, scenario){
    annual_reduction <- ifelse(scenario == "WB2C", ANNUAL_REDUCTION_WB2C, ANNUAL_REDUCTION_1.5C)
    reduction <- emissions * annual_reduction
    # Take the cumsum and reduce reduction amount by reduction[1], because we want to start reducing in the 
    # year following the base year (no reductions should apply to base year itself):
    reduction <- cumsum(reduction) - reduction[1]
    out <- emissions - reduction
    return(out)
  }
  
  # 2. Sectoral decarbonization approach (SDA)
  # https://sciencebasedtargets.org/resources/files/Sectoral-Decarbonization-Approach-Report.pdf
  calculate_sda <- function(emissions, scenario) {
    sector_emission_intensity_2050 <- unlist(get_etp_2050_value("Emissions") / get_etp_2050_value("Output"))
    emissions <- emissions[1]
    initial_co2_intensity <- emissions / input$base_year_output
    # Step 1: initial performance
    initial_performance <- initial_co2_intensity - sector_emission_intensity_2050
    # # Step 2: market share parameter
    market_share_parameter <- (input$base_year_output / sector_activity()[input$base_year,]) / (company_activity() / sector_activity())
    # Cap market share parameter to 1, as described in "Foundations of SBT target setting"
    # (refer to https://sciencebasedtargets.org/wp-content/uploads/2019/04/foundations-of-SBT-setting.pdf, p.31)
    market_share_parameter[market_share_parameter > 1] <- 1
    # Step 3: decarbonization index
    decarbonization_index <- (sector_emission_intensity() - sector_emission_intensity_2050) / (sector_emission_intensity()[input$base_year,] - sector_emission_intensity_2050)
    # Step 4: intensity target company
    intensity_target <- initial_performance * decarbonization_index * market_share_parameter + sector_emission_intensity_2050
    emission_target <- as.double(unlist(intensity_target * company_activity()))
    return(emission_target)
  }

  # MAIN PLOT ####
  
  prepare_main_plot <- function(scenario) {
    if(input$target_setting_method == "Absolute Contraction Approach") {
      target_setting_function <- calculate_absolute_contraction
    } else {
      target_setting_function <- calculate_sda
    }
    out <- base_dataframe() %>%
      mutate(scope1 = target_setting_function(scope1, scenario),
             scope2 = target_setting_function(scope2, scenario)) %>%
      pivot_longer(!year, names_to = "scope", values_to = "emissions") %>%
      mutate(scenario = scenario,
             # Keep only values >= 0
             emissions = ifelse(emissions >= 0, emissions, 0))
    return(out)
  }
  
  result_main_plot = reactive({
    req(input$scope_1_emissions, input$scope_2_emissions)
    if(input$target_setting_method == "Absolute Contraction Approach") {
      scenarios <- c("WB2C", "1.5C")
    } else {
      scenarios <- c(input$sda_scenario)
    }
    out <- data.frame()
    for (scenario in scenarios) {
      results <- prepare_main_plot(scenario)
      out <- rbind(out, results)
    }
    out <- out %>%
      rownames_to_column("data_id")
    return(out)
  })

  plot_main_plot <- function(data) {
    out <- ggplot(data, aes(x = year, y = emissions, col = scope)) +
      geom_point_interactive(aes(tooltip = paste0(scope, " (", year, "): ", emissions), data_id = data_id)) +
      geom_line_interactive(aes(tooltip = scope)) +
      scale_x_continuous(breaks = seq(2015, 2050, by = 5)) +
      scale_y_continuous(limits = c(0, NA)) +
      facet_wrap(~scenario, ncol = 1) +
      # labs(title = paste0("Scenario ", selected_scenario),  x = "Year", y = "Emissions") +
      theme_minimal()
    return(out)
    }

  output$main_plot <- renderGirafe(
    girafe(ggobj = plot_main_plot(result_main_plot()), width_svg = 10, height_svg = 3.5) %>%
      girafe_options(opts_hover(css = "stroke:grey; stroke-width:2px; fill:none; fill-opacity:0"))
  )
  
  # RESULTS TABLE ####
  
  results_table <- reactive({
    out <- result_main_plot() %>%
      filter(year %in% c(input$base_year, input$target_year)) %>%
      group_by(year, scope, scenario) %>%
      summarize(emissions = sum(emissions)) %>%
      pivot_wider(names_from = "year", values_from = "emissions") %>%
      arrange(scenario, scope) %>%
      ungroup() %>%
      rename(`Base year` = !!names(.[3]), `Target year` = !!names(.[4]), Scenario = scenario, Scope = scope) %>%
      mutate(Reduction = paste0(round((1 - `Target year` / `Base year`) * 100), "%"),
             `Base year` = paste0(round(`Base year`, 2), " t CO2e"),
             `Target year` = paste0(round(`Target year`, 2), " t CO2e"))
  })
  
  output$results_table <- renderDataTable(
    results_table(),
    options = list(
      paging = FALSE,
      searching = FALSE,
      info = FALSE
    )
  )
  
  # INTENSITY PLOT ####
  
  observe(print(intensity_table()))
  
  intensity_table <- reactive({
    # result_main_plot() %>%
    #   mutate()
    # sector_emission_intensity()
    ETP_DATA
  })
  
  # SHINYJS ####
  
  hide_elements <- function(output_names){
    lapply(output_names, function(output_name){
      shinyjs::hide(output_name)
    })
  }
  
  show_elements <- function(output_names){
    lapply(output_names, function(output_name){
      shinyjs::show(output_name)
    })
  }
  
  observeEvent(input$target_setting_method, {
    if(input$target_setting_method == "Absolute Contraction Approach") {
      hide_elements(c("sda_scenario", "sda_sector", "projected_output_measure"))
    } else if(input$target_setting_method == "Sectoral Decarbonization Approach") {
      show_elements(c("sda_scenario", "sda_sector", "projected_output_measure"))
    }
  })
  
  observeEvent(input$sda_sector, {
    if(input$sda_sector == "Power") {
      show_elements("sda_scenario")
    } else {
      hide_elements("sda_scenario")
    }
  })
  
}