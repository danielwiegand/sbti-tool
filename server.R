# CONSTANTS

ANNUAL_REDUCTION_WB2C = 0.025 # Absolute contraction: annual percental reduction WB2C
ANNUAL_REDUCTION_1.5C = 0.042 # Absolute contraction: annual percental reduction 1.5C

ETP_DATA <- read.csv("./data/etp.csv", stringsAsFactors = F) %>%
  pivot_longer(cols = starts_with("x"), names_to = "year", values_to = "value") %>%
  mutate(year = str_replace(year, "X", ""),
         year = as.numeric(year))

server <- function(input, output, session) {
  
  # SOURCE
  # source("pathway.R", local = T)
  
  # BASE DATA ####
  
  filter_etp_data <- function(filter_by, sector = input$sda_sector, filter_years = TRUE) {
    out <- ETP_DATA %>%
      filter(Sector.ETP == sector, Flow.1 == filter_by, Region == "World", Scenario == input$sda_scenario)
    if(filter_years == TRUE) {
      out <- out %>% filter(year >= input$base_year, year <= input$target_year)
    }
    out <- out %>%
      column_to_rownames("year") %>%
      select(value)
  }
  
  normalize_column <- function(x) {
    return(x / x[1])
  }
  
  sector_activity <- reactive({
    filter_etp_data("Output")
  })
  
  normalized_sector_activity <- reactive({
    sector_activity() %>%
      mutate(value = normalize_column(value))
  })
  
  power_sector_emission_intensity <- reactive({
    filter_etp_data("Emissions intensity", "Power")
  })
  
  sector_scope1_emissions <- reactive({
    filter_etp_data("Emissions")
  })
  
  sector_scope2_emissions <- reactive({
    sector_electricity() * (power_sector_emission_intensity() / 3.6)
  })
  
  sector_scope2_emissions_2050 <- reactive({
    filter_etp_data("Electricity", filter_years = FALSE)["2050",] *
      (filter_etp_data("Emissions intensity", sector = "Power", filter_years = FALSE)["2050",] / 3.6)
  })
  
  sector_scope1_emission_intensity <- reactive({
    (sector_scope1_emissions() * 1000000 / sector_activity()) %>%
      rename("Scope 1 (sector)" = "value")
  })
  
  sector_scope2_emission_intensity <- reactive({
    (sector_scope2_emissions() * 1000000 / sector_activity()) %>%
    rename("Scope 2 (sector)" = "value")
  })
  
  sector_electricity <- reactive({
    filter_etp_data("Electricity")
  })
  
  normalized_company_activity <- reactive({
    
    data <- data.frame(
      year = seq(input$base_year, input$target_year),
      value = input$growth / 100 + 1
    )
    
    if(input$projected_output_measure == "Growth rate") {
      out <- data %>%
        mutate(value = case_when(year == input$base_year ~ 1, TRUE ~ value),
               value = cumprod(value)) %>%
        column_to_rownames("year") %>%
        select(value)
    } else if(input$projected_output_measure == "Target year output") {
      yearly_difference <- (input$growth - input$base_year_output) / (input$target_year - as.numeric(input$base_year))
      out <- data %>%
        mutate(absolute_value = yearly_difference,
               absolute_value = case_when(year == input$base_year ~ as.double(input$base_year_output),
                                          TRUE ~ absolute_value),
               absolute_value = cumsum(absolute_value),
               value = normalize_column(absolute_value)) %>%
        column_to_rownames("year") %>%
        select(value)
    } else if(input$projected_output_measure == "Fixed market share") {
      out <- data %>%
        mutate(value = normalized_sector_activity()) %>%
        column_to_rownames("year") %>%
        select(value)
    }
    # return(out)
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
  
  activity_unit <- reactive({
    all_activity_units <- data.frame(
      sda_sector = c("Power", "Iron and steel", "Cement", "Aluminium", "Pulp and paper", "Services - Buildings"),
      activity_unit = c("MWh gross electricity (+ heat)", "Tonnes of crude steel", "Tonnes of cement", "Tonnes of aluminium", "Tonnes of paper and board", "Square meters")
    )
    out <- all_activity_units$activity_unit[all_activity_units$sda_sector == input$sda_sector]
  })
  
  recode_for_plot <- function(x) {
    x %>% recode(
      "scope1" = "Scope 1",
      "scope2" = "Scope 2"
    )
  }
  
  rename_for_plot <- function(x) {
    x %>% rename(
      "Scope" = scope,
      "Emissions" = emissions,
      "Year" = year
    )
  }
  
  # SCIENCE-BASED TARGET CALCULATION ####
  
  # 1. Absolute contraction
  calculate_absolute_contraction <- function(emissions, scenario, scope){
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
  calculate_sda <- function(emissions, scenario, scope) {
    if(scope == "scope1") {
      sector_emission_intensity <- sector_scope1_emission_intensity()
      sector_emission_intensity_2050 <- unlist(filter_etp_data("Emissions", filter_years = FALSE)["2050",] * 1000000 / filter_etp_data("Output", filter_years = FALSE)["2050",])
    } else if(scope == "scope2") {
      sector_emission_intensity <- sector_scope2_emission_intensity()
      sector_emission_intensity_2050 <- unlist(sector_scope2_emissions_2050() * 1000000 / filter_etp_data("Output", filter_years = FALSE)["2050",])
    }
    
    emissions <- emissions[1]
    initial_co2_intensity <- emissions / input$base_year_output
    # Step 1: initial performance
    initial_performance <- initial_co2_intensity - sector_emission_intensity_2050
    # Step 2: market share parameter
    market_share_parameter <- (input$base_year_output / sector_activity()[input$base_year,]) / (company_activity() / sector_activity())
    # Cap market share parameter to 1, as described in "Foundations of SBT target setting"
    # (refer to https://sciencebasedtargets.org/wp-content/uploads/2019/04/foundations-of-SBT-setting.pdf, p.31)
    market_share_parameter[market_share_parameter > 1] <- 1
    # Step 3: decarbonization index
    decarbonization_index <- (sector_emission_intensity - sector_emission_intensity_2050) / (sector_emission_intensity[input$base_year,] - sector_emission_intensity_2050)
    # Step 4: intensity target company
    intensity_target <- initial_performance * decarbonization_index * market_share_parameter + sector_emission_intensity_2050
    emission_target <- as.double(unlist(intensity_target * company_activity()))
    return(emission_target)
  }

  # MAIN PLOT ####
  
  prepare_emission_path <- function(scenario) {
    if(input$target_setting_method == "Absolute Contraction Approach") {
      target_setting_function <- calculate_absolute_contraction
    } else {
      target_setting_function <- calculate_sda
    }
    out <- base_dataframe() %>%
      mutate(scope1 = target_setting_function(scope1, scenario, "scope1"),
             scope2 = target_setting_function(scope2, scenario, "scope2")) %>%
      pivot_longer(!year, names_to = "scope", values_to = "emissions") %>%
      mutate(scenario = scenario,
             # Keep only values >= 0
             emissions = ifelse(emissions >= 0, emissions, 0))
    return(out)
  }
  
  emission_path = reactive({
    req(input$scope_1_emissions, input$scope_2_emissions)
    if(input$target_setting_method == "Absolute Contraction Approach") {
      scenarios <- c("WB2C", "1.5C")
    } else {
      scenarios <- c(input$sda_scenario)
    }
    out <- data.frame()
    for (scenario in scenarios) {
      results <- prepare_emission_path(scenario)
      out <- rbind(out, results)
    }
    out <- out %>%
      rownames_to_column("data_id") %>%
      mutate(scope = recode_for_plot(scope))
    return(out)
  })

  plot_main_plot <- function(data) {
    out <- ggplot(data, aes(x = year, y = emissions, col = scope, linetype = scope)) +
      geom_hline(yintercept = 0, colour = "#d3d3d3") +
      geom_line(alpha = 0.9, size = 1, show.legend = FALSE) +
      geom_point_interactive(aes(tooltip = paste0(scope, " (", year, "): ", emissions), data_id = data_id), size = 1, alpha = 0.3) +
      scale_x_continuous(breaks = seq(2015, 2050, by = 5)) +
      scale_y_continuous(limits = c(0, NA)) +
      facet_wrap(~scenario, ncol = 2) +
      scale_color_manual(values = c("#2e86c1", "#ec7063")) +
      scale_linetype_manual(values = c("solid", "solid")) +
      labs(title = "Emission path", x = "Year", y = "Emissions (t CO2e)", color = "Scope") +
      # labs(title = paste0("Scenario ", selected_scenario),  x = "Year", y = "Emissions") +
      theme_minimal() +
      theme(legend.position = c(0.85, 0.8), legend.title = element_blank())
    return(out)
  }

  output$main_plot <- renderGirafe(
    girafe(ggobj = plot_main_plot(emission_path()), width_svg = 10, height_svg = 3.5) %>%
      girafe_options(opts_hover(css = "stroke:grey; stroke-width:2px; fill:none; fill-opacity:0"))
  )
  
  emission_intensity_path <- reactive({
    company_activity() %>%
      rownames_to_column("year") %>%
      mutate(year = as.numeric(year)) %>%
      rename("activity" = "value") %>%
      left_join(emission_path()) %>%
      mutate(intensity = emissions / activity)
  })
  
  result_intensity_plot <- reactive({
    sector_data <- cbind(sector_scope1_emission_intensity(), sector_scope2_emission_intensity()) %>%
      rownames_to_column("year") %>%
      pivot_longer(!year, names_to = "scope", values_to = "intensity")
    company_data <- emission_intensity_path() %>%
      mutate(scope = paste0(scope, " (company)")) %>%
      select(year, scope, intensity)
    out <- rbind(company_data, sector_data) %>%
      rownames_to_column("data_id") %>%
      mutate(year = as.numeric(year))
  })
  
  plot_intensity_plot <- function(data) {
    out <- ggplot(data, aes(x = year, y = intensity, col = scope, linetype = scope)) +
      geom_hline(yintercept = 0, colour = "#d3d3d3") +
      geom_line(alpha = 0.9, size = 1) +
      geom_point_interactive(aes(tooltip = paste0(scope, " (", year, "): ", intensity), data_id = data_id), size = 1, alpha = 0.6, show.legend = FALSE) +
      scale_x_continuous(breaks = seq(2015, 2050, by = 5)) +
      scale_y_continuous(limits = c(0, NA)) +
      scale_color_manual(values = c("#2e86c1", "#ec7063", "#a9cce3", "#f1948a")) +
      scale_linetype_manual(values = c("solid", "solid", "blank", "blank")) +
      labs(title = "Emission intensity path", x = "Year", y = "Intensity (t CO2e / t)", color = "Scope") +
      # facet_wrap(~scenario, ncol = 1) +
      theme_minimal() +
      theme(legend.position = c(0.85, 0.7), legend.title = element_blank())
    return(out)
  }
  
  output$intensity_plot <- renderGirafe(
    girafe(ggobj = plot_intensity_plot(result_intensity_plot()), width_svg = 6.5, height_svg = 3) %>%
      girafe_options(opts_hover(css = "stroke:grey; stroke-width:2px; fill:none; fill-opacity:0"))
  )
  
  # RESULTS TABLE ####
  
  results_table <- reactive({
    out <- emission_path() %>%
      filter(year %in% c(input$base_year, input$target_year)) %>%
      group_by(year, scope, scenario) %>%
      summarize(emissions = sum(emissions)) %>%
      pivot_wider(names_from = "year", values_from = "emissions") %>%
      arrange(scenario, scope) %>%
      ungroup() %>%
      rename(`Base year` = !!names(.[3]), `Target year` = !!names(.[4]), Scenario = scenario, Scope = scope) %>%
      mutate(Scope = recode_for_plot(Scope)) %>%
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
      updateSelectInput(session, inputId = "sda_scenario", selected = "ETP B2DS")
      hide_elements(c("sda_scenario", "sda_sector", "projected_output_measure", "base_year_output", "growth"))
    } else if(input$target_setting_method == "Sectoral Decarbonization Approach") {
      show_elements(c("sda_scenario", "sda_sector", "projected_output_measure", "base_year_output", "growth"))
    }
  })
  
  observeEvent(input$sda_sector, {
    if(input$target_setting_method == "Sectoral Decarbonization Approach" & input$sda_sector == "Power") {
      show_elements("sda_scenario")
    } else {
      hide_elements("sda_scenario")
      updateSelectInput(session, inputId = "sda_scenario", selected = "ETP B2DS")
    }
  })
  
  observeEvent(input$sda_sector, {
    updateSelectInput(session, "base_year_output", label = paste0("Base year output in ", activity_unit()))
  })
  
  observeEvent({input$projected_output_measure
               input$sda_sector}, {
    if(input$projected_output_measure == "Growth rate") {
      show_elements("growth")
      updateSelectInput(session, "growth", label = "Annual growth rate (%)")
    } else if(input$projected_output_measure == "Target year output") {
      show_elements("growth")
      updateSelectInput(session, "growth", label = paste0("Target year output in ", activity_unit()))
    } else if(input$projected_output_measure == "Fixed market share") {
      hide_elements("growth")
    }
  })

}