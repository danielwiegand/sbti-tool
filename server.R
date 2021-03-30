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
  
  filter_etp_data <- function(filter_by) {
    ETP_DATA %>%
      # Here: Years > 2050 are filtered out for the etp data
      filter(Sector.ETP == input$sda_sector, Flow.1 == filter_by, Region == "World", year <= 2050) %>%
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
    req(input$scope_1_emissions, input$scope_2_emissions)
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

  output$abs_contr_wb2c <- renderGirafe(
    girafe(ggobj = plot_absolute_contraction(result_absolute_contraction(), "WB2C"), width_svg = 10, height_svg = 3.5) %>%
      girafe_options(opts_hover(css = "stroke:grey; stroke-width:2px; fill:none; fill-opacity:0"))
  )
  
  output$abs_contr_1.5c <- renderGirafe(
    girafe(ggobj = plot_absolute_contraction(result_absolute_contraction(), "1.5C"), width_svg = 10, height_svg = 3.5) %>%
      girafe_options(opts_hover(css = "stroke:grey; stroke-width:2px; fill:none; fill-opacity:0"))
  )
  
  # SECTORAL DECARBONIZATION APPROACH
  
  # https://sciencebasedtargets.org/resources/files/Sectoral-Decarbonization-Approach-Report.pdf
  
  calculate_sda <- function(emissions) {

    initial_co2_intensity <- emissions / input$base_year_output

    # Step 1: initial performance
    initial_performance <- initial_co2_intensity - sector_emission_intensity()["2050",]
    
    # Step 2: market share parameter
    market_share_parameter <- (input$base_year_output / sector_activity()[input$base_year,]) / (company_activity() / sector_activity())
    
    # Cap market share parameter to 1, as described in "Foundations of SBT target setting"
    # (refer to https://sciencebasedtargets.org/wp-content/uploads/2019/04/foundations-of-SBT-setting.pdf, p.31)
    market_share_parameter[market_share_parameter > 1] <- 1
    
    # Step 3: decarbonization index
    decarbonization_index <- (sector_emission_intensity() - sector_emission_intensity()["2050",]) / (sector_emission_intensity()[input$base_year,] - sector_emission_intensity()["2050",])
    
    # Step 4: intensity target company
    intensity_target <- initial_performance * decarbonization_index * market_share_parameter + sector_emission_intensity()["2050",]
    
    emission_target <- as.double(unlist(intensity_target * company_activity()))
    
    return(emission_target)
    
  }
  
  #!!! DOPPELUNG MIT ABS CONTR AUFLÖSEN!!!
  prepare_sda_dataframe <- function() {
    req(input$scope_1_emissions, input$scope_2_emissions)
    out <- base_dataframe() %>%
      mutate(scope1 = calculate_sda(input$scope_1_emissions),
             scope2 = calculate_sda(input$scope_2_emissions)
             ) %>%
      pivot_longer(!year, names_to = "scope", values_to = "emissions") %>%
      mutate(emissions = ifelse(emissions >= 0, emissions, 0)) # Keep only values >= 0
    return(out)
  }
  
  #!!! DOPPELUNG MIT ABS CONTR AUFLÖSEN!!!
  result_sda = reactive({
    req(input$scope_1_emissions, input$scope_2_emissions)
    prepare_sda_dataframe() %>%
      rownames_to_column("data_id")
  })
  
  observe(print(
    result_sda()
  ))
}