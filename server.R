# CONSTANTS

ANNUAL_REDUCTION_WB2C = 0.025 # Absolute contraction: annual percental reduction WB2C
ANNUAL_REDUCTION_1.5C = 0.042 # Absolute contraction: annual percental reduction 1.5C

ETP_DATA <- read.csv("./data/etp.csv", stringsAsFactors = F) %>%
  pivot_longer(cols = starts_with("x"), names_to = "year", values_to = "value") %>%
  mutate(year = str_replace(year, "X", ""),
         year = as.numeric(year))

server <- function(input, output, session) {
  
  # SOURCE ####
  
  source("modules/sbti_calculation.R", local = T)
  source("modules/plots.R", local = T)
  source("modules/observers.R", local = T)
  
  # BASE DATA ####
  
  filter_etp_data <- function(filter_by, sector = input$sda_sector, filter_years = TRUE) {
    "Function to filter the ETP data.
    Inputs:
      filter_by: Keyword to filter the column 'Flow.1'
      sector: filter data by sector. Default: input$sda_sector
      filter_years: whether to filter by base_year and target_year
    Returns:
      out: filtered ETP data"
    
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
    "Function to normalize a data.frame column (starting then with the value 1)"
    return(x / x[1])
  }
  
  recode_for_plot <- function(x) {
    "Function to recode a column for plotting so that the factor levels are displayed 'nicely'"
    x %>% recode(
      "scope1" = "Scope 1",
      "scope2" = "Scope 2"
    )
  }
  
  sector_activity <- reactive({
    "Activity (output) of the selected SDA sector"
    filter_etp_data("Output")
  })
  
  normalized_sector_activity <- reactive({
    "Normalized activity (output) of the selected SDA sector"
    sector_activity() %>%
      mutate(value = normalize_column(value))
  })
  
  power_sector_emission_intensity <- reactive({
    "Emission intensity of the power sector. Used to calculate a sector's Scope 2 emissions."
    filter_etp_data("Emissions intensity", "Power")
  })
  
  sector_scope1_emissions <- reactive({
    "Scope 1 emissions of the selected SDA sector"
    filter_etp_data("Emissions")
  })
  
  sector_scope2_emissions <- reactive({
    "Scope 2 emissions of the selected SDA sector"
    if(input$sda_sector == "Power" & input$sda_scenario == "SBTi 1.5C") {
      data.frame(
        year = seq(input$base_year, input$target_year),
        value = 0 # The power sector does not have Scope 2 emissions
      ) %>% column_to_rownames("year")
    } else {
      sector_electricity() * (power_sector_emission_intensity() / 3.6)
    }
  })
  
  sector_scope2_emissions_2050 <- reactive({
    "Scope 2 emissions of the selected SDA sector in the year 2050"
    filter_etp_data("Electricity", filter_years = FALSE)["2050",] *
      (filter_etp_data("Emissions intensity", sector = "Power", filter_years = FALSE)["2050",] / 3.6)
  })
  
  sector_scope1_emission_intensity <- reactive({
    "Scope 1 emission intensity of the selected SDA sector"
    (sector_scope1_emissions() * 1000000 / sector_activity()) %>%
      rename("Scope 1 (sector)" = "value")
  })
  
  sector_scope2_emission_intensity <- reactive({
    "Scope 2 emission intensity of the selected SDA sector"
    (sector_scope2_emissions() * 1000000 / sector_activity()) %>%
    rename("Scope 2 (sector)" = "value")
  })
  
  sector_electricity <- reactive({
    "Electricity consumption of the selected SDA sector (PJ)"
    filter_etp_data("Electricity")
  })
  
  normalized_company_activity <- reactive({
    "Normalized company activity (starting with value 1). Calculation depends upon `input$projected_output_measure`."
    data <- data.frame(
      year = seq(input$base_year, input$target_year),
      value = input$growth / 100 + 1
    )
    if(input$projected_output_measure == "Growth rate") {
      # User indicates a growth rate by which the activity grows per year
      out <- data %>%
        mutate(value = case_when(year == input$base_year ~ 1, TRUE ~ value),
               value = cumprod(value)) %>%
        column_to_rownames("year") %>%
        select(value)
    } else if(input$projected_output_measure == "Target year output") {
      # User indicates the activity (output) in the target year. Assumption: linear activity growth between base and target year
      yearly_difference <- (input$growth - input$base_year_output) / (input$target_year - as.numeric(input$base_year)) # yearly incremental change of activity
      out <- data %>%
        mutate(absolute_value = yearly_difference,
               absolute_value = case_when(year == input$base_year ~ as.double(input$base_year_output), # add the base year activity in the base year row
                                          TRUE ~ absolute_value),
               absolute_value = cumsum(absolute_value),
               value = normalize_column(absolute_value)) %>%
        column_to_rownames("year") %>%
        select(value)
    } else if(input$projected_output_measure == "Fixed market share") {
      # User indicates that activity grows identical to the sector activity
      out <- data %>%
        mutate(value = normalized_sector_activity()) %>%
        column_to_rownames("year") %>%
        select(value)
    }
    return(out)
  })
  
  company_activity <- reactive({
    "Company activity (output) in absolute terms"
    normalized_company_activity() * input$base_year_output
  })
  
  activity_unit <- reactive({
    "Activity unit of the selected SDA sector"
    all_activity_units <- data.frame(
      sda_sector = c("Power", "Iron and steel", "Cement", "Aluminium", "Pulp and paper", "Services - Buildings"),
      activity_unit = c("MWh gross electricity (+ heat)", "Tonnes of crude steel", "Tonnes of cement", "Tonnes of aluminium", "Tonnes of paper and board", "Square meters")
    )
    out <- all_activity_units$activity_unit[all_activity_units$sda_sector == input$sda_sector]
  })
  
  # RESULTS ####
  
  prepare_emission_path <- function(scenario) {
    "Function which prepares the Science Based Target.
    Input:
      scenario (string): The SDA scenario selected. One of c('WB2C', '1.5C', 'ETP B2DS').
    Output:
      out (data.frame): data.frame with the emission paths per scenario for Scope 1 and Scope 2"
    
    if(input$target_setting_method == "Absolute Contraction Approach") {
      target_setting_function <- calculate_absolute_contraction
    } else {
      target_setting_function <- calculate_sda
    }
    
    out <- data.frame(
      year = seq(input$base_year, input$target_year),
      scope1 = input$scope_1_emissions,
      scope2 = input$scope_2_emissions
    ) %>%
      mutate(scope1 = target_setting_function(scope1, scenario, "scope1"),
             scope2 = target_setting_function(scope2, scenario, "scope2")) %>%
      pivot_longer(!year, names_to = "scope", values_to = "emissions") %>%
      mutate(scenario = scenario,
             # Keep only values >= 0
             emissions = ifelse(emissions >= 0, emissions, 0))
    return(out)
  }
  
  emission_path <- reactive({
    "The resulting emission paths (= the Science Based Target)"
    
    # Set required input fields and scenarios
    if(input$target_setting_method == "Absolute Contraction Approach") {
      req(input$scope_1_emissions, input$scope_2_emissions)
      scenarios <- c("WB2C", "1.5C")
    } else {
      scenarios <- c(input$sda_scenario)
      if(input$projected_output_measure == "Fixed market share") {
        req(input$scope_1_emissions, input$scope_2_emissions, input$base_year_output)
      } else {
        req(input$scope_1_emissions, input$scope_2_emissions, input$base_year_output, input$growth)
      }
    }
    
    out <- data.frame()
    for (scenario in scenarios) {
      results <- prepare_emission_path(scenario)
      out <- rbind(out, results)
    }
    out <- out %>%
      rownames_to_column("data_id") %>%
      mutate(scope = recode_for_plot(scope),
             emissions = round(emissions, 2))
    return(out)
  })

}