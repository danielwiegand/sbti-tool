"This file contains the calculations to establish a Science Based Target"

# Absolute contraction ####
calculate_absolute_contraction <- function(emissions, scenario, scope){
  "Function to calculate the absolute contraction target.
  Inputs:
    emissions (numeric vector): emissions between base and target year
    scenario (string): The SDA scenario selected. One of c('WB2C', '1.5C', 'ETP B2DS')
    scope (string): One of c('scope1', 'scope2')
  Output:
    out (numeric vector): Science Based Target pathway"
  
  annual_reduction <- ifelse(scenario == "WB2C", ANNUAL_REDUCTION_WB2C, ANNUAL_REDUCTION_1.5C)
  reduction <- emissions * annual_reduction
  # Take the cumsum and reduce reduction amount by reduction[1], because we want to start reducing in the 
  # year following the base year (no reductions should apply to base year itself):
  reduction <- cumsum(reduction) - reduction[1]
  out <- emissions - reduction
  return(out)
}

# Sectoral decarbonization approach (SDA) ####
# Calculation steps follow those in the official manual (https://sciencebasedtargets.org/resources/files/Sectoral-Decarbonization-Approach-Report.pdf)

calculate_sda <- function(emissions, scenario, scope) {
  "Function to calculate the SDA target.
  Inputs:
    emissions (numeric vector): emissions between base and target year
    scenario (string): The SDA scenario selected. One of c('WB2C', '1.5C', 'ETP B2DS')
    scope (string): One of c('scope1', 'scope2')
  Output:
    out (numeric vector): Science Based Target pathway"
  
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