################################
# SDA regional pathways:
# Load packages, define functions, and spell out project globals
# (including directory paths)
################################

# Load packages
library(ggplot2)
library(rstudioapi)

# current directory
script_dir <- dirname(getSourceEditorContext()$path)
repo_dir <- dirname(script_dir)

# Custom functions

# Calculate intensity pathway.
# Args:
#  - company activity (vector): activity (m2) of company by year, from base
#    year to target year
#  - sector activity (vector): activity (m2) of sector by year, from base year
#    to target year
#  - sector_emissions (vector): emissions (MtCO2) of sector by year, from base
#    year to target year
#  - company_emissions_base (scalar): emissions (MtCO2) of company in base year
# Returns:
#  - company_intensity target (vector), a vector of intensity (kCO2/m2) from
#    base year to target year
# IMPORTANT: target year must be 2050
CalcIntensityPathway <- function(
    company_activity, company_emissions_base, 
    sector_activity, sector_emissions){
  sector_intensity <- (sector_emissions * 1000000) / sector_activity
  SI_2050 <- sector_intensity[length(sector_intensity)]  # final item must describe 2050
  CI_base <- (company_emissions_base * 1000) / company_activity[1]  # TODO check units here
  d_diff_param <- CI_base - SI_2050
  m_param <- CalcMParam(company_activity, sector_activity)
  p_convergence_idx <- (
    (sector_intensity - SI_2050) / (sector_intensity[1] - SI_2050))
  company_intensity_target <- (
    d_diff_param * p_convergence_idx * m_param + SI_2050)
  return(company_intensity_target)
}

# Default calculation of m parameter.
# Args:
#   - company activity (vector): activity (m2) of company by year
#   - sector activity (vector): activity (m2) of sector by year
# The two vectors must have identical length and must end with the target year.
CalcMParam <- function(company_activity, sector_activity){
  target_year_ratio <- (
    company_activity[length(company_activity)] /
      sector_activity[length(sector_activity)])
  m_param <- (company_activity / sector_activity) / target_year_ratio
  m_param[m_param > 1] <- 1
  return(m_param)
}

# Project directories
wd <- list()
wd$raw_data <- paste0(repo_dir, '/data/raw/')
wd$processed_data <- paste0(repo_dir, '/data/processed/')
wd$figs <- paste0(repo_dir, '/figures/')
