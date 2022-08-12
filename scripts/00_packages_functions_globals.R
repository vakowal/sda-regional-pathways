################################
# SDA regional pathways:
# Load packages, define functions, and spell out project globals
# (including directory paths)
################################

# Load packages
library(tidyverse)
library(rstudioapi)

# globals
BASE_YEAR = 2020
TARGET_YEAR = 2050

script_dir <- dirname(getSourceEditorContext()$path)
repo_dir <- dirname(script_dir)

# Custom functions

# Calculate intensity pathway.
# Args:
#  - company_activity (vector): activity (m2) of company by year, from base
#    year to target year
#  - sector_activity (vector): activity (m2) of sector by year, from base year
#    to target year
#  - sector_emissions (vector): emissions (MtCO2) of sector by year, from base
#    year to target year
#  - company_emissions_base (scalar): emissions (MtCO2) of company in base year
#  - m_flag: determines calculation of m parameter. 0 - default/standard calculation,
#    1 - m set equal to 1, 2 - m floor set at 0.2. Defaults to 0.
# Returns:
#  - a data frame containing company_intensity target in kCO2/m2 and year
# IMPORTANT: target year must be 2050
CalcIntensityPathway <- function(
    company_activity, company_emissions_base, 
    sector_activity, sector_emissions, m_flag){
  sector_intensity <- (sector_emissions * 1000000) / sector_activity
  SI_2050 <- sector_intensity[length(sector_intensity)]  # final item must describe 2050
  CI_base <- (company_emissions_base * 1000) / company_activity[1]  # TODO check units here
  d_diff_param <- CI_base - SI_2050
  m_param <- if(m_flag == 1){
    CalcMParam_v1(company_activity, sector_activity)
  } else if (m_flag == 2){
    CalcMParam_v2(company_activity, sector_activity)
  } else {
    CalcMParam_v0(company_activity, sector_activity)
  }
  p_convergence_idx <- (
    (sector_intensity - SI_2050) / (sector_intensity[1] - SI_2050))
  company_intensity_target <- (
    d_diff_param * p_convergence_idx * m_param + SI_2050)
  result_df <- data.frame(
    year=seq(BASE_YEAR, TARGET_YEAR, by=1),
    intensity_SDA=company_intensity_target)
  return(result_df)
}

# Default calculation of m parameter.
# Args:
#   - company activity (vector): activity (m2) of company by year
#   - sector activity (vector): activity (m2) of sector by year
# Returns:
# A vector showing m parameter for each year between base year and target year.
# The two vectors - company_activity and sector_activity - must have identical 
# length and must end with the target year.
CalcMParam_v0 <- function(company_activity, sector_activity){
  target_year_ratio <- (
    company_activity[length(company_activity)] /
      sector_activity[length(sector_activity)])
  m_param <- (company_activity / sector_activity) / target_year_ratio
  m_param[m_param > 1] <- 1
  return(m_param)
}

# Version 1 calculation of m parameter - m set equal to 1.
# Args:
#   - company activity (vector): activity (m2) of company by year
#   - sector activity (vector): activity (m2) of sector by year
# Returns:
# A vector showing m parameter for each year between base year and target year.
# The two vectors - company_activity and sector_activity - must have identical 
# length and must end with the target year.
CalcMParam_v1 <- function(company_activity, sector_activity){
  target_year_ratio <- (
    company_activity[length(company_activity)] /
      sector_activity[length(sector_activity)])
  m_param <- (company_activity / sector_activity) / target_year_ratio
  m_param <- 1
  return(m_param)
}

# Version 2 calculation of m parameter - m floor set to 0.2.
# Args:
#   - company activity (vector): activity (m2) of company by year
#   - sector activity (vector): activity (m2) of sector by year
# Returns:
# A vector showing m parameter for each year between base year and target year.
# The two vectors - company_activity and sector_activity - must have identical 
# length and must end with the target year.
CalcMParam_v2 <- function(company_activity, sector_activity){
  target_year_ratio <- (
    company_activity[length(company_activity)] /
      sector_activity[length(sector_activity)])
  m_param <- (company_activity / sector_activity) / target_year_ratio
  m_param[m_param < 0.2] <- 0.2
  return(m_param)
}

# Project directories
wd <- list()
wd$raw_data <- paste0(repo_dir, '/data/raw/')
wd$processed_data <- paste0(repo_dir, '/data/processed/')
wd$figs <- paste0(repo_dir, '/figures/')
