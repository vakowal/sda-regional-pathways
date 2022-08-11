################################
# SDA regional pathways:
# Clean and process raw data
################################

# get location of related scripts
library(rstudioapi)
script_dir <- dirname(getSourceEditorContext()$path)

# package imports, function definitions, and globals including directory
source(paste0(script_dir, "/00_packages_functions_globals.R"))

# read data, clean and process
multi_family <- read.csv(paste0(wd$raw_data, "2022-08-10_CRREM-multi-family.csv"))
single_family <- read.csv(paste0(wd$raw_data, "2022-08-10_CRREM-single-family.csv"))

multi_family <- multi_family %>% 
  pivot_longer(!ï..YEAR, names_to = "country", values_to = "emissions_intensity")  
  
multi_family$building_type <- "Multi-family residential"
colnames(multi_family)[1] <- "year"

single_family <- single_family %>% 
  pivot_longer(!ï..YEAR, names_to = "country", values_to = "emissions_intensity")

single_family$building_type <- "Single-family residential"
colnames(single_family)[1] <- "year"

CRREM_EU_residential <- union(multi_family, single_family)

# calculate SDA
buildings_sector_data <- read.csv(
  paste0(wd$raw_data, 'buildings_sector_data.csv'))
activity_table <- read.csv(
  paste0(wd$raw_data, 'buildings_empirical_activity.csv'))
emissions_table <- read.csv(
  paste0(wd$raw_data, 'buildings_empirical_emissions.csv'))

# tractable combinations right now: separate residential/commercial, scope 1/2
comb_info_list <- list(
  c('IPCC_normative', 'RECC', 'North America', 'Residential', 1),
  c('IPCC_normative', 'RECC', 'North America', 'Residential', 2),
  c('IPCC_normative', 'RECC', 'Europe and Eurasia', 'Residential', 1),
  c('IPCC_normative', 'RECC', 'Europe and Eurasia', 'Residential', 2),
  c('IPCC_normative', 'RECC', 'Africa', 'Residential', 1),
  c('IPCC_normative', 'RECC', 'Africa', 'Residential', 2),
  c('IPCC_normative', 'RECC', 'Asia Pacific Developed', 'Residential', 1),
  c('IPCC_normative', 'RECC', 'Asia Pacific Developed', 'Residential', 2),
  c('IPCC_normative', 'RECC', 'Latin America and Caribbean', 'Residential', 1),
  c('IPCC_normative', 'RECC', 'Latin America and Caribbean', 'Residential', 2),
  c('IPCC_normative', 'RECC', 'Southeast Asia and Developing Pacific', 'Residential', 1),
  c('IPCC_normative', 'RECC', 'Southeast Asia and Developing Pacific', 'Residential', 2),
  c('IPCC_normative', 'RECC', 'Eastern Asia', 'Residential', 1),
  c('IPCC_normative', 'RECC', 'Eastern Asia', 'Residential', 2),
  c('IPCC_normative', 'RECC', 'Middle East', 'Residential', 1),
  c('IPCC_normative', 'RECC', 'Middle East', 'Residential', 2),
  c('IPCC_normative', 'RECC', 'Southern Asia', 'Residential', 1),
  c('IPCC_normative', 'RECC', 'Southern Asia', 'Residential', 2))
df_list <- list()
for(list_idx in 1:length(comb_info_list)) {
  comb_info <- comb_info_list[[list_idx]]
  ref_key <- comb_info[1]
  scen_key <- comb_info[2]
  reg_key <- comb_info[3]
  sect_key <- comb_info[4]
  scope_key <- comb_info[5]
  
  company_activity_base <- subset(
    activity_table,
    (Reference_key == ref_key) & (Scenario_key == scen_key) &
      (Region == reg_key) & (Sector == sect_key) & (Year == BASE_YEAR),
    select=Activity_millions_m2)[[1]] * 1000000
  company_activity_target <- subset(
    activity_table,
    (Reference_key == ref_key) & (Scenario_key == scen_key) &
      (Region == reg_key) & (Sector == sect_key) & (Year == TARGET_YEAR),
    select=Activity_millions_m2)[[1]] * 1000000
  company_activity <- seq(
    company_activity_base, company_activity_target,
    length.out=(TARGET_YEAR-BASE_YEAR) + 1)
  sector_activity <- buildings_sector_data[
    (buildings_sector_data$year >= BASE_YEAR &
       buildings_sector_data$Sector == sect_key), 'Sector_activity']
  
  company_emissions_base <- subset(
    emissions_table,
    (Reference_key == ref_key) & (Scenario_key == scen_key) &
      (Region == reg_key) & (Sector == sect_key) & (Scope == scope_key) &
      (Year == BASE_YEAR),
    select=Emissions_MtCO2)[[1]] * 1000000
  if(scope_key == 1) {
    sector_emissions <- buildings_sector_data[
      (buildings_sector_data$year >= BASE_YEAR &
         buildings_sector_data$Sector == sect_key), 'Sector_Scope_1_emissions']
  } else if(scope_key == 2) {
    sector_emissions <- buildings_sector_data[
      (buildings_sector_data$year >= BASE_YEAR &
         buildings_sector_data$Sector == sect_key), 'Sector_Scope_2_emissions']
  } else {
    sector_emissions <- 'NA'
  }
  intensity_df <- CalcIntensityPathway(
    company_activity, company_emissions_base, sector_activity, sector_emissions)
  intensity_df$Reference_key <- ref_key
  intensity_df$Scenario_key <- scen_key
  intensity_df$Region <- reg_key
  intensity_df$Sector <- sect_key
  intensity_df$Scope <- scope_key
  df_list[[list_idx]] <- intensity_df
}
results_combined <- do.call(rbind, df_list)
write.csv(
  results_combined,
  paste0(wd$processed_data,'SDA_pathways_RECC_residential.csv'),
  row.names=FALSE)
