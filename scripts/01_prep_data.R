################################
# SDA regional pathways:
# Clean and process raw data
################################

# get location of related scripts
library(rstudioapi)
script_dir <- dirname(getSourceEditorContext()$path)

# package imports, function definitions, and globals including directory
source(paste0(script_dir, "/00_packages_functions_globals.R"))

# process empirical activity and emissions data
activity_table <- read.csv(
  paste0(wd$raw_data, 'buildings_empirical_activity.csv'))
emissions_table <- read.csv(
  paste0(wd$raw_data, 'buildings_empirical_emissions.csv'))

# some pathways have base years other than 2020
Yu_interp_activity <- activity_table[
  (activity_table$Reference_key == 'Yu' & activity_table$Year == 2010), ]
Yu_interp_activity[, 'Year'] <- BASE_YEAR
Yu_interp_activity[, 'Activity_millions_m2'] <- 17500

Ost_interp_activity <- activity_table[
  (activity_table$Reference_key == 'Ostermeyer' & activity_table$Year == 2018), ]
Ost_interp_activity[, 'Year'] <- BASE_YEAR
Ost_interp_activity[, 'Activity_millions_m2'] <- 2189
activity_table <- rbind(
  activity_table, Yu_interp_activity, Ost_interp_activity)

Yu_interp_emissions <- emissions_table[
  (emissions_table$Reference_key == 'Yu' & emissions_table$Year == 2010), ]
Yu_interp_emissions[, 'Year'] <- BASE_YEAR
Yu_interp_emissions[, 'Emissions_MtCO2'] <- 268

Ost_interp_emissions <- emissions_table[
  (emissions_table$Reference_key == 'Ostermeyer' & emissions_table$Year == 2018), ]
Ost_interp_emissions[, 'Year'] <- BASE_YEAR
Ost_interp_emissions[, 'Emissions_MtCO2'] <- 89.8
emissions_table <- rbind(
  emissions_table, Yu_interp_emissions, Ost_interp_emissions)

# some pathways have combined sector activity data, and separated emissions
agg_emissions_df <- aggregate(
  Emissions_MtCO2~Reference_key + Scenario_key + Region + Year + Scope,
  data=emissions_table, FUN=sum, subset=Scenario_key %in% c('IMAGE', 'SDS'))
agg_emissions_df$Sector <- 'Combined'
agg_emissions_df <- agg_emissions_df[, colnames(emissions_table)]
emissions_processed <- rbind(
  agg_emissions_df,
  emissions_table[emissions_table$Scenario_key %in% c('-', 'RECC'), ])

# calculate sector data for combined residential+services: sum
buildings_sector_data <- read.csv(
  paste0(wd$raw_data, 'buildings_sector_data.csv'))
combined_s1_emissions <- aggregate(
  Sector_Scope_1_emissions~year, data=buildings_sector_data, FUN=sum)
combined_s2_emissions <- aggregate(
  Sector_Scope_2_emissions~year, data=buildings_sector_data, FUN=sum)
combined_activity <- aggregate(
  Sector_activity~year, data=buildings_sector_data, FUN=sum)
combined_emissions <- merge(combined_s1_emissions, combined_s2_emissions)
combined_sector <- merge(combined_emissions, combined_activity)
combined_sector$Sector <- 'Combined'
sector_data_processed <- rbind(
  combined_sector, buildings_sector_data[, colnames(combined_sector)])

# calculate SDA pathways
comb_info_df <- unique(
  emissions_processed[
    (emissions_processed$Year == BASE_YEAR &
       emissions_processed$Scope %in% c(1, 2)),
    c('Reference_key', 'Scenario_key', 'Region', 'Sector', 'Scope')])
df_list <- list()
for(row_idx in 1:nrow(comb_info_df)) {
  ref_key <- comb_info_df[row_idx, 1]
  scen_key <- comb_info_df[row_idx, 2]
  reg_key <- comb_info_df[row_idx, 3]
  sect_key <- comb_info_df[row_idx, 4]
  scope_key <- comb_info_df[row_idx, 5]

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
  sector_activity <- sector_data_processed[
    (sector_data_processed$year >= BASE_YEAR &
       sector_data_processed$Sector == sect_key), 'Sector_activity']
  
  company_emissions_base <- subset(
    emissions_processed,
    (Reference_key == ref_key) & (Scenario_key == scen_key) &
      (Region == reg_key) & (Sector == sect_key) & (Scope == scope_key) &
      (Year == BASE_YEAR),
    select=Emissions_MtCO2)[[1]] * 1000000
  if(scope_key == 1) {
    sector_emissions <- sector_data_processed[
      (sector_data_processed$year >= BASE_YEAR &
         sector_data_processed$Sector == sect_key), 'Sector_Scope_1_emissions']
  } else if(scope_key == 2) {
    sector_emissions <- sector_data_processed[
      (sector_data_processed$year >= BASE_YEAR &
         sector_data_processed$Sector == sect_key), 'Sector_Scope_2_emissions']
  } else {
    sector_emissions <- 'NA'  # TODO handle combined scope 1 & 2 emissions
  }
  intensity_df <- CalcIntensityPathway(
    company_activity, company_emissions_base, sector_activity, sector_emissions)
  intensity_df$Reference_key <- ref_key
  intensity_df$Scenario_key <- scen_key
  intensity_df$Region <- reg_key
  intensity_df$Sector <- sect_key
  intensity_df$Scope <- scope_key
  df_list[[row_idx]] <- intensity_df
}
results_combined <- do.call(rbind, df_list)
write.csv(
  results_combined,
  paste0(wd$processed_data,'SDA_pathways_normative_scenarios.csv'),
  row.names=FALSE)

# TODO calculate empirical intensity

