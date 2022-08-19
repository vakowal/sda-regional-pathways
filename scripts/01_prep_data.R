################################
# SDA regional pathways:
# Clean and process raw data
################################

# get location of related scripts
library(rstudioapi)
script_dir <- dirname(getSourceEditorContext()$path)

# package imports, function definitions, and globals including directory
source(paste0(script_dir, "/00_packages_functions_globals.R"))

# read in data, clean and process - CRREM EU residential pathways
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
CRREM_EU_residential$scope <- "Combined"

# process empirical activity data, if they don't already exist
if (!file.exists(paste0(wd$processed_data, 'processed_empirical_activity.csv'))) {
  activity_table <- read.csv(
    paste0(wd$raw_data, 'buildings_empirical_activity.csv'))
  
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
  write.csv(
    activity_table, paste0(wd$processed_data, 'processed_empirical_activity.csv'),
    row.names=FALSE)
  print("Processed empirical activity data")
} else {
  print("Processed empirical activity already exists - no changes made")
}

# process empirical emissions data, if they don't already exist
if (!file.exists(paste0(wd$processed_data, 'processed_empirical_emissions.csv'))) {
  emissions_table <- read.csv(
    paste0(wd$raw_data, 'buildings_empirical_emissions.csv'))
  
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
  
  # calculate combined scope 1 & 2 emissions for sources reporting both
  emissions_scope_1_2 <- aggregate(
    Emissions_MtCO2~Reference_key + Scenario_key + Region + Year + Sector,
    data=emissions_processed, FUN=sum)
  write.csv(
    emissions_scope_1_2,
    paste0(wd$processed_data, 'processed_empirical_emissions.csv'),
    row.names=FALSE)
  print("Processed empirical emissions data")
} else {
  print("Processed empirical emissions already exists - no changes made")
}

# process sector data
if (!file.exists(paste0(wd$processed_data, 'processed_sector_data.csv'))) {
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
  
  # calculate combined scope 1 & 2 sector emissions
  sector_data_processed$Sector_Scope_1_2_emissions <- (
    sector_data_processed$Sector_Scope_1_emissions + 
      sector_data_processed$Sector_Scope_2_emissions)
  write.csv(
    sector_data_processed, paste0(wd$processed_data, 'processed_sector_data.csv'),
    row.names=FALSE)
  print("Generated processed sector data")
} else {
  print("Processed sector data already exists - no changes made")
}
 
# calculate SDA pathways if they don't already exist
if (!file.exists(paste0(wd$processed_data,'SDA_pathways_m_0_1_2.csv'))) {
  processed_emp_emissions <- read.csv(
    paste0(wd$processed_data, 'processed_empirical_emissions.csv'))
  processed_emp_activity <- read.csv(
    paste0(wd$processed_data, 'processed_empirical_activity.csv'))
  processed_sector_data <- read.csv(
    paste0(wd$processed_data, 'processed_sector_data.csv'))
  
  comb_info_df <- unique(
    processed_emp_emissions[
      (processed_emp_emissions$Year == BASE_YEAR),
      c('Reference_key', 'Scenario_key', 'Region', 'Sector')])
  df_list <- list()
  df_idx <- 1
  for(row_idx in 1:nrow(comb_info_df)) {
    ref_key <- comb_info_df[row_idx, 1]
    scen_key <- comb_info_df[row_idx, 2]
    reg_key <- comb_info_df[row_idx, 3]
    sect_key <- comb_info_df[row_idx, 4]
    
    company_activity_base <- subset(
      processed_emp_activity,
      (Reference_key == ref_key) & (Scenario_key == scen_key) &
        (Region == reg_key) & (Sector == sect_key) & (Year == BASE_YEAR),
      select=Activity_millions_m2)[[1]] * 1000000
    company_activity_target <- subset(
      processed_emp_activity,
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
      processed_emp_emissions,
      (Reference_key == ref_key) & (Scenario_key == scen_key) &
        (Region == reg_key) & (Sector == sect_key) & (Year == BASE_YEAR),
      select=Emissions_MtCO2)[[1]] * 1000000
    
    sector_emissions <- sector_data_processed[
      (sector_data_processed$year >= BASE_YEAR &
         sector_data_processed$Sector == sect_key), 'Sector_Scope_1_2_emissions']
    
    # different ways of calculating m
    for(m_flag in c(0, 1, 2)) {
      intensity_df <- CalcIntensityPathway(
        company_activity, company_emissions_base, sector_activity,
        sector_emissions, m_flag)
      intensity_df$Reference_key <- ref_key
      intensity_df$Scenario_key <- scen_key
      intensity_df$Region <- reg_key
      intensity_df$Sector <- sect_key
      intensity_df$m_flag <- m_flag
      df_list[[df_idx]] <- intensity_df
      df_idx <- df_idx + 1
    }
  }
  results_combined <- do.call(rbind, df_list)
  write.csv(
    results_combined,
    paste0(wd$processed_data,'SDA_pathways_m_0_1_2.csv'),
    row.names=FALSE)
  print("Generated SDA pathways from empirical emissions and activity")
} else {
  print("SDA pathways already exist - no changes made")
}

# calculate empirical intensity
if (!file.exists(paste0(wd$processed_data, 'intensity_paths_all.csv'))) {
  processed_emp_emissions <- read.csv(
    paste0(wd$processed_data, 'processed_empirical_emissions.csv'))
  processed_emp_activity <- read.csv(
    paste0(wd$processed_data, 'processed_empirical_activity.csv'))
  
  empirical_intensity <- merge(processed_emp_activity, processed_emp_emissions)
  empirical_intensity <- empirical_intensity %>% 
    mutate(intensity = (Emissions_MtCO2 * 1000) / Activity_millions_m2) %>% 
    select(!c(Activity_millions_m2, Emissions_MtCO2))
  empirical_intensity$method <- "Literature"
  
  # merge empirical and SDA intensity pathways
  SDA_pathways <- read.csv(paste0(wd$processed_data,'SDA_pathways_m_0_1_2.csv'))
  SDA_pathways$method <- "SDA"
  colnames(SDA_pathways)[2] <- "intensity"
  colnames(SDA_pathways)[1] <- "Year"
  
  intensity_paths_all <- merge(SDA_pathways, empirical_intensity, all = TRUE)
  intensity_paths_all <- arrange(intensity_paths_all, Reference_key, Region, 
                                 Scenario_key, Year)
  write.csv(
    intensity_paths_all, paste0(wd$processed_data, 'intensity_paths_all.csv'),
    row.names=FALSE)
  print("Calculated empirical intensity pathways")
} else {
  print("Empirical intensity pathways already exist - no changes made")
}

# clean up environment
rm(list=ls())
