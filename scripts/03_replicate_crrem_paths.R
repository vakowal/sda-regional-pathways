################################
# SDA regional pathways:
# Calculate SDA pathways from 
# CRREM inputs and compare to
# CRREM pathways
################################

# prep data files if they do not already exist
# get location of related scripts
library(rstudioapi)
script_dir <- dirname(getSourceEditorContext()$path)
source(paste0(script_dir, '/01_prep_data.R'), encoding='utf-8')

# package imports, function definitions, and globals including directory
library(rstudioapi)
script_dir <- dirname(getSourceEditorContext()$path)
source(paste0(script_dir, "/00_packages_functions_globals.R"))

crrem_resi_int_df <- read_excel(
  paste0(wd$raw_data, 'Residential Pathway_Floor Area_2022_08_26.xlsx'),
  sheet='Residential Pathways')
crrem_resi_act_df <- read_excel(
  paste0(wd$raw_data, 'Residential Pathway_Floor Area_2022_08_26.xlsx'),
  sheet='Europe Floor Area')

processed_sector_data <- read.csv(
    paste0(wd$processed_data, 'processed_sector_data.csv'))

df_list <- list()
region_list <- c('AT', 'BU', 'EE', 'FI', 'DE', 'NL')
for (region in region_list) {
	region_activity_base <- crrem_resi_act_df[
	  (crrem_resi_act_df$Year == BASE_YEAR) &
	    (!is.na(crrem_resi_act_df$Year)), paste0(region, '.RESI.FA'), drop=TRUE]
	region_activity_target <- crrem_resi_act_df[
	  (crrem_resi_act_df$Year == TARGET_YEAR) &
	    (!is.na(crrem_resi_act_df$Year)), paste0(region, '.RESI.FA'), drop=TRUE]
	region_activity <- seq(
		region_activity_base, region_activity_target,
		length.out=(TARGET_YEAR - BASE_YEAR) + 1)

	region_intensity_base <- crrem_resi_int_df[
	  (crrem_resi_int_df$Year == BASE_YEAR) &
	    (!is.na(crrem_resi_int_df$Year)), paste0(region, '.RESI.CO2-INT'),
	  drop=TRUE]
	# calculate emissions in MtCO2 from intensity in kgCO2 / m2
	region_emissions_base <- (
	  (region_intensity_base * region_activity_base) / KG_T_CONV)

	sector_activity <- processed_sector_data[
        (processed_sector_data$year >= BASE_YEAR &
           processed_sector_data$Sector == 'Residential' &
           processed_sector_data$Scenario == 'SBTi_1.5C'), 'Sector_activity']
	sector_emissions <- processed_sector_data[
	  (processed_sector_data$year >= BASE_YEAR &
	     processed_sector_data$Sector == 'Residential' &
	     processed_sector_data$Scenario == 'SBTi_1.5C'),
	  'Sector_Scope_1_2_emissions']
	
	intensity_df <- CalcIntensityPathway(
	  region_activity, region_emissions_base, sector_activity,
	  sector_emissions, m_flag=0)
	intensity_df$region <- region
	df_list[[region]] <- intensity_df
}
crrem_calc_df <- do.call(rbind, df_list)
