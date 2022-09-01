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
	abs_emissions_df <- CalcAbsolutePathway(
	  region_activity, intensity_df$intensity_SDA)
	sda_df <- merge(intensity_df, abs_emissions_df)
	colnames(sda_df) <- c('Year', 'Intensity', 'Emissions')
	sda_df$source <- 'SDA'
	sda_df$region <- region
	df_list[[region]] <- sda_df
}
crrem_calc_df <- do.call(rbind, df_list)

# compare to intensity supplied by CRREM
df_list <- list()
for (region in region_list) {
  crrem_activity <- crrem_resi_act_df[
    (!is.na(crrem_resi_act_df$Year)), paste0(region, '.RESI.FA'), drop=TRUE]
  crrem_intensity <- crrem_resi_int_df[
    (!is.na(crrem_resi_act_df$Year)), c(
      'Year', paste0(region, '.RESI.CO2-INT'))]
  crrem_emissions <- CalcAbsolutePathway(
    crrem_activity, crrem_intensity[, paste0(region, '.RESI.CO2-INT')])
  colnames(crrem_emissions) <- c('Year', 'Emissions')
  crrem_df <- merge(crrem_intensity, crrem_emissions)
  colnames(crrem_df) <- c('Year', 'Intensity', 'Emissions')
  crrem_df$source <- 'CRREM'
  crrem_df$region <- region
  df_list[[region]] <- crrem_df
}
crrem_rep_df <- do.call(rbind, df_list)

comb_df <- rbind(crrem_calc_df, crrem_rep_df)

# plot the differences
p <- ggplot(comb_df, aes(x=Year, y=Intensity, group=source)) +
  geom_line(aes(linetype=source)) + facet_wrap(~region, scales='free') +
  ylab("Intensity (kg CO2 / m2)") +
  theme(axis.text.x=element_text(angle=45), axis.title.x=element_blank())
print(p)
filename <- paste0(wd$figs, "crrem_intensity_vs_sda_intensity_09012022.png")
png(filename, width=7.5, height=5, units='in', res=300)
print(p)
dev.off()

# calculate absolute emissions and difference in cumulative emissions
sum_emissions_df <- aggregate(Emissions~source + region, data=comb_df,
                              FUN=sum)
sum_res <- reshape(sum_emissions_df, direction='wide',
                   idvar=c('region'), timevar=c('source'),
                   v.names='Emissions')
sum_res$perc_diff <- (sum_res$Emissions.CRREM - sum_res$Emissions.SDA) /
  sum_res$Emissions.SDA * 100
# add these percent diff numbers to the plot by hand

