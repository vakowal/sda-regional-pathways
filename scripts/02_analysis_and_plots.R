################################
# SDA regional pathways:
# M parameter sensitivity analysis
# and exploratory plots
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

# plot SDA pathways calculated with different values of m parameter
sda_pathways <- read.csv(
  paste0(wd$processed_data,'SDA_pathways_m_0_1_2.csv'))

# do not plot the m floor option
sda_pathways_subs <- sda_pathways[sda_pathways$m_flag < 2, ]
sda_pathways_subs[, 'm parameter option'] <- factor( 
  sda_pathways_subs$m_flag, levels <- c(0, 1), labels=c('default', 'm = 1'))

# hack so that different references appear in one row
sda_pathways_subs[
  sda_pathways_subs$Scenario_key == "-", 'Scenario_key'] <- sda_pathways_subs[
    sda_pathways_subs$Scenario_key == "-", 'Reference_key']

# plot pathways with different values of m
for(region in unique(sda_pathways_subs$Region)) {
  subs_df <- sda_pathways_subs[sda_pathways_subs$Region == region, ]
  num_cols <- length(unique(subs_df$Scenario_key))
  p <- ggplot(subs_df, aes(x=year, y=intensity_SDA, group=`m parameter option`))
  p <- p + geom_line(aes(linetype=`m parameter option`))
  p <- p + facet_wrap(~Scenario_key)
  p <- p + labs(fill="m parameter option")
  p <- p + ylab("SDA Intensity (kg CO2 / m2)") + 
                ggtitle(region, subtitle = "M parameter testing") + 
                theme(axis.text.x = element_text(angle = 45))
  filename <- paste0(wd$figs, paste0("m_param_", region, ".png"))
  png(filename, width=((num_cols * 2.5) + 2.5), height=4, units='in', res=300)
  print(p)
  dev.off()
}

# make a table: difference between default m and m=1, in the year 2030
m_2030_df <- sda_pathways_subs[sda_pathways_subs$year == 2030, ]
m_2030_res <- reshape(m_2030_df, direction='wide',
                      idvar=c('Reference_key', 'Scenario_key', 'Region',
                              'Sector'), timevar='m parameter option',
                      v.names='intensity_SDA', drop='m_flag')
m_2030_res$m_diff <- (
  m_2030_res$`intensity_SDA.m = 1` - m_2030_res$intensity_SDA.default)
# write out for reshaping and formatting by hand
write.csv(m_2030_res, file=paste0(
  wd$processed_data, 'diff_SDA_intensity_m_default_m=1_2030.csv'),
  row.names=FALSE)

# calculate cumulative emissions difference between versions of m
sum_emissions_df <- aggregate(
  abs_emissions~Reference_key + Scenario_key + Region + `m parameter option`,
  data=sda_pathways_subs, FUN=sum)  # TODO potentially sum up other ranges
sum_emissions_res <- reshape(sum_emissions_df, direction='wide',
                             idvar=c('Reference_key', 'Scenario_key', 'Region'),
                             timevar='m parameter option',
                             v.names='abs_emissions')
sum_emissions_res$diff_MtCO2 <- (
  sum_emissions_res$`abs_emissions.m = 1` -
    sum_emissions_res$abs_emissions.default) / 1000
# write out for reshaping and formatting by hand
write.csv(sum_emissions_res, file=paste0(
  wd$processed_data, 'diff_SDA_cumulative_emissions_m_default_m=1_2020-2050.csv'),
  row.names=FALSE)

# display cumulative emissions for regions that show a difference
comb_info_df <- sum_emissions_res[sum_emissions_res$diff > 0, ]
df_list <- list()
df_idx <- 1
for(row_idx in 1:nrow(comb_info_df)) {
  ref_key <- comb_info_df[row_idx, 1]
  scen_key <- comb_info_df[row_idx, 2]
  reg_key <- comb_info_df[row_idx, 3]
  for (m_key in unique(sda_pathways_subs$m_flag)) {
    subs_df <- sda_pathways_subs[(
      (sda_pathways_subs$Reference_key == ref_key) &
        (sda_pathways_subs$Scenario_key == scen_key) &
        (sda_pathways_subs$Region == reg_key) &
        (sda_pathways_subs$m_flag == m_key)), ]
    subs_df <- subs_df[order(subs_df$year), ]
    subs_df$cumulative_emissions <- cumsum(subs_df$abs_emissions)
    df_list[[df_idx]] <- subs_df
    df_idx <- df_idx + 1
  }
}
cumulative_df <- do.call(rbind, df_list)

p <- ggplot(cumulative_df, aes(x=year, y=cumulative_emissions / 1000000,
                               group=`m parameter option`)) +
  geom_line(aes(linetype=`m parameter option`)) +
  facet_grid(Scenario_key~Region, scales='free') +
  ylab("Cumulative emissions (MtCO2)") + xlab("") +
  theme(axis.text.x = element_text(angle = 45), legend.position='bottom',
        legend.title=element_blank())
filename <- paste0(wd$figs, "cumulative_emissions_m_versions.png")
png(filename, width=8, height=5, units = 'in', res = 300)
print(p)
dev.off()

# plot pathways for IPCC normative models, literature data vs SDA data
# subset intensity pathways df to include only default m param pathways from SDA 
# and pathways from literature
intensity_paths_all <- read.csv(
  paste0(wd$processed_data, 'intensity_paths_all.csv'))
intensity_paths_subs <- intensity_paths_all[
  (intensity_paths_all$m_flag == 0) | (is.na(intensity_paths_all$m_flag)), ]
intensity_paths_subs[
  intensity_paths_subs$Scenario_key == "-", 'Scenario_key'] <- intensity_paths_subs[
    intensity_paths_subs$Scenario_key == "-", 'Reference_key']
for (region in unique(intensity_paths_subs$Region)) {
  int_subs <- intensity_paths_subs[intensity_paths_subs$Region == region, ]
  p <- ggplot(int_subs, aes(x = Year, y = intensity, group = method)) +
      geom_line(aes(colour = method)) +
      facet_grid(Region ~ Scenario_key) +
      labs(fill = "Method") +
      ggtitle(region, subtitle = "SDA vs. pathways from literature") +
      xlab("") +
      theme(axis.text.x = element_text(angle = 45)) +
      ylab("Intensity (kg CO2 / m2)")
  filename <- paste0(wd$figs, paste0("SDA_vs_Lit_", region, ".png"))
  png(filename, width = 6, height = 3, units = 'in', res = 300)
  print(p)
  dev.off()
  print(region)
}

# plot CRREM EU country-level pathways vs Ostermeyer country-level bottom-up pathways
# subset df to include only countries included in both sources: 
# France, Germany, Poland
CRREM_ost <- read.csv(paste0(wd$processed_data, "CRREM_ostermeyer.csv"))
CRREM_ost_subs <- subset(CRREM_ost, country %in% c("France", "Germany", 
                                                   "Poland"))

for (country in unique(CRREM_ost_subs$country)) {
  EU_pathway <- CRREM_ost_subs[CRREM_ost_subs$country == country, ]
  p <- ggplot(EU_pathway, aes(x = year, y = intensity, group = Scenario_key)) +
       geom_line(aes(colour = Scenario_key)) +
       labs(fill = "Scenario_key") +
       ggtitle(country, subtitle = "CRREM vs bottom-up country pathways") +
       xlab("") +
       theme(axis.text.x = element_text(angle = 45)) +
       ylab("Intensity (kg CO2 / m2)")
  filename <- paste0(wd$figs, paste0("CRREM_vs_Ostermeyer_", country, ".png"))
  png(filename, width = 6, height = 3, units = 'in', res = 300)
  print(p)
  dev.off()
  print(country)
}