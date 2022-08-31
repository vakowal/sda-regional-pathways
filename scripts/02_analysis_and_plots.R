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
  paste0(wd$processed_data,'SDA_pathways_m_0_1_2_3.csv'))

# do not plot the m floor option
sda_pathways_subs <- sda_pathways[sda_pathways$m_flag != 2, ]
sda_pathways_subs[, 'm parameter option'] <- factor( 
  sda_pathways_subs$m_flag, levels <- c(0, 1, 3), 
  labels=c('default', 'm = 1', 'no cap'))

# hack so that different references appear in one row
sda_pathways_subs[
  sda_pathways_subs$Scenario_key == "-", 'Scenario_key'] <- sda_pathways_subs[
    sda_pathways_subs$Scenario_key == "-", 'Reference_key']

# plot pathways with different values of m
for(region in unique(sda_pathways_subs$Region)) {
  subs_df <- sda_pathways_subs[sda_pathways_subs$Region == region, ]
  num_cols <- length(unique(subs_df$Scenario_key))
  p <- ggplot(subs_df, aes(x=year, y=intensity_SDA, group=`m parameter option`))
  p <- p + geom_line(aes(linetype=`m parameter option`)) +
       scale_linetype_manual(values = c("solid", "dashed", "dotted"))
  p <- p + facet_wrap(~Scenario_key)
  p <- p + labs(fill="m parameter option")
  p <- p + ylab("SDA Intensity (kg CO2 / m2)") + 
                ggtitle(region, subtitle = "M parameter testing") + 
                theme(axis.text.x = element_text(angle = 45))
  filename <- paste0(wd$figs, paste0("m_no_cap", region, ".png"))
  png(filename, width=((num_cols * 2.5) + 2.5), height=4, units='in', res=300)
  print(p)
  dev.off()
}

m_facet_2 <- subset(sda_pathways_subs, 
                   Region %in% c("North America", "Europe and Eurasia",
                                 "Africa", "Southern Asia"))
m_facet_2$Region <- str_replace_all(m_facet_2$Region, 
                                    "Europe and Eurasia",
                                    "Europe & Eurasia")
m_facet_2$Region <- factor(m_facet_2$Region,
                           levels=c('Europe & Eurasia', 'North America',
                                    'Africa', 'Southern Asia'))
m_facet_2$Scenario_key <- factor(m_facet_2$Scenario_key, 
                               levels = c("IMAGE", "SDS", "RECC"))

p <- ggplot(m_facet_2, aes(x=year, y=intensity_SDA, group=`m parameter option`)) +
  geom_line(aes(linetype=`m parameter option`)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotted")) +
  facet_grid(Region ~ Scenario_key, scales='free') +
  labs(fill="m parameter option") +
  xlab("") + theme(axis.text=element_text(angle=45), legend.position='bottom',
                   legend.title=element_blank()) +
  ylab("SDA Intensity (kg CO2 / m2)")
print(p)
filename <- paste0(wd$figs, "m_param_3_versions_selected_regions.png")
png(filename, width=5, height=6.3, units='in', res=300)
print(p)
dev.off()

# plot faceted grid showing subset of regions for m param testing
m_facet <- subset(sda_pathways_subs, 
                     Region %in% c("Africa", "Middle East", 
                                   "Southeast Asia and Developing Pacific", 
                                   "Southern Asia"))

m_facet$Region <- str_replace_all(m_facet$Region, 
                                     "Southeast Asia and Developing Pacific",
                                     "SE Asia & Dev. Pacific")

m_facet$Scenario_key <- factor(m_facet$Scenario_key, 
                                  levels = c("IMAGE", "SDS", "RECC"))


p <- ggplot(m_facet, aes(x = year, y = intensity_SDA, group = `m parameter option`)) +
     geom_line(aes(linetype=`m parameter option`)) +
     scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
     facet_grid(Region ~ Scenario_key) +
     labs(fill = "m parameter option") +
     ggtitle("M parameter testing") +
     xlab("") +
     theme(axis.text = element_text(angle = 45)) +
     ylab("SDA Intensity (kg CO2 / m2)")
filename <- paste0(wd$figs, "m_param_no_cap_facet.png")
png(filename, width = 7, height = 7, units = 'in', res = 300)
print(p)
dev.off()



# calculate cumulative emissions difference between versions of m
sum_emissions_df <- aggregate(
  abs_emissions~Reference_key + Scenario_key + Region + `m parameter option`,
  data=sda_pathways_subs, FUN=sum)  # TODO potentially sum up other ranges
sum_emissions_res <- reshape(sum_emissions_df, direction='wide',
                             idvar=c('Reference_key', 'Scenario_key', 'Region'),
                             timevar='m parameter option',
                             v.names='abs_emissions')
sum_emissions_res$perc_diff_m1 <- (
  (sum_emissions_res$`abs_emissions.m = 1` - sum_emissions_res$abs_emissions.default ) /
    sum_emissions_res$abs_emissions.default * 100)
sum_emissions_res$perc_diff_nocap <- (
  (sum_emissions_res$`abs_emissions.no cap` - sum_emissions_res$abs_emissions.default ) /
    sum_emissions_res$abs_emissions.default * 100)

p <- ggplot(sum_emissions_res, aes(x=perc_diff_m1)) +
  geom_histogram() + xlab("Percent difference: m = 1 vs default m")
filename <- paste0(wd$figs, "perc_diff_cumulative_emissions_m=1.png")
png(filename, width=4, height=4, units = 'in', res = 300)
print(p)
dev.off()

p <- ggplot(sum_emissions_res, aes(x=perc_diff_nocap)) +
  geom_histogram() + xlab("Percent difference: m no cap vs default m")
filename <- paste0(wd$figs, "perc_diff_cumulative_emissions_m_no_cap.png")
png(filename, width=4, height=4, units = 'in', res = 300)
print(p)
dev.off()

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
# Remove no cap m flag
cumulative_df <- subset(cumulative_df, m_flag != 3)

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

# Repeat above analysis and plotting for m no cap vs default m
sum_emissions_nocap_res <- sum_emissions_res
sum_emissions_nocap_res$diff_MtCO2 <- (
  sum_emissions_nocap_res$`abs_emissions.no cap` -
    sum_emissions_res$abs_emissions.default) / 1000000

comb_info_df_nocap <- sum_emissions_nocap_res[sum_emissions_nocap_res$diff_MtCO2 > 0, ]
df_list_nocap <- list()
df_idx_nocap <- 1
for(row_idx in 1:nrow(comb_info_df_nocap)) {
  ref_key <- comb_info_df_nocap[row_idx, 1]
  scen_key <- comb_info_df_nocap[row_idx, 2]
  reg_key <- comb_info_df_nocap[row_idx, 3]
  for (m_key in unique(sda_pathways_subs$m_flag)) {
    subs_df_nocap <- sda_pathways_subs[(
      (sda_pathways_subs$Reference_key == ref_key) &
        (sda_pathways_subs$Scenario_key == scen_key) &
        (sda_pathways_subs$Region == reg_key) &
        (sda_pathways_subs$m_flag == m_key)), ]
    subs_df_nocap <- subs_df_nocap[order(subs_df_nocap$year), ]
    subs_df_nocap$cumulative_emissions <- cumsum(subs_df_nocap$abs_emissions)
    df_list_nocap[[df_idx_nocap]] <- subs_df_nocap
    df_idx_nocap <- df_idx_nocap + 1
  }
}

cumulative_df_nocap <- do.call(rbind, df_list_nocap)
cumulative_df_nocap <- subset(cumulative_df_nocap, m_flag != 1)

p <- ggplot(cumulative_df_nocap, aes(x=year, y=cumulative_emissions / 1000000,
                               group=`m parameter option`)) +
  geom_line(aes(linetype=`m parameter option`)) +
  facet_grid(Scenario_key~Region, scales='free') +
  ylab("Cumulative emissions (MtCO2)") + xlab("") +
  theme(axis.text.x = element_text(angle = 45), legend.position='bottom',
        legend.title=element_blank())
filename <- paste0(wd$figs, "cumulative_emissions_m_nocap.png")
png(filename, width=8, height=5, units = 'in', res = 300)
print(p)
dev.off()


# Plot faceted column chart comparing cumulative emissions for different m options
p <- ggplot(sum_emissions_df, aes(x = `m parameter option`,
                                  y = abs_emissions / 1000000)) +
  geom_col(aes(fill = `m parameter option`)) +
  facet_grid(Scenario_key~Region, scales='free') +
  ylab("Cumulative emissions (MtCO2)") + xlab("") +
  theme(axis.text.x = element_blank(), legend.position='bottom',
        legend.title=element_blank())
filename <- paste0(wd$figs, "cumulative_emissions_all_m.png")
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


# plot faceted grid showing above IPCC normative models lit data vs SDA data for 
# only Africa, Europe & Eurasia, N. America, SE Asia & Developing Pacific
facet_subs <- subset(intensity_paths_subs, 
                     Region %in% c("Africa", "Europe and Eurasia",
                                   "North America", "Southeast Asia and Developing Pacific"))

facet_subs$Region <- str_replace_all(facet_subs$Region, 
                                     "Southeast Asia and Developing Pacific",
                                     "SE Asia & Dev. Pacific")

facet_subs$Region <- str_replace_all(facet_subs$Region, 
                                     "Europe and Eurasia",
                                     "Europe & Eurasia")

facet_subs$Scenario_key <- factor(facet_subs$Scenario_key, 
                                  levels = c("IMAGE", "SDS", "RECC"))



p <- ggplot(facet_subs, aes(x = Year, y = intensity, group = method)) +
     geom_line(aes(colour = method)) +
     facet_grid(Region ~ Scenario_key) +
     labs(fill = "Method") +
     ggtitle("SDA vs. top-down literature pathways") +
     xlab("") +
     theme(axis.text = element_text(angle = 45)) +
     ylab("Intensity (kg CO2 / m2)")
filename <- paste0(wd$figs, "SDA_vs_Lit_CRREM_review.png")
png(filename, width = 7, height = 7, units = 'in', res = 300)
print(p)
dev.off()

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

# plot bottom-up country level examples from Camarasa
camarasa <- subset(intensity_paths_subs, Region %in% c("China", "US"))

p <- ggplot(camarasa, aes(x = Year, y = intensity, group = method)) +
     geom_line(aes(colour = method)) +
     facet_grid(vars(Region)) +
     labs(fill = "Method") +
     ggtitle("SDA vs. bottom-up literature pathways") +
     xlab("") +
     theme(axis.text.x = element_text(angle = 45)) +
     ylab("Intensity (kg CO2 / m2")
filename <- paste0(wd$figs, "Camarasa_China_US.png")
png(filename, width = 7, height = 7, units = 'in', res = 300)
print(p)
dev.off()

# plot faceted grid showing CRREM EU country-level pathways vs Ostermeyer
p <- ggplot(CRREM_ost_subs, aes(x = year, y = intensity, group = Scenario_key)) +
     geom_line(aes(colour = Scenario_key)) +
     facet_grid(vars(country)) +
     labs(fill = "Scenario_key") +
     ggtitle("CRREM vs. bottom-up country pathways") +
     xlab("") +
     theme(axis.text.x = element_text(angle = 45)) +
     ylab("Intensity (kg CO2 / m2")
filename <- (paste0(wd$figs, "CRREM_vs_Ostermeyer_review.png"))
png(filename, width = 7, height = 7, units = 'in', res = 300)
print(p)
dev.off()
