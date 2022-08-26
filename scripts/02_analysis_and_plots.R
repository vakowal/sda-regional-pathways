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
