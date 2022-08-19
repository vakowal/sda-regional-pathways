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

sda_pathways$m_parameter_option <- factor(
  sda_pathways$m_flag, levels <- c(0, 1, 2),
  labels=c('default', 'm = 1', 'm floor = 0.8'))
for(region in unique(sda_pathways$Region)) {
  subs_df <- sda_pathways[sda_pathways$Region == region, ]
  p <- ggplot(subs_df, aes(x=year, y=intensity_SDA, group=m_parameter_option))
  p <- p + geom_line(aes(colour=m_parameter_option))
  p <- p + facet_grid(Reference_key~Scenario_key)
  p <- p + labs(fill="m parameter option")
  p <- p + ylab("SDA Intensity (kg CO2 / m2)") + 
            ggtitle(region, subtitle = "M parameter testing") + 
            theme(axis.text.x = element_text(angle = 45))
  filename <- paste0(wd$figs, paste0("m_param_", region, ".png"))
  png(filename, width=6, height=3, units='in', res=300)
  print(p)
  dev.off()
}

# plot pathways for IPCC normative models, literature data vs SDA data
# subset intensity pathways df to include only default m param pathways from SDA 
# and pathways from literature
intensity_paths_all <- read.csv(
  paste0(wd$processed_data, 'intensity_paths_all.csv'))
intensity_paths_subs <- intensity_paths_all[
  (intensity_paths_all$m_flag == 0) | (is.na(intensity_paths_all$m_flag)), ]
ipcc_paths <- filter(intensity_paths_subs, Reference_key ==  "IPCC_normative")

for (region in unique(ipcc_paths$Region)) {
  int_subs <- ipcc_paths[ipcc_paths$Region == region, ]
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
