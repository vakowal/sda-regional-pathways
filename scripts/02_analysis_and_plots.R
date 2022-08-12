################################
# SDA regional pathways:
# M parameter sensitivity analysis
# and exploratory plots
################################

# get location of related scripts
library(rstudioapi)
script_dir <- dirname(getSourceEditorContext()$path)

# package imports, function definitions, and globals including directory
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
  p <- p + ylab("SDA Intensity (kg CO2 / m2)") + ggtitle(region)
  filename <- paste0(wd$figs, paste0("m_param_", region, ".png"))
  png(filename, width=6, height=3, units='in', res=300)
  print(p)
  dev.off()
}


# TODO visualize empirical and SDA pathways

## DEMONSTRATION, THROWAWAY ##
# demo: do analysis
example_processed_data <- read.csv(
  paste0(wd$processed_data, 'example_processed_data.csv'))

# demo: make plots
 example_plot <- ggplot(example_processed_data, aes(
    x = Model...Scenario, y = intensity_target_year_scope1_2))


example_plot <- example_plot + geom_point() + facet_wrap(~Region) 
ggsave(filename = "example_plot.png", 
       plot = example_plot,
       path = wd$figs,
       width = 6,
       height = 6,
       units = "in",
       dpi = 300)

example_plot <- ggplot(
  example_processed_data, aes(
    x=Model...Scenario, y=intensity_target_year_scope1_2))
example_plot <- example_plot + geom_point() + facet_wrap(~Region)
pngname <- paste0(wd$figs, "example_plot.png")
png(filename=pngname, width=6, height=6, units='in', res=300)
print(example_plot)
dev.off()

