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

# plot SDA pathways calculated from empirical data
sda_pathways <- read.csv(
  paste0(wd$processed_data,'SDA_pathways_RECC_residential.csv'))
p <- ggplot(sda_pathways, aes(x=year, y=intensity_SDA))
p <- p + geom_line()
p <- p + facet_grid(Region~Scope, scales='free')
pngname <- paste0(wd$figs, "SDA_pathways_RECC_residential.png")
ggsave(pngname, width=4, height=10)


# TODO visualize empirical and SDA pathways

# subset intensity pathways df to include only default m param pathways from SDA 
# and pathways from literature
intensity_paths_subs <- intensity_paths_all[(intensity_paths_all$m_flag == 0) | 
                                              (is.na(intensity_paths_all$m_flag)), ]


lit_sda_africa <- intensity_paths_subs %>% 
  filter(Reference_key == "IPCC_normative" & Region == "Africa") %>% 
  ggplot(aes(x = Year, y = intensity, color = method)) +
  geom_line(aes(group = method)) +
  facet_grid(Region ~ Scenario_key)


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

