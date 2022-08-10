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

# do analysis


# make plots




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



