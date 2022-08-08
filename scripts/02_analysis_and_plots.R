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
## WTF this is not working!???!

# do analysis
example_processed_data <- read.csv(
  paste0(wd$processed_data, 'example_processed_data.csv'))

# make plots
example_plot <- ggplot(
  example_processed_data, aes(
    x=Model...Scenario, y=intensity_target_year_scope1_2))
example_plot <- example_plot + geom_point() + facet_wrap(~Region)
pngname <- paste0(wd$figs, "example_plot.png")
png(filename=pngname, width=6, height=6, units='in', res=300)
print(example_plot)
dev.off()
