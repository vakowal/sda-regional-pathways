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

# calculate an example intensity pathway
res_sector_data <- read.csv(
  paste0(wd$raw_data, 'residential_buildings_sector_data.csv'))

# example input data
base_year <- 2020
company_emissions_base <- 7000000
company_activity_base <- 10000000
company_activity_target <- 12000000
# calculate yearly activity
company_activity <- seq(
  company_activity_base, company_activity_target,
  length.out=(2050-base_year) + 1)
sector_activity <- res_sector_data[
  res_sector_data$year >= base_year, 'Sector_activity']
sector_emissions <- res_sector_data[
  res_sector_data$year >= base_year, 'Sector_Scope_1_emissions']

example_pathway <- CalcIntensityPathway(
  company_activity, company_emissions_base, sector_activity, sector_emissions)

## DEMONSTRATION, THROWAWAY ##
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
