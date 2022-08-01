################################
# SDA regional pathways:
# Clean and process raw data
################################

# package imports, function definitions, and globals including directory
source("00_packages_functions_globals.R")  # get containing folder relative to current directory

# read data, clean and process
example_data <- read.csv(paste0(wd$raw_data, "example_file.csv"))