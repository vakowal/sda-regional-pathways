################################
# SDA regional pathways:
# Clean and process raw data
################################

# get location of related scripts
library(rstudioapi)
script_dir <- dirname(getSourceEditorContext()$path)

# package imports, function definitions, and globals including directory
source(paste0(script_dir, "/00_packages_functions_globals.R"))


# read data, clean and process
multi_family <- read.csv(paste0(wd$raw_data, "2022-08-10_CRREM-multi-family.csv"))
single_family <- read.csv(paste0(wd$raw_data, "2022-08-10_CRREM-single-family.csv"))

multi_family <- multi_family %>% 
  pivot_longer(!ï..YEAR, names_to = "country", values_to = "emissions_intensity")  
  
multi_family$building_type <- "Multi-family residential"
colnames(multi_family)[1] <- "year"

single_family <- single_family %>% 
  pivot_longer(!ï..YEAR, names_to = "country", values_to = "emissions_intensity")

single_family$building_type <- "Single-family residential"
colnames(single_family)[1] <- "year"

CRREM_EU_residential <- union(multi_family, single_family)




## DEMONSTRATION, THROWAWAY ##
# demo: read data, clean and process
example_data <- read.csv(paste0(wd$raw_data, "sda_pathways.csv"))
example_data$intensity_base_year_scope1_2 <- (
  (example_data$Base.year.scope.1.emissions..tCO2. + 
     example_data$Base.year.scope.2.emissions..tCO2.) / 
  example_data$Base.year.activity..square.meters.)
example_data$intensity_target_year_scope1_2 <- (
  (example_data$Target.year.scope.1.emissions..tCO2. +
     example_data$Target.year.scope.2.emissions..tCO2.) /
    example_data$Target.year.activity..square.meters.)

example_processed <- example_data[, c(
  colnames(example_data)[c(1:6, 14:15)])]

write.csv(
  example_processed, paste0(wd$processed_data, 'example_processed_data.csv'),
  row.names=FALSE)
