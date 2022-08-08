################################
# SDA regional pathways:
# Load packages, define functions, and spell out project globals
# (including directory paths)
################################

# Load packages
library(ggplot2)
library(rstudioapi)

# current directory
script_dir <- dirname(getSourceEditorContext()$path)
repo_dir <- dirname(script_dir)

# Custom functions
CalcIntensityPathway <- function()

# Project directories
wd <- list()
wd$raw_data <- paste0(repo_dir, '/data/raw/')
wd$processed_data <- paste0(repo_dir, '/data/processed/')
wd$figs <- paste0(repo_dir, '/figures/')