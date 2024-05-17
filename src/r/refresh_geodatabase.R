
# Commented lines do not need to be run every time

source("src/r/data_preparation/01_load_data.R")
#source("src/r/data_preparation/02_get_census_data.R")
source("src/r/data_preparation/03_clean_data.R")
source("src/r/data_preparation/04_get_bounds_overlapping_counties.R")
source("src/r/data_preparation/05_spatial_joining.R")

#source("src/r/analysis/01_calculate_spatial_neighbors.R")
source("src/r/analysis/02_spatial_analysis.R")
