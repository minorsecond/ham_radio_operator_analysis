library(spdep)
library(arcgisbinding)  # To save to File Geodatabase
library(automap)
library(tidyverse)

data <- readRDS("data/processed/spatially_joined_dataset.rds")
neighbors_knn <- readRDS("data/processed/transformed_tracts_4nn.rds")

# The projected CRS we want
epsg_102005 <- "+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs"

# Create a vector of all column names related to status_ and total_ops_
count_columns <- grep("status_|total_ops_Active|Active_", names(data), value = TRUE)

# Replace NA with 0 in all status_ and total_ops_ columns
data <- data %>%
    mutate(across(all_of(count_columns), ~replace(., is.na(.), 0)))

# Filter out non-CONUS areas
data_conus <- data %>%
    filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))

# Project the data
data_conus_transformed <- st_transform(data_conus, crs = epsg_102005)
data_conus_transformed <- data_conus_transformed[!is.na(data_conus_transformed$Total_Population), ]

if (length(neighbors_knn) != nrow(data_conus_transformed)) {
    stop("Mismatch between data rows and neighbor structure lengths")
}

# Get spatial empirical Bayes smoothed rates, to account for the small numbers
# problem due to low population count census tracts.
# see https://biomedware.com/small_numbers_problem1/
smoothed_rates <- lapply(count_columns, function(column_name) {
    count <- data_conus_transformed[[column_name]]
    population_active <- data_conus_transformed$total_ops_Active
    population_total <- data_conus_transformed$Total_Population
    
    # Only add 1 to population_active where it is 0
    population_active <- ifelse(population_active == 0, 1, population_active)
    
    results <- list()
    
    # Apply EBlocal if any population counts are greater than 0
    if (any(population_active > 0)) {
        eb_active <- EBlocal(count, population_active, nb = neighbors_knn)
        results$raw_rate_active <- eb_active$raw
        results$seb_rate_active <- eb_active$est
    } else {
        results$raw_rate_active <- rep(NA, length(count))
        results$seb_rate_active <- rep(NA, length(count))
    }
    
    if (any(population_total > 0)) {
        eb_total <- EBlocal(count, population_total, nb = neighbors_knn)
        results$raw_rate_total <- eb_total$raw
        results$seb_rate_total <- eb_total$est
    } else {
        results$raw_rate_total <- rep(NA, length(count))
        results$seb_rate_total <- rep(NA, length(count))
    }
    
    return(results)
})

smoothed_rates_df <- data_conus_transformed

for (i in seq_along(count_columns)) {
    column_name <- count_columns[i]
    # Create new column names for rates calculated over total_ops_Active
    raw_rate_active_name <- paste0(tolower(column_name), "_raw_rate_active_hams_percent")
    seb_rate_active_name <- paste0(tolower(column_name), "_seb_rate_active_hams_percent")
    # Create new column names for rates calculated over Total_Population
    raw_rate_total_name <- paste0(tolower(column_name), "_raw_rate_total_pop_10k")
    seb_rate_total_name <- paste0(tolower(column_name), "_seb_rate_total_pop_10k")
    
    # Add raw and SEB rates to the data frame
    smoothed_rates_df[[raw_rate_active_name]] <- smoothed_rates[[i]]$raw_rate_active * 100
    smoothed_rates_df[[seb_rate_active_name]] <- smoothed_rates[[i]]$seb_rate_active * 100
    smoothed_rates_df[[raw_rate_total_name]] <- smoothed_rates[[i]]$raw_rate_total * 10000
    smoothed_rates_df[[seb_rate_total_name]] <- smoothed_rates[[i]]$seb_rate_total * 10000
}

# Replace NA with 0 in all SEB rate columns
seb_rate_columns <- grep("seb_rate", names(smoothed_rates_df), value = TRUE)
smoothed_rates_df <- smoothed_rates_df %>%
    mutate(across(all_of(seb_rate_columns), ~replace(., is.na(.), 0)))

smoothed_rates_df <- smoothed_rates_df %>%
    select(-total_ops_active_raw_rate_active_hams_percent, 
           -total_ops_active_seb_rate_active_hams_percent)

saveRDS(smoothed_rates_df, "data/processed/smoothed_rates.rds")
# Keep only the required columns for the File Geodatabase
base_columns <- c(
    "STATEFP", "COUNTYFP", "COUNTYNS", "AFFGEOID", "GEOID", "NAMELSAD", "STUSPS",
    "STATE_NAME", "Total_Population", "total_ops_Active"
)

rate_column_patterns <- c(
    "active_advanced_raw_rate_active_hams_percent",
    "active_advanced_seb_rate_active_hams_percent",
    "active_advanced_raw_rate_total_pop_10k",
    "active_advanced_seb_rate_total_pop_10k",
    "active_extra_raw_rate_active_hams_percent",
    "active_extra_seb_rate_active_hams_percent",
    "active_extra_raw_rate_total_pop_10k",
    "active_extra_seb_rate_total_pop_10k",
    "active_general_raw_rate_active_hams_percent",
    "active_general_seb_rate_active_hams_percent",
    "active_general_raw_rate_total_pop_10k",
    "active_general_seb_rate_total_pop_10k",
    "active_technician_raw_rate_active_hams_percent",
    "active_technician_seb_rate_active_hams_percent",
    "active_technician_raw_rate_total_pop_10k",
    "active_technician_seb_rate_total_pop_10k",
    "active_na_raw_rate_active_hams_percent",
    "active_na_seb_rate_active_hams_percent",
    "active_na_raw_rate_total_pop_10k",
    "active_na_seb_rate_total_pop_10k",
    "active_novice_raw_rate_active_hams_percent",
    "active_novice_seb_rate_active_hams_percent",
    "active_novice_raw_rate_total_pop_10k",
    "active_novice_seb_rate_total_pop_10k",
    "total_ops_active_raw_rate_total_pop_10k",
    "total_ops_active_seb_rate_total_pop_10k"
)

count_columns <- c("Active_Advanced", "Active_Extra", "Active_General", 
                   "Active_Technician", "Active_NA",
                   "Cancelled_Extra", "Cancelled_General", "Cancelled_P", 
                   "Cancelled_Technician", "Cancelled_NA",
                   "Expired_Advanced", "Expired_Extra", "Expired_General", 
                   "Expired_Novice", "Expired_P",
                   "Expired_Technician", "Expired_NA", "Active_Novice", 
                   "Cancelled_Advanced",
                   "Terminated_Technician", "Terminated_Extra", 
                   "Terminated_NA", "Cancelled_Novice",
                   "Terminated_General", "Terminated_P", 
                   "Terminated_Advanced", "Unknown_Extra",
                   "total_ops_Cancelled", "total_ops_Expired",
                   "total_ops_Terminated", "total_ops_NA")

# Combine the specific rate columns with the additional columns
required_columns <- c(base_columns, count_columns, rate_column_patterns)

# Optionally, ensure these columns exist in your dataset to avoid errors
existing_columns <- names(smoothed_rates_df)
required_columns <- required_columns[required_columns %in% existing_columns]

# Now you can use 'required_columns' to select these columns from your data frame
data_reduced <- smoothed_rates_df[, required_columns, drop = FALSE]

# Define arc license, project to WGS 84 and write to File Geodatabase
arc.check_product() 
data_reduced <- st_transform(data_reduced, 4326)
arc.write("data/processed/smoothed_rates.gdb/smoothed_rates", data_reduced, overwrite = TRUE)
