library(tidyverse)
library(sf)
library(arcgisbinding)  # To save to File Geodatabase

arc.check_product()
acs_geo_data <- readRDS("data/processed/acs_data_geo.rds")
amateur_data <- readRDS("data/processed/cleaned_data.rds")
amateur_data_sf <- st_as_sf(amateur_data, coords = c("longitude", "latitude"), crs = 4326)
arc_ops_data <- st_transform(amateur_data_sf, crs = 4326)
arc_ops_data <- arc_ops_data %>%
    mutate(is_po_box = as.integer(!is.na(po_box))) %>%
    select(-geom, -geometry, -entity_type, -id, -po_box)
arc.write("data/processed/smoothed_rates.gdb/amateur_op_data", arc_ops_data, overwrite = T)
rm(arc_ops_data)

# Remove PO boxes as these are sometimes geocoded to the wrong county
# TODO: Also filture out 5000 and 9000 level ratings where the corresponding
# GEOID is in bad_zips or bad_places.
amateur_data_sf <- amateur_data_sf %>% 
    filter(is.na(po_box) & rating <= 4999)

religion_data <- readRDS("data/processed/religion_data.rds")

# Assuming previous steps are correct and the data is loaded and transformed properly
amateur_data_sf <- st_transform(amateur_data_sf, st_crs(acs_geo_data))

# Join with ACS data based on spatial location
points_in_tracts <- st_join(amateur_data_sf, acs_geo_data, join = st_within)

# Summarize data by GEOID, license_status, and operator_class
summarized_data <- points_in_tracts %>%
    group_by(GEOID, license_status, operator_class) %>%
    summarise(count = n(), .groups = 'drop') %>%
    ungroup()

# Drop geometry for further operations like pivoting
summarized_data_no_geom <- st_set_geometry(summarized_data, NULL)

summarized_data_no_geom <- summarized_data_no_geom %>%
    mutate(
        license_status = case_when(
            license_status == "A" ~ "Active",
            license_status == "C" ~ "Cancelled",
            license_status == "T" ~ "Terminated",
            license_status == "E" ~ "Expired",
            TRUE ~ license_status  # Keeps original value if none of the above
        ),
        operator_class = case_when(
            operator_class == "A" ~ "Advanced",
            operator_class == "E" ~ "Extra",
            operator_class == "G" ~ "General",
            operator_class == "T" ~ "Technician",
            operator_class == "N" ~ "Novice",  # This case included for completeness
            TRUE ~ as.character(operator_class)  # Handles NA or any other unforeseen cases
        )
    )

# Pivot data to wide format, filling missing counts with 0
wide_data <- summarized_data_no_geom %>%
    pivot_wider(
        id_cols = GEOID,
        names_from = c(license_status, operator_class),
        values_from = count,
        values_fill = list(count = 0)
    )

# Calculating total operators per GEOID and license_status
total_ops_data <- summarized_data_no_geom %>%
    group_by(GEOID, license_status) %>%
    summarise(total_ops = sum(count), .groups = 'drop') %>%
    pivot_wider(
        names_from = license_status,
        values_from = total_ops,
        names_prefix = "total_ops_",
        values_fill = list(total_ops = 0)
    )

# Joining total_ops_data to the wide_data
final_data <- left_join(wide_data, total_ops_data, by = "GEOID")

# Optional: Rename 'NA' to 'Unknown' for clarity if there are NA classes
final_data <- final_data %>%
    rename_with(~ str_replace(., "^NA", "Unknown"), starts_with("NA"))

religion_data <- religion_data %>%
    mutate(GEOID = sprintf("%05d", as.integer(fips)),  # Format FIPS as a 5-digit character string
           LDS_Adherents = LDSADH_2020,
           MainlineProtestant_Adherents = MPRTADH_2020,
           Evang_Adherents = EVANADH_2020,
           Catholic_Adherents = CTHADH_2020,
           Muslim_Adherents = MSLMADH_2020,
           ReformJudaism_Adherents = RFRMADH_2020,
           ConJudaism_Adherents =  CJUDADH_2020,
           OrthoJudaism_Adherents = OJUDADH_2020) %>%
    select(GEOID, fips, LDS_Adherents, MainlineProtestant_Adherents, Evang_Adherents, 
           Catholic_Adherents, Muslim_Adherents, ReformJudaism_Adherents, 
           ConJudaism_Adherents, OrthoJudaism_Adherents)

final_joined_data <- acs_geo_data %>%
    left_join(as.data.frame(final_data), by = "GEOID") %>%
    left_join(as.data.frame(religion_data), by = "GEOID")

# Find records where religious adherents exceed total population
exceeding_religious_adherents <- final_joined_data %>%
    mutate(
        Flag_LDS_Exceeds = if_else(LDS_Adherents > Total_Population, 1, 0),
        Flag_Catholic_Exceeds = if_else(Catholic_Adherents > Total_Population, 1, 0),
        Flag_MainlineProtestant_Exceeds = if_else(MainlineProtestant_Adherents > Total_Population, 1, 0),
        Flag_Evang_Exceeds = if_else(Evang_Adherents > Total_Population, 1, 0),
        Flag_Muslim_Exceeds = if_else(Muslim_Adherents > Total_Population, 1, 0),
        Flag_ReformJudaism_Exceeds = if_else(ReformJudaism_Adherents > Total_Population, 1, 0),
        Flag_ConJudaism_Exceeds = if_else(ConJudaism_Adherents > Total_Population, 1, 0),
        Flag_OrthoJudaism_Exceeds = if_else(OrthoJudaism_Adherents > Total_Population, 1, 0)
    )

# Summarize the data to count the number of exceedances
counts_exceeds <- exceeding_religious_adherents %>%
    summarise(
        Count_LDS_Exceeds = sum(Flag_LDS_Exceeds),
        Count_Catholic_Exceeds = sum(Flag_Catholic_Exceeds),
        Count_MainlineProtestant_Exceeds = sum(Flag_MainlineProtestant_Exceeds),
        Count_Evang_Exceeds = sum(Flag_Evang_Exceeds),
        Count_Muslim_Exceeds = sum(Flag_Muslim_Exceeds),
        Count_ReformJudaism_Exceeds = sum(Flag_ReformJudaism_Exceeds),
        Count_ConJudaism_Exceeds = sum(Flag_ConJudaism_Exceeds),
        Count_OrthoJudaism_Exceeds = sum(Flag_OrthoJudaism_Exceeds)
    )

# Exceeding counts: LDS, Catholic, Evangelical

final_joined_data_uncapped <- final_joined_data %>%
    mutate(
        Rate_LDS_Adherents_Uncapped = (LDS_Adherents / Total_Population) * 1000,
        Rate_Evangelical_Adherents_Uncapped = (Evang_Adherents / Total_Population) * 1000,
        Rate_Catholic_Adherents_Uncapped = (Catholic_Adherents / Total_Population) * 1000
    )

# Calculate rates with capping
final_joined_data_capped <- final_joined_data %>%
    mutate(
        Adjusted_LDS_Adherents = if_else(LDS_Adherents > Total_Population, Total_Population, LDS_Adherents),
        Rate_LDS_Adherents_Capped = (Adjusted_LDS_Adherents / Total_Population) * 1000,
        Adjusted_Evangelical_Adherents = if_else(Evang_Adherents > Total_Population, Total_Population, Evang_Adherents),
        Rate_Evangelical_Adherents_Capped = (Adjusted_Evangelical_Adherents / Total_Population) * 1000,
        Adjusted_Catholic_Adherents = if_else(Catholic_Adherents > Total_Population, Total_Population, Catholic_Adherents),
        Rate_Catholic_Adherents_Capped = (Adjusted_Catholic_Adherents / Total_Population) * 1000,
    )

final_joined_data_uncapped <- as.data.frame(final_joined_data_uncapped)
final_joined_data_capped <- as.data.frame(final_joined_data_capped)

# Combine the uncapped and capped data for comparison
comparison_data <- final_joined_data %>%
    select(GEOID, Total_Population, LDS_Adherents, Evang_Adherents, Catholic_Adherents) %>%
    left_join(select(final_joined_data_uncapped, GEOID, Rate_LDS_Adherents_Uncapped,
                     Rate_Evangelical_Adherents_Uncapped,
                     Rate_Catholic_Adherents_Uncapped), by = "GEOID") %>%
    left_join(select(final_joined_data_capped, GEOID, Rate_LDS_Adherents_Capped,
                     Rate_Evangelical_Adherents_Capped,
                     Rate_Catholic_Adherents_Capped), by = "GEOID")

# View summary statistics or plot to compare
library(ggplot2)
ggplot(comparison_data, aes(x = Rate_LDS_Adherents_Uncapped, y = Rate_LDS_Adherents_Capped)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(title = "Comparison of Uncapped vs. Capped Rates for LDS Adherents",
         x = "Uncapped Rate",
         y = "Capped Rate")

ggplot(comparison_data, aes(x = Rate_Evangelical_Adherents_Uncapped, y = Rate_Evangelical_Adherents_Capped)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(title = "Comparison of Uncapped vs. Capped Rates for Evangelical Adherents",
         x = "Uncapped Rate",
         y = "Capped Rate")

ggplot(comparison_data, aes(x = Rate_Catholic_Adherents_Uncapped, y = Rate_Catholic_Adherents_Capped)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(title = "Comparison of Uncapped vs. Capped Rates for Catholic Adherents",
         x = "Uncapped Rate",
         y = "Capped Rate")


# Adjusting Adherent Counts Before Calculating Rates
# Issue: In some cases, the reported number of adherents for a religion exceeds the total population of the county.
# This discrepancy can occur due to reporting errors, misinterpretation of membership (including non-residents),
# or potential data entry errors. Such situations lead to artificially high and statistically implausible rates
# when calculating the number of adherents per 1000 population.
#
# Solution: To address this issue, we cap the adherent counts at the total population of each county. This adjustment
# ensures that our calculated rates per 1000 population are plausible and reflective of the actual population limits.
# Capping the counts prevents the rate from exceeding 1000 per 1000 population, which would imply more adherents than
# residents, an impossible scenario under normal demographic conditions. The capped values are used to calculate 
# adherent rates, ensuring that our statistical analyses remain valid and meaningful.
#
# The following code implements this capping for each religious group and recalculates the rates accordingly:

final_joined_data <- final_joined_data %>%
    mutate(
        Adjusted_LDS_Adherents = if_else(LDS_Adherents > Total_Population, Total_Population, LDS_Adherents),
        Adjusted_MainlineProtestant_Adherents = if_else(MainlineProtestant_Adherents > Total_Population, Total_Population, MainlineProtestant_Adherents),
        Adjusted_Evang_Adherents = if_else(Evang_Adherents > Total_Population, Total_Population, Evang_Adherents),
        Adjusted_Catholic_Adherents = if_else(Catholic_Adherents > Total_Population, Total_Population, Catholic_Adherents),
        Adjusted_Muslim_Adherents = if_else(Muslim_Adherents > Total_Population, Total_Population, Muslim_Adherents),
        Adjusted_ReformJudaism_Adherents = if_else(ReformJudaism_Adherents > Total_Population, Total_Population, ReformJudaism_Adherents),
        Adjusted_ConJudaism_Adherents = if_else(ConJudaism_Adherents > Total_Population, Total_Population, ConJudaism_Adherents),
        Adjusted_OrthoJudaism_Adherents = if_else(OrthoJudaism_Adherents > Total_Population, Total_Population, OrthoJudaism_Adherents),
        
        Rate_LDS_Adherents = (Adjusted_LDS_Adherents / Total_Population) * 1000,
        Rate_MainlineProtestant_Adherents = (Adjusted_MainlineProtestant_Adherents / Total_Population) * 1000,
        Rate_Evang_Adherents = (Adjusted_Evang_Adherents / Total_Population) * 1000,
        Rate_Catholic_Adherents = (Adjusted_Catholic_Adherents / Total_Population) * 1000,
        Rate_Muslim_Adherents = (Adjusted_Muslim_Adherents / Total_Population) * 1000,
        Rate_ReformJudaism_Adherents = (Adjusted_ReformJudaism_Adherents / Total_Population) * 1000,
        Rate_ConJudaism_Adherents = (Adjusted_ConJudaism_Adherents / Total_Population) * 1000,
        Rate_OrthoJudaism_Adherents = (Adjusted_OrthoJudaism_Adherents / Total_Population) * 1000
    )

# Save the final spatially joined dataset
saveRDS(final_joined_data, "data/processed/spatially_joined_dataset.rds")
