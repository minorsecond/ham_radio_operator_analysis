library(tidycensus)
library(tigris)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyverse)

options(tigris_use_cache = TRUE)

census_year = 2022
# Set your Census API key
census_api_key("728083e8ebb522935279c284a900a4cd9d491f12")

# Define the list of variables
variables <- c(
    "B01001_001", "B01002_001",  "B02001_004", "B02001_005", "B02001_006", "B02001_007", "B03002_003", "B03002_004", 
    "B15003_022", "B15003_023", "B19013_001", "B25001_001", "B25002_001", "B25003_001", "B25003_002", "B25003_003", "B23025_005", 
    "B23025_003", "B23025_004", "B23025_007"
)

state_codes <- c(
    "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
    "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
    "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
    "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
    "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"
)

all_census_data <- list()

for (state_code in state_codes) {
    census_data <- get_acs(
        geography = "county",
        variables = variables,
        year = census_year,
        survey = "acs5",
        state = state_code
    )
    all_census_data[[state_code]] <- census_data
}

combined_data <- do.call(rbind, all_census_data)

variable_names <- c(
    "B01001_001" = "Total_Population",
    "B01002_001" = "Median_Age",
    "B02001_004" = "White_Alone",
    "B02001_005" = "Black_or_African_American_Alone",
    "B02001_006" = "American_Indian_and_Alaska_Native_Alone",
    "B02001_007" = "Asian_Alone",
    "B03002_003" = "Hispanic_or_Latino",
    "B03002_004" = "White_Alone_Not_Hispanic_or_Latino",
    "B15003_022" = "Bachelor_Degree_or_Higher",
    "B15003_023" = "Graduate_or_Professional_Degree",
    "B19013_001" = "Median_Household_Income",
    "B25001_001" = "Total_Housing_Units",
    "B25002_001" = "Occupied_Housing_Units",
    "B25003_001" = "Vacant_Housing_Units",
    "B25003_002" = "Vacant_Housing_Units_For_Rent",
    "B25003_003" = "Vacant_Housing_Units_For_Sale",
    "B23025_005" = "Population_Age_16_and_Over",
    "B23025_003" = "In_Labor_Force",
    "B23025_004" = "Civilian_Unemployed",
    "B23025_007" = "Not_in_Labor_Force",
    "C24010_007" = "Male_Educational_Attainment_Less_Than_High_School",
    "C24010_008" = "Male_Educational_Attainment_High_School_Graduate",
    "C24010_009" = "Male_Educational_Attainment_Some_College",
    "C24010_010" = "Male_Educational_Attainment_Bachelor_Degree",
    "C24010_011" = "Male_Educational_Attainment_Graduate_or_Professional_Degree",
    "C24010_012" = "Female_Educational_Attainment_Less_Than_High_School",
    "C24010_013" = "Female_Educational_Attainment_High_School_Graduate",
    "C24010_014" = "Female_Educational_Attainment_Some_College",
    "C24010_015" = "Female_Educational_Attainment_Bachelor_Degree",
    "C24010_016" = "Female_Educational_Attainment_Graduate_or_Professional_Degree"
)

combined_acs_data <- combined_data %>%
    mutate(variable = recode(variable, !!!variable_names))

counties_us <- counties(cb = TRUE, class = "sf", year=census_year)  

combined_acs_data$GEOID <- trimws(as.character(combined_data$GEOID))
counties_us$GEOID <- trimws(as.character(counties_us$GEOID))

# Make each variable a column
wide_data <- combined_acs_data %>%
    select(-moe) %>%
    pivot_wider(
        names_from = variable,
        values_from = estimate
    )

acs_data_geo <- left_join(counties_us, wide_data, by = "GEOID")
wide_data[grepl("Connecticut", wide_data$NAME), ]
counties_us[grepl("09001", counties_us$GEOID), ]
acs_data_geo[grepl("09001", acs_data_geo$GEOID), ]

# Examine the data
population_plot <- ggplot(data = acs_data_geo) +
    geom_sf(aes(fill = Total_Population), color = NA) +  # Fill tracts based on population
    scale_fill_viridis_c(option = "C", direction = -1, name = "Total Population") +  # Use a color scale that is perceptually uniform
    labs(title = "Map of Total Population by Census Tract", 
         subtitle = "US Census Data",
         caption = "Source: U.S. Census Bureau") +
    theme_minimal() +
    theme(legend.position = "right")

saveRDS(acs_data_geo, "data/processed/acs_data_geo.rds")
