library(tidyverse)

data <- readRDS("data/processed/merged_amateur_data.rds")

# Drop columns with all NA values
data <- data %>% 
    select_if(~!all(is.na(.)))

data$operator_class <- as.factor(data$operator_class)
data$region_code <- as.factor(data$region_code)
data$state <- as.factor(data$state)
data$city <- as.factor(data$city)

# Drop unneeded columns
data <- data %>%
    select(-entity_name, -first_name, -mi, -last_name, -street_address, 
           -sgin, -fcc_registration_number, -applicant_type_code, 
           -attention_line, -sgin, -fcc_registration_number, 
           -applicant_type_code, -record_type.x, -group_code, 
           -trustee_call_sign, -trustee_indicator, -vanity_call_sign_change,
           -trustee_name, -suffix, - call_sign.y, -record_type.y)

# Clean city, state, and zip_code columns
data <- data %>%
    mutate(city = str_to_title(city),
           state = str_to_upper(state),
           zip_code = substr(zip_code, 1, 5))

# Drop records with non-NA po_box values. We don't want these for this analysis,
# as they will likely be geolocated in at the post office location.
#data <- data %>% filter(is.na(po_box))

head(data)

# Save the cleaned data to a new R data file
saveRDS(data, file = "data/processed/cleaned_data.rds")
