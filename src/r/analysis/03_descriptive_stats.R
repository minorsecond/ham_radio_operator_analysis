library(tidyverse)
library(dplyr)

data <- readRDS("data/processed/smoothed_rates.rds")
ham_data <- readRDS("data/processed/cleaned_data.rds")
no_geom_data <- data
no_geom_data$geometry <- NULL
count_columns <- grep("status_|total_ops_", names(data), value = TRUE)

descriptive_stats <- no_geom_data %>%
    select(all_of(count_columns)) %>%
    summarise(across(everything(), list(
        mean = ~mean(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        q25 = ~quantile(., probs = 0.25, na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        q75 = ~quantile(., probs = 0.75, na.rm = TRUE),
        max = ~max(., na.rm = TRUE)
    ), .names = "{.col}_{.fn}")) %>%
    pivot_longer(everything(), names_to = c("variable", "statistic"), names_pattern = "(.*)_(mean|sd|min|q25|median|q75|max)")

# Viewing the summary statistics
print(descriptive_stats)
saveRDS(descriptive_stats, "data/processed/descriptive_statistics.rds")

hist(data$total_ops_A, breaks = 20, main = "Histogram of Total Operators", xlab = "Total Operators", col = "gray")
hist(data$total_ops_C, breaks = 20, main = "Histogram of Total Operators", xlab = "Total Operators", col = "gray")
hist(data$total_ops_E, breaks = 20, main = "Histogram of Total Operators", xlab = "Total Operators", col = "gray")
hist(data$total_ops_T, breaks = 20, main = "Histogram of Total Operators", xlab = "Total Operators", col = "gray")
hist(data$total_ops_N, breaks = 20, main = "Histogram of Total Operators", xlab = "Total Operators", col = "gray")

# Get upgrade stats
# Define the order of license classes
class_order <- c("N", "T", "G", "A", "E")

# Filter out records where both previous_operator_class and operator_class are not NA
filtered_data <- ham_data %>%
    filter(!is.na(previous_operator_class), !is.na(operator_class))

# Convert license classes to ordered factor
filtered_data <- mutate(filtered_data,
                        previous_operator_class = factor(previous_operator_class, levels = class_order),
                        operator_class = factor(operator_class, levels = class_order))

# Convert factors to integers for comparison
previous_class_int <- as.integer(filtered_data$previous_operator_class)
current_class_int <- as.integer(filtered_data$operator_class)

# Count the number of records where previous_operator_class is lower than operator_class
upgrade_count <- sum(previous_class_int < current_class_int, na.rm = TRUE)

# Calculate the total count of filtered records
total_count <- nrow(filtered_data)

# Calculate the percentage of ham operators who upgraded their license classes
upgrade_percentage <- (upgrade_count / total_count) * 100

# Print the percentage
print(upgrade_percentage)



# Filter out records where both previous_operator_class and operator_class are not NA
filtered_data <- ham_data %>%
    filter(!is.na(previous_operator_class), !is.na(operator_class))

# Define the order of license classes
class_order <- c("N", "T", "G", "A", "E")

# Convert license classes to ordered factors
filtered_data <- mutate(filtered_data,
                        previous_operator_class = factor(previous_operator_class, levels = class_order),
                        operator_class = factor(operator_class, levels = class_order))

# Count the number of records where the previous license class is lower than the current license class
upgrade_counts <- count(filtered_data, previous_operator_class, operator_class) %>%
    filter(as.integer(previous_operator_class) < as.integer(operator_class))

# Calculate the total count of filtered records for each previous license class
total_counts <- count(filtered_data, previous_operator_class)

# Calculate the percentage of operators who upgraded from each license class to another specific license class
upgrade_percentages <- left_join(total_counts, upgrade_counts, by = "previous_operator_class") %>%
    mutate(percentage = ifelse(is.na(n.y), 0, (n.y / n.x) * 100))

# Print the percentage for each previous class
print(upgrade_percentages)
