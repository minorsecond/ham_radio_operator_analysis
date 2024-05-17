library(ggplot2)
library(tidyverse)

# Load the model results
total_ops_A_model <- readRDS("data/processed/nb_model.rds")

# Summarize the model coefficients
coef_summary <- as.data.frame(summary(total_ops_A_model)$coefficients)

# Create a data frame for plotting
coef_df <- coef_summary %>%
    rownames_to_column(var = "Variable") %>%
    mutate(Significance = ifelse(`Pr(>|z|)` < 0.001, "***",
                                 ifelse(`Pr(>|z|)` < 0.01, "**",
                                        ifelse(`Pr(>|z|)` < 0.05, "*", ""))))

# Add exponentiated coefficients for interpretation
coef_df <- coef_df %>%
    mutate(Exp_Coef = exp(Estimate),
           LowerCI = exp(Estimate - 1.96 * `Std. Error`),
           UpperCI = exp(Estimate + 1.96 * `Std. Error`))

# Exclude the intercept for the main plot
variables_df <- coef_df %>%
    filter(Variable != "(Intercept)")

# Rename variables for better readability
variables_df$Variable <- recode(variables_df$Variable,
                                'Total_Population' = 'Total Population',
                                'spatial_lag' = 'Spatial Lag',
                                'Median_Household_Income' = 'Median Household Income',
                                'white_perc' = '% White Population',
                                'black_perc' = '% Black Population',
                                'housing_perc' = 'Houses per 1,000 Population',
                                'bach_perc' = "Percent with Bachelor's or Above",
                                'Median_Age' = 'Median Age',
                                'Rate_LDS_Adherents' = 'LDS Adherents per 1,000',
                                'Rate_MainlineProtestant_Adherents' = 'Mainline Protestants per 1,000',
                                'Rate_Evang_Adherents' = 'Evangelicals per 1,000',
                                'Rate_Catholic_Adherents' = 'Catholics per 1,000')

# Plot the effect sizes for the variables
ggplot(variables_df, aes(x = reorder(Variable, Exp_Coef), y = Exp_Coef)) +
    geom_pointrange(aes(ymin = LowerCI, ymax = UpperCI), color = "blue") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    geom_text(aes(label = Significance), hjust = -0.5) +  # Add stars for significant coefficients
    coord_flip() +
    xlab("Variables") +
    ylab("Effect Size (Exponentiated Coefficients)") +
    ggtitle("Effects of Predictors on Counts of Total Active Ham Radio Operators") +
    theme_minimal() +
    annotate("text", x = Inf, y = Inf, label = "Significance levels:\n***: p < 0.001\n**: p < 0.01\n*: p < 0.05", hjust = 1, vjust = 1)

# Calculate marginal effects
marginal_effects <- coef_df %>%
    filter(Variable != "(Intercept)") %>%
    mutate(Marginal_Effect = Estimate / exp(Estimate))

# Rename variables for better readability
marginal_effects$Variable <- recode(marginal_effects$Variable,
                                    'Total_Population' = 'Total Population',
                                    'spatial_lag' = 'Spatial Lag',
                                    'Median_Household_Income' = 'Median Household Income',
                                    'white_perc' = '% White Population',
                                    'black_perc' = '% Black Population',
                                    'housing_perc' = 'Houses per 1,000 Population',
                                    'bach_perc' = "Percent with Bachelor's or Above",
                                    'Median_Age' = 'Median Age',
                                    'Rate_LDS_Adherents' = 'LDS Adherents per 1,000',
                                    'Rate_MainlineProtestant_Adherents' = 'Mainline Protestants per 1,000',
                                    'Rate_Evang_Adherents' = 'Evangelicals per 1,000',
                                    'Rate_Catholic_Adherents' = 'Catholics per 1,000')

# Plot the marginal effects
ggplot(marginal_effects, aes(x = reorder(Variable, Marginal_Effect), y = Marginal_Effect)) +
    geom_pointrange(aes(ymin = Marginal_Effect - 1.96 * `Std. Error`, ymax = Marginal_Effect + 1.96 * `Std. Error`), color = "blue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_text(aes(label = Significance), hjust = -0.5) +  # Add stars for significant coefficients
    coord_flip() +
    xlab("Variables") +
    ylab("Marginal Effect on Counts") +
    ggtitle("Marginal Effects of Predictors on Counts of Total Active Ham Radio Operators") +
    theme_minimal() +
    annotate("text", x = Inf, y = Inf, label = "Significance levels:\n***: p < 0.001\n**: p < 0.01\n*: p < 0.05", hjust = 1, vjust = 1)
