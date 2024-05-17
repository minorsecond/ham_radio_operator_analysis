# Load necessary library
library(MASS)
library(pscl)
library(spdep)
library(sf)
library(glmmTMB)

data <- readRDS("data/processed/smoothed_rates.rds")

variables <- c("Total_Population", "Median_Household_Income", "White_Alone",
               "Black or African_American_Alone",
               "Bachelor_Degree_or_Higher", "Not_in_Labor_Force", "total_ops_Active",
               "Total_Housing_Units", "Rate_LDS_Adherents", "Rate_MainlineProtestant_Adherents",
               "Rate_Evang_Adherents", "Rate_Catholic_Adherents", "Rate_Muslim_Adherents",
               "Rate_ReformJudaism_Adherents", "Rate_ConJudaism_Adherents", "Rate_OrthoJudaism_Adherents")
complete_cases <- complete.cases(data$Total_Population,
                                 data$Median_Household_Income,
                                 data$Bachelor_Degree_or_Higher,
                                 data$Hispanic_or_Latino,
                                 data$In_Labor_Force,
                                 data$Total_Housing_Units)
complete_data <- data[complete_cases, ]
complete_data$white_perc <- (complete_data$White_Alone / complete_data$Total_Population) * 100
complete_data$black_perc <- (complete_data$Black_or_African_American_Alone / complete_data$Total_Population) * 100
complete_data$housing_perc <- (complete_data$Total_Housing_Units / complete_data$Total_Population) * 1000
complete_data$bach_perc <- (complete_data$Bachelor_Degree_or_Higher / complete_data$Total_Population) * 100

# Determine spatial autocorrelation
epsg_102005 <- "+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs"
complete_data <- st_transform(complete_data, crs = epsg_102005)
centroids <- st_centroid(complete_data)
coordinates <- st_coordinates(centroids)

knearneighbors <- knearneigh(coordinates, k=4) # Warning: this will take forever
nb <- knn2nb(knearneighbors)
W <- nb2listw(nb)

# Calculate spatially lagged variable
spatial_lag <- lag.listw(W, complete_data$total_ops_A)


# Merge spatial lag variable with dataset
complete_data <- cbind(complete_data, spatial_lag)

# Select the variables for the scatterplot matrix
#variables <- c("Total_Population", "spatial_lag", "Median_Household_Income", 
#               "white_perc", "housing_perc", "bach_perc", "Median_Age", "total_ops_A")

# Subset the data to include only the selected variables
scatterplot_data <- complete_data[variables]
scatterplot_data$geometry <- NULL
# Create the scatterplot matrix
pairs(scatterplot_data)

total_ops_A_model_poisson <- glm(total_ops_Active ~ Total_Population + 
                             spatial_lag +
                             Median_Household_Income + 
                             white_perc +
                             black_perc + 
                             housing_perc +
                             bach_perc +
                             Median_Age +
                             Rate_LDS_Adherents +
                             Rate_MainlineProtestant_Adherents +
                             Rate_Evang_Adherents +
                             Rate_Catholic_Adherents +
                             Rate_Muslim_Adherents +
                             Rate_ReformJudaism_Adherents +
                             Rate_ConJudaism_Adherents +
                             Rate_OrthoJudaism_Adherents,
                         data = complete_data,
                         family = poisson(link = "log"),
                         control = glm.control(maxit = 999))

# Get the residual deviance and degrees of freedom
residual_deviance <- deviance(total_ops_A_model_poisson)
residual_df <- df.residual(total_ops_A_model_poisson)

# Calculate the dispersion parameter
dispersion <- residual_deviance / residual_df

# Check for overdispersion
if (dispersion > 1) {
    cat("Overdispersion detected:", dispersion, "\n")
} else {
    cat("No overdispersion detected.", "\n")
}

#start_values <- coef(total_ops_A_model_poisson)
mean_log <- log(mean(complete_data$total_ops_Active))
start_values <- rep(0, length(coef(total_ops_A_model_poisson)))  # Start with zeros or small numbers for simplicity
names(start_values) <- names(coef(total_ops_A_model_poisson))
start_values[1] <- mean_log  # Assuming the first coefficient is the intercept

total_ops_A_model <- glm.nb(total_ops_Active ~ Total_Population + 
                                spatial_lag +
                                Median_Household_Income + 
                                white_perc +
                                black_perc + 
                                housing_perc +
                                bach_perc +
                                Median_Age +
                                Rate_LDS_Adherents +
                                Rate_MainlineProtestant_Adherents +
                                Rate_Evang_Adherents +
                                Rate_Catholic_Adherents,
                            data = complete_data,
                            control = glm.control(maxit = 999))

vif_values <- car::vif(total_ops_A_model)


# Combine starting values for both components
start_values <- list(count = count_start, zero_infl = zero_infl_start)

# Fit ZINB model
zinb_model <- zeroinfl(total_ops_Active ~ log(Total_Population) +
                           log(spatial_lag + 1) +
                           log(Median_Household_Income) +
                           log(white_perc + 1) | log(Total_Population) + log(spatial_lag + 1) + log(Median_Household_Income),
                       data = complete_data, dist = "negbin")

# Summary of the model
summary(zinb_model)

saveRDS(total_ops_A_model, "data/processed/nb_model.rds")

# Try a mixed effects model
# Fit the model with an observation-level random effect using glmmTMB
total_ops_A_model_nb_re <- glmmTMB(total_ops_Active ~ Total_Population + 
                                       spatial_lag +
                                       Median_Household_Income + 
                                       white_perc +
                                       housing_perc +
                                       bach_perc +
                                       Median_Age +
                                       log(Rate_LDS_Adherents + 1) +
                                       log(Rate_MainlineProtestant_Adherents + 1) +
                                       log(Rate_Evang_Adherents + 1) +
                                       log(Rate_Catholic_Adherents + 1) +
                                       (1 | GEOID),  # Adding the observation-level random effect
                                   data = complete_data,
                                   family = nbinom2(link = "log"),  # Specifying a negative binomial family
                                   control = glmmTMBControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

# Summary of the model
summary(total_ops_A_model_nb_re)
