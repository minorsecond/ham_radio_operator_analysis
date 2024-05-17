library(spdep)
library(dplyr)

# Warning: please run this outside of rstudio server as the server WILL
# timeout befire the neighbors are finished calculating. I recommending
# running in a screen session if on Linux. If running RStudio or R on a desktop
# system, you should be fine.

data <- readRDS("data/processed/spatially_joined_dataset.rds")


# Remove non-CONUS areas
data_conus <- data %>%
    filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))

# The projected CRS we want (USA Contiguous Equidistant, EPSG 102005)
epsg_102005 <- "+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +type=crs"


# Project the data
data_conus_transformed <- st_transform(data_conus, crs = epsg_102005)
data_conus_transformed <- data_conus_transformed[!is.na(data_conus_transformed$Total_Population), ]
centroids <- st_centroid(data_conus_transformed)
coordinates <- st_coordinates(centroids)

print(nrow(data_conus_transformed))
print(nrow(coordinates))

knearneighbors <- knearneigh(coordinates, k=4) # Warning: this will take forever
nb <- knn2nb(knearneighbors)

saveRDS(nb, "data/processed/transformed_tracts_4nn.rds")
