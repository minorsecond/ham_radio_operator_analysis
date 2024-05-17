library("RODBC")
library("odbc")
library("RPostgres")
library(tidycensus)
library(dplyr)
library(haven)

postgres_host = ""
geocode_con <- dbConnect(RPostgres::Postgres(),dbname="", host=postgres_host,
                            port = 0000,user = "", password = "")

main_con <- dbConnect(RPostgres::Postgres(),dbname="", host=postgres_host,
                         port = 0000,user = "", password = "")

# Query the data from the table
query <- "SELECT * FROM ham_ops.geocoded_entity_data"
entity_query <- "SELECT * FROM ham_ops.entity_data"
main_query <- "SELECT ad.*, ls.license_status, ls.effective_date
    FROM ham_ops.amateur_data ad
    LEFT JOIN ham_ops.license_status ls ON ls.unique_system_identifier = ad.unique_system_identifier"
geocoded_data <- dbGetQuery(geocode_con, query)
main_data <- dbGetQuery(main_con, main_query)
entity_data <- dbGetQuery(main_con, entity_query)

dbDisconnect(geocode_con)
dbDisconnect(main_con)

merged_data <- merge(main_data, geocoded_data, by = "unique_system_identifier")
merged_data <- merge(merged_data, entity_data, by = "unique_system_identifier")

# Rename the new call_sign.x column to call_sign
merged_data <- merged_data %>%
    rename(call_sign = call_sign.x)

# Religion data from https://www.thearda.com/data-archive?fid=RCMSCY20&tab=3
religion_data <- read_dta("data/raw/us_religion_census_denoms.dta")

saveRDS(merged_data, file = "data/processed/merged_amateur_data.rds")
saveRDS(religion_data, file = "data/processed/religion_data.rds")
