library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)

# BEA API key
api_key <- Sys.getenv("BEA_API_KEY")

# Census API key
census_api_key(Sys.getenv("CENSUS_API_KEY"))

# Base BEA API URL
base_url <- "https://apps.bea.gov/api/data/"

###########################################################################################
# STATE DATA
###########################################################################################
#------------------------------------------
# 2023 Values for Comparison Across States
#------------------------------------------
# Parameters for 2023 Per Capita Personal Income 
# Line Code 3
params_PCPI <- list(
  UserID = api_key,
  method = "GetData",
  datasetname = "Regional",
  TableName = "SAINC1",         
  LineCode = 3,         
  GeoFips = "STATE",
  Year = 2023,
  ResultFormat = "JSON"
)

res_PCPI <- GET(url = base_url, query = params_PCPI)
data_PCPI <- fromJSON(content(res_PCPI, as = "text"))$BEAAPI$Results$Data


state_PCPI <- as.data.frame(data_PCPI) %>%
  filter(grepl("^[0-9]{5}$", GeoFips)) %>%        
  mutate(
    DataValue = as.numeric(gsub(",", "", DataValue)),
    Year = as.integer(TimePeriod)
  ) %>%
  select(GeoFips, GeoName, Year, DataValue) %>%
  rename(pc_personal_income = DataValue)

###########################################################################################
# Parameters for Regional Price Parities at state level for 2023
# Line Code 5
params_RPP_2023 <- list(
  UserID = api_key,
  method = "GetData",
  datasetname = "Regional",
  TableName = "SARPP",         # Real GDP
  LineCode = 5,         # All counties
  GeoFips = "STATE",
  Year = 2023,
  ResultFormat = "JSON"
)

res_RPP <- GET(url = base_url, query = params_RPP_2023)
data_RPP <- fromJSON(content(res_RPP, as = "text"))$BEAAPI$Results$Data


state_RPP_2023 <- as.data.frame(data_RPP) %>%
  filter(grepl("^[0-9]{5}$", GeoFips)) %>%        
  mutate(
    DataValue = as.numeric(gsub(",", "", DataValue)),
    Year = as.integer(TimePeriod)
  ) %>%
  select(GeoFips, GeoName, Year, DataValue) %>%
  rename(rpp_all_goods = DataValue)

### Combine per capita income and RPP
state_combined_2023 <- state_PCPI %>%
  left_join(state_RPP_2023 %>% select(GeoFips, GeoName, rpp_all_goods), 
            by = c("GeoFips")
  ) %>% rename(GeoName = GeoName.y) %>% select(GeoFips, GeoName, everything()) %>% select(-GeoName.x)

# Calculate local purchasing power
state_combined_2023 <- state_combined_2023  %>%
  mutate(
    RPP_adj_pc_income = pc_personal_income / (rpp_all_goods / 100)
  )

###########################################################################################
# Parameters for Regional Price Parities HOUSING State 2023
# Line Code 7
params_RPP_housing_2023 <- list(
  UserID = api_key,
  method = "GetData",
  datasetname = "Regional",
  TableName = "SARPP",         
  LineCode = 7,         
  GeoFips = "STATE",
  Year = 2023,
  ResultFormat = "JSON"
)

res_RPP <- GET(url = base_url, query = params_RPP_housing_2023)
data_RPP <- fromJSON(content(res_RPP, as = "text"))$BEAAPI$Results$Data

state_housing_RPP_2023 <- as.data.frame(data_RPP) %>%
  filter(grepl("^[0-9]{5}$", GeoFips)) %>%       
  mutate(
    DataValue = as.numeric(gsub(",", "", DataValue)),
    Year = as.integer(TimePeriod)
  ) %>%
  select(GeoFips, GeoName, Year, DataValue) %>%
  rename(rpp_housing = DataValue)

state_combined_2023 <- state_combined_2023 %>%
  left_join(state_housing_RPP_2023 %>% select(GeoFips, rpp_housing), 
            by = c("GeoFips")
  ) %>% 
  mutate(
    val_100_dollars = (100/ rpp_all_goods)*100
  )

# Remove NA rows (These were just regions not states)
state_combined_2023 <- state_combined_2023 %>%
  filter(!is.na(GeoName))

###########################################################################################
#------------------------------------------
# Values for Comparison Across Time (Last 10 Years)
#------------------------------------------
# Parameters for Real Per Capita Personal Income (National Dollars) (Constant 2017 Dollars)
# Real per Capita adjusts per capita income for inflation as well as RPP 
# Line Code 2
params_RPCPI <- list(
  UserID = api_key,
  method = "GetData",
  datasetname = "Regional",
  TableName = "SARPP",         
  LineCode = 2,         
  GeoFips = "STATE",
  Year = "LAST10",
  ResultFormat = "JSON"
)

res_RPCPI <- GET(url = base_url, query = params_RPCPI)
data_RPCPI <- fromJSON(content(res_RPCPI, as = "text"))$BEAAPI$Results$Data


state_RPCPI <- as.data.frame(data_RPCPI) %>%
  filter(grepl("^[0-9]{5}$", GeoFips)) %>%        
  mutate(
    DataValue = as.numeric(gsub(",", "", DataValue)),
    Year = as.integer(TimePeriod)
  ) %>%
  select(GeoFips, GeoName, Year, DataValue) %>%
  rename(real_pc_personal_income = DataValue)

###########################################################################################
# Parameters for Regional Price Parities at state level
# Line Code 5
params_RPP <- list(
  UserID = api_key,
  method = "GetData",
  datasetname = "Regional",
  TableName = "SARPP",         
  LineCode = 5,         
  GeoFips = "STATE",
  Year = "LAST10",
  ResultFormat = "JSON"
)

res_RPP <- GET(url = base_url, query = params_RPP)
data_RPP <- fromJSON(content(res_RPP, as = "text"))$BEAAPI$Results$Data


state_RPP <- as.data.frame(data_RPP) %>%
  filter(grepl("^[0-9]{5}$", GeoFips)) %>%        
  mutate(
    DataValue = as.numeric(gsub(",", "", DataValue)),
    Year = as.integer(TimePeriod)
  ) %>%
  select(GeoFips, GeoName, Year, DataValue) %>%
  rename(rpp_all_goods = DataValue)

### Combine real per capita income and RPP
state_combined <- state_RPCPI %>%
  left_join(state_RPP %>% select(GeoFips, Year, rpp_all_goods), 
            by = c("GeoFips", "Year")
            ) 

###########################################################################################
# Parameters for Regional Price Parities HOUSING State
# Line Code 7
params_RPP_housing <- list(
  UserID = api_key,
  method = "GetData",
  datasetname = "Regional",
  TableName = "SARPP",         
  LineCode = 7,         
  GeoFips = "STATE",
  Year = "LAST10",
  ResultFormat = "JSON"
)


res_RPP <- GET(url = base_url, query = params_RPP_housing)
data_RPP <- fromJSON(content(res_RPP, as = "text"))$BEAAPI$Results$Data


state_housing_RPP <- as.data.frame(data_RPP) %>%
  filter(grepl("^[0-9]{5}$", GeoFips)) %>%       
  mutate(
    DataValue = as.numeric(gsub(",", "", DataValue)),
    Year = as.integer(TimePeriod)
  ) %>%
  select(GeoFips, GeoName, Year, DataValue) %>%
  rename(rpp_housing = DataValue)


state_combined <- state_combined %>%
  left_join(state_housing_RPP %>% select(GeoFips, Year, rpp_housing), 
            by = c("GeoFips", "Year")
  ) 


###########################################################################################
# METRO AREA DATA
###########################################################################################
#------------------------------------------
# 2023 Values for Comparison Across Metro Areas
#------------------------------------------
# Parameters for Per Capita Personal Income Metro level 2023
params_PCPI_2023 <- list(
  UserID = api_key,
  method = "GetData",
  datasetname = "Regional",
  TableName = "CAINC1",         
  LineCode = 3,         
  GeoFips = "MSA",
  Year = 2023,
  ResultFormat = "JSON"
)


res_PCPI <- GET(url = base_url, query = params_PCPI_2023)
data_PCPI <- fromJSON(content(res_PCPI, as = "text"))$BEAAPI$Results$Data


metro_PCPI_2023 <- as.data.frame(data_PCPI) %>%
  filter(grepl("^[0-9]{5}$", GeoFips)) %>%        
  mutate(
    DataValue = as.numeric(gsub(",", "", DataValue)),
    Year = as.integer(TimePeriod)
  ) %>%
  select(GeoFips, GeoName, Year, DataValue) %>%
  rename(pc_personal_income = DataValue)

###########################################################################################
# Parameters for Regional Price Parities at metro level for 2023
# Line Code 5
params_RPP_2023 <- list(
  UserID = api_key,
  method = "GetData",
  datasetname = "Regional",
  TableName = "MARPP",       
  LineCode = 3,         
  GeoFips = "MSA",
  Year = 2023,
  ResultFormat = "JSON"
)

res_RPP <- GET(url = base_url, query = params_RPP_2023)
data_RPP <- fromJSON(content(res_RPP, as = "text"))$BEAAPI$Results$Data


metro_RPP_2023 <- as.data.frame(data_RPP) %>%
  filter(grepl("^[0-9]{5}$", GeoFips)) %>%        
  mutate(
    DataValue = as.numeric(gsub(",", "", DataValue)),
    Year = as.integer(TimePeriod)
  ) %>%
  select(GeoFips, GeoName, Year, DataValue) %>%
  rename(rpp_all_goods = DataValue)

### Combine per capita income and RPP
metro_combined_2023 <- metro_PCPI_2023 %>%
  left_join(metro_RPP_2023 %>% select(GeoFips, GeoName, rpp_all_goods), 
            by = c("GeoFips")
  ) %>% rename(GeoName = GeoName.y) %>% select(GeoFips, GeoName, everything()) %>% select(-GeoName.x)

# Calculate local purchasing power
metro_combined_2023 <- metro_combined_2023  %>%
  mutate(
    RPP_adj_pc_income = pc_personal_income / (rpp_all_goods / 100)
    )

###########################################################################################
# Parameters for Regional Price Parities HOUSING State 2023
# Line Code 7
params_RPP_housing_2023 <- list(
  UserID = api_key,
  method = "GetData",
  datasetname = "Regional",
  TableName = "MARPP",         
  LineCode = 5,         
  GeoFips = "MSA",
  Year = 2023,
  ResultFormat = "JSON"
)

res_RPP <- GET(url = base_url, query = params_RPP_housing_2023)
data_RPP <- fromJSON(content(res_RPP, as = "text"))$BEAAPI$Results$Data

metro_housing_RPP_2023 <- as.data.frame(data_RPP) %>%
  filter(grepl("^[0-9]{5}$", GeoFips)) %>%       
  mutate(
    DataValue = as.numeric(gsub(",", "", DataValue)),
    Year = as.integer(TimePeriod)
  ) %>%
  select(GeoFips, GeoName, Year, DataValue) %>%
  rename(rpp_housing = DataValue)

metro_combined_2023 <- metro_combined_2023 %>%
  left_join(metro_housing_RPP_2023 %>% select(GeoFips, rpp_housing), 
            by = c("GeoFips")
  ) %>% 
  mutate(
    val_100_dollars = (100/ rpp_all_goods)*100
  )

# Remove NA rows (These were just US values [uneeded])
metro_combined_2023 <- metro_combined_2023 %>%
  filter(!is.na(GeoName))

###########################################################################################
#------------------------------------------
# Values for Metro Comparison Across Time (Last 10 Years)
#------------------------------------------
# Parameters for Real Per Capita Personal Income (National Dollars) (Constant 2017 Dollars)
# Real per Capita adjusts per capita income for inflation as well as RPP 
# Line Code 2
params_RPCPI <- list(
  UserID = api_key,
  method = "GetData",
  datasetname = "Regional",
  TableName = "MARPP",         
  LineCode = 2,         
  GeoFips = "MSA",
  Year = "LAST10",
  ResultFormat = "JSON"
)

res_RPCPI <- GET(url = base_url, query = params_RPCPI)
data_RPCPI <- fromJSON(content(res_RPCPI, as = "text"))$BEAAPI$Results$Data


metro_RPCPI <- as.data.frame(data_RPCPI) %>%
  filter(grepl("^[0-9]{5}$", GeoFips)) %>%        
  mutate(
    DataValue = as.numeric(gsub(",", "", DataValue)),
    Year = as.integer(TimePeriod)
  ) %>%
  select(GeoFips, GeoName, Year, DataValue) %>%
  rename(real_pc_personal_income = DataValue)

###########################################################################################
# Parameters for Regional Price Parities Metro Areas
# Line code 3
params_RPP <- list(
  UserID = api_key,
  method = "GetData",
  datasetname = "Regional",
  TableName = "MARPP",        
  LineCode = 3,    
  GeoFips = "MSA",
  Year = "LAST10",
  ResultFormat = "JSON"
)


res_RPP <- GET(url = base_url, query = params_RPP)
data_RPP <- fromJSON(content(res_RPP, as = "text"))$BEAAPI$Results$Data


metro_RPP <- as.data.frame(data_RPP) %>%
  filter(grepl("^[0-9]{5}$", GeoFips)) %>%       
  mutate(
    DataValue = as.numeric(gsub(",", "", DataValue)),
    Year = as.integer(TimePeriod)
  ) %>%
  select(GeoFips, GeoName, Year, DataValue) %>%
  rename(rpp_all_goods = DataValue)


metro_combined <- metro_RPCPI %>%
  left_join(metro_RPP %>% select(GeoFips, Year, rpp_all_goods), 
            by = c("GeoFips", "Year")
  ) 


###########################################################################################
# Parameters for Regional Price Parities HOUSING Metro Areas
# Line code 5
params_RPP_housing <- list(
  UserID = api_key,
  method = "GetData",
  datasetname = "Regional",
  TableName = "MARPP",         
  LineCode = 5,         
  GeoFips = "MSA",
  Year = "LAST10",
  ResultFormat = "JSON"
)


res_RPP <- GET(url = base_url, query = params_RPP_housing)
data_RPP <- fromJSON(content(res_RPP, as = "text"))$BEAAPI$Results$Data


metro_housing_RPP <- as.data.frame(data_RPP) %>%
  filter(grepl("^[0-9]{5}$", GeoFips)) %>%       
  mutate(
    DataValue = as.numeric(gsub(",", "", DataValue)),
    Year = as.integer(TimePeriod)
  ) %>%
  select(GeoFips, GeoName, Year, DataValue) %>%
  rename(rpp_housing = DataValue)


metro_combined <- metro_combined %>%
  left_join(metro_housing_RPP %>% select(GeoFips, Year, rpp_housing), 
            by = c("GeoFips", "Year")
  ) 

###########################################################################################
# Create a dataframe for state/metro 2023 data 
state_metro_2023 <- bind_rows(state_combined_2023, metro_combined_2023)

# Create a dataframe for state/metro time series data
state_metro_time_series <- bind_rows(state_combined, metro_combined) %>% mutate(Year = as.numeric(Year)) %>% arrange(Year)

# Write to data folder
write_xlsx(state_metro_2023, "../data/income_cost_of_living_2023.xlsx")
write_xlsx(state_metro_time_series, "../data/income_cost_of_living_time_series.xlsx")

# Write to R Shiny App folder
write_xlsx(state_metro_2023, "../R_Shiny_App/income_cost_of_living_2023.xlsx")
write_xlsx(state_metro_time_series, "../R_Shiny_App/income_cost_of_living_time_series.xlsx")


