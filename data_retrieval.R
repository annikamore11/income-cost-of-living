library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(tidycensus)
library(writexl)

# BEA API key
api_key <- Sys.getenv("BEA_API_KEY")

# Census API key
census_api_key(Sys.getenv("CENSUS_API_KEY"), install = TRUE, overwrite = TRUE)

# Base BEA API URL
base_url <- "https://apps.bea.gov/api/data/"

###########################################################################################
# STATE DATA
###########################################################################################
# Parameters for Real Per Capita Personal Income (National Dollars) (Constant 2017 Dollars)
# Line Code 2
params_RPCPI <- list(
  UserID = api_key,
  method = "GetData",
  datasetname = "Regional",
  TableName = "SARPP",         # Real GDP
  LineCode = 2,         # All counties
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
  TableName = "SARPP",         # Real GDP
  LineCode = 5,         # All counties
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
  left_join(state_RPP %>% select(GeoName, Year, rpp_all_goods), 
            by = c("GeoName", "Year")
            ) 

# Calculate local purchasing power
state_combined <- state_combined  %>%
  mutate(
    local_purchasing_power = real_pc_personal_income / (rpp_all_goods / 100)
  )

###########################################################################################
# Parameters for Regional Price Parities HOUSING State
# Line Code 7
params_RPP <- list(
  UserID = api_key,
  method = "GetData",
  datasetname = "Regional",
  TableName = "SARPP",         
  LineCode = 7,         
  GeoFips = "STATE",
  Year = "LAST10",
  ResultFormat = "JSON"
)


res_RPP <- GET(url = base_url, query = params_RPP)
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
  left_join(state_housing_RPP %>% select(GeoName, Year, rpp_housing), 
            by = c("GeoName", "Year")
  ) 

###########################################################################################
## Retrieve med household income state

# Years 2014-2023
years <- 2014:2023

# Loop to get median household income for all states
median_income_list <- lapply(years, function(y) {
  get_acs(
    geography = "state",
    variables = "B19013_001",
    year = y,
    survey = "acs5"
  ) %>%
    select(GEOID, NAME, estimate) %>%
    mutate(year = y)
})

# Combine into one dataframe
median_income_df <- bind_rows(median_income_list) %>% 
  mutate( GEOID_5 = str_pad(GEOID, width = 2, pad = "0"),
          GEOID_5 = paste0(GEOID_5, "000")  
  )

state_combined <- state_combined %>%
  left_join(median_income_df %>% select(GEOID_5, estimate, year),
            by = c("GeoFips" = "GEOID_5", "Year" = "year")) %>%
  rename(
    med_household_income = estimate
  ) %>%
  mutate(
    med_income_RPP_all = med_household_income / (rpp_all_goods / 100)
  )

###########################################################################################
# METRO AREA DATA
###########################################################################################
# Parameters for Real Per Capita Personal Income (National Dollars) (Constant 2017 Dollars) Metro level
params_RPCPI <- list(
  UserID = api_key,
  method = "GetData",
  datasetname = "Regional",
  TableName = "MARPP",         # Real GDP
  LineCode = 2,         # All counties
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
params_RPP <- list(
  UserID = api_key,
  method = "GetData",
  datasetname = "Regional",
  TableName = "MARPP",         # Real GDP
  LineCode = 3,         # All counties
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
  left_join(metro_RPP %>% select(GeoName, Year, rpp_all_goods), 
            by = c("GeoName", "Year")
  ) 

metro_combined <- metro_combined  %>%
  mutate(
    local_purchasing_power = real_pc_personal_income / (rpp_all_goods / 100)
  )

###########################################################################################
# Parameters for Regional Price Parities HOUSING Metro Areas
params_RPP <- list(
  UserID = api_key,
  method = "GetData",
  datasetname = "Regional",
  TableName = "MARPP",         
  LineCode = 5,         
  GeoFips = "MSA",
  Year = "LAST10",
  ResultFormat = "JSON"
)


res_RPP <- GET(url = base_url, query = params_RPP)
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
  left_join(metro_housing_RPP %>% select(GeoName, Year, rpp_housing), 
            by = c("GeoName", "Year")
  ) 

###########################################################################################
## Retrieve med household income metros
# Loop to get median household income for all states
median_income_list <- lapply(years, function(y) {
  get_acs(
    geography = "metropolitan statistical area/micropolitan statistical area",
    variables = "B19013_001",
    year = y,
    survey = "acs5"
  ) %>%
    select(GEOID, NAME, estimate) %>%
    mutate(year = y)
})

# Combine into one dataframe
median_income_df <- bind_rows(median_income_list)

# Some metro areas changed over the years (geoid or name changed). Counties may have been added which is something to note when displaying data.
# Map new geoids to the old ones or whichever geoid is used for the metro area in BEA data.
cbsa_crosswalk <- tribble(
  ~old_geoid, ~new_geoid, ~new_name,
  "17460",    "17410",    "Cleveland, OH Metro Area",       ### Change all geoid to the old one
  "15680",    "30500",    "Lexington Park, MD Metro Area",  ### Change all geoid to the old one
  "19430",    "19380",    "Dayton-Kettering-Beavercreek, OH Metro Area", # Chang all geoid to the old one
  "39100",    "28880",    "Kiryas Joel-Poughkeepsie-Newburgh, NY Metro Area", ## get rid of years earlier than 2019, change geoid to old one
  "39150",    "39140",    "Prescott Valley-Prescott, AZ (Metropolitan Statistical Area)", # for pres, change all geoid to the old geoid
  "45540",    "48680",    "Wildwood-The Villages, FL Metro Area" # Change all geoid to the old geoid
  
)

# Apply the crosswalk
median_income_df_fixed <- median_income_df %>%
  left_join(cbsa_crosswalk, by = c("GEOID" = "new_geoid")) %>%
  mutate(
    GEOID = if_else(!is.na(old_geoid), old_geoid, GEOID),   
  )

# Both geoids have data from BEA but no Census data attached
get_rid <- c("31460", "36140")

# drop the 2 geoids with no data
median_income_df_fixed <- median_income_df_fixed %>%
  filter(!(GEOID %in% get_rid)) 


metro_combined <- metro_combined %>%
  left_join(median_income_df_fixed %>% select(GEOID, estimate, year),
            by = c("GeoFips" = "GEOID", "Year" = "year")) %>%
  rename(
    med_household_income = estimate
  ) %>%
  mutate(
    med_income_RPP_all = med_household_income / (rpp_all_goods / 100)
  )

###########################################################################################
# Create dataframe for both and write to excel
# bind them together
state_metro <- bind_rows(state_combined, metro_combined)

# Delete any rows where BEA did not retrieve data
# Delete any rows where med_household_income was not retrieved
state_metro <- state_metro %>%
  filter(
    real_pc_personal_income != 0,
    !is.na(med_household_income)
  ) %>%
filter(!(GeoFips == "39100" & Year < 2019)) %>% # Handle special case: drop Poughkeepsie before 2019, Poughkeepsie data not found before 2019
  mutate(Year = as.numeric(Year)) %>% 
  arrange(Year) 

write_xlsx(state_metro, "income_cost_of_living.xlsx")



