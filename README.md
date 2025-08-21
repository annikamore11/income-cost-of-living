# Bureau of Health and Economic Analysis (BEA) Data

BEA data provides insights into the economic state of different regions of the country. In this project, the focus is on income and cost-of-living.

## Table of Contents

-   [BEA API User Guide](#bea-api-user-guide)
-   [Project Folders](project-folders)
    -   [Scripts](#scripts)
    -   [R_Shiny_App](#r_shiny_app)
    -   [Data](#data)
-   [Geography Levels](#geography-levels)
-   [Measures](#measures)
    -   [2023 Measures](#2023-measures)
    -   [Time Series Measures](#time-series-measures)
-   [Calculations](#calculations)
-   [Installation](#installation)
-   [Required Packages](#required-packages)
-   [Important Notes for Metro Data](#important-notes-for-metro-data)
-   [Update Schedule](#update-schedule)
    -   [State](#state)
    -   [Metro](#metro)
    -   [Summary](#summary)
-   [R Shiny App Guide](#r-shiny-app-guide)

## BEA API User Guide 

<https://apps.bea.gov/api/_pdf/bea_web_service_api_user_guide.pdf>

## Project Folders

### Scripts: 

-   `data_retrieval.R`: a script to query the BEA API and retrieve income and cost of living metrics. Additionally, combine, and mutate data to create new economic metrics.

### R_Shiny_App: 

-   `app.R`: R Shiny app code to prototype ways to display data.
    -   **R Shiny app**: <https://annikamore.shinyapps.io/R_Shiny_App/>
    -   Refer to [R Shiny App Guide](#r-shiny-app-guide)

### Data:

-   `income_cost_of_living_2023.xlsx`: Excel file containing cleaned BEA output data from for 2023.
-   Refer to [2023 Measures](#2023-measures)
-   `income_cost_of_living_time_series.xlsx`: Excel file containing cleaned BEA output data for years 2014 - 2023.
-   Refer to [Time Series Measures](#time-series-measures)

## Geography Levels:

-   US
-   State
-   Metropolitan Statistical Area

## Measures: 

-   BEA description of Regional Price Parities: <https://www.bea.gov/data/prices-inflation/regional-price-parities-state-and-metro-area>
-   BEA description of Real Personal Income: <https://www.bea.gov/data/income-saving/real-personal-income-states-and-metropolitan-areas>

### 2023 Measures:

| Field | Description |   
|----|----|
| pc_personal_income | Per Capita Personal Income |  
| rpp_all_goods | Regional Price Parity (Price differences) for all goods |  
| RPP_adj_pc_income | Per capita personal income adjusted for RPP of all goods (adjusted for cost-of-living) |  
| rpp_housing | Regional Price Parity (Price differences) for housing costs |  
| val_100_dollars | The value of \$100 (Calculated using RPP of all goods) |  

### Time Series Measures: 

| Field | Description | Years |
|----|----|----|
| real_pc_personal_income | Per capita income adjusted for inflation and adjusted for local RPP of all goods (cost of living). Allows you to compare over time. | 2014 - 2023 |
| rpp_all_goods | Regional Price Parity (Price differences) for all goods | 2014-2023 |
| rpp_housing | Regional Price Parity (Price differences) for housing costs | 2014 - 2023 |

## Calculations 

-   RPP_adj_pc_income = pc_personal_income / (rpp_all_good / 100)
    -   Cost of living adjusted per capita income = Per capita income / (cost of living index / 100)
-   val_100_dollars = (100/ rpp_all_goods)\*100

## Installation 

-   Clone the repository
-   Store API keys in a `.Renviron` file
-   Run `data_retrieval.R` to query API's and update or change data.
-   Download files from `data/` separately if no updates are needed.

## Required Packages 

-   `dplyr`
-   `tidyr`
-   `httr`
-   `writexl`
-   `stringr`
-   `jsonlite`

## Important Notes for Metro Data 

-   Every few years, The Census updates metropolitan/micropolitan delineations and these take immediate effect in the data and shapefiles offered by Census.
-   However, BEA delineation updates can lag behind Census, so new changes may not be updated in BEA data until later.
-   Because of this, when mapping BEA metro data, use 2021 shapefiles.

## Update Schedule 

### State 

-   State Per Capita Personal Income for 2024 is released. However, 2024 metro data will not be released until December, so 2023 data is stored for comparability.
    -   State per capita personal income is released quarterly, with Q1, 2025 released on June 27, 2025.
-   **September 26, 2025**: Per Capita Personal Income Q2 will be released (Don't need to update until 2025 data needed)
-   **December 11, 2025**: RPP all items, RPP housing, Real per capita personal income

### Metro

-   **December 3, 2025**: Per Capita Personal Income will be released.
-   **December 11, 2025**: RPP all items, RPP housing, Real per capita personal income

### Summary 

-   All 2024 data will be available by December 11, so this will be the time to go in and update data.
-   'Year' in the API parameters should be changed to latest year available. Only where "LAST10" is not used.

## R Shiny App Guide 

-   **R Shiny app**: <https://annikamore.shinyapps.io/R_Shiny_App/>
-   The app is fairly intuitive with the heat maps showing data from the 2023 data set.
-   When a state or metro area is clicked, different time series plots will appear according to the metric category chosen.
    -   These time series plots are built using the time series data.
