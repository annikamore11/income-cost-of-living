library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(readxl)
library(tigris)
library(scales)

# -----------------------------
# Load Data
# -----------------------------
state_metro_measures <- read_excel("income_cost_of_living.xlsx")

options(tigris_use_cache = TRUE)

# States
states_sf <- states(cb = TRUE) %>%
  st_as_sf() %>%
  mutate(GeoFips = paste0(GEOID, "000")) %>%
  select(GeoFips, geometry, NAME)

# Metros
metros_sf <- core_based_statistical_areas(cb = TRUE) %>%
  st_as_sf() %>%
  filter(
    LSAD == "M1"
  ) %>%
  rename(
    GeoFips = GEOID
  ) %>%
  select(GeoFips, geometry, NAME)

# Combine state and metro shapes
state_metro_sf <- bind_rows(states_sf, metros_sf)

# Join measures
state_metro_data <- state_metro_measures %>%
  left_join(state_metro_sf, by = "GeoFips")

state_metro_data <- st_as_sf(state_metro_data) %>%
  st_transform(crs = 4326)

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("US Income & Cost of Living"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("geo_level", "Geography Level:", choices = c("State", "Metro"), selected = "State"),
      selectInput("metric_category", "Metric Category:", choices = c("Income", "Cost of Living"), selected = "Income"),
      uiOutput("metric_ui")
    ),
    
    mainPanel(
      leafletOutput("map", height = "700px"),
      hr(),
      h3(textOutput("geo_name_title")),
      
      # Income plots
      conditionalPanel(
        condition = "input.metric_category == 'Income'",
        plotlyOutput("plot_med_household_income"),
        plotlyOutput("plot_per_capita_income")
      ),
      
      # Cost of Living plots
      conditionalPanel(
        condition = "input.metric_category == 'Cost of Living'",
        plotlyOutput("plot_rpp_percent"),
        plotlyOutput("plot_rpp")
      )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session) {
  
  # Dynamic metric selector
  output$metric_ui <- renderUI({
    req(input$metric_category, input$geo_level)
    
    if(input$metric_category == "Income") {
      choices <- c(
        "Average Income per Person" = "real_pc_personal_income",
        "Median Household Income" = "med_household_income",
        "Cost-of-Living Adjusted Median Household Income" = "med_income_RPP_all"
      )
    } else {  # Cost of Living
      choices <- c(
        "Overall Cost-of-Living Index" = "rpp_all_goods",
        "Housing Cost Index" = "rpp_housing"
      )
    }
    
    selectInput("metric", "Select Metric:", choices = choices)
  })
  
  metric_labels <- c(
    real_pc_personal_income = "Average Income per Person",
    med_household_income    = "Median Household Income",
    med_income_RPP_all      = "Adjusted Median Household Income",
    rpp_all_goods           = "Overall Cost-of-Living Index",
    rpp_housing             = "Housing Cost Index"
  )
  
  map_data <- reactive({
    latest_year <- max(state_metro_data$Year, na.rm = TRUE)
    
    if(input$geo_level == "State") {
      state_metro_data %>%
        filter(
          nchar(GeoFips) == 5 & substr(GeoFips,3,5) == "000",
          Year == latest_year
        )
    } else {
      state_metro_data %>%
        filter(
          nchar(GeoFips) == 5 & substr(GeoFips,3,5) != "000",
          Year == latest_year
        )
    }
  })
  
  # Initialize map
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98, lat = 39, zoom = 4)
  })
  
  # Update map on metric selection
  observe({
    df <- map_data()
    req(input$metric)
    
    # ------------------------
    # Set diverging color palette around 100 for Cost of Living
    # ------------------------
    if(input$geo_level %in% c("State", "Metro") & input$metric_category == "Cost of Living") {
      
      min_val <- min(df[[input$metric]], 100, na.rm = TRUE)  # ensure 100 is in range
      max_val <- max(df[[input$metric]], 100, na.rm = TRUE)

      n_colors <- 100
      n_blue <- pmax(0, round(n_colors * (100 - min_val) / (max_val - min_val)))
      n_red  <- pmax(0, round(n_colors * (max_val - 100) / (max_val - min_val)))
      
      palette <- c(
        if(n_blue > 0) colorRampPalette(c("blue", "white"))(n_blue) else NULL,
        if(n_red  > 0) colorRampPalette(c("white", "red"))(n_red) else NULL
      )
      
      pal <- colorNumeric(
        palette =  palette,
        domain = c(min_val, max_val),
        na.color = "transparent"
      )
      
    } else {
      pal <- colorNumeric(
        palette = "RdBu",
        domain = df[[input$metric]],
        na.color = "transparent"
      )
    }
    
    if(input$metric_category == "Income"){
      labels <- sprintf("<strong>%s</strong><br>%s: %s",
                        df$GeoName,
                        metric_labels[input$metric],
                        dollar(df[[input$metric]])) %>%
        lapply(htmltools::HTML)
    } else{
      labels <- sprintf("<strong>%s</strong><br>%s: %s", 
                        df$GeoName, metric_labels[input$metric], round(df[[input$metric]],2)) %>% 
        lapply(htmltools::HTML)
    }

    
    leafletProxy("map", data = df) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~pal(df[[input$metric]]),
        weight = 1,
        color = "white",
        fillOpacity = 0.8,
        label = labels,
        layerId = ~GeoFips
      ) %>%
      addLegend(pal = pal, values = df[[input$metric]], title = as.character(metric_labels[input$metric]), position = "bottomright")
  })
  
  # Track clicked region
  clicked_geo <- reactiveVal(NULL)
  
  observeEvent(input$map_shape_click, {
    clicked_geo(input$map_shape_click$id)
  })
  
  # Geo name for title
  output$geo_name_title <- renderText({
    req(clicked_geo())
    df <- state_metro_data %>% filter(GeoFips == clicked_geo())
    df$GeoName[1]
  })
  
  # Plot 1: Per capita income or first metric
  output$plot_per_capita_income <- renderPlotly({
    req(clicked_geo())
    req(input$metric_category == "Income")
    df <- state_metro_data %>% filter(GeoFips == clicked_geo())
    
    plot_ly(df, x = ~Year) %>%
      add_trace(y = ~real_pc_personal_income, type = 'scatter', mode = 'lines+markers',
                name = 'Average Income per Person', line = list(color = 'blue', width = 2)) %>%
      layout(title = "Inflation Adjusted Average Income per Person Over Time",
             yaxis = list(title = "Inflation-Adj Income (USD)"))
  })
  
  # Plot 2: Median household income vs RPP-adjusted
  output$plot_med_household_income <- renderPlotly({
    req(clicked_geo())
    req(input$metric_category == "Income")
    df <- state_metro_data %>% filter(GeoFips == clicked_geo())
    
    plot_ly(df, x = ~Year) %>%
      add_trace(y = ~med_household_income, type = 'scatter', mode = 'lines+markers',
                name = 'Median Household Income', line = list(color = 'blue', width = 2)) %>%
      add_trace(y = ~med_income_RPP_all, type = 'scatter', mode = 'lines+markers',
                name = 'Cost-of-Living Adj Income', line = list(color = 'red', width = 2)) %>%
      layout(title = "Median Household Income Over Time (Nominal vs Cost-of-Living Adjusted)",
             yaxis = list(title = "Inflation-Adj Income (USD)"))
  })
  
  # RPP over time
  output$plot_rpp <- renderPlotly({
    req(clicked_geo(), input$metric_category == "Cost of Living")
    df <- state_metro_data %>% filter(GeoFips == clicked_geo()) %>%
      mutate(rpp_all_pct = (rpp_all_goods / lag(rpp_all_goods) - 1) * 100,
             rpp_housing_pct = (rpp_housing / lag(rpp_housing) - 1) * 100)
    
    plot_ly(df, x = ~Year) %>%
      add_trace(y = ~rpp_all_pct, type = 'scatter', mode = 'lines+markers',
                name = 'Overall Cost of Living % Change', line = list(color = 'blue', width = 2)) %>%
      add_trace(y = ~rpp_housing_pct, type = 'scatter', mode = 'lines+markers',
                name = 'Housing Cost % Change', line = list(color = 'red', width = 2)) %>%
      layout(title = "% Change in Overall Cost-of-Living and Housing Cost Over Time",
             yaxis = list(title = "RPP"))
  }
  )
  
  # RPP percent change over time
  output$plot_rpp_percent <- renderPlotly({
    req(clicked_geo())
    req(input$metric_category == "Cost of Living")
    df <- state_metro_data %>% filter(GeoFips == clicked_geo())
    plot_ly(df, x = ~Year) %>%
      add_trace(y = ~rpp_all_goods, type = 'scatter', mode = 'lines+markers',
                name = "Overall Cost Index", line = list(color = 'blue', width = 2)) %>%
      add_trace(y = ~rpp_housing, type = 'scatter', mode = 'lines+markers',
                name = 'Housing Cost Index', line = list(color = 'red', width = 2)) %>%
      # Dashed baseline as an invisible trace for the legend
      add_trace(x = ~Year, y = rep(100, nrow(df)),
                type = 'scatter', mode = 'lines',
                name = 'National Average Baseline (100)',
                line = list(dash = 'dash', color = 'black'),
                showlegend = TRUE) %>%
      layout(title = "Overall Cost of Living and Housing Cost Index Over Time",
             yaxis = list(title = "Index Value"))
  })
  

  
}

# -----------------------------
# Run App
# -----------------------------
shinyApp(ui, server)
