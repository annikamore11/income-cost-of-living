library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(readxl)
library(tigris)
library(scales)
library(DT)

# -----------------------------
# Load Data
# -----------------------------
state_metro_2023 <- read_excel("income_cost_of_living_2023.xlsx")
state_metro_time_series <- read_excel("income_cost_of_living_time_series.xlsx")

options(tigris_use_cache = TRUE)

# States
states_sf <- states(cb = TRUE, year = 2023) %>%
  st_as_sf() %>%
  mutate(GeoFips = paste0(GEOID, "000")) %>%
  select(GeoFips, geometry, NAME)

# Metros
metros_sf <- core_based_statistical_areas(cb = TRUE, year = 2021) %>%
  st_as_sf() %>%
  filter(LSAD == "M1") %>%
  rename(GeoFips = GEOID) %>%
  select(GeoFips, geometry, NAME)

metros_sf %>% filter(GeoFips == 17460)

# Combine state and metro shapes
state_metro_sf <- bind_rows(states_sf, metros_sf)

# Join measures for 2023
state_metro_2023_final <- state_metro_2023 %>%
  left_join(state_metro_sf %>% select(-NAME), by = "GeoFips")
state_metro_2023_final <- st_as_sf(state_metro_2023_final) %>%
  st_transform(crs = 4326)

# Join time series 
state_metro_TS_final <- state_metro_time_series %>%
  left_join(state_metro_sf %>% select(-NAME), by = "GeoFips")
state_metro_TS_final <- st_as_sf(state_metro_TS_final) %>%
  st_transform(crs = 4326)

latest_year <- max(state_metro_2023_final$Year, na.rm = TRUE)
# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("US Income & Cost of Living in 2023"),
  
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
        DT::dataTableOutput("income_data"),
        br(),
        br(),
        plotlyOutput("plot_real_pc_income")
      ),
      
      # Cost of Living plots
      conditionalPanel(
        condition = "input.metric_category == 'Cost of Living'",
        DT::dataTableOutput("COL_table"),
        br(),
        br(),
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
        "Per Capita Income" = "pc_personal_income",
        "Cost-of-Living Adjusted (RPP) Personal Income" = "RPP_adj_pc_income"
      )
    } else {  # Cost of Living
      choices <- c(
        "Overall Cost-of-Living Index (RPP)" = "rpp_all_goods",
        "Housing Cost Index (RPP)" = "rpp_housing",
        "Value of $100" = "val_100_dollars"
      )
    }
    
    selectInput("metric", "Select Metric:", choices = choices)
  })
  
  metric_labels <- c(
    pc_personal_income = "Per Capita Income",
    RPP_adj_pc_income = "Cost-of-Living Adjusted (RPP) Personal Income",
    rpp_all_goods           = "Overall Cost-of-Living Index (RPP)",
    rpp_housing             = "Housing Cost Index (RPP)",
    val_100_dollars = "Value of $100"
  )
  
  map_data <- reactive({
    if(input$geo_level == "State") {
      state_metro_2023_final %>%
        filter(
          nchar(GeoFips) == 5 & substr(GeoFips,3,5) == "000",
          Year == latest_year
        )
    } else {
      state_metro_2023_final %>%
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
    # Diverging palette around 100
    if ((input$geo_level %in% c("State", "Metro")) & (input$metric_category == "Cost of Living")) {
      
      min_val <- min(df[[input$metric]], 100, na.rm = TRUE)
      max_val <- max(df[[input$metric]], 100, na.rm = TRUE)
      n_colors <- 100
      
      if(input$metric == "val_100_dollars"){
        # Flip colors for Value of $100
        n_blue <- pmax(0, round(n_colors * (100 - min_val) / (max_val - min_val)))
        n_red  <- pmax(0, round(n_colors * (max_val - 100) / (max_val - min_val)))
        palette <- c(
          if(n_red  > 0) colorRampPalette(c("red", "white"))(n_red) else NULL,
          if(n_blue > 0) colorRampPalette(c("white", "blue"))(n_blue) else NULL
        )
      } else {
        # Regular Cost of Living metrics
        n_red  <- pmax(0, round(n_colors * (max_val - 100) / (max_val - min_val)))
        n_blue <- pmax(0, round(n_colors * (100 - min_val) / (max_val - min_val)))
        palette <- c(
          if(n_blue > 0) colorRampPalette(c("blue", "white"))(n_blue) else NULL,
          if(n_red  > 0) colorRampPalette(c("white", "red"))(n_red) else NULL
        )
      }
      
      pal <- colorNumeric(
        palette = palette,
        domain = c(min_val, max_val),
        na.color = "transparent"
      )
      
    } else {
      pal <- colorNumeric(
        palette = "YlOrRd",
        domain = df[[input$metric]],
        na.color = "transparent"
      )
    }
    
    # Set labels 
    national <- state_metro_2023_final %>% filter(GeoFips == "00000")
    national_pc <- pull(national, pc_personal_income)
    national_adj <- pull(national, RPP_adj_pc_income)
    if(input$metric_category == "Income"){
      if(input$metric == "Per Capita Income"){
        labels <- sprintf("<strong>%s</strong><br>%s: %s<br>National Avg: %s",
                          df$GeoName,
                          metric_labels[input$metric],
                          dollar(df[[input$metric]]),
                          dollar(national_pc)) %>%
          lapply(htmltools::HTML)
      }else{
        labels <- sprintf("<strong>%s</strong><br>%s: %s<br>National Avg: %s",
                          df$GeoName,
                          metric_labels[input$metric],
                          dollar(df[[input$metric]]),
                          dollar(national_adj)) %>%
          lapply(htmltools::HTML)
      }
      
    } else{
      labels <- sprintf("<strong>%s</strong><br>%s: %s", 
                        df$GeoName, metric_labels[input$metric], round(df[[input$metric]],2)) %>% 
        lapply(htmltools::HTML)
    }
    
    
    # Set Map
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
    df <- state_metro_2023_final %>% filter(GeoFips == clicked_geo())
    df$GeoName[1]
  })
  
  national_TS <- state_metro_TS_final %>% filter(GeoFips == "00000")
  
  # Plot real per capita income over time
  output$plot_real_pc_income <- renderPlotly({
    req(clicked_geo())
    req(input$metric_category == "Income")
    df <- state_metro_TS_final %>% filter(GeoFips == clicked_geo())
    
    plot_ly(df, x = ~Year) %>%
      add_trace(y = ~real_pc_personal_income, type = 'scatter', mode = 'lines+markers',
                name = 'Real Per Capita Income', line = list(color = 'blue', width = 2),
                showlegend = TRUE) %>%
      add_trace(y = national_TS$real_pc_personal_income, type = 'scatter', mode = 'lines+markers',
                name = 'US Avg Real Per Capita Income', line = list(dash = 'dash', color = 'black', width = 2),
                showlegend = TRUE) %>%
      layout(title = "Inflation and Cost of Living Adjusted Per Capita Income Over Time",
             yaxis = list(title = "Inflation-COL-Adj Income (USD)"))
  })
  
  # add data table 
  output$income_data <- DT::renderDataTable({
    req(clicked_geo())
    req(input$metric_category == "Income")

    df <- state_metro_2023_final %>% 
      st_drop_geometry() %>%
      filter(GeoFips %in% c(clicked_geo(), "00000")) %>%
      select(GeoName, pc_personal_income, RPP_adj_pc_income) %>%
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      rename(
        'Geography' = GeoName,
        'Per Capita Personal Income' = pc_personal_income,
        "Cost-of-Living Adjusted (RPP) Personal Income" = RPP_adj_pc_income
    )
    
    DT::datatable(
      df,
      rownames = FALSE,
      options = list(
        dom = "t",   
        pageLength = 10,
        autoWidth = TRUE
      ),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: center; font-size: 16px; font-weight: bold;",
        "Income Metrics in 2023"
      )
    )
  })

  
  # RPP over time
  output$plot_rpp <- renderPlotly({
    req(clicked_geo())
    req(input$metric_category == "Cost of Living")
    df <- state_metro_TS_final %>% filter(GeoFips == clicked_geo())
    plot_ly(df, x = ~Year) %>%
      add_trace(y = ~rpp_all_goods, type = 'scatter', mode = 'lines+markers',
                name = "Overall Cost Index", line = list(color = 'blue', width = 2)) %>%
      add_trace(y = ~rpp_housing, type = 'scatter', mode = 'lines+markers',
                name = 'Housing Cost Index', line = list(color = 'red', width = 2)) %>%
      add_trace(x = ~Year, y = rep(100, nrow(df)),
                type = 'scatter', mode = 'lines',
                name = 'National Average Baseline (100)',
                line = list(dash = 'dash', color = 'black'),
                showlegend = TRUE) %>%
      layout(title = "Overall Cost of Living and Housing Cost Index Over Time",
             yaxis = list(title = "Index Value"))
  })
  
  # add data table 
  output$COL_table <- DT::renderDataTable({
    req(clicked_geo())
    req(input$metric_category == "Cost of Living")
    df <- state_metro_2023_final %>% 
      st_drop_geometry() %>%
      filter(GeoFips %in% c(clicked_geo(), "00000")) %>%
      select(GeoName, rpp_all_goods, rpp_housing, val_100_dollars) %>%   
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      rename(
        "Geography" = GeoName,
        "Overall Cost-of-Living Index (RPP)" = rpp_all_goods,
        "Housing Cost Index (RPP)" = rpp_housing,
        "Value of $100" = val_100_dollars
      )

    DT::datatable(
      df,
      rownames = FALSE,
      options = list(
        dom = "t",  
        pageLength = 10,
        autoWidth = TRUE
      ),
      caption = htmltools::tags$caption(
        style = "caption-side: top; text-align: center; font-size: 16px; font-weight: bold;",
        "Cost-of-Living Metrics in 2023"
      )
    )
  })
  
  
}

# -----------------------------
# Run App
# -----------------------------
shinyApp(ui, server)
