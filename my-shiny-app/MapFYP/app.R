library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(leaflet.extras)
library(leaflet)
library(plotly)

library(xtable)
library(htmlwidgets)
library(rsconnect)
#rsconnect::deployApp('https://sylvvvvvvvia.shinyapps.io/FIT3164-G10-NEW/')


#Database
library(DBI)
library(RMySQL)

con <- dbConnect(RMySQL::MySQL(),
                 dbname = "weather_test",
                 host = "fypdb1.cn4qqc08qp2s.ap-southeast-2.rds.amazonaws.com",  # This is the endpoint from RDS
                 port = 3306,                # Default MySQL port
                 user = "root",
                 password = "Stfnjstn03.")

# Prepare a SQL query to fetch all data
query <- "SELECT * FROM weather_data"

# Execute the query and store the result in a data frame
weather_data <- dbGetQuery(con, query)

names(weather_data)

# Process the data: convert dates and extract year and month
weather_data <- weather_data %>%
  mutate(weatherDate = ymd_hms(weatherDate),  # Ensure 'date' is in the correct datetime format
         year = year(weatherDate),
         month = month(weatherDate, label = TRUE, abbr = TRUE))  # 'label = TRUE' gives month names; 'abbr = TRUE' for abbreviated names

# Example of a simple operation: Count entries per year
yearly_data_count <- weather_data %>%
  count(year)

# Example of a simple operation: Count entries per year
yearly_data_count <- weather_data %>%
  count(year)

# Print the processed data frame (optional)
print(weather_data)
print(yearly_data_count)

# Close the database connection
dbDisconnect(con)

# Rename the columns
weather_data <- weather_data %>%
  rename(temperature_2m = temperature,  # Rename 'temperature' to 'temperature_2m'
         Latitude = latitude,           # Rename 'latitude' to 'Latitude'
         Longitude = longitude) 

#UI
ui <- fillPage(
  tags$head(
    tags$style(HTML("
      .leaflet-control .legend {
        width: 100px;
        height: 200px;
      }
      #map {
        height: 100vh; 
      }
      .absolute-panel {
        background-color: white;
        padding: 15px;
        border-radius: 8px;
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.2);
        z-index: 1000;
      }
      .checkbox-group {
        margin-bottom: 20px; 
      }
    "))
  ),
  
  # use tabsetPanel switch map
  tabsetPanel(
    tabPanel("Temperature Map", 
             leafletOutput("weatherMapTemp", height = "800px")),
    tabPanel("Rain Map", 
             leafletOutput("weatherMapRain", height = "800px"))
  ),
  
  # 悬浮的可拖动过滤面板
  absolutePanel(id = "controlPanel", class = "absolute-panel", top = 50, right = 20, width = 350, draggable = TRUE,
                h3("Filters"),
                sliderInput("year", 
                            "Select Year:", 
                            min = min(weather_data$year), 
                            max = max(weather_data$year), 
                            value = max(weather_data$year), 
                            step = 1,
                            animate = animationOptions(interval = 1000, loop = FALSE)),
                sliderInput("month", 
                            "Select Month:", 
                            min = 1, 
                            max = 12, 
                            value = 8, 
                            step = 1,
                            animate = animationOptions(interval = 500, loop = FALSE)),
                # 用 tags$div 包装 checkboxGroupInput 并应用 class
                tags$div(
                  checkboxGroupInput("state", 
                                     "Select State(s):", 
                                     choices = c("All", unique(weather_data$state[weather_data$state != "Unknown"])), 
                                     selected = unique(weather_data$state[weather_data$state != "Unknown"])),
                  class = "checkbox-group"
                )
  )
)


server <- function(input, output, session) {
  
  # "All" checkbox behavior
  observeEvent(input$state, {
    if ("All" %in% input$state) {
      updateCheckboxGroupInput(session, "state", selected = unique(weather_data$state[weather_data$state != "Unknown"]))
    }
  })
  
  # Reactive filtered data based on inputs
  selected_data <- reactive({
    month_selected <- month.abb[input$month]
    weather_data %>%
      filter(year == input$year, month == month_selected, state %in% input$state)
  })
  
  # For temperature map
  observe({
    data <- selected_data()
    
    pal_temp <- colorNumeric(
      palette = c("blue", "cyan", "yellow", "orange", "red"),
      domain = temperature_range
    )
    
    output$weatherMapTemp <- renderLeaflet({
      leaflet(data) %>%
        addTiles() %>%
        addCircleMarkers(lng = ~Longitude, 
                         lat = ~Latitude, 
                         radius = 5, 
                         color = ~pal_temp(temperature_2m), 
                         popup = ~paste("Temperature:", round(temperature_2m, 1), "°C")) %>%
        addLegend("bottomright", 
                  pal = pal_temp, 
                  values = temperature_range,
                  title = "Temperature (°C)",
                  labFormat = labelFormat(suffix = "°C"),
                  opacity = 1)
    })
  })
  
  # Handle map click to show temperature line chart in modal
  observeEvent(input$weatherMapTemp_marker_click, {
    click <- input$weatherMapTemp_marker_click
    
    if (!is.null(click)) {
      clicked_data <- weather_data %>%
        filter(round(Latitude, 3) == round(click$lat, 3), 
               round(Longitude, 3) == round(click$lng, 3)) %>%
        group_by(year) %>%
        summarize(avg_temp = mean(temperature_2m, na.rm = TRUE))
      
      if (nrow(clicked_data) == 0) {
        showModal(modalDialog(
          title = "No Data Available",
          "No temperature data available for this location.",
          easyClose = TRUE,
          size = "s"
        ))
      } else {
        showModal(modalDialog(
          title = paste("Average Temperature for Location (Lat:", click$lat, "Lng:", click$lng, ")"),
          plotOutput("tempChartLocation"),
          easyClose = TRUE,
          size = "l"
        ))
        
        output$tempChartLocation <- renderPlot({
          ggplot(clicked_data, aes(x = year, y = avg_temp)) +
            geom_line(color = "blue", size = 1) +
            geom_point(color = "red", size = 2) +
            labs(title = "Average Temperature Over Years", 
                 y = "Average Temperature (°C)", x = "Year") +
            theme_minimal()
        })
      }
    }
  })
  
  # For rain map with reversed color palette
  observe({
    data <- selected_data()
    
    pal_rain <- colorNumeric(
      palette = c("lightblue", "blue", "darkblue", "darkblue", "darkblue", "darkblue"),  
      domain = c(0,250)
    )
    
    output$weatherMapRain <- renderLeaflet({
      leaflet(data) %>%
        addTiles() %>%
        addCircleMarkers(lng = ~Longitude, 
                         lat = ~Latitude, 
                         radius = 5, 
                         color = ~pal_rain(rain), 
                         popup = ~paste("Rainfall:", rain, "mm")) %>%
        addLegend("bottomright", 
                  pal = pal_rain, 
                  values = c(0, 250),
                  title = "Rain (mm)",
                  labFormat = labelFormat(suffix = "mm"),
                  opacity = 1)
    })
  })
  
  # Handle map click to show rainfall line chart in modal
  observeEvent(input$weatherMapRain_marker_click, {
    click <- input$weatherMapRain_marker_click
    
    if (!is.null(click)) {
      # Filter the data for the clicked location (based on lat and lng)
      clicked_data <- weather_data %>%
        filter(round(Latitude, 3) == round(click$lat, 3), 
               round(Longitude, 3) == round(click$lng, 3)) %>%
        group_by(year) %>%
        summarize(avg_rain = mean(rain, na.rm = TRUE))
      
      if (nrow(clicked_data) == 0) {
        showModal(modalDialog(
          title = "No Data Available",
          "No rainfall data available for this location.",
          easyClose = TRUE,
          size = "s"
        ))
      } else {
        # Show modal with line chart for clicked location
        showModal(modalDialog(
          title = paste("Average Rainfall for Location (Lat:", click$lat, "Lng:", click$lng, ")"),
          plotOutput("rainChartLocation"),
          easyClose = TRUE,
          size = "l"
        ))
        
        # Render line chart for the clicked location
        output$rainChartLocation <- renderPlot({
          ggplot(clicked_data, aes(x = year, y = avg_rain)) +
            geom_line(color = "blue", size = 1) +
            geom_point(color = "red", size = 2) +
            labs(title = "Average Rainfall Over Years", 
                 y = "Average Rainfall (mm)", x = "Year") +
            theme_minimal()
        })
      }
    }
  })
  
}

# Run the shiny app
shinyApp(ui = ui, server = server)
