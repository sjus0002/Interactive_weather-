library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
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

# UI part
ui <- fillPage(
  sidebarLayout(
    # Filters panel (on the right side)
    sidebarPanel(
      sliderInput("year", 
                  "Select Years:", 
                  min = min(weather_data$year), 
                  max = max(weather_data$year), 
                  value = max(weather_data$year), 
                  step = 1,
                  animate = animationOptions(interval = 1000, loop = FALSE)),
      checkboxGroupInput("state", 
                         "Select State(s):", 
                         choices = c("All", unique(weather_data$state[weather_data$state != "Unknown"])), 
                         selected = unique(weather_data$state[weather_data$state != "Unknown"])),
      width = 3 # Makes the filter box narrower
    ),
    
    # Graph panel (on the left side)
    mainPanel(
      tabsetPanel(
        tabPanel("Average Temperature by State", 
                 plotlyOutput("tempChart", height = 500)),
        tabPanel("Average Rainfall by State", 
                 plotlyOutput("rainChart", height = 500))
      ),
      width = 9 # Makes the graph area wider
    )
  )
)

# Server part
server <- function(input, output, session) {
  
  # "All" checkbox behavior
  observeEvent(input$state, {
    if ("All" %in% input$state) {
      updateCheckboxGroupInput(session, "state", selected = unique(weather_data$state[weather_data$state != "Unknown"]))
    }
  })
  
  # Reactive filtered data based on input year and state
  filtered_data <- reactive({
    weather_data %>%
      filter(year <= input$year, state %in% input$state)
  })
  
  # Render the Average Temperature chart
  output$tempChart <- renderPlotly({
    data <- filtered_data()
    
    avg_temp_data <- data %>%
      group_by(state, year) %>%
      summarize(avg_temp = round(mean(temperature_2m, na.rm = TRUE), 1), .groups = "drop")
    
    p <- ggplot(avg_temp_data, aes(x = year, y = avg_temp, color = state, group = state)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = "Average Temperature by State Over Years", 
           y = "Average Temperature (°C)", x = "Year") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("year", "avg_temp", "state"))
  })
  
  # Render the Average Rainfall chart
  output$rainChart <- renderPlotly({
    data <- filtered_data()
    
    avg_rain_data <- data %>%
      group_by(state, year) %>%
      summarize(avg_rain = round(mean(rain, na.rm = TRUE), 1), .groups = "drop")
    
    p <- ggplot(avg_rain_data, aes(x = year, y = avg_rain, color = state, group = state)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = "Average Rainfall by State Over Years", 
           y = "Average Rainfall (mm)", x = "Year") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("year", "avg_rain", "state"))
  })
}

# Run the shiny app
shinyApp(ui = ui, server = server)
