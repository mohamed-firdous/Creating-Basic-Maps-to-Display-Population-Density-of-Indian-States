# Load libraries
library(shiny)
library(DT)
library(leaflet)
library(dplyr)


state_coords <- data.frame(
  State = c("Uttar Pradesh", "Bihar", "Maharashtra", "West Bengal", "Tamil Nadu", 
            "Rajasthan", "Karnataka", "Gujarat", "Andhra Pradesh", "Madhya Pradesh"),
  Lat = c(26.85, 25.59, 19.75, 22.57, 11.12, 27.02, 15.31, 22.26, 15.91, 23.52),
  Lon = c(80.91, 85.13, 75.71, 88.36, 78.15, 74.22, 75.71, 72.57, 79.74, 77.81)
)


ui <- fluidPage(
  titlePanel("Density Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File (State, Density)", accept = ".csv"),
      selectInput("filter_type", "Filter Population Density:",
                  choices = c("All", "Highest Density", "Lowest Density"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Table View", DTOutput("density_table")),
        tabPanel("Map View", leafletOutput("density_map", height = 600))
      )
    )
  )
)

server <- function(input, output) {
  
  user_data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    
    
    df$Density <- as.numeric(gsub(",", "", df$Density))
    
    )
    merged <- df %>%
      inner_join(state_coords, by = "State")
    
    merged
  })
  
  filtered_data <- reactive({
    data <- user_data()
    if (input$filter_type == "Highest Density") {
      data %>% filter(Density == max(Density, na.rm = TRUE))
    } else if (input$filter_type == "Lowest Density") {
      data %>% filter(Density == min(Density, na.rm = TRUE))
    } else {
      data
    }
  })
  
  output$density_table <- renderDT({
    datatable(filtered_data()[, c("State", "Density")], options = list(pageLength = 10))
  })
  
  output$density_map <- renderLeaflet({
    req(filtered_data())
    pal <- colorNumeric(palette = c("green", "yellow", "red"), domain = user_data()$Density)
    
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = filtered_data(),
                       lat = ~Lat,
                       lng = ~Lon,
                       radius = 10,
                       color = ~pal(Density),
                       stroke = TRUE,
                       fillOpacity = 0.8,
                       label = ~paste(State, ": ", Density)) %>%
      addLegend("bottomright", pal = pal, values = user_data()$Density,
                title = "Population Density")
  })
}


shinyApp(ui = ui, server = server)
