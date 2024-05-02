library(shiny)
library(bslib)


# Load data ----
nyc_data <- st_read("NYC_nbrhd_data.geojson", quiet = TRUE)
nyc_data <- st_make_valid(nyc_data)
nyc_data <- st_transform(nyc_data, crs = 4326)

# Define UI ----
ui <- 
  page_sidebar(
    title = "NYC SDOH App",
    sidebar = sidebar(
      
      helpText("Select different variables from the dropdown menus to explore the data."),
      
      selectInput("color", "Self-Identified Race & Ethnicity:",
                  choices = c("Percent Black" = "pctblack",
                              "Percent Hispanic" = "pcthisp",
                              "Percent White" = "pctwhite"),
                  selected = "pctblack")),
      
      mainPanel(
        ## Add a map
        (leafletOutput("map", width = "100%")
        )))
  


# Define server logic ----
server <- function(input, output, session) {
    
    # Map output for Racial Demographics
    output$map <- renderLeaflet({
      map_data <- st_transform(nyc_data, crs = 4326)
      
      valid_data <- map_data[!is.na(map_data[[input$color]]), ]
      pal <- colorQuantile("PuBuGn", valid_data[[input$color]], n = 5)
      
      leaflet(valid_data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~pal(valid_data[[input$color]]),
          fillOpacity = 0.7, weight = 1, color = "white",
          popup = ~paste(NTAName, "<br>",
                         paste(input$color, ":", round(valid_data[[input$color]], 2), "%"))
        ) %>%
        setView(lng = -73.935242, lat = 40.730610, zoom = 10)
    })
    
  }


# Run the app ----
shinyApp(ui = ui, server = server)