# Load necessary libraries
library(shiny)
library(leaflet)
library(sf)
library(plotly)
library(dplyr)

# Load and prepare the data
nyc_data <- st_read("NYC_nbrhd_data.geojson", quiet = TRUE)
nyc_data <- st_make_valid(nyc_data)
nyc_data <- st_transform(nyc_data, crs = 4326)

# Define UI for the first tab
tab1_ui <- tabPanel("Self-Identified Race & Ethnicty",
                    sidebarLayout(
                      sidebarPanel(
                        p("Select different variables from the dropdown menus to explore the data."),
                        selectInput("color", "Self-Identified Race & Ethnicity:",
                                    choices = c("Percent Black" = "pctblack",
                                                "Percent Hispanic" = "pcthisp",
                                                "Percent White" = "pctwhite"),
                                    selected = "pctblack"),
                     
                        selectInput("neighborhood", "NYC Neighborhood:",
                                    choices = nyc_data$NTAName,
                                    selected = "Pelham Bay-Country Club-City Island"),
                        
                        helpText("Data source: NYC Neighborhood Data"),
                        br(),
                        h3("About This App"),
                        p("This application provides a visual representation of demographic and health data across New York City's neighborhoods."),
                      ),
                      mainPanel(
                        fluidRow(leafletOutput("map"),
                                 br(),
                        fluidRow(plotlyOutput("racialDemoChart"))
                        ))
                      )
                    )


# Define server logic for the first tab
tab1_server <- function(input, output, session) {
  # Map output for Racial Demographics
  output$map <- renderLeaflet({
    map_data <- st_transform(nyc_data, crs = 4326)
    
    valid_data <- map_data[!is.na(map_data[[input$color]]), ]
    pal <- colorQuantile("PuBuGn", valid_data[[input$color]], n = 5)
    
    leaflet(valid_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(valid_data[[input$color]]),
        fillOpacity = 0.7, weight = 0.8, color = "white",
        popup = ~paste(NTAName, "<br>",
                       paste(input$color, ":", round(valid_data[[input$color]], 2), "%"))
      ) %>%
      setView(lng = -73.935242, lat = 40.730610, zoom = 10)
  })
  
  # Racial Demographics chart
  output$racialDemoChart <- renderPlotly({
    chart_data <- nyc_data[nyc_data$NTAName == input$neighborhood, ]
    
    # Lose the spatial component for the chart data
    racial_data <- st_drop_geometry(chart_data)
    
    # Subselect to only variables we're interested in
    racial_data <- racial_data[, c("pctblack", "pcthisp", "pctwhite", "pctapi", "pctother")]
    
    racial_data <- t(racial_data)
    racial_data <- as.data.frame(racial_data)
    racial_data <- cbind(Race = rownames(racial_data), Percentage = racial_data[, 1])
    rownames(racial_data) <- NULL
    
    plot_ly(data = as.data.frame(racial_data), x = ~Race, y = ~Percentage, type = 'bar', color = ~Race) %>%
      layout(title = paste("Self-Identified Race -", input$neighborhood),
             xaxis = list(title = "Race"),
             yaxis = list(title = "Percentage", range = c(0, 100), tickvals = seq(0, 100, 20)))
  })
}

# Define UI for the second tab
tab2_ui <- tabPanel("Socioeconomic Demographics",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("color_socio", "Demographic variable:",
                                    choices = c("Percent in Poverty" = "pctpov",
                                                "Rent < 30% of Income" = "rent.30",
                                                "Rent < 50% of Income" = "rent.50"),
                                    selected = "pctpov"),
                        selectInput("neighborhood_socio", "Select Neighborhood:",
                                    choices = nyc_data$NTAName,
                                    selected = "Pelham Bay-Country Club-City Island"),
                        helpText("Data source: NYC Neighborhood Data"),
                        br(),
                        h3("About This App"),
                        p("This application provides a visual representation of demographic and health data across New York City's neighborhoods."),
                      ),
                      mainPanel(
                        fluidRow(leafletOutput("map_socio"),
                                 br(),
                        fluidRow(plotlyOutput("socioDemoChart"))
                        )
                      )
                    )
)

# Define server logic for the second tab
tab2_server <- function(input, output, session) {
  # Map output for Socioeconomic Demographics
  output$map_socio <- renderLeaflet({
    map_data <- st_transform(nyc_data, crs = 4326)
    
    valid_data <- map_data[!is.na(map_data[[input$color_socio]]), ]
    pal <- colorQuantile("PuBuGn", valid_data[[input$color_socio]], n = 5)
    
    leaflet(valid_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(valid_data[[input$color_socio]]),
        fillOpacity = 0.7, weight = 1, color = "white",
        popup = ~paste(NTAName, "<br>",
                       paste(input$color_socio, ":", round(valid_data[[input$color_socio]], 2), "%"))
      ) %>%
      setView(lng = -73.935242, lat = 40.730610, zoom = 10)
  })
  
  # Socioeconomic Demographics chart
  output$socioDemoChart <- renderPlotly({
    chart_data <- nyc_data[nyc_data$NTAName == input$neighborhood_socio, ]
    
    # Extract data and remove "geometry" column
    socio_data <- as.data.frame(chart_data)
    socio_data <- socio_data[, -which(names(socio_data) == "geometry")]
    
    socio_data <- socio_data[, c("pctpov", "rent.30", "rent.50")]
    
    socio_data <- t(socio_data)
    socio_data <- as.data.frame(socio_data)
    socio_data <- cbind(Category = rownames(socio_data), Percentage = socio_data[, 1])
    rownames(socio_data) <- NULL
    
    plot_ly(data = as.data.frame(socio_data), x = ~Category, y = ~Percentage, type = 'bar', color = ~Category) %>%
      layout(title = paste("Socioeconomic Demographics -", input$neighborhood_socio),
             xaxis = list(title = "Category"),
             yaxis = list(title = "Percentage", range = c(0, 100), tickvals = seq(0, 100, 20)))
  })
}

# Define UI for the third tab
tab3_ui <- tabPanel("Severe Maternal Morbidity & Preterm Birth Rates",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("color_health", "Health variable:",
                                    choices = c("Severe Maternal Morbidity Rate" = "smmrate",
                                                "Preterm Birth Rate" = "ptbrate"),
                                    selected = "smmrate"),
                        helpText("Data source: NYC Neighborhood Data"),
                        br(),
                        h3("About This App"),
                        p("This application provides a visual representation of demographic and health data across New York City's neighborhoods."),
                      ),
                      mainPanel(
                        fluidRow(leafletOutput("map_health"),
                                 br(),
                        fluidRow(plotlyOutput("healthScatterChart"))
                        )
                      )
                    )
)

# Define server logic for the third tab
tab3_server <- function(input, output, session) {
  # Map output for Health Demographics
  output$map_health <- renderLeaflet({
    map_data <- st_transform(nyc_data, crs = 4326)
    
    valid_data <- map_data[!is.na(map_data[[input$color_health]]), ]
    pal <- colorQuantile("PuBuGn", valid_data[[input$color_health]], n = 5)
    
    leaflet(valid_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(valid_data[[input$color_health]]),
        fillOpacity = 0.7, weight = 1, color = "white",
        popup = ~paste(NTAName, "<br>",
                       paste(input$color_health, ":", round(valid_data[[input$color_health]], 2)))
      ) %>%
      setView(lng = -73.935242, lat = 40.730610, zoom = 10)
  })
  
  # Health Demographics scatter plot
  output$healthScatterChart <- renderPlotly({
    plot_ly(nyc_data, x = ~smmrate, y = ~ptbrate, text = ~NTAName, mode = 'markers') %>%
      layout(title = "Severe Maternal Morbidity vs Preterm Birth Rates",
             xaxis = list(title = "Severe Maternal Morbidity Rate"),
             yaxis = list(title = "Preterm Birth Rate", range = c(0, 15)))
  })
}



# Define UI for the fourth tab
tab4_ui <- tabPanel("About",
                    h3("About This App"),
                    p("This application provides a visual representation of demographic and health data across New York City's neighborhoods. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Nisi porta lorem mollis aliquam ut. Dui id ornare arcu odio ut sem nulla pharetra diam. Velit dignissim sodales ut eu sem integer vitae justo eget. Diam donec adipiscing tristique risus nec feugiat in fermentum. Velit euismod in pellentesque massa placerat duis ultricies lacus sed. Eget dolor morbi non arcu risus. Eget mauris pharetra et ultrices neque ornare aenean. Sed vulputate odio ut enim blandit volutpat maecenas volutpat. Laoreet non curabitur gravida arcu ac tortor dignissim. Blandit massa enim nec dui. Adipiscing bibendum est ultricies integer quis. Sagittis vitae et leo duis ut. Id interdum velit laoreet id."),
                    p("Select different demographic and health variables from the dropdown menus to explore the data. Nulla pellentesque dignissim enim sit amet venenatis urna cursus. Eget gravida cum sociis natoque penatibus. Feugiat sed lectus vestibulum mattis ullamcorper velit sed ullamcorper morbi. Mi ipsum faucibus vitae aliquet nec ullamcorper sit. Sit amet consectetur adipiscing elit. Urna id volutpat lacus laoreet non curabitur gravida arcu ac. Aliquet sagittis id consectetur purus ut faucibus pulvinar elementum. Nibh sit amet commodo nulla. Porta lorem mollis aliquam ut porttitor leo a. Arcu odio ut sem nulla. Orci a scelerisque purus semper eget duis at. Mi sit amet mauris commodo quis imperdiet massa. Sem et tortor consequat id. Consectetur libero id faucibus nisl. Pellentesque habitant morbi tristique senectus et netus et malesuada fames. Turpis nunc eget lorem dolor sed. Lobortis scelerisque fermentum dui faucibus in ornare quam viverra orci. Lacus laoreet non curabitur gravida arcu ac tortor dignissim."),
                    p("Data source: NYC Neighborhood Data, provided in GeoJSON format and visualized using Leaflet and Plotly in a Shiny application.")
)

# Define UI for the app
ui <- fluidPage(
  titlePanel("NYC Neighborhood Demographics"),
  tabsetPanel(
    tab1_ui,
    tab2_ui,
    tab3_ui,
    tab4_ui
  )
)

# Combine server logic for all tabs
server <- function(input, output, session) {
  tab1_server(input, output, session)
  tab2_server(input, output, session)
  tab3_server(input, output, session)
}

# Run the application
shinyApp(ui = ui, server = server)
