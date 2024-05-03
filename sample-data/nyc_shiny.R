# Load necessary libraries
library(shiny)
library(shinythemes)
library(leaflet)
library(sf)
library(plotly)
library(tidyverse)

# Load and prepare the data
nyc_data <- st_read("NYC_nbrhd_data.geojson", quiet = TRUE)
nyc_data <- st_make_valid(nyc_data)
nyc_data <- st_transform(nyc_data, crs = 4326)
nyc_data_df <- st_drop_geometry(nyc_data)

# Define UI for the first tab
tab1_ui <- tabPanel("Self-Identified Race & Ethnicty",
                    sidebarLayout(
                      sidebarPanel(
                        p("Select different variables from the dropdown menus to explore the data."),
                        selectInput("race", "Self-Identified Race & Ethnicity:",
                                    choices = c("Percent Asian & Pacific Islander" = "pctapi",
                                                "Percent Black" = "pctblack",
                                                "Percent Hispanic" = "pcthisp",
                                                "Percent White" = "pctwhite",
                                                "Percent Other Identified Race" = "pctother"),
                                    selected = "pctblack"),
                     
                        selectInput("neighborhood", "NYC Neighborhood:",
                                    choices = str_sort(nyc_data$NTAName),
                                    selected = "Pelham Bay-Country Club-City Island"),
                        
                        helpText("Data source: NYC Neighborhood Data"),
                        
                        br(),
                        
                        h3("Racial & Ethnic Disparities"),
                        
                        p("Extensive research has shown that racial and ethnic disparities in 
                        quality of care and use of services exist and persist in the United States.
                        Disparities may emerge from unequal access to health care, critical resources
                        such as health foods, housing, and transportation."),
                        br(),
                        p("Explore racial and ethnic population distributions by NYC neighborhood in this tab,
                        and then explore socioeconomic and health trends acrosos the rest of the applications.
                        Identify locations for further analysis."),
                        
                        helpText("Read More: The Commonwealth Fund 2024 State Health Disparities Report")
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
    
    valid_data <- nyc_data[!is.na(nyc_data[[input$race]]), ]
    pal <- colorQuantile("viridis", valid_data[[input$race]], n = 5)
    
    leaflet(valid_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(valid_data[[input$race]]),
        fillOpacity = 0.7, weight = 1, color = "white",
        popup = ~paste(NTAName, "<br>",
                       paste(input$race, ":", round(valid_data[[input$race]], 2), "%"))
      ) %>%
      
      addLegend(
        position = "bottomright",
        pal = pal,
        values = valid_data[[input$race]],
        title = "% of population"
      )  %>%
      
      setView(lng = -73.935242, lat = 40.730610, zoom = 10)
  })
  
  # Racial Demographics chart
  output$racialDemoChart <- renderPlotly({
    chart_data <- nyc_data[nyc_data$NTAName == input$neighborhood, ]
    
    # Extract data and remove "geometry" column
    racial_data <- st_drop_geometry(chart_data)
    
    racial_data <- racial_data[, c("pctblack", "pcthisp", "pctwhite", "pctapi", "pctother")]
    
    racial_data <- t(racial_data)
    racial_data <- as.data.frame(racial_data)
    racial_data <- cbind(Race = rownames(racial_data), Percentage = racial_data[, 1])
    rownames(racial_data) <- NULL
    
    plot_ly(data = as.data.frame(racial_data), x = ~Race, y = ~Percentage, type = 'bar', color = ~Race) %>%
      layout(title = paste("Racial Demographics -", input$neighborhood),
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
                                    choices = str_sort(nyc_data$NTAName),
                                    selected = "Pelham Bay-Country Club-City Island"),
                        
                        helpText("Data source: NYC Neighborhood Data"),
                        
                        br(),
                        
                        h3("Socioeconomic Disparities"),
                        
                        p("Nulla suscipit, purus ac varius sagittis, velit lorem condimentum ipsum, sit amet auctor sem tellus a leo. Aenean faucibus hendrerit diam non rutrum. Proin nec nisi dolor. Nam egestas dolor sapien, eget pellentesque neque tincidunt nec. Phasellus mattis pulvinar tincidunt. Phasellus eget condimentum nisl. Praesent dapibus dui elit, id fringilla quam interdum vel. Praesent vestibulum nulla et rutrum ornare. Donec cursus felis dui, et auctor nisi pulvinar ac. Suspendisse placerat ex sed arcu semper volutpat. Donec commodo consequat ornare. Aenean est lectus, semper at luctus sit amet, bibendum vitae augue. Donec risus felis, commodo eget tristique vitae, imperdiet in risus."),
                        
                        helpText("Read More: Include text here ")
                      ),
                      mainPanel(
                        fluidRow(leafletOutput("map_socio"),
                                 br(),
                        fluidRow(plotlyOutput("socioDemoChart"))
                        )
                      )
                    )
)

# Define server for the second tab
tab2_server <- function(input, output, session) {
  
  # Map output for Socioeconomic Demographics
  output$map_socio <- renderLeaflet({
    
    valid_data <- nyc_data[!is.na(nyc_data[[input$color_socio]]), ]
    pal <- colorQuantile("viridis", valid_data[[input$color_socio]], n = 5)
    
    leaflet(valid_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(valid_data[[input$color_socio]]),
        fillOpacity = 0.7, weight = 1, color = "white",
        popup = ~paste(NTAName, "<br>",
                       paste(input$color_socio, ":", round(valid_data[[input$color_socio]], 2), "%"))
      ) %>%
      
      addLegend(
        position = "bottomright",
        pal = pal,
        values = valid_data[[input$color_socio]],
        title = "% of population"
      )  %>%
      
      setView(lng = -73.935242, lat = 40.730610, zoom = 10)
  })
  
  # Socioeconomic Demographics chart
  output$socioDemoChart <- renderPlotly({
    chart_data <- nyc_data_df[nyc_data_df$NTAName == input$neighborhood_socio, ]
    
    # Extract data and remove "geometry" column
    socio_data <- st_drop_geometry(chart_data)
    
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
                        
                        h3("Maternal Health Outcomes"),
                        
                        p("Pellentesque nisl ipsum, bibendum non porttitor eget, lobortis sit amet arcu. Aliquam et erat nec nisi fermentum aliquet non a massa. Mauris vel sapien justo. Sed fermentum sed purus ut fringilla. Aliquam pulvinar, ligula ac ornare rutrum, est ipsum tristique metus, non imperdiet nibh ligula id elit. Proin ac dui in ligula finibus facilisis. Quisque at vulputate nulla, sit amet varius nunc. In eu cursus quam. In diam est, tristique sit amet nunc nec, vehicula hendrerit odio. Phasellus est turpis, vulputate eu suscipit sit amet, semper at enim. Vivamus sit amet risus leo. Vestibulum porttitor feugiat ipsum, ut volutpat erat pharetra quis. Suspendisse interdum ultrices nisi vel finibus. Aliquam lobortis sed arcu eget ornare."),
                        
                        helpText("Read More: Include text here ")
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
    
    valid_data <- nyc_data[!is.na(nyc_data[[input$color_health]]), ]
    pal <- colorBin("viridis", valid_data[[input$color_health]], pretty = FALSE, n = 5)
    
    leaflet(valid_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(valid_data[[input$color_health]]),
        fillOpacity = 0.7, weight = 1, color = "white",
        popup = ~paste(NTAName, "<br>",
                       paste(input$color_health, ":", round(valid_data[[input$color_health]], 2)))
      ) %>%
      
      addLegend(
        position = "bottomright",
        pal = pal,
        values = valid_data[[input$color_health]],
        title = "Rate per X,000 persons"
      )  %>%
      
      setView(lng = -73.935242, lat = 40.730610, zoom = 10)
  })
  
  # Health Demographics scatter plot
  
  output$healthScatterChart <- renderPlotly({
    
    plot_ly(nyc_data_df, x = ~smmrate, y = ~ptbrate, text = ~NTAName, type = 'scatter') %>%
      layout(title = "Severe Maternal Morbidity vs Preterm Birth Rates",
             xaxis = list(title = "Severe Maternal Morbidity Rate", zeroline = TRUE),
             yaxis = list(title = "Preterm Birth Rate", zeroline = TRUE))
  })
}


# Define UI for the fourth tab
tab4_ui <- tabPanel("About",
                    sidebarLayout( 
                      sidebarPanel(
                        
                        h3("Data"),
                        
                        p("Mauris vel sapien justo. Sed fermentum sed purus ut fringilla. Aliquam pulvinar, 
                          ligula ac ornare rutrum, est ipsum tristique metus, non imperdiet nibh ligula id 
                          elit."), 
                                                           br(),
                          
                        
                        h3("Methodology"),
                        
                        p("Proin ac dui in ligula finibus facilisis. Quisque at vulputate nulla, sit
                          amet varius nunc. In eu cursus quam. In diam est, tristique sit amet nunc nec, 
                          vehicula hendrerit odio. "),
                        
                        helpText("Read More: Include text here ")
                      ),
                      mainPanel(
                        fluidRow( 
                        
                          h2("Motivations & Background"),
                          p("Phasellus est turpis, vulputate eu suscipit sit amet, 
                          semper at enim. Vivamus sit amet risus leo. Vestibulum porttitor feugiat ipsum, 
                          ut volutpat erat pharetra quis. Suspendisse interdum ultrices nisi vel finibus. 
                          Aliquam lobortis sed arcu eget ornare."),
                                
                                br(),
                        
                          h2("Study Findings"),
                          p("Phasellus est turpis, vulputate eu suscipit sit amet, 
                            semper at enim. Vivamus sit amet risus leo. Vestibulum porttitor feugiat ipsum, 
                            ut volutpat erat pharetra quis. Suspendisse interdum ultrices nisi vel finibus. 
                            Aliquam lobortis sed arcu eget ornare."),
                        
                        br(),
                        
                        h2("Team"),
                        p("Phasellus est turpis, vulputate eu suscipit sit amet, 
                          semper at enim. Vivamus sit amet risus leo. Vestibulum porttitor feugiat ipsum, 
                          ut volutpat erat pharetra quis. Suspendisse interdum ultrices nisi vel finibus. 
                          Aliquam lobortis sed arcu eget ornare."),
                        
                        br(),
                        
                        h2("Questions? Contact Us."),
                        p("Email person@person.com for more information."),
                        
                        ))))

                    

# Define UI for the app
ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("NYC Neighborhood & Health Demographics"),
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
