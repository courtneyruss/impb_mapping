# Load required libraries
library(shiny)
library(sf)
library(httr)
library(leaflet)
library(keyring)

# Set access key for positionstack API
access_key <- key_get(service = "positionstack", username = "api")

# Read GeoJSON data
geojson <- st_read("impb_geometry.geojson", crs = 2193, quiet = TRUE)

# Default location
default_latitude <- -36.85921
default_longitude <- 174.7706
default_address <- "2 Park Road, Grafton, Auckland"

# Info Box
source_text <- "Data Source: "
source_link <- "https://github.com/minhealthnz/iwi-maori-partnership-boards/tree/main/data"
author_text <- "App Developer: Courtney Russ"
github_link <- "https://github.com/courtneyruss/impb_mapping"
site_link <- "https://www.datascienceportfol.io/CourtneyRuss"

# Disclaimer link
disclaimer_link <- "https://www.termsfeed.com/live/eb324f86-2dfa-4aaa-998c-ad7aa17fddbc"

license_text <- '<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/"><span property="dct:title">IMPB Address Mapping App</span> by <a rel="cc:attributionURL dct:creator" property="cc:attributionName" href="https://github.com/courtneyruss">Courtney Russ</a> is marked with <a href="http://creativecommons.org/publicdomain/zero/1.0?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC0 1.0<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1"><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/zero.svg?ref=chooser-v1"></a></p>'

# Define UI
ui <- fluidPage(
  titlePanel("IMPB Lookup"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("addressInput", "Enter address:", default_address),
      actionButton("lookupButton", "Submit"),
      div(
        id = "infoBox",
        style = "padding: 20px; background-color: white; border: 2px solid #ccc; border-radius: 10px; margin-top: 20px;",
        HTML(paste0(
          source_text, tags$a(href = source_link, "Ministry of Health"), "<br> <br>",
          "<div style='font-size: 14px;'>",  # Adjust the font size here
          author_text, "<br>",
          tags$a(href = github_link, "GitHub"), " | ",
          tags$a(href = disclaimer_link, "Disclaimer"), " | ",
          tags$a(href = site_link, "Contact"),
          "</div><br>",
          license_text
        ))
      )
    ),
    mainPanel(
      leafletOutput("map", height = "600px"),
      htmlOutput("areaName")
    )
  )
)

# Define server logic
server <- function(input, output) {
  result <- reactiveVal(NULL)  
  
  # Initial display 
  observe({
    result(list(data = list(latitude = default_latitude, longitude = default_longitude)))
  })
  
  observeEvent(input$lookupButton, {
    # Check if the button has been clicked
    if (input$lookupButton > 0) {
      # Make the API request to positionstack for address lookup
      response <- httr::GET("http://api.positionstack.com/v1/forward",
                            query = list(access_key = access_key, query = input$addressInput))
      
      if (http_type(response) == "application/json") {
        result_content <- httr::content(response, "parsed")
        
        if (!is.null(result_content$data) && length(result_content$data) > 0 &&
            !is.null(result_content$data[[1]]$latitude) && !is.null(result_content$data[[1]]$longitude)) {
          # Update the result reactive value
          result(list(data = list(latitude = as.numeric(result_content$data[[1]]$latitude),
                                  longitude = as.numeric(result_content$data[[1]]$longitude)),
                      api_response = result_content$data))  # Save API response in result
        } else {
          # Set result to NULL if no valid data found
          result(NULL)
        }
      } else {
        # Set result to NULL in case of an API error
        result(NULL)
      }
    }
  })
  
  output$map <- renderLeaflet({
    if (!is.null(result())) {
      # Extract latitude and longitude values
      lat <- result()$data$latitude
      lon <- result()$data$longitude
      
      leaflet() %>%
        addProviderTiles("OpenStreetMap.Mapnik") %>%
        setView(lng = 172, lat = -42, zoom = 5) %>% 
        addMarkers(lng = lon, lat = lat, popup = "Selected Location")
    }
  })
  
  output$areaName <- renderUI({
    if (!is.null(result())) {
      # Update the area name
      point_sf <- st_sfc(st_point(c(result()$data$longitude, result()$data$latitude)), crs = 4326)
      intersection <- st_intersection(geojson, st_transform(point_sf, st_crs(geojson)))
      
      if (!is.null(intersection) && nrow(intersection) > 0) {
        area_name <- na.omit(intersection$impb)
        if (length(area_name) > 0) {
          HTML(paste("<br><div style='font-size: 18px;'>The IMPB for the entered address is <span>", unique(area_name), "</span></div>"))
        } else {
          HTML("<div style='font-size: 18px;'>IMPB not found. Try including the suburb or city.</div>")
        }
      } else {
        HTML("<div style='font-size: 18px;'>IMPB not found. Try including the suburb or city.</div>")
      }
    } else {
      HTML("<div style='font-size: 18px;'>IMPB not found. Try including the suburb or city.</div>")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
