# Load required libraries
library(sf)
library(httr)
library(leaflet)
library(shiny)

# Set access key for Geoapify API
access_key <- ***access key***

# Read GeoJSON data
geojson <- st_read("impb_geometry.geojson", crs = 2193, quiet = TRUE)

# Define UI
ui <- fluidPage(
  titlePanel("IMPB Lookup"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("addressInput", "Enter address:", "2 Park Road, Grafton"),
      actionButton("lookupButton", "Submit"),
      htmlOutput("areaInfo"),
      div(
        id = "infoBox",
        style = "padding: 20px; background-color: white; border: 2px solid #ccc; border-radius: 10px; margin-top: 20px;",
        HTML(paste0(
          "Data Source: ", tags$a(href = "https://github.com/minhealthnz/iwi-maori-partnership-boards/tree/main/data", "Ministry of Health"), "<br> <br>",
          "<div style='font-size: 14px;'>",  # Adjust the font size here
          "App Developer: Courtney Russ", "<br>",
          tags$a(href = "https://github.com/courtneyruss", "GitHub"), " | ",
          tags$a(href = "https://www.termsfeed.com/live/eb324f86-2dfa-4aaa-998c-ad7aa17fddbc", "Disclaimer"), " | ",
          tags$a(href = "https://www.datascienceportfol.io/CourtneyRuss", "Contact"),
          "</div><br>",
          "<p xmlns:cc='http://creativecommons.org/ns#' xmlns:dct='http://purl.org/dc/terms/'>",
          "<span property='dct:title'>IMPB Address Mapping App</span> by ",
          "<a rel='cc:attributionURL dct:creator' property='cc:attributionName' href='https://github.com/courtneyruss'>Courtney Russ</a> is marked with ",
          "<a href='http://creativecommons.org/publicdomain/zero/1.0?ref=chooser-v1' target='_blank' rel='license noopener noreferrer' style='display:inline-block;'>",
          "CC0 1.0<img style='height:22px!important;margin-left:3px;vertical-align:text-bottom;' src='https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1'>",
          "<img style='height:22px!important;margin-left:3px;vertical-align:text-bottom;' src='https://mirrors.creativecommons.org/presskit/icons/zero.svg?ref=chooser-v1'></a></p>"
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
  result <- reactiveVal(list(data = list(latitude = -36.85921, longitude = 174.7706),
                             api_response = NULL))
  
  observeEvent(input$lookupButton, {
    # Make the API request to Geoapify for address lookup
    tryCatch({
      response <- httr::GET(
        url = "https://api.geoapify.com/v1/geocode/search",
        query = list(
          text = input$addressInput,
          apiKey = access_key
          # add more optional parameters if needed
        )
      )
      
      if (http_type(response) == "application/json") {
        result_content <- httr::content(response, "parsed")
        
        if (!is.null(result_content$features) && length(result_content$features) > 0 &&
            !is.null(result_content$features[[1]]$geometry) && 
            !is.null(result_content$features[[1]]$geometry$coordinates)) {
          result(list(data = list(
            latitude = as.numeric(result_content$features[[1]]$geometry$coordinates[2]),
            longitude = as.numeric(result_content$features[[1]]$geometry$coordinates[1])
          ),
          api_response = result_content$features))
        } else {
          result(list(data = list(latitude = -36.85921, longitude = 174.7706),
                      api_response = NULL))
        }
      } else {
        result(list(data = list(latitude = -36.85921, longitude = 174.7706),
                    api_response = NULL))
      }
    }, error = function(e) {
      # Handle error gracefully
      cat("Error in API request:", e$message, "\n")
      result(list(data = list(latitude = -36.85921, longitude = 174.7706),
                  api_response = NULL))
    })
  })
  
  output$map <- renderLeaflet({
    if (!is.null(result())) {
      lat <- result()$data$latitude
      lon <- result()$data$longitude
      
      # Set the center and zoom level for New Zealand
      nz_center <- c(-40.9006, 174.8860)
      zoom_level <- 5  
      
      leaflet() %>%
        addProviderTiles("OpenStreetMap.Mapnik") %>%
        setView(lng = nz_center[2], lat = nz_center[1], zoom = zoom_level) %>%
        addMarkers(lng = lon, lat = lat, popup = "Selected Location")
    }
  })
  
  output$areaInfo <- renderText({
    if (!is.null(result())) {
      point_sf <- st_sfc(st_point(c(result()$data$longitude, result()$data$latitude)), crs = 4326)
      intersection <- st_intersection(geojson, st_transform(point_sf, st_crs(geojson)))
      
      if (!is.null(intersection) && nrow(intersection) > 0) {
        area_name <- na.omit(intersection$impb)
        if (length(area_name) > 0) {
          paste("The IMPB for the entered address is:", unique(area_name))
        } else {
          "IMPB not found. Try including the suburb or city."
        }
      } else {
        "IMPB not found. Try including the suburb or city."
      }
    } else {
      "IMPB not found. Try including the suburb or city."
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
