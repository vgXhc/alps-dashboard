#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(httr2)
library(sf)


library(tidyverse)

# function to get current weather and return a data frame
get_weather <- function(location) {
  # geocode location for coordinates etc.
  coords <- openmeteo::geocode(location)
  # build request for httr2
  req <- request("https://api.open-meteo.com/v1/forecast")
  resp <- req %>% 
    req_url_path("v1") %>% 
    req_url_path_append("forecast") %>% 
    req_url_query(latitude = coords$latitude,
                  longitude = coords$longitude,
                  current = "temperature_2m,precipitation,cloud_cover,wind_speed_10m,wind_direction_10m") %>% 
    req_perform() %>% 
    resp_body_json()
  weather <- data.frame(location = location,
             elevation = resp$elevation,
             current_temp = resp$current$temperature_2m,
             precipitation = resp$current$precipitation,
             cloud_cover = resp$current$cloud_cover,
             wind_speed = resp$current$wind_speed_10m,
             wind_direction = resp$current$wind_direction_10m)
  weather %>% mutate(
    color = case_when(current_temp <= 5 ~ "blue",
                      current_temp > 5 & current_temp <= 15 ~ "light-blue",
                      current_temp > 15 & current_temp <= 22 ~"green",
                      current_temp > 22 & current_temp <= 28 ~ "orange",
                      current_temp > 28 ~ "red"),
    temp_f = round(current_temp * 1.8 +32, 0),
    temperature_formatted = paste0(current_temp, "°C (", temp_f, "F)"),
    wind_formatted = paste0(wind_speed, " km/h from ", wind_direction),
    icon = case_when(cloud_cover > 50 & precipitation > 0 ~ "cloud-rain",
                     cloud_cover > 50 & precipitation == 0 ~ "cloud",
                     cloud_cover <= 50 & cloud_cover > 25 & precipitation > 0 ~ "cloud-sun-rain",
                     cloud_cover <= 50 & cloud_cover > 25 & precipitation == 0 ~ "cloud-sun",
                     cloud_cover <= 25 & precipitation == 0 ~ "sun",
                     TRUE ~ ""),
    location_formatted = paste0(location, " (", elevation, "m)")
    
  )
}

locations <- c("Welzheim",
"Singen",
"Appenzell",
"Kunkelspass",
"Thusis",
"Albulapass",
"Berninapass",
"Tirano",
"Tonale",
"Bolzano",
"Passo Sella",
"Kranjska Gora",
"Sonnenalpe Nassfeld"
)

# function to get coordinates for all locations
get_coords <- function(location) {
  openmeteo::geocode(location)
}
coords <- map_dfr(locations, get_coords)

# turn into sf object
loc_sf <- st_as_sf(coords, coords = c("longitude", "latitude"))

#load track as sf object
track <- st_read("track.gpx", layer = "tracks")




create_value_box <- function(location) {
  x <- get_weather(location)
  valueBox(value = x$temperature_formatted, subtitle = x$location_formatted, icon = icon(x$icon), color = x$color)
}




# UI
ui <- dashboardPage(
  dashboardHeader(title = "Alps 2024"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Current conditions", tabName = "current", icon = icon("clock")),
      menuItem("Forecast", tabName = "forecast", icon = icon("plus"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "current",
              fluidRow(p("Data shown for ", lubridate::round_date(lubridate::now(tzone = "Europe/Zurich"), unit = "minutes"), "(European Central Summer Time)")),
             
    fluidRow(
      valueBoxOutput("Welzheim"),
      valueBoxOutput("Singen"),
      valueBoxOutput("Appenzell"),
      valueBoxOutput("Kunkelspass"),
      valueBoxOutput("Thusis"),
      valueBoxOutput("Albulapass"),
      valueBoxOutput("Berninapass"),
      valueBoxOutput("Tirano"),
      valueBoxOutput("TonalePass"),
      valueBoxOutput("Bolzano"),
      valueBoxOutput("PassoSella"),
      valueBoxOutput("KranjskaGora"),
      valueBoxOutput("Nassfeldpass")
    ),
    fluidRow(
      box(
        title = "Map of weather stations",
        width = 12,
        leaflet(loc_sf) %>% 
          addTiles() %>% 
          addMarkers(popup = loc_sf$name) %>% 
          addPolylines(data = track)
    )
    )
  ),
tabItem(tabName = "forecast",
fluidRow(valueBoxOutput("test"))
)
)
)
)

# Server
server <- function(input, output) {
  # Value boxes
  output$Welzheim <- renderValueBox({
    create_value_box("Welzheim")
  })
  output$Singen <- renderValueBox({
    create_value_box("Singen")
  })
  output$Appenzell <- renderValueBox({
    create_value_box("Appenzell")
  })
  output$Kunkelspass <- renderValueBox({
    create_value_box("Kunkelspass")
  })
  output$Thusis <- renderValueBox({
    create_value_box("Thusis")
  })
  output$Albulapass <- renderValueBox({
    create_value_box("Albulapass")
  })
  output$Berninapass <- renderValueBox({
    create_value_box("Berninapass")
  })
  output$Tirano <- renderValueBox({
    create_value_box("Tirano")
  })
  output$TonalePass <- renderValueBox({
    create_value_box("Tonale")
  })
  output$Bolzano <- renderValueBox({
    create_value_box("Bolzano")
  })
  output$PassoSella <- renderValueBox({
    create_value_box("Passo Sella")
  })
  output$KranjskaGora <- renderValueBox({
    create_value_box("Kranjska Gora")
  })
  output$Nassfeldpass <- renderValueBox({
    create_value_box("Sonnenalpe Nassfeld")
  })
  output$test <- renderValueBox({
    valueBox(value = 0, subtitle = "under development")
  })
  # Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -98.583, lat = 39.833, zoom = 4) # Center on USA
  })
}

# Run the app
shinyApp(ui = ui, server = server)

# ben <- st_read("https://share.garmin.com/Feed/Share/RC4JD")
# 
# leaflet(ben[2,]) %>%
#   addTiles() %>%
#   addPolylines()
# # ben
# # 
# library(xml2)
# download.file("https://share.garmin.com/Feed/Share/RC4JD", destfile = "tracking.kml")
# tracking <- read_xml("tracking.kml")
# # xml_find_all(tracking, "//Coordinates", xml_ns("d1"))
# 
# tracking <- read_xml("tracking.kml")
# 
# read_xml("tracking.kml")
# 
# read_kml <- function(file_path) {
#   # Read the KML file
#   kml_doc <- read_xml(file_path)
# 
#   # Extract all placemarks
#   placemarks <- xml_find_all(kml_doc, "//d1:Placemark", ns = xml_ns(kml_doc))
# 
#   # Function to extract data from a single placemark
#   extract_placemark_data <- function(placemark) {
#     name <- xml_text(xml_find_first(placemark, ".//d1:name", ns = xml_ns(kml_doc)))
#     coords <- xml_text(xml_find_first(placemark, ".//d1:coordinates", ns = xml_ns(kml_doc)))
#     coords_split <- strsplit(coords, ",")[[1]]
# 
#     list(
#       name = name,
#       longitude = as.numeric(coords_split[1]),
#       latitude = as.numeric(coords_split[2]),
#       altitude = as.numeric(coords_split[3])
#     )
#   }
# 
#   # Apply the extraction function to all placemarks
#   placemark_data <- lapply(placemarks, extract_placemark_data)
# 
#   # Convert the list of placemarks to a data frame
#   result_df <- do.call(rbind, lapply(placemark_data, data.frame))
# 
#   return(result_df)
# }
# 
# # Usage example
# kml_file_path <- "tracking.kml"
# kml_data <- read_kml(kml_file_path)
# 
# xml_ns(tracking)
# # View the first few rows of the resulting data frame
# print(head(kml_data))
