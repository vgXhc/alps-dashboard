#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(openmeteo)
library(purrr)
openmeteo::weather_now("Kunkelspass") |> View()
openmeteo::weather_forecast("Kunkelspass", 
                            daily = c("temperature_2m_max", 
                                      "temperature_2m_min",
                                      "precipitation_hours")) |> View()

get_current_weather <- function(location){
  weather_now(location = location)
}



locations <- c("Welzheim", 
               "Singen", 
               "Kunkelspass", 
               "Chur", 
               "Albulapass", 
               "Berninapass",
               "Pontresina",
               "Bolzano",
               "Passo Sella",
               "Cortina d'Ampezzo",
               "Lienz")

x <- map_dfr(locations, get_current_weather, .id = "location") |> View()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Weather for Harald and Ben's Alps 2024 trip"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
