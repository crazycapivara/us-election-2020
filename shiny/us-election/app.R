library(shiny)
library(leaflet)

us_states <- readRDS("data/us-states.rds")

ui <- fluidPage(
    titlePanel("US Election"),
    sidebarLayout(
        sidebarPanel(
        ),
        mainPanel(
           leafletOutput("map")
        )
    )
)

server <- function(input, output) {
    output$map <- renderLeaflet({#
        leaflet() %>%
            addTiles() %>%
            addPolygons(
                data = us_states,
                weight = 1,
                color = "white",
                fillColor = "blue",
                popup = ~ sprintf("%s (%i)", state_name, number_of_votes)
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
