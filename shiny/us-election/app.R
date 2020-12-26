library(shiny)
library(leaflet)

us_states <- readRDS("data/us-states.rds")

ui <- fluidPage(
    titlePanel("US Election"),
    sidebarLayout(
        sidebarPanel(
            selectInput("state", "State", choices = us_states$state_name)
        ),
        mainPanel(
           leafletOutput("map")
        )
    )
)

server <- function(input, output, session) {
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addPolygons(
                data = us_states,
                weight = 1,
                color = "white",
                fillColor = "blue"
                , layerId = ~ state_name
                #, popup = ~ sprintf("%s (%i)", state_name, number_of_votes)
            )
    })
    
    observeEvent(input$map_shape_click, {
        print(input$map_shape_click)
        updateSelectInput(session, "state", selected = input$map_shape_click$id)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
