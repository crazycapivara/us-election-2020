library(shiny)
library(dplyr)
library(leaflet)

us_states <- readRDS("data/us-states.rds") %>%
    mutate(biden = 0L, trump = 0L)

.appv <- reactiveValues(us_states = us_states)

ui <- fluidPage(
    titlePanel("US Election"),
    sidebarLayout(
        sidebarPanel(
            selectInput("state", "State", choices = us_states$state_name)
            , textInput("biden", "Biden", value = 0)
            , textInput("trump", "Trump", value = 0)
            , actionButton("update", "Update")
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
    
    observeEvent(input$state, {
        x <- .appv$us_states %>%
            filter(state_name == input$state)
        updateTextInput(session, "biden", value = x$biden)
        updateTextInput(session, "trump", value = x$trump)
    }) 
    
    observeEvent(input$map_shape_click, {
        # print(input$map_shape_click)
        state <- input$map_shape_click$id
        #x <- .appv$us_states %>%
        #    filter(state_name == state)
        #print(x)
        updateSelectInput(session, "state", selected = state)
        #updateTextInput(session, "biden", value = x$biden)
        #updateTextInput(session, "trump", value = x$trump)
        print(.appv$us_states)
    })
    
    observeEvent(input$update, {
        biden <- as.integer(input$biden)
        trump <- as.integer(input$trump)
        state <- input$state
        print(biden)
        print(trump)
        print(state)
        .appv$us_states[.appv$us_states$state_name == state, c("biden", "trump")] <- c(biden, trump)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
