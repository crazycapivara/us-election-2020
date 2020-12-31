library(shiny)
library(dplyr)
library(tibble)
library(leaflet)
library(ggplot2)

LAYER_ID <- "us-states"

us_states <- readRDS("data/us-states.rds") %>%
    mutate(biden = 0, trump = 0, color = "grey")

.appv <- reactiveValues(
    us_states = us_states
    , votes = tibble(Biden = 0L, Trump = 0L)
)

ui <- fluidPage(
    titlePanel("US Election"),
    sidebarLayout(
        sidebarPanel(
            selectInput("state", "State", choices = us_states$state_name)
            #, sliderInput("test", "test", 0, 100, 0, step = 0.1)
            , textInput("biden", "% Biden", value = 0)
            , textInput("trump", "% Trump", value = 0)
            , actionButton("update", "Update")
            #, plotOutput("barplot")
        ),
        mainPanel(
           leafletOutput("map")
           , tableOutput("votes")
           , tableOutput("overview")
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
                fillColor = ~ color
                , layerId = ~ state_name
                #, popup = ~ sprintf("%s (%i)", state_name, number_of_votes)
            ) %>%
            setView(lng = -97.56935, lat = 40.58058, zoom = 4)
    })
    
    observeEvent(input$state, {
        x <- .appv$us_states %>%
            filter(state_name == input$state)
        updateTextInput(session, "biden", value = x$biden)
        updateTextInput(session, "trump", value = x$trump)
    }) 
    
    observeEvent(input$map_shape_click, {
        print(input$map_shape_click)
        state <- input$map_shape_click$id
        updateSelectInput(session, "state", selected = state)
        print(.appv$us_states)
    })
    
    observeEvent(input$update, {
        biden <- as.numeric(input$biden)
        trump <- as.numeric(input$trump)
        state <- input$state
        #print(biden)
        #print(trump)
        #print(state)
        .appv$us_states[.appv$us_states$state_name == state, c("biden", "trump")] <- c(biden, trump)
        .appv$votes$Biden <- filter(.appv$us_states, biden > trump)$number_of_votes %>% sum()
        .appv$votes$Trump <- filter(.appv$us_states, trump > biden)$number_of_votes %>% sum()
        
        .x <- .appv$us_states
        idx_biden <- .x$biden > .x$trump
        #print(idx_biden)
        if (any(idx_biden)) .appv$us_states[idx_biden, ]$color <- "blue"
        
        idx_trump <- .x$biden < .x$trump
        #print(idx_trump)
        if (any(idx_trump)) .appv$us_states[idx_trump, ]$color <- "red"
        
        idx_equal <- .x$biden == .x$trump
        if (any(idx_equal)) .appv$us_states[idx_equal, ]$color <- "true"
        
        leafletProxy("map", data = .appv$us_states) %>%
            clearShapes() %>%
            addPolygons(
                weight = 1,
                color = "white",
                fillColor = ~ color
                , layerId = ~ state_name
            )
    })
    
    #output$barplot <- renderPlot({
    #    votes <- c(sum(.appv$us_states$biden), sum(.appv$us_states$trump))
    #    x <- tibble(name = c("biden", "trump"), votes = votes)
    #    ggplot(data = x, aes(x = name, y = votes)) +
    #        geom_bar(stat = "identity", fill = c("blue", "red")) +
    #        theme_minimal()
    #})
    
    output$votes <- renderTable({
        .appv$votes
    })
    
    output$overview <- renderTable({
        x <- .appv$us_states
        #x$geometry <- NULL
        tibble(
            State = x$state_name,
            Votes = x$number_of_votes,
            `% Biden` = x$biden,
            `% Trump` = x$trump
        )
        #x
    })
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
