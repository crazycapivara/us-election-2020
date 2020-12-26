library(magrittr)
library(sf)
library(leaflet)

us_states <- readRDS("data/us-states.rds")

us_states %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(
    weight = 1,
    color = "white",
    fillColor = "blue",
    popup = ~ sprintf("%s (%i)", state_name, number_of_votes)
  )
