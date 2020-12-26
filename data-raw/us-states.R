library(magrittr)
library(dplyr)

url_us_states <- "https://docs.mapbox.com/mapbox-gl-js/assets/us_states.geojson"
us_states <- sf::st_read(url_us_states)
names(us_states) <- tolower(names(us_states))

tibble::tibble(
  id = us_states$state_id,
  name = us_states$state_name
) %>%
  write.csv2("data-raw/us-states.csv", row.names = FALSE, quote = FALSE)

us_states %<>%
  inner_join(read.csv2("data-raw/us-states-postal-code.csv")) %>%
  inner_join(read.csv2("data-raw/us-states-number-of-votes.csv")) %>%
  select(- abbreviation)

saveRDS(us_states, "data/us-states.rds")
