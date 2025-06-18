library(shiny)
library(tidyverse)
library(ggplot2)
library(sf)
library(leaflet)
library(plotly)
library(ggthemes)
library(gganimate)

#DATA PREP

## IMPORT
#agency, calendar, calendar_date, routes, stop_times, stop, trips
filelist_wanted <- c("agency", "calendar_dates", "routes", "shapes", "stop_times", "stops", "trips")
filelist <- list.files(path = "import/GO", pattern = ".*.txt")
filelist_wanted <- paste(filelist_wanted, ".txt", sep = "")
file <- filelist[filelist %in% filelist_wanted] %>%
  sort()
file <- paste("import/GO/", file, sep = "")

data <- lapply(file, FUN = read_csv)

calendar_dates <- data[[2]]
routes <- data[[3]]
shapes <- data[[4]]
stop_times <- data[[5]]
stops <- data[[6]]
trips <- data[[7]]

#line map
df_lines <- data[[4]] %>%
  arrange(shape_id, shape_pt_sequence) %>%
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat")) %>% 
  group_by(shape_id) %>% 
  summarize(do_union = FALSE) %>%
  st_cast("MULTILINESTRING") %>% 
  st_set_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>% 
  st_transform(crs="+proj=longlat +datum=WGS84")

#big DF
df <- data[[5]] %>%
  left_join(
    data[[6]] %>%
      mutate(
        id = ifelse(is.na(parent_station), stop_id, parent_station)) %>%
      select(stop_id, stop_lat, stop_lon, stop_name)
  ) %>%
  arrange(trip_id, stop_sequence) %>%
  left_join(data[[7]] %>%
              select(trip_id, route_id, service_id, shape_id, direction_id)
  ) %>%
  left_join(data[[3]] %>%
              select(route_id, route_long_name, route_type, route_color)
  ) %>%
  left_join(df_lines) %>%
  select(-c(pickup_type, drop_off_type, stop_headsign, stop_id, departure_time, route_id, service_id, shape_id))

# animated
test <- df %>%
  filter(trip_id == paste(c(as.vector(slice_sample(df %>% select(trip_id), n = 10)))$trip_id, sep = ", ")) %>%
  ggplot() + 
    labs(
    title = "Time: {arrival_time}",
    x = "Lon",
    y = "Lat"
  ) + 
  transition_time(arrival_time) +
  geom_sf(
    data = df %>% select(geometry) %>% distinct(),
    aes(geometry = geometry)
  ) +
  geom_point(
    aes(
      x = stop_lon,
      y = stop_lat 
    ),
    color = "red"
  )
test

## SHINY

ui <- fluidPage(
  leafletOutput(outputId = "map")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    map 
    
  })
}

shinyApp(ui, server)
