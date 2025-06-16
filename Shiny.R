library(shiny)
library(tidyverse)
library(ggplot2)
library(sf)
library(leaflet)

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

#df_sf <- st_as_sf(data[[4]], coords = c("shape_pt_lon", "shape_pt_lat"), crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
#df_sf1 <- st_simplify(df_sf, preserveTopology = TRUE, dTolerance =  100)
#t1 <- st_multilinestring()

#df_sf_t <- st_simplify(df_sf_t, preserveTopology = FALSE, dTolerance =  1000)
#leaflet(df_sf_t %>% filter(shape_id == "395104")) %>%
#  addTiles() %>%
#  addPolylines() %>%
#  addMarkers(
#    lng = t$shape_pt_lon,
#    lat = t$shape_pt_lat,
#    color = "black",
#    label = t$shape_pt_sequence,
#    fillOpacity = 1,
#    stroke = FALSE
#  )
# plot(df_sf_t)

df <- data[[5]] %>%
  left_join(
    data[[6]] %>%
      mutate(id = ifelse(is.na(parent_station), stop_id, parent_station)) %>%
      select(stop_id, stop_lat, stop_lon, stop_name)
  ) %>%
  arrange(trip_id, stop_sequence) %>%
  left_join(data[[7]] %>%
              select(trip_id, route_id, service_id, shape_id, direction_id)
  ) %>%
  #full_join(data[[4]]) %>%
  #            select(shape_id, route_short_name, agency_id, route_type)
  #) %>%
  left_join(data[[3]] %>%
              select(route_id, route_long_name, route_type, route_color)
  ) %>%
  select(-c(pickup_type, drop_off_type, stop_headsign, stop_id))

#stop map
df_stop <- df %>%
  select(stop_lat, stop_lon, stop_name)

#line map
df_j <- df %>% select(stop_name, shape_id, stop_lon, stop_lat)
test <- data[[4]] %>%
  left_join(df_j,
            by = c("shape_id" = "shape_id",
                   "shape_pt_lat" = "stop_lat",
                   "shape_pt_lon" = "stop_lon"))
leaflet() %>%
  addTiles() %>%
  addMarkers(
    lat = test$shape_pt_lat,
    lng = test$shape_pt_lon,
    label = test$stop_name
  )

df_shp <- data[[4]] %>%
  left_join(df %>%
              select(stop_name, shape_id)
  ) %>%
  #na.omit() %>% 
  arrange(shape_id, shape_pt_sequence) %>%
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat")) %>% 
  group_by(shape_id) %>% 
  summarize(do_union = FALSE) %>%
  st_cast("MULTILINESTRING") %>% 
  st_set_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>% 
  st_transform(crs="+proj=longlat +datum=WGS84")

map <- leaflet() %>%
  addTiles() %>%
  addMarkers(
    data = df_stop,
    lng = df_stop$stop_lon,
    lat = df_stop$stop_lat,
    label = df_stop$stop_name
  )

ui <- fluidPage(
  leafletOutput(outputId = "map")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    map 
    
  })
}

shinyApp(ui, server)
