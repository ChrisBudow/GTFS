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
  mutate(arrival_time = as.integer(arrival_time)) %>%
  mutate(testsec = as.integer(1:nrow(df))) %>%
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
  #full_join(data[[4]]) %>%
  #            select(shape_id, route_short_name, agency_id, route_type)
  #) %>%
  left_join(data[[3]] %>%
              select(route_id, route_long_name, route_type, route_color)
  ) %>%
  select(-c(pickup_type, drop_off_type, stop_headsign, stop_id))

#stop map
df_stop <- df %>%
  select(stop_lat, stop_lon, stop_name) %>%
  unique()
  
#line map
df_lines <- data[[4]] %>%
  arrange(shape_id, shape_pt_sequence) %>%
  st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat")) %>% 
  group_by(shape_id) %>% 
  summarize(do_union = FALSE) %>%
  st_cast("MULTILINESTRING") %>% 
  st_set_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>% 
  st_transform(crs="+proj=longlat +datum=WGS84")

# animated

test <- df %>%
  ggplot() +
  geom_point(
    data = df_stop,
    aes(
      x = stop_lon,
      y = stop_lat 
    ),
    color = "red"
  ) +
  geom_sf(
    data = df_lines
  ) +
  labs(
    title = "Time: {arrival_time}",
    x = "Lon",
    y = "Lat"
  ) + 
  transition_manual(testsec)
test

anim <- ggplot(airquality, aes(Day, Temp)) +
  geom_point(aes(colour = factor(Month))) +
  transition_time(Day)


length(df$arrival_time)
length(na.omit(df$arrival_time))

summary(df$arrival_time)
class(df$arrival_time)
class(c(22260, 86399))

library(gapminder)
library(gganimate)
library(dplyr)

a <- gapminder %>%
  ggplot(aes(gdpPercap, lifeExp, color = continent, group = country)) +
  geom_point() +
  transition_time(-year) +
  labs(title = "Year: {-frame_time}")

animate(a, nframes = 50, duration = 5)
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
