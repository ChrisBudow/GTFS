library(tidyverse)
library(leaflet)
library(htmlwidgets)

# Workspace
print(getwd())

## IMPORT
#agency, calendar, calendar_date, routes, stop_times, stop, trips
filelist_wanted <- c("agency", "calendar", "calendar_dates", "routes", "stop_times", "stops", "trips")
filelist <- list.files(path = "import/", pattern = ".*.txt")
filelist_wanted <- paste(filelist_wanted, ".txt", sep = "")
file <- filelist[filelist %in% filelist_wanted] %>%
  sort()
file <- paste("import/", file, sep = "")

data <- lapply(file, FUN = read_csv)

agency <- data[[1]]
calendar <- data[[2]]
calendar_dates <- data[[3]]
routes <- data[[4]]
stop_times <- data[[5]]
stops <- data[[6]]
trips <- data[[7]]

df <- data[[5]] %>%
  filter(trip_id == "538")

df <- data[[5]] %>%
  left_join(
    data[[6]] %>%
      mutate(id = ifelse(is.na(parent_station), stop_id, parent_station)) %>%
      select(stop_id, stop_lat, stop_lon, stop_name, id)
    ) %>%
  left_join(data[[7]] %>%
              select(trip_id, route_id, service_id)
            ) %>%
  left_join(data[[4]] %>%
              mutate(route_type = gsub("[0-9 ]", "", data[[4]]$route_short_name)) %>%
              select(route_id, route_short_name, agency_id, route_type)
            ) %>%
  left_join(data[[1]] %>%
              select(agency_id, agency_name)
            ) %>%
  left_join(data[[3]] %>%
              select(service_id, date)
            ) %>%
  select(-c(pickup_type, drop_off_type, agency_id, stop_id))

df %>% count(route_type)
sum(is.na(df$date))
length(df$date)
calendar_dates %>% count(date)

pal <- colorFactor(
  palette = c('red'),
  domain = test1$route_type
)

test <- head(df, 2)
test <- data[[6]] %>% 
  mutate(id = ifelse(is.na(parent_station), stop_id, parent_station))

df <- df %>%
  filter(trip_id == "538")
  
unique(gsub("[0-9 ]", "", data[[4]]$route_short_name))


#stop_id=service_id, stop_times

## MAP
#basemap
basemap <- leaflet() %>%
  addTiles() %>%
  setView(
    lng = mean(df$stop_lon),
    lat = mean(df$stop_lat),
    zoom = 8
  ) %>%
  addCircleMarkers(
    lng = df$stop_lon,
    lat = df$stop_lat,
    color = "black",
    label = df$stop_name,
    fillOpacity = 1,
    stroke = FALSE
  ) %>%
  addPolylines(
    lng  = df$stop_lon,
    lat = df$stop_lat,
    opacity = 1,
    label = paste(df$route_type, " ", df$trip_id, sep = ""),
    weight = 2,
    group = df$route_type
  ) %>%
  addLayersControl(
    overlayGroups = df$route_type,
    options = layersControlOptions(collapsed = FALSE)
  )
basemap


saveWidget(basemap, file = "/Users/budo20124/OneDrive - Hatch Ltd/Documents/map1.html")
