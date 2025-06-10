library(tidyverse)
library(leaflet)

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

test <- data[[5]] %>%
  filter(trip_id == "538")

test1 <- test %>% left_join(data[[6]] %>% select(stop_id, stop_lat, stop_lon))
  
#stop_id=service_id, stop_times

## MAP
#basemap
basemap <- leaflet() %>%
  addTiles() %>%
  setView(
    lng = mean(test1$stop_lon),
    lat = mean(test1$stop_lat),
    zoom = 5
  ) %>%
  addCircleMarkers(
    lng = test1$stop_lon,
    lat = test1$stop_lat,
    color = "black",
    label = data[[6]]$stop_name
  )
basemap
