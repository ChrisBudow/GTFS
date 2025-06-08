library(tidyverse)
library(leaflet)

# Workspace
print(getwd())

## IMPORT
#agency, calendar, calendar_date, routes, stop_times, stop, trips
filelist_wanted <- c("agency", "calendar", "calendar_dates", "routes", "stop_times", "stops", "trips")
filelist <- list.files(path = "import/", pattern = ".*.txt")
filelist_wanted <- paste(filelist_wanted, ".txt", sep = "")
file <- filelist[filelist %in% filelist_wanted]
file <- paste("import/", file, sep = "")

data <- lapply(file, FUN = read_csv)

data[[6]]


## MAP
#basemap
basemap <- leaflet() %>%
  addTiles() %>%
  setView(
    lng = mean(data[[6]]$stop_lon),
    lat = mean(data[[6]]$stop_lat),
    zoom = 5
  ) %>%
  addCircleMarkers(
    lng = data[[6]]$stop_lon,
    lat = data[[6]]$stop_lat,
    color = "black"
  )
basemap
