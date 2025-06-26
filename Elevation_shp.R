library(elevatr)
library(tidyverse)
library(sf)
library(leaflet)
library(geosphere)
library(shiny)
library(plotly)
library(collapse)

getwd()
setwd(getwd())

## FUNCTIon

# Read-in shapefile function
#Read_Shapefile <- function(shp_path) {
#  shp_path <- "import/Translink/shp"
#  infiles <- shp_path$datapath # get the location of files
#  dir <- unique(dirname(infiles)) # get the directory
#  outfiles <- file.path(dir, shp_path$name) # create new path name
#  name <- strsplit(shp_path$name[1], "\\.")[[1]][1] # strip name 
#  purrr::walk2(infiles, outfiles, ~file.rename(.x, .y)) # rename files
#  x <- read_sf(file.path(dir, paste0(name, ".shp"))) # read-in shapefile
#  return(x)
#}

# shp to tibble
shp_to_df <- function(shp) {
  df_shp <- shp %>%
    select(object_id, street_name, segment_len, geometry)
  
  df_shp$geometry <- st_sample(df_shp, size = 100)
  df_shp_t_p <- st_cast(df_shp, "POINT")
  
  crs_dd <- 4326
  df_h <- get_elev_point(df_shp_t_p, prj = crs_dd, src = "aws")
  
  df_h <- df_h %>%
    mutate(length = cumsum(segment_len))
  
  df <- df_h %>%
    mutate(
      lon = st_coordinates(.)[,1],
      lat = st_coordinates(.)[,2]
    )
  return (df)
}

# tibble to df
df_shp <- function(shpdf) {
  df <- shpdf %>%
    st_drop_geometry()
  return(df)
}

# leaflet map
map_l <- function(t) {
  map <- t %>%
    leaflet() %>%
    addTiles() %>%
    addPolylines(
      opacity = 1,
      weight = 2.5
    )
  return(map)
}

#shp <- Read_Shapefile()
shp <- read_sf(dsn = "import/Translink/bikeways/")
shp_t <- shp %>%
  filter(object_id == "2581")
df1 <- shp_to_df(shp_t)
df <- df_shp(df1)

## Plots

t <- shp_t %>%
  leaflet() %>%
  addTiles() %>%
  addPolylines(
    opacity = 1,
    weight = 2.5
  )
t <- map_l(shp_t)
t

l <- df %>%
  ggplot(
    aes(x = length,
        y = elevation)
  ) +
  geom_line() +
  stat_smooth() +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank()
  )
l
ggplotly(l)

## SHINY
#https://stackoverflow.com/questions/71024105/why-wont-renderdt-return-a-table-on-my-shiny-app

ui <- fluidPage(
    tableOutput("t") 
)

server <- function(input, output, session) {
  output$t <- renderTable(df)
}

shinyApp(ui, server)
