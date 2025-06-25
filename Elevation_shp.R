library(elevatr)
library(tidyverse)
library(sf)
library(leaflet)
library(geosphere)
library(shiny)
library(plotly)

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
  
  df_shp_t <- df_shp %>%
    filter(object_id == "2581")
  
  df_shp_t$geometry <- st_sample(df_shp_t, size = 100)
  df_shp_t_p <- st_cast(df_shp_t, "POINT")
  
  crs_dd <- 4326
  df_h <- get_elev_point(df_shp_t_p, prj = crs_dd, src = "aws")
  
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
    st_drop_geometry() %>%
    as.data.frame()
  return(df)
}

#shp <- Read_Shapefile()
shp <- read_sf(dsn = "import/Translink/bikeways/")
df1 <- shp_to_df(shp)
df <- df_shp(df1)

up <- df %>%
  ggplot() +
  geom_sf(
    aes(geometry = geometry)
  )

down <- df %>%
  ggplot() +
  geom_line(
    aes(x = lon,
        y = elevation)
  )


## SHINY
#https://stackoverflow.com/questions/71024105/why-wont-renderdt-return-a-table-on-my-shiny-app

ui <- fluidPage(
    tableOutput("t") 
)

server <- function(input, output, session) {
  output$t <- renderTable(df)
}

shinyApp(ui, server)
