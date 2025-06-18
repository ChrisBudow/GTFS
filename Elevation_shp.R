library(elevatr)
library(tidyverse)
library(sf)
library(leaflet)
library(geosphere)

getwd()
setwd(getwd())

shp <- read_sf(dsn = "import/Translink/bikeways/")
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

point1 <- c(1, 2)
point2 <- c(4, 6)
a <- rbind(point1, point2)
distance <- dist(a)

dist(df$geometry)

leaflet(df_shp_t_p)

plot(df_shp_t_p)

leaflet(df_h) %>%
  addTiles() %>%
  addPolylines(df_h)



b0 = st_polygon(list(rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))))
b1 = b0 + 2
b2 = b0 + c(-0.2, 2)
x = st_sfc(b0, b1, b2)
st_area(x)
line = st_sfc(st_linestring(rbind(c(30,30), c(40,40))), crs = 4326)
st_length(df_shp_t)
