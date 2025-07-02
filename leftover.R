#leftoversnippets

shp_to_df <- function(shp) {
  df_shp <- shp %>%
    select(object_id, bike_route_, street_name, segment_len, geometry)
  
  df_shp$geometry <- df_shp %>%
    st_sample(exact = TRUE,
              size = round(df_shp$segment_len/100))
  df_shp_t_p <- st_cast(df_shp, "POINT")
  
  crs_dd <- 4326
  df_h <- get_elev_point(df_shp_t_p,
                         prj = crs_dd,
                         src = "aws",
                         z = 12)
  
  df_h <- df_h %>%
    mutate(length = cumsum(segment_len))
  
  df <- df_h %>%
    mutate(
      lon = st_coordinates(.)[,1],
      lat = st_coordinates(.)[,2]
    )
  return (df)
}