library(elevatr)
library(tidyverse)
library(sf)
library(leaflet)
library(geosphere)
library(shiny)
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

# Elevation Profile
map_ep <- function(t) {
  l <- t %>%
    ggplot(
      aes(x = length,
          y = elevation)
    ) +
    stat_smooth(
      se = FALSE
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank()
    ) + 
    labs(
      x = "Length",
      y = "Elevation",
      title = "Elevation Profile"
    ) 
  return(l)
}

#shp <- Read_Shapefile()
#shp <- read_sf(dsn = "import/Translink/bikeways/")
#shp_t <- shp %>%
#  filter(object_id == "2581")
#df1 <- shp_to_df(shp_t)
#df <- df_shp(df1)

## Plots
#t <- map_l(shp_t)
#t

#l <- map_ep(df)
#l
#ggplotly(l)

## SHINY
#https://stackoverflow.com/questions/71024105/why-wont-renderdt-return-a-table-on-my-shiny-app
#https://www.paulamoraga.com/book-geospatial/sec-shinyexample.html
#https://stackoverflow.com/questions/64911003/r-shiny-how-to-select-input-form-data-frame-column-reactive

ui <- fluidPage(
  titlePanel(
    p("Titel",
      style = "color:#3474A7")
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "filemap",
                label = "Upload map. Choose shapefile",
                multiple = TRUE,
                accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
      uiOutput("column_ui"),
      uiOutput("value_ui"),
      actionButton("plot_btn", "Generate Plot")
      ),
  mainPanel(
    leafletOutput("leamap"),
    plotOutput("eleplot")
  )
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100*1024^2)
  
  map <- reactive({
    req(input$filemap)
    shpdf <- input$filemap
    tempdirname <- dirname(shpdf$datapath[1])
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }
    map <- read_sf(paste(tempdirname,
                         shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                         sep = "/"))
  })
  
  output$column_ui <- renderUI({
    req(map())
    selectInput("column", "Select Column",
                choices = names(map()))
  })
  
  output$value_ui <- renderUI({
    req(input$column)
    selectInput("value", "Select Value(s)", 
                choices = map()[[input$column]]
    )
  })
  
  plot_data <- eventReactive(input$plot_btn, {
    req(input$value)
    df <- map()
    df[df[[input$column]] %in% input$value, ] %>%
      shp_to_df()
  })
  
  output$leamap <- renderLeaflet({
    req(plot_data())
    map_l(plot_data())
  })
  
  plot_data1 <- eventReactive(input$plot_btn, {
    req(plot_data)
    df <- plot_data()
    df %>%
      df_shp()
  })
  
  output$eleplot <- renderPlot({
    req(plot_data1())
    print(map_ep(plot_data1()))
  })
}

shinyApp(ui, server)
