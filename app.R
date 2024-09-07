# global ------------------------------------------------------------------
# Rhino / shinyApp entrypoint. Do not edit.
# rhino::app()
library(shiny)
library(bslib)

library(tidyverse)
library(leaflet)
library(terra)
library(sf)
library(fs)

# helpers -----------------------------------------------------------------
fecha_a_temporada <- function(x =  as.Date(c("2022-05-15", "2022-07-01"))){
  
  # si es ene-jun la temporada es la anterior
  temp <- year(x) + if_else(month(x) <= 6, -1, 0)
  
}


# data --------------------------------------------------------------------
huertos_gpks <- fs::dir_ls("data/vectorial/")
huertos_gpks

walk(huertos_gpks, function(huerto_gpk = "data/vectorial/la_esperanza.gpkg"){
  
  huerto <- tools::file_path_sans_ext(basename(huerto_gpk))
  
  cli::cli_h2("Huerto {huerto}")
  
  fout <- str_glue("data/potencial_dataframe/{huerto}.rds")
  
  if(file.exists(fout)) return(TRUE)
  
  cli::cli_inform("Leer gpk: {huerto_gpk}")
  
  huerto_sf <- read_sf(huerto_gpk, layer = 'sectores_riego') |>
    st_transform(32719)
  
  cli::cli_inform("Leer rasters: {huerto_gpk}")
  
  tif_files <- dir_ls("data/potencial_predict/") |> 
    str_subset(huerto)
  
  potencial <- rast(tif_files)
  
  cli::cli_inform("Variación temporal del potencial: {huerto_gpk}")
  
  #variación temporal del potencial en los sectores de riego
  data <- terra::extract(potencial, huerto_sf, fun = mean)
  data <- data |> 
    as_tibble() |> 
    pivot_longer(-ID, names_to = "fecha", values_to = "potencial") |>
    mutate(fecha = ymd(fecha))
  
  data |> 
    mutate(month(fecha))
  data |> 
    mutate(temporada = if_else(month <))
  
  
  saveRDS(data, fout)
  
})

# theme -------------------------------------------------------------------
theme_app <- bs_theme()

# sidebar -----------------------------------------------------------------
opts_huertos <- huertos_gpks |>
  basename() |> 
  tools::file_path_sans_ext()

opts_huertos_names <- opts_huertos |> 
  str_replace_all("_", " ") |> 
  str_to_title()

opts_huertos <- set_names(opts_huertos, opts_huertos_names)

readRDS("data/potencial_dataframe/la_esperanza.rds") |> 
  ggplot(aes(fecha, potencial)) +
  geom_point()

opt_fechas <- datal |> 
  distinct(date) |> 
  arrange(date) |> 
  mutate(id = row_number(), .before = 1) |> 
  pull(date)

sidebar_app <- sidebar(
  # sector:
  # rio claro / esperanza
  selectizeInput("fecha", label = "Fecha", choices = opt_fechas, selected = max(opt_fechas))
  # opciones para mostrar unidades o no
  
)


# ui ----------------------------------------------------------------------
x <- value_box(
  title = "Contenido",
  value = "123",
  showcase = bsicons::bs_icon("globe-americas"),
  theme_color = "light"
)


ui <- bslib::page_navbar(
  title = "Plataforma",
  theme = theme_app,
  sidebar = sidebar_app,
  nav_panel(
    title = "",
    layout_columns(
      col_widths = c(8, 4),
      card(
        full_screen = TRUE,
        leafletOutput("mapa")
      ),
      layout_columns(
        col_widths = 12,
        card("grafico"),
        x,
        x,
        x
      )
    )
  )
)


# server ------------------------------------------------------------------
# input <- list(fecha = sample(opt_fechas, 1))

server <- function(input, output, session) {
  
  output$mapa <- renderLeaflet({
    
    # grafico ultimo valor
    r <- potencial[[length(opt_fechas)]]
    
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r), na.color = "transparent")
    
    leaflet()  |>
      addTiles() |>
      addRasterImage(r, colors = pal, opacity = 0.8, group = "raster") |>
      addLegend(pal = pal, values = values(r), title = "Potencial", group = "raster")
    
  })
  
  observeEvent(input$fecha, {
    
    print(input$fecha)
    
    cli::cli_inform(str_glue("observeEvent inpt$fecha: {input$fecha}"))
    
    idfecha <- which(opt_fechas == input$fecha)
    
    r <- potencial[[idfecha]]
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r), na.color = "transparent")
    
    leafletProxy("mapa") |>
      # leaflet() |> addTiles() |>
      clearGroup(group = "raster") |>
      clearControls() |> # remueve la Legenda
      addRasterImage(r, colors = pal, opacity = 0.8, group = "raster") |>
      addLegend(pal = pal, values = values(r), title = "Potencial") |>
      identity()
    
  })
  
}

shinyApp(ui, server)

# preguntas
# 1. cual será el flujo usual: elegir sector, fecha, 
# 1. aparte del map y grafico, que otra información se debe mostrar
# 1. 
# 1. cuantos sectores?
# 1.mapa siempre se muestra por raster y no por unidad/shape?
