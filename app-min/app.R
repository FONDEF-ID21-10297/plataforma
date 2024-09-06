library(shiny)
library(bslib)

library(tidyverse)
library(leaflet)
library(terra)
library(sf)
library(fs)


# st_layers('data/vectorial/rio_claro.gpkg')
# st_layers('data/vectorial/la_esperanza.gpkg')

sf_le <- read_sf(here::here('data/vectorial/rio_claro.gpkg'), layer = 'sectores_rio_claro') |>
  st_transform(32719)

sf_le
sf_le |> count(equipo)
sf_le |> count(sector)


# plot(sf_le$geom)
# plot(sf_le)

files <- dir_ls(here::here('data/potencial_predict'), regexp = 'rio_claro.*tif$') 

potencial <- rast(files)

#variación temporal del potencial en los sectores de riego
data <- terra::extract(potencial, sf_le, fun = mean)

datal <- data |>
  pivot_longer(-ID) |>
  mutate(date = ymd(name))

ggplot(datal, aes(date, value, color = ID)) +
  geom_point(size = .1) +
  geom_line(lwd = .1) +
  theme_bw()

r <- potencial[[1]]
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r), na.color = "transparent")

leaflet()  |>
  addTiles() |>
  addRasterImage(r, colors = pal, opacity = 0.8) |>
  addLegend(pal = pal,
            values = values(r),
            title = "Potencial")



# theme -------------------------------------------------------------------
theme_app <- bs_theme()

# sidebar -----------------------------------------------------------------
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
