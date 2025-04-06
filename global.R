# packages ----------------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(bslib)
library(tidyverse)
library(leaflet)
library(terra)
library(sf)
library(fs)
library(highcharter)
library(bsicons)

options(bslib.color_contrast_warnings = FALSE)

# helpers -----------------------------------------------------------------
fecha_a_temporada <- function(x =  as.Date(c("2022-05-15", "2022-07-01", "2022-01-01"))){
  
  # si es ene-jun la temporada es la anterior
  temp1 <-  year(x) + if_else(month(x) <= 6, -1, 0)
  temp2 <- as.numeric(format(x, "%y")) + if_else(month(x) <= 6, -1, 0)
  temporada <- str_glue("{temp1}-{temp2+1}")
  temporada
  
}

equipo_sector_a_sector_equipo <- function(x = c("a_b", "c_d")){
  x |> 
    str_split("_") |> 
    map_chr(function(l){
      str_c(l[2], "_", l[1])
    })
}

sector_equipo_a_lbl <- function(x = c("5_3", "4_5")){

  x |> 
    str_split("_") |> 
    map_chr(function(l){
      str_glue("Sector {l[1]} - Equipo {l[2]}")
    })

}

# data --------------------------------------------------------------------
huertos_gpks <- fs::dir_ls("data/vectorial/", regexp = ".gpkg")
huertos_gpks

# options -----------------------------------------------------------------
newlang_opts <- getOption("highcharter.lang")

newlang_opts$weekdays <- c("domingo", "lunes", "martes", "miércoles", "jueves", "viernes", "sábado")
newlang_opts$months <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
                         "agosto", "septiembre", "octubre", "noviembre", "diciembre")
newlang_opts$shortMonths <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep",
                              "oct", "nov", "dic")

newlang_opts$loading      <- "Cargando información"
newlang_opts$downloadCSV  <- "Descargar CSV"
newlang_opts$downloadJPEG <- "Descargar JPEG"
newlang_opts$downloadPDF  <- "Descargar PDF"
newlang_opts$downloadPNG  <- "Descargar PNG"
newlang_opts$downloadSVG  <- "Descargar SVG"
newlang_opts$downloadXLS  <- "Descargar XLS"
newlang_opts$printChart   <- "Imprimir gráfico"
newlang_opts$viewFullscreen <- "Ver pantalla completa"
newlang_opts$resetZoom    <- "Resetear zoom"

newlang_opts$thousandsSep <- "."
newlang_opts$decimalPoint <- ","

options(
  highcharter.lang = newlang_opts,
  highcharter.theme = hc_theme_smpl(
    # color = parametros$color,
    chart = list(style = list(fontFamily = "Inria Sans")),
    plotOptions = list(
      series = list(marker = list(symbol = "circle")),
      line = list(marker = list(symbol = "circle")),
      area = list(marker = list(symbol = "circle"))
    )
  )
)

# theme -------------------------------------------------------------------
colors_app <- c("#d01407", "#31683f", "#064aac")

colors_lvl <- c("#8CD47E", "#F8D66D", "#FF6961")

theme_app <- bs_theme(
  primary ="#3457D5",
  fg = "#454545",
  bg = "white",
  base_font = font_google("Inria Sans")
) |>
  identity()

lbl_opts_style <- list(
  "font-family"  = "Inria Sans",
  "box-shadow"   = "2px 2px rgba(0,0,0,0.15)",
  "font-size"    = "10px",
  "padding"      = "10px",
  "z-index" = 10000,
  "border-color" = "rgba(0,0,0,0.15)"
)

highlight_opts <- highlightOptions(
  color        = "white",
  weight       = 5,
  bringToFront = TRUE
  )

# sidebar -----------------------------------------------------------------
opts_huertos <- huertos_gpks |>
  basename() |> 
  tools::file_path_sans_ext()

opts_huertos_names <- opts_huertos |> 
  str_replace_all("_", " ") |> 
  str_to_title()

opts_huertos <- set_names(opts_huertos, opts_huertos_names)

opts_temporada <- read_csv("https://raw.githubusercontent.com/FONDEF-ID21-10297/datos_plataforma_gha/refs/heads/main/data/potencial-raster/dates.csv", show_col_types = FALSE) |>
  mutate(temporada = fecha_a_temporada(date)) |> 
  distinct(temporada) |> 
  pull()

opts_fecha <- read_csv("https://raw.githubusercontent.com/FONDEF-ID21-10297/datos_plataforma_gha/refs/heads/main/data/potencial-raster/dates.csv", show_col_types = FALSE) |>
  # filter(temporada == max(temporada)) |> 
  distinct(date) |> 
  pull()

sidebar_app <- sidebar(
  id = "mainsidebar",
  tags$strong("Panel de control"),
  selectizeInput("huerto", label = "Huerto", choices = opts_huertos),
  selectizeInput("temporada", label = "Temporada", choices = opts_temporada, selected = max(opts_temporada)),
  dateInput("fecha", label = "Fecha", value = tail(opts_fecha, 1)),
  actionButton("hoy", "Hoy", class = "btn-sm btn-primary", style = "font-size:80%", width = "30%"),
  radioGroupButtons(
    inputId = "layer",
    label = "Mapa",
    choices = c("Estrés" = "nivel", "Potencial" = "shape", "Raster" = "raster"),
    justified = TRUE,
    size = "sm"
  )
)
