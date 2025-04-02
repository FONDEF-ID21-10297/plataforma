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

# walk(huertos_gpks, function(huerto_gpk = "data/vectorial/la_esperanza.gpkg"){
  
#   huerto <- tools::file_path_sans_ext(basename(huerto_gpk))
  
#   cli::cli_h2("Huerto {huerto}")
  
#   fout <- str_glue("data/potencial_dataframe/{huerto}.rds")
  
#   if(file.exists(fout)) return(TRUE)
  
#   cli::cli_inform("Leer gpk: {huerto_gpk}")
  
#   huerto_sf <- read_sf(huerto_gpk, layer = 'sectores_riego') |>
#     st_transform(32719) |>
#     mutate(id = row_number()) |> 
#     mutate(equipo_sector = coalesce(equipo_sector, "1_6")) |> 
#     mutate(sector_equipo = equipo_sector_a_sector_equipo(equipo_sector))
  
#   huerto_sf
  
#   plot(huerto_sf)
  
#   cli::cli_inform("Leer rasters: {huerto_gpk}")
  
#   tif_files <- dir_ls("data/potencial_predict_m/") |> 
#     str_subset(huerto)
  
#   potencial <- rast(tif_files)
  
#   saveRDS(potencial, str_glue("data/raster_rds/{huerto}.rds"))
  
#   cli::cli_inform("Variación temporal del potencial: {huerto_gpk}")
  
#   #variación temporal del potencial en los sectores de riego
#   data <- terra::extract(potencial, huerto_sf, fun = mean)
#   data <- data |> 
#     as_tibble() |> 
#     pivot_longer(-ID, names_to = "fecha", values_to = "potencial") |>
#     mutate(fecha = ymd(fecha)) |> 
#     rename(id = ID)
  
#   data <- data |> 
#     mutate(temporada = fecha_a_temporada(fecha))
  
#   saveRDS(data, fout)
  
# })

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
  # primary ="#31683f",
  fg = "#454545",
  bg = "white",
  base_font = font_google("Inria Sans"),
  # `navbar-light-contrast` =
  # "navbar-bg" = "green",                    # works
  # "navbar-brand-color" = "red !important",  # does not work!!!
  # "navbar-brand-hover-color" = "salmon !important", # does not work!!!
  # "navbar-active-color" = "gray !important", # does not work!!!
  # "nav-text-color" = "blue !important",
  "nav-link-color" = "#31683f",   # works
  # "nav-link-hover-color" = "orange !important", # works
) |>
  # bs_add_rules(
  #   # "body { background-color: $body-bg; }"
  #   # sass::sass_file("www/mdb.min.css")
  #   sass::as_sass(readLines("www/mdb.min.css"))
  #   ) |> 
  # bs_add_variables(
  #   "--bs-primary" = "#31683f"
  # ) |> 
  identity()

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
  # sliderTextInput("fecha", "Fecha", choices = opts_fecha, selected = c(tail(opts_fecha, 8 * 7)[1], tail(opts_fecha, 1))),
  dateInput("fecha", label = "Fecha", value = tail(opts_fecha, 1)),
  # bslib::input_task_button("hoy", "hoy"),
  actionButton("hoy", "Hoy", class = "btn-sm btn-primary", style = "font-size:80%", width = "30%"),
  radioGroupButtons(
    inputId = "layer", label = "Mapa", choices = c("Potencial" = "shape", "Estrés" = "raster"),
    justified = TRUE,
    size = "sm"
  )
)
