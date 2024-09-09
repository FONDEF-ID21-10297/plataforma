# global ------------------------------------------------------------------
# Rhino / shinyApp entrypoint. Do not edit.
# rhino::app()
library(shiny)
library(shinyWidgets)
library(bslib)

library(tidyverse)
library(leaflet)
library(terra)
library(sf)
library(fs)

# helpers -----------------------------------------------------------------
fecha_a_temporada <- function(x =  as.Date(c("2022-05-15", "2022-07-01", "2022-01-01"))){
  
  # si es ene-jun la temporada es la anterior
  temp1 <-  year(x) + if_else(month(x) <= 6, -1, 0)
  temp2 <- as.numeric(format(x, "%y")) + if_else(month(x) <= 6, -1, 0)
  temporada <- str_glue("{temp1}-{temp2+1}")
  temporada
  
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
  
  saveRDS(potencial, str_glue("data/raster_rds/{huerto}.rds"))
  
  cli::cli_inform("Variación temporal del potencial: {huerto_gpk}")
  
  #variación temporal del potencial en los sectores de riego
  data <- terra::extract(potencial, huerto_sf, fun = mean)
  data <- data |> 
    as_tibble() |> 
    pivot_longer(-ID, names_to = "fecha", values_to = "potencial") |>
    mutate(fecha = ymd(fecha)) |> 
    rename(id = ID)
  
  data <- data |> 
    mutate(temporada = fecha_a_temporada(fecha))
  
  saveRDS(data, fout)
  
})

# theme -------------------------------------------------------------------
colors_app <- c("#d01407", "#31683f", "#064aac")
theme_app <- bs_theme(
  # primary ="#31683f",
  fg = "#454545",
  bg = "white",
  base_font = font_google("Roboto"),
  # `navbar-light-contrast` =
  # "navbar-bg" = "green",                    # works
  # "navbar-brand-color" = "red !important",  # does not work!!!
  # "navbar-brand-hover-color" = "salmon !important", # does not work!!!
  # "navbar-active-color" = "gray !important", # does not work!!!
  # "nav-text-color" = "blue !important",
  "nav-link-color" = "#31683f",   # works
  # "nav-link-hover-color" = "orange !important", # works
  ) |>
  bs_add_rules(
    # "body { background-color: $body-bg; }"
    # sass::sass_file("www/mdb.min.css")
    sass::as_sass(readLines("www/mdb.min.css"))
    ) |> 
  bs_add_variables(
    "--bs-primary" = "#31683f"
  )

# sidebar -----------------------------------------------------------------
opts_huertos <- huertos_gpks |>
  basename() |> 
  tools::file_path_sans_ext()

opts_huertos_names <- opts_huertos |> 
  str_replace_all("_", " ") |> 
  str_to_title()

opts_huertos <- set_names(opts_huertos, opts_huertos_names)

opts_temporada <- read_rds(str_glue("data/potencial_dataframe/{opts_huertos[1]}.rds")) |> 
  distinct(temporada) |> 
  pull()

opts_fecha <- read_rds(str_glue("data/potencial_dataframe/{opts_huertos[1]}.rds")) |> 
  filter(temporada == max(temporada)) |> 
  distinct(fecha) |> 
  pull()

sidebar_app <- sidebar(
  selectizeInput("huerto", label = "Huerto", choices = opts_huertos),
  selectizeInput("temporada", label = "Temporada", choices = opts_temporada, selected = max(opts_temporada)),
  sliderTextInput("fecha", "Fecha", choices = opts_fecha, selected = c(tail(opts_fecha, 4 * 7)[1], tail(opts_fecha, 1))),
  radioGroupButtons(
    inputId = "layer", label = "Layer", choices = c("Shape" = "shape", "Raster" = "raster"),
    justified = TRUE,
    size = "sm"
    )
  )

# ui ----------------------------------------------------------------------
x <- value_box(
  title = "Contenido",
  value = "123",
  showcase = bsicons::bs_icon("globe-americas"),
  # theme_color = "light"
)

ui <- bslib::page_navbar(
  title =  tags$img(src = "logo1.png", height = "30px", style = "margin-top: -5px"),
  window_title = "SATORI",
  theme = theme_app,
  sidebar = sidebar_app,
  nav_panel(
    title = "Panel",
    tags$head(
      tags$link(href = "favicon.png", rel = "icon"),
      # tags$script(src = "https://www.googletagmanager.com/gtag/js?id=G-CYG993XQRT", async = ""),
      # tags$script(src = "js/ga.js"),
      # includeCSS("www/css/styles.css"),
    ),
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
  ),
  nav_panel(
    title = "Acerca de la Aplicación",
  )
)


# server ------------------------------------------------------------------
# input <- list(huerto = "la_esperanza", temporada = "2022-23", fecha = c("2024-04-15", "2024-04-28"))

server <- function(input, output, session) {

  data_list <- reactive({

    # datas
    data_sf <- read_sf(str_glue("data/vectorial/{input$huerto}.gpkg"), layer = 'sectores_riego') |>
      st_transform(32719)
    
    data_potencial <- read_rds(str_glue("data/potencial_dataframe/{input$huerto}.rds"))
    
    data_raster <- read_rds(str_glue("data/raster_rds/{input$huerto}.rds")) |> 
      terra::unwrap()
    
    # previo a enviar datos actualizar select: temporada y fecha 
    
    
    # 
    data_list <- list(sf = data_sf, pt = data_potencial, rt = data_raster)
    data_list
  }) |> 
    # data cambia con HUERTO
    bindEvent(input$huerto)
  
  output$mapa <- renderLeaflet({

    data_list <- data_list()
    
    data_sf <- data_list$sf
    data_pt <- data_list$pt
    data_rt <- data_list$rt
    
    data_sf_map <- left_join(
      data_sf,
      data_pt |>filter(fecha == input$fecha[2]),
      by = join_by(id)
    ) |> 
      st_transform("+init=epsg:4326")
    
    data_rt_map <- data_rt[[which(names(data_rt) == input$fecha[2])]]
    
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(data_rt_map), na.color = "transparent")
    
    
    m <- leaflet() |> addTiles() |>
      clearGroup(group = "layer") |>
      clearControls() |> # remueve la Legenda
      addLegend(pal = pal, values = values(data_rt_map), title = "Potencial") |> 
      # addLegend(
      #   position  = "topright",
      #   na.label  = "No disponible",
      #   pal       = pal,
      #   values    = colorData,
      #   # labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE)),
      #   layerId   = "colorLegend",
      #   title     = dparvar |>
      #     filter(variable == input$variable) |>
      #     str_glue_data("{desc} {ifelse(is.na(unidad), '', str_c('(',unidad, ')'))}") |>
      #     str_c(str_glue("<br/>{fmt_fecha(fmax)}"))
      # ) |> 
      identity()
    
    if(isolate(input$layer == "shape")) {
      m <- m |> 
        leaflet::addPolygons(
          group = "layer",
          data = data_sf_map,
          fillColor        = ~ pal(potencial),
          weight           = .5,
          dashArray        = "3",
          stroke           = NULL,
          fillOpacity      = 0.7,
          layerId          = ~ id,
          # popup            = popp,
          # label            = lb,
          # highlightOptions = highlightOptions(
          #   color        = "white",
          #   weight       = 2,
          #   fillColor    = parametros$color,
          #   bringToFront = TRUE
          # ),
          # labelOptions = labelOptions(
          #   style = list(
          #     "font-family"  = parametros$font_family,
          #     "box-shadow"   = "2px 2px rgba(0,0,0,0.15)",
          #     "font-size"    = "15px",
          #     "padding"      = "15px",
          #     "border-color" = "rgba(0,0,0,0.15)"
          #   )
          # )
        ) |>
        identity()
    } else {
      m <- m |> 
        addRasterImage(data_rt_map, colors = pal, opacity = 0.8, group = "layer") |> 
        identity()
    }
    
    m

  })
   
  observeEvent(c(input$fecha, input$layer), {

    print(input$fecha)

    cli::cli_inform(str_glue("observeEvent inpt$fecha: {input$fecha[2]}"))

    data_list <- data_list()
    
    data_sf <- data_list$sf
    data_pt <- data_list$pt
    data_rt <- data_list$rt
    
    data_sf_map <- left_join(
      data_sf,
      data_pt |>filter(fecha == input$fecha[2]),
      by = join_by(id)
      ) |> 
      st_transform("+init=epsg:4326")
    
    data_rt_map <- data_rt[[which(names(data_rt) == input$fecha[2])]]
    
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(data_rt_map), na.color = "transparent")
    
    
    m <- leafletProxy("mapa") |>
      # leaflet() |> addTiles() |>
      clearGroup(group = "layer") |>
      clearControls() |> # remueve la Legenda
      addLegend(pal = pal, values = values(data_rt_map), title = "Potencial") |> 
    # addLegend(
    #   position  = "topright",
    #   na.label  = "No disponible",
    #   pal       = pal,
    #   values    = colorData,
    #   # labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE)),
    #   layerId   = "colorLegend",
    #   title     = dparvar |>
    #     filter(variable == input$variable) |>
    #     str_glue_data("{desc} {ifelse(is.na(unidad), '', str_c('(',unidad, ')'))}") |>
    #     str_c(str_glue("<br/>{fmt_fecha(fmax)}"))
    # ) |> 
    identity()
    
    if(input$layer == "shape") {
      m <- m |> 
        leaflet::addPolygons(
          group = "layer",
          data = data_sf_map,
          fillColor        = ~ pal(potencial),
          weight           = .5,
          dashArray        = "3",
          stroke           = NULL,
          fillOpacity      = 0.7,
          layerId          = ~ id,
          # popup            = popp,
          # label            = lb,
          # highlightOptions = highlightOptions(
          #   color        = "white",
          #   weight       = 2,
          #   fillColor    = parametros$color,
          #   bringToFront = TRUE
          # ),
          # labelOptions = labelOptions(
          #   style = list(
          #     "font-family"  = parametros$font_family,
          #     "box-shadow"   = "2px 2px rgba(0,0,0,0.15)",
          #     "font-size"    = "15px",
          #     "padding"      = "15px",
          #     "border-color" = "rgba(0,0,0,0.15)"
          #   )
          # )
        ) |>
        identity()
    } else {
      m <- m |> 
        addRasterImage(data_rt_map, colors = pal, opacity = 0.8, group = "layer") |> 
        identity()
    }
    
    m
    
  })
  
}

shinyApp(ui, server)