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
library(highcharter)

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

# data --------------------------------------------------------------------
huertos_gpks <- fs::dir_ls("data/vectorial/", regexp = ".gpkg")
huertos_gpks

walk(huertos_gpks, function(huerto_gpk = "data/vectorial/la_esperanza.gpkg"){
  
  huerto <- tools::file_path_sans_ext(basename(huerto_gpk))
  
  cli::cli_h2("Huerto {huerto}")
  
  fout <- str_glue("data/potencial_dataframe/{huerto}.rds")
  
  if(file.exists(fout)) return(TRUE)
  
  cli::cli_inform("Leer gpk: {huerto_gpk}")
  
  huerto_sf <- read_sf(huerto_gpk, layer = 'sectores_riego') |>
    st_transform(32719) |>
    mutate(id = row_number()) |> 
    mutate(equipo_sector = coalesce(equipo_sector, "1_6")) |> 
    mutate(sector_equipo = equipo_sector_a_sector_equipo(equipo_sector))
  
  huerto_sf
  
  plot(huerto_sf)
  
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
  id = "mainsidebar",
  tags$strong("Panel de control"),
  selectizeInput("huerto", label = "Huerto", choices = opts_huertos),
  selectizeInput("temporada", label = "Temporada", choices = opts_temporada, selected = max(opts_temporada)),
  # sliderTextInput("fecha", "Fecha", choices = opts_fecha, selected = c(tail(opts_fecha, 8 * 7)[1], tail(opts_fecha, 1))),
  dateInput("fecha", label = "Fecha", value = tail(opts_fecha, 1)),
  radioGroupButtons(
    inputId = "layer", label = "Mapa", choices = c("Potencial" = "shape", "Estrés" = "raster"),
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
      includeScript("www/custom.js"),
      # includeCSS("www/styles.css"),
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header(uiOutput("txt_mapa_huerto")),
        full_screen = TRUE,
        leafletOutput("mapa")
      ),
      layout_columns(
        col_widths = 12,
        card(card_header("Potencial por sectores de riego"), full_screen = TRUE, highchartOutput("potencial")),
        card(card_header("Datos meteorológicos del huerto"), full_screen = TRUE, highchartOutput("clima")
        )
      )
    )
  ),
  nav_panel(
    title = "Resumen Temporada",
    layout_columns(
      col_widths = 12,
      card(full_screen = TRUE, highchartOutput("potencial_temporada")),
      )
    )
)


# server ------------------------------------------------------------------
# input <- list(
#   huerto = "la_esperanza",
#   temporada = "2022-23",
#   # fecha = c("2024-04-15", "2024-04-28")
#   fecha = "2024-04-28"
# )

server <- function(input, output, session) {

  showModal(
    modalDialog(
      # title = tags$small("Bienvenido al"),
      tags$p("Bienvenido a ", tags$strong("SATORI")),
      tags$hr(),
      tags$p("SATORI es una herramienta que permite analizar el potencial de riego en huertos a lo largo de distintas temporadas."),
      tags$p("Selecciona un huerto, define el rango de fechas y accede a visualizaciones detalladas en un mapa interactivo y una línea temporal, ayudando a optimizar decisiones agrícolas con datos precisos y en tiempo real."),
      easyClose = TRUE,
      footer = NULL
    )
  )
  
  
  data_list <- reactive({

    # datas
    data_sf <- read_sf(str_glue("data/vectorial/{input$huerto}.gpkg"), layer = 'sectores_riego') |>
      st_transform(32719)
    
    data_potencial <- read_rds(str_glue("data/potencial_dataframe/{input$huerto}.rds"))
    
    data_raster <- read_rds(str_glue("data/raster_rds/{input$huerto}.rds")) |> 
      terra::unwrap()
    
    data_umbral <- read_rds("data/umbral_tlp.rds") |>
      filter(sitio == input$huerto)  |> 
      mutate(across(everything(), function(x) set_names(x, NULL)))
    
    # previo a enviar datos actualizar select: temporada y fecha 
    opts_fecha <- data_potencial |> 
      filter(temporada == input$temporada) |> 
      distinct(fecha) |> 
      pull() |>
      sort()
    
    # updateSliderTextInput(session = session,
    updateDateInput(
      session = session,
      inputId = "fecha",
      value = tail(opts_fecha, 1)
      # choices = opts_fecha,
      # selected = c(tail(opts_fecha, 8 * 7)[1], tail(opts_fecha, 1))
      )
    
    data_list <- list(
      sf = data_sf, 
      pt = data_potencial, 
      rt = data_raster,
      um = data_umbral
      )
    
    data_list
    
  }) |> 
    # data cambia con HUERTO
    bindEvent(input$huerto, input$temporada)
  
  output$txt_mapa_huerto <- renderUI({
    str_glue("Mapa del huerto {names(which(input$huerto == opts_huertos))} para {max(input$fecha)}")
    })
  
  output$mapa <- renderLeaflet({

    data_list <- data_list()
    
    data_sf <- data_list$sf
    data_pt <- data_list$pt
    data_rt <- data_list$rt
    
    data_sf <- data_sf |> 
      mutate(id = row_number()) |> 
      mutate(equipo_sector = coalesce(equipo_sector, "1_6")) |> 
      mutate(sector_equipo = equipo_sector_a_sector_equipo(equipo_sector))
    
    data_sf_map <- left_join(
      data_sf,
      data_pt |> filter(fecha == input$fecha),
      # data_pt |> filter(fecha == input$fecha[2]),
      by = join_by(id)
    ) |> 
      st_transform("+init=epsg:4326")
    
    # data_rt_map <- data_rt[[which(names(data_rt) == input$fecha[2])]]
    data_rt_map <- data_rt[[which(names(data_rt) == input$fecha)]]
    
    m <- leaflet() |> 
      # addTiles() |>
      addProviderTiles(providers$Esri.WorldImagery) |>
      clearGroup(group = "layer") |>
      clearControls() |> # remueve la Legenda
      identity()
    
    if(isolate(input$layer == "shape")) {
      
      pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), 
                          # values(data_rt_map),
                          data_sf_map$potencial,
                          na.color = "transparent")
        
      m <- m |> 
        addLegend(pal = pal, values =  data_sf_map$potencial, title = "Potencial") |> 
        leaflet::addPolygons(
          group = "layer",
          data = data_sf_map,
          fillColor        = ~ pal(potencial),
          weight           = .5,
          dashArray        = "3",
          stroke           = NULL,
          fillOpacity      = 0.7,
          layerId          = ~ id,
          # popup            = ~ str_glue("Sector/equipo: {sector_equipo}<br/>Potencial {round(potencial,2)}"),
          label            =  ~ str_glue("Sector/equipo: {sector_equipo}\nPotencial {round(potencial,2)}")
        ) |>
        identity()
    
    } else {
      
      pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), 
                          values(data_rt_map),
                          # data_sf_map$potencial,
                          na.color = "transparent")
      
      m <- m |> 
        addLegend(pal = pal, values =   values(data_rt_map), title = "Potencial") |> 
        addRasterImage(data_rt_map, colors = pal, opacity = 0.8, group = "layer") |> 
        identity()
    }
    
    m

  })
  
  output$potencial <- renderHighchart({
    
    data_list <- data_list()
    
    data_list$pt |> 
      # filter(ymd(min(input$fecha)) <= fecha, fecha <= ymd(max(input$fecha))) |>
      filter(ymd(input$fecha) - days(7 - 1) <= fecha, fecha <= ymd(input$fecha)) |>
      mutate(potencial = -potencial / 10) |> 
      hchart("line", hcaes(fecha, potencial, group = id)) |> 
      hc_tooltip(sort = TRUE, table = TRUE, valueDecimals = 2) |> 
      hc_plotOptions(series = list(animation = FALSE)) |> 
      hc_yAxis(
        title = list(text = "Potencial"),
        min = -3,
        max = 0,
        plotBands = list(
          list(
            from = data_list$um$u_minimo,
            to = 100,
            color = "#8CD47E55"
          ),
          list(
            from = data_list$um$u_maximo,
            to = data_list$um$u_minimo,
            color = "#F8D66D55"
          ),
          list(
            from = -5,
            to = data_list$um$u_maximo,
            color = "#FF696199"
          )
        )
        ) |> 
      hc_xAxis(title = list(text = "Fecha"))

  })
  
  output$potencial_temporada <- renderHighchart({
    
    data_list <- data_list()
    
    data_list$pt |> 
      filter(temporada == input$temporada) |> 
      # filter(ymd(min(input$fecha)) <= fecha, fecha <= ymd(max(input$fecha))) |>
      # filter(ymd(input$fecha) - days(7 - 1) <= fecha, fecha <= ymd(input$fecha)) |>
      mutate(potencial = -potencial / 10) |> 
      hchart("line", hcaes(fecha, potencial, group = id)) |> 
      hc_tooltip(sort = TRUE, table = TRUE, valueDecimals = 2) |> 
      hc_plotOptions(series = list(animation = FALSE)) |> 
      hc_yAxis(
        title = list(text = "Potencial"),
        min = -3,
        max = 0,
        plotBands = list(
          list(
            from = data_list$um$u_minimo,
            to = 100,
            color = "#8CD47E55"
          ),
          list(
            from = data_list$um$u_maximo,
            to = data_list$um$u_minimo,
            color = "#F8D66D55"
          ),
          list(
            from = -5,
            to = data_list$um$u_maximo,
            color = "#FF696199"
          )
        )
      ) |> 
      hc_xAxis(title = list(text = "Fecha"))
    
  })
  
  output$clima <-  renderHighchart({
    
    data_list <- data_list()
    
    # fechas <- data_list$pt |>
    #   filter(between(fecha, ymd(min(input$fecha)), ymd(min(input$fecha)))) |> 
    #   # filter(temporada == max(temporada)) |> 
    #   # filter(fecha >= (max(fecha) - days(14))) |> 
    #   # filter(between(fecha) >= (max(fecha) - days(14))) |> 
    #   summarise(min(fecha), max(fecha)) |> 
    #   as.list()
    
    # readRDS("data/clima_dia.rds") |> count(temporada)
    
    readRDS("data/clima_dia.rds") |> 
      # filter(temporada == max(temporada)) |> 
      filter(sitio == input$huerto) |> 
      # filter(ymd(min(input$fecha)) <= fecha, fecha <= ymd(max(input$fecha))) |> 
      filter(ymd(input$fecha) - days(7 - 1) <= fecha, fecha <= ymd(input$fecha)) |> 
      select(fecha, temperatura_media = t_media, evapotransipracion_referencia = eto,
             deficiencia_de_presión_de_vapor_promedio = vpd_medio) |> 
      pivot_longer(cols = -fecha) |> 
      mutate(fecha = ymd(fecha)) |> 
      mutate(
        name = str_replace_all(name, "_", " "),
        name = str_to_title(name)
      ) |> 
      hchart("line", hcaes(fecha, value, group = name)) |> 
      hc_plotOptions(series = list(animation = FALSE)) |> 
      hc_tooltip(valueDecimals = 2, table = TRUE) |> 
      hc_yAxis(title = list(text = "")) |> 
      hc_xAxis(title = list(text = "Fecha")) 
    
  })
  
  observeEvent(c(input$fecha, input$layer), {

    cli::cli_inform(str_glue("observeEvent inpt$fecha: {input$fecha[2]}"))
    cli::cli_inform(str_glue("observeEvent inpt$fecha: {input$fecha}"))

    data_list <- data_list()
    
    data_sf <- data_list$sf
    data_pt <- data_list$pt
    data_rt <- data_list$rt
    
    data_sf <- data_sf |> 
      mutate(id = row_number()) |> 
      mutate(equipo_sector = coalesce(equipo_sector, "1_6")) |> 
      mutate(sector_equipo = equipo_sector_a_sector_equipo(equipo_sector))
    
    data_sf_map <- left_join(
      data_sf,
      # data_pt |>filter(fecha == input$fecha[2]),
      data_pt |>filter(fecha == input$fecha),
      by = join_by(id)
      ) |> 
      st_transform("+init=epsg:4326")
    
    # data_rt_map <- data_rt[[which(names(data_rt) == input$fecha[2])]]
    data_rt_map <- data_rt[[which(names(data_rt) == input$fecha)]]
    
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(data_rt_map), na.color = "transparent")
    
    m <- leafletProxy("mapa") |>
      # leaflet() |> addTiles() |>
      clearGroup(group = "layer") |>
      clearControls() |> 
      identity()
    
    if(input$layer == "shape") {
      
      pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), 
                          # values(data_rt_map),
                          data_sf_map$potencial,
                          na.color = "transparent")
      
      m <- m |> 
        addLegend(pal = pal, values =  data_sf_map$potencial, title = "Potencial") |> 
        leaflet::addPolygons(
          group = "layer",
          data = data_sf_map,
          fillColor        = ~ pal(potencial),
          weight           = .5,
          dashArray        = "3",
          stroke           = NULL,
          fillOpacity      = 0.7,
          layerId          = ~ id,
          # popup            = ~ str_glue("Sector/equipo: {sector_equipo}<br/>Potencial {round(potencial,2)}"),
          label            =  ~ str_glue("Sector/equipo: {sector_equipo}\nPotencial {round(potencial,2)}")
        ) |>
        identity()
      
    } else {
      
      pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), 
                          values(data_rt_map),
                          # data_sf_map$potencial,
                          na.color = "transparent")
      
      m <- m |> 
        addLegend(pal = pal, values =   values(data_rt_map), title = "Potencial") |> 
        addRasterImage(data_rt_map, colors = pal, opacity = 0.8, group = "layer") |> 
        identity()
    }
    
    m
    
  })
  
}

shinyApp(ui, server)