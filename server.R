# source("global.R")
# input <- list(
#   huerto = "la_esperanza",
#   temporada = "2023-24",
#   # fecha = c("2024-04-15", "2024-04-28")
#   fecha = "2024-04-28"
# )

function(input, output, session) {
  
  # showModal(
  #   modalDialog(
  #     # title = tags$small("Bienvenido al"),
  #     tags$p("Bienvenido a ", tags$strong("SATORI")),
  #     tags$hr(),
  #     tags$p("SATORI es una herramienta que permite analizar el potencial de riego en huertos a lo largo de distintas temporadas."),
  #     tags$p("Selecciona un huerto, define el rango de fechas y accede a visualizaciones detalladas en un mapa interactivo y una línea temporal, ayudando a optimizar decisiones agrícolas con datos precisos y en tiempo real."),
  #     easyClose = TRUE,
  #     footer = NULL
  #   )
  # )

  # data --------------------------------------------------------------------
  # variable que señala que sector graficar
  
  sector <- reactiveVal(NULL)
  
  data_list <- reactive({
    
    # cuando se cambia de huerto se resetea
    sector(NULL)
    
    # datas
    data_sf <- read_sf(str_glue("data/vectorial/{input$huerto}.gpkg"), layer = 'sectores_riego') |>
      st_transform(32719)
    
    data_potencial <- read_rds(str_glue("data/potencial_dataframe/{input$huerto}.rds")) |> 
      filter(temporada == input$temporada) 
    
    data_raster <- read_rds(str_glue("data/raster_rds/{input$huerto}.rds")) |> 
      terra::unwrap()
    
    data_umbral <- read_rds("data/umbral_tlp.rds") |>
      filter(sitio == input$huerto)  |> 
      mutate(across(everything(), function(x) set_names(x, NULL)))
    
    # data_etc <- readRDS("data/ETc.rds")
    # 
    # # input$fecha <- "2022-11-15"
    # data_etc |> 
    #   filter(sitio == input$huerto) |> 
    #   arrange(sector_id, fecha) |> 
    #   filter(between(fecha, ymd(input$fecha) - days(2), ymd(input$fecha))) |> 
    #   group_by(sector_id) |> 
    #   summarise(
    #     fecha = max(fecha),
    #     t_horas = sum(ETc, na.rm = TRUE)/2
    #   ) |> 
    #   mutate(
    #     t_min = t_horas * 60 
    #   )
    # 
    data_clima <- readRDS("data/clima_dia.rds") |> 
      mutate(
        fecha = ymd(fecha),
        temporada = fecha_a_temporada(fecha)
        ) |> 
      filter(temporada == input$temporada) |>
      filter(sitio == input$huerto) 
    
    data_potencial |> summarise(min(fecha), max(fecha))
    data_clima     |> summarise(min(fecha), max(fecha))
    
    # previo a enviar datos actualizar select: temporada y fecha 
    opts_fecha <- data_potencial |> 
      # filter(temporada == input$temporada) |> 
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
      um = data_umbral,
      cl = data_clima
    )
    
    data_list
    
  }) |> 
    # data cambia con HUERTO
    bindEvent(input$huerto, input$temporada)
  
  output$txt_mapa_huerto <- renderUI({
    str_glue("Mapa del huerto {names(which(input$huerto == opts_huertos))} para {max(input$fecha)}")
  })
  
  output$value_boxes <- renderUI({
    
    data_list <- data_list()
    
    daux <- data_list$pt |> 
      filter(fecha == input$fecha) |> 
      mutate(potencial = -potencial / 10) |> 
      mutate(
        cat = cut(
          potencial,
          c(-Inf,  data_list$um$u_maximo, data_list$um$u_minimo, Inf),
          labels = c("malo", "intermedio", "bueno")
          )
        ) |> 
      count(cat) |> 
      tidyr::complete(cat, fill = list(n = 0)) |> 
      deframe() |> 
      as.list()
    
    vb1 <- value_box(
      theme = value_box_theme(bg = colors_lvl[1], fg = "white"),
      title = NULL,
      value = daux$bueno, "sectores sin estrés",
      showcase = bs_icon("droplet-fill", class = "text-primary")
    )
    
    vb2 <- value_box(
      theme = value_box_theme(bg = colors_lvl[2], fg = "white"),
      title = NULL,
      value = daux$intermedio, "sectores con estrés moderado",
      showcase = bs_icon("droplet-half", class = "text-primary")
    )
    
    vb3 <- value_box(
      theme = value_box_theme(bg = colors_lvl[3], fg = "white"),
      title = NULL,
      value = daux$malo, "sectores con estrés severo",
      showcase = bs_icon("droplet", class = "text-primary")
    )
    
    layout_columns(vb1, vb2, vb3)
    
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
      st_transform("+init=epsg:4326") |>
      mutate(potencial = - potencial / 10)
    
    # data_rt_map <- data_rt[[which(names(data_rt) == input$fecha[2])]]
    data_rt_map <- data_rt[[which(names(data_rt) == input$fecha)]]
    
    # transformar potencial
    data_rt_map[[1]] <- - data_rt_map[[1]]/10 
    
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
    
    cli::cli_inform("output `potencial` - sector: {sector()}")
    
    data_list <- data_list()
  
    # plotbands 
    pb <- list(
      list(
        from = data_list$um$u_minimo,
        to = 100,
        color = "#8CD47E44"
      ),
      list(
        from = data_list$um$u_maximo,
        to = data_list$um$u_minimo,
        color = "#F8D66D44"
      ),
      list(
        from = -5,
        to = data_list$um$u_maximo,
        color = "#FF696144"
      )
    )
    
    # axis
    # modificacion: se crean 4 para que la ultima se paree con la primera
    axis <- create_axis(naxis = 3, lineWidth = 2, title = list(text = NULL), heights = c(2, 1, 1))
    axis[[1]]$plotBands <- pb
    axis[[1]]$min       <- -3
    axis[[1]]$max       <- 0
    
    axis[[1]]$title     <- list(text = "Potencial")
    axis[[2]]$title     <- list(text = "Cantidad Riesgo")
    axis[[3]]$title     <- list(text = "Datos Meteorológicos")
    
    axis[[4]] <- axis[[1]]
    axis[[4]]$linkedTo <- 0
    axis[[4]]$opposite <- TRUE
    axis[[4]]$title <- list(text = "")
    
    # days
    fmax <- ymd(input$fecha)
    fmin <- ymd(input$fecha) - days(7 - 1)
    
    
    # data potential
    daux_pt <- data_list$pt |> 
      # filter(ymd(min(input$fecha)) <= fecha, fecha <= ymd(max(input$fecha))) |>
      filter(fmin <= fecha, fecha <= fmax) |>
      mutate(potencial = -potencial / 10) 
    
    # seleccionamos sector o el minimo si es que no está definido
    sec <- sector()
    sec <- coalesce(
      sec,
      daux_pt |> filter(fecha == max(fecha)) |> filter(potencial == min(potencial)) |> pull(id)
      )
    
    daux_pt <- daux_pt |> 
      filter(id == sec) |>  
      mutate(id = str_glue("Sector {id}"))
    
    axis[[4]]$tickPositioner = JS(
      "function(min,max){
               var data = this.chart.yAxis[0].series[0].processedYData;
               //last point
               return [Math.round(1000 * data[data.length-1])/1000];
            }"
    )
    
    # data clima
    daux_cl <- data_list$cl |> 
      filter(fmin <= fecha, fecha <= fmax) |> 
      select(
        fecha,
        temp_media = t_media,
        evapotransipracion_ref = eto,
        deficiencia_presión_vapor_promedio = vpd_medio
        ) |>
      pivot_longer(cols = -fecha) |>
      mutate(fecha = highcharter::datetime_to_timestamp(fecha)) |>
      mutate(
        name = str_replace_all(name, "_", " "),
        name = str_to_title(name)
        ) |> 
      rename(x = fecha, y = value)
    
    # data riego 
    set.seed(fmax)
    daux_rg <- daux_pt |> 
      select(name = id, x = fecha) |> 
      mutate(x = datetime_to_timestamp(x)) |> 
      mutate(y = 100 * runif(n(), 0, 1) * rbinom(1, n = n(), p = 0.1)) |> 
      filter(y > 0) |> 
      mutate(name = str_glue("Riesgo en {name}"))
    
    hchart(daux_pt, "line", hcaes(fecha, potencial, group = id), animation = TRUE) |> 
      hc_tooltip(sort = FALSE, table = TRUE, valueDecimals = 2) |> 
      hc_plotOptions(series = list(animation = FALSE)) |> 
      hc_yAxis_multiples(axis) |> 
      hc_add_series(daux_cl, type = "line", hcaes(x, y, group = name), yAxis = 2) |> 
      hc_add_series(daux_rg, type = "column", hcaes(x, y, group = name), yAxis = 1, color = "#ADD8E6", pointWidth = 10, stacking = 'normal') |> 
      hc_xAxis(title = list(text = "Fecha")) |> 
      hc_subtitle(text = daux_pt |> distinct(id) |> pull())
    
  })
  
  observeEvent(input$mapa_shape_click, {
    cli::cli_inform("mapa_shape_click: {input$mapa_shape_click$id}")
    # comuna_reactive(input$dash_map_shape_click$id)
    if(coalesce(input$mapa_shape_click$id, -2) != coalesce(sector(), -1)) sector(input$mapa_shape_click$id)
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
    
    # data_list <- data_list()
    # 
    # fechas <- data_list$pt |>
    #   filter(between(fecha, ymd(min(input$fecha)), ymd(min(input$fecha)))) |> 
    #   # filter(temporada == max(temporada)) |> 
    #   # filter(fecha >= (max(fecha) - days(14))) |> 
    #   # filter(between(fecha) >= (max(fecha) - days(14))) |> 
    #   summarise(min(fecha), max(fecha)) |> 
    #   as.list()
    # 
    # readRDS("data/clima_dia.rds") |> count(temporada)
    # 
    # readRDS("data/clima_dia.rds") |> 
    #   # filter(temporada == max(temporada)) |> 
    #   filter(sitio == input$huerto) |> 
    #   # filter(ymd(min(input$fecha)) <= fecha, fecha <= ymd(max(input$fecha))) |> 
    #   filter(ymd(input$fecha) - days(7 - 1) <= fecha, fecha <= ymd(input$fecha)) |> 
    #   select(fecha, temperatura_media = t_media, evapotransipracion_referencia = eto,
    #          deficiencia_de_presión_de_vapor_promedio = vpd_medio) |> 
    #   pivot_longer(cols = -fecha) |> 
    #   mutate(fecha = ymd(fecha)) |> 
    #   mutate(
    #     name = str_replace_all(name, "_", " "),
    #     name = str_to_title(name)
    #   ) |> 
    #   hchart("line", hcaes(fecha, value, group = name)) |> 
    #   hc_plotOptions(series = list(animation = FALSE)) |> 
    #   hc_tooltip(valueDecimals = 2, table = TRUE) |> 
    #   hc_yAxis(title = list(text = "")) |> 
    #   hc_xAxis(title = list(text = "Fecha")) 
    
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
      st_transform("+init=epsg:4326") |>
      mutate(potencial = - potencial / 10)
    
    # data_rt_map <- data_rt[[which(names(data_rt) == input$fecha[2])]]
    data_rt_map <- data_rt[[which(names(data_rt) == input$fecha)]]
    
    # transformar potencial
    data_rt_map[[1]] <- - data_rt_map[[1]]/10
    
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
  
  observeEvent(input$hoy, {
    
    data_list <- data_list()
    data_potencial <- data_list$pt
    opts_fecha <- data_potencial |> 
      # filter(temporada == input$temporada) |> 
      distinct(fecha) |> 
      pull() |>
      sort()
    
    # va a la ultima, no a la actual real.
    # updateSliderTextInput(session = session,
    updateDateInput(
      session = session,
      inputId = "fecha",
      value = tail(opts_fecha, 1)
      # choices = opts_fecha,
      # selected = c(tail(opts_fecha, 8 * 7)[1], tail(opts_fecha, 1))
    )
    
  })
  
}