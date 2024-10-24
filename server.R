# input <- list(
#   huerto = "la_esperanza",
#   temporada = "2022-23",
#   # fecha = c("2024-04-15", "2024-04-28")
#   fecha = "2024-04-28"
# )

function(input, output, session) {
  
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