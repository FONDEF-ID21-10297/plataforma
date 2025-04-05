# source("global.R")
# input <- list(
#   huerto = "rio_claro",
#   temporada = "2024-25",
#   # fecha = c("2024-04-15", "2024-04-28")
#   fecha = "2025-04-02",
#   layer = "nivel"
# )

function(input, output, session) {
  
  data_potencial_repo <- read_csv("https://raw.githubusercontent.com/FONDEF-ID21-10297/datos_plataforma_gha/refs/heads/main/data/potencial-csv/potencial-sites.csv", show_col_types = FALSE)
  data_clima_repo     <- read_csv("https://raw.githubusercontent.com/FONDEF-ID21-10297/datos_plataforma_gha/refs/heads/main/data/climate/climate-sites.csv", show_col_types = FALSE) 

  # data --------------------------------------------------------------------
  # variable que señala que sector graficar
  
  sector <- reactiveVal(NULL)
  
  data_list <- reactive({
    
    # cuando se cambia de huerto se resetea
    sector(NULL)
    
    # datas
    data_sf <- read_sf(str_glue("data/vectorial/{input$huerto}.gpkg"), layer = 'sectores_riego') |>
      st_transform(32719) |> 
      mutate(id = row_number()) |> 
      mutate(
        equipo_sector = coalesce(equipo_sector, "1_6"),
        sector_equipo = equipo_sector_a_sector_equipo(equipo_sector),
        sector_equipo_lbl = sector_equipo_a_lbl(sector_equipo)
      ) 
    
    data_potencial <- data_potencial_repo |>
      filter(site == input$huerto) |> 
      mutate(temporada = fecha_a_temporada(date)) |> 
      filter(temporada == input$temporada) 
    
    # data_raster <- read_rds(str_glue("data/raster_rds/{input$huerto}.rds")) |> 
    #   terra::unwrap()
    
    data_umbral <- read_rds("data/umbral_tlp.rds") |>
      filter(sitio == input$huerto)  |> 
      mutate(across(everything(), function(x) set_names(x, NULL)))
    
    data_clima <- data_clima_repo |> 
      mutate(
        fecha = date,
        temporada = fecha_a_temporada(fecha)
        ) |> 
      filter(temporada == input$temporada) |>
      filter(site == input$huerto) 
    
    data_potencial |> summarise(min(date), max(date))
    data_clima     |> summarise(min(date), max(date))
    
    # previo a enviar datos actualizar select: temporada y fecha 
    opts_fecha <- data_potencial |> 
      # filter(temporada == input$temporada) |> 
      distinct(date) |> 
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
      # rt = data_raster,
      um = data_umbral,
      cl = data_clima
    )
    
    data_list
    
  }) |> 
    # data cambia con HUERTO
    bindEvent(input$huerto, input$temporada)
  
  data_list_fecha <- reactive({

    data_list <- data_list()

    # st_transform(32719) 
    # st_transform("+init=epsg:4326") 
    
    data_sf_map <- data_list$sf |> 
      left_join(data_list$pt |> filter(date == input$fecha), by = join_by(id)) |> 
      separate(sector_equipo_lbl, c("sector", "equipo"), sep = " - ") |> 
      mutate(
        nivel = cut(
          potencial,
          c(-Inf,  data_list$um$u_maximo, data_list$um$u_minimo, Inf),
          labels = c("Severo", "Intermedio", "Sin estrés")
          )
      )
    
    data_rt_map <- rast(str_glue("https://github.com/FONDEF-ID21-10297/datos_plataforma_gha/raw/refs/heads/main/data/potencial-raster/{input$huerto}/{input$fecha}.tif"))
  
    data_list_fecha <- list(
      data_sf_map = data_sf_map,
      data_rt_map = data_rt_map
    )

    data_list_fecha
      
  }) 
  
  output$txt_mapa_huerto <- renderUI({
    str_glue("Mapa del huerto {names(which(input$huerto == opts_huertos))} para {max(input$fecha)}")
  })
  
  output$value_boxes <- renderUI({
    
    data_list_fecha <- data_list_fecha()
    
    daux <- data_list_fecha$data_sf_map |>
      st_drop_geometry() |> 
      group_by(nivel, sector) |> 
      summarise(
        n = n(),
        equipos = str_c(equipo, collapse = ", ")
      ) |> 
      mutate(
        equipos = str_remove(equipos, " Equipo"),
        equipos = str_replace(equipos, "Equipo", "Equipo(s)"),
        sec_equipos = str_c(sector, equipos, sep = " ")
      ) |> 
      group_by(nivel) |> 
      summarise(
        n = sum(n),
        secs_equipos = tagList(map(sec_equipos, function(x) tags$small(x, tags$br())))
      ) |> 
      tidyr::complete(nivel, fill = list(n = 0)) 

    daux1 <- daux |> select(nivel, n) |> deframe() |> as.list()
    daux2 <- daux |> select(nivel, secs_equipos) |> deframe() |> as.list()
 
    vb1 <- value_box(
      theme = value_box_theme(bg = colors_lvl[1], fg = "white"),
      title = "Sectores sin estrés",
      value = daux1$Bueno,
      # tags$small(daux2$bueno),
      showcase = bs_icon("droplet-fill", class = "text-primary")
    )

    vb2 <- value_box(
      theme = value_box_theme(bg = colors_lvl[2], fg = "white"),
      title = "Sectores estrés moderado",
      value = daux1$intermedio,
      tags$small(daux2$intermedio),
      showcase = bs_icon("droplet-half", class = "text-primary")
    )

    vb2
    
    vb3 <- value_box(
      theme = value_box_theme(bg = colors_lvl[3], fg = "white"),
      title = "Sectores estrés severo",
      value = daux1$malo, 
      tags$small(daux2$malo),
      showcase = bs_icon("droplet", class = "text-primary")
    )
    
    layout_columns(vb3, vb2, vb1)
    
  })
  
  output$mapa <- renderLeaflet({
    
    data_list_fecha <- data_list_fecha()
      
    if(interactive()) plot(data_list_fecha$data_sf_map) 
    if(interactive()) plot(data_list_fecha$data_rt_map)
    
    m <- leaflet() |> 
      addProviderTiles(providers$Esri.WorldImagery) |>
      clearGroup(group = "layer") |>
      clearControls() |> # remueve la Legenda
      identity()

    cli::cli_inform("output$mapa  {input$shape}")
    
    input_layer <- isolate(input$layer)

    if(input_layer == "shape") {
      
      data_sf_map <- data_list_fecha$data_sf_map |> 
        st_transform("+init=epsg:4326")

      pal <- colorNumeric(
        c("#0C2C84", "#41B6C4", "#FFFFCC"),
        data_sf_map$potencial,
        na.color = "transparent"
      )
      
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
          label            =  ~ str_glue("{sector} {equipo} Potencial {round(potencial,2)}"),
          labelOptions = labelOptions(style = lbl_opts_style)
        ) |>
        identity()

      m
    } else if (input_layer == "nivel") {
      
      data_sf_map <- data_list_fecha$data_sf_map |> 
        st_transform("+init=epsg:4326")
      
      pal <- colorFactor(
        palette = rev(colors_lvl),
        domain = data_sf_map$nivel,
        na.color = "#ccc"
      )
      
      m <- m |> 
        addLegend(pal = pal, values =  data_sf_map$nivel, title = "Nivel") |> 
        leaflet::addPolygons(
          group = "layer",
          data = data_sf_map,
          fillColor        = ~ pal(data_sf_map$nivel),
          weight           = .5,
          dashArray        = "3",
          stroke           = NULL,
          fillOpacity      = 0.7,
          layerId          = ~ id,
          label            =  ~ str_glue("{sector} {equipo} {nivel}"),
          labelOptions = labelOptions(style = lbl_opts_style)
        ) |>
        identity()

      m

    } else {
      
      data_rt_map <- data_list_fecha$data_rt_map

      pal <- colorNumeric(
        c("#0C2C84", "#41B6C4", "#FFFFCC"),
        values(data_rt_map),
        na.color = "transparent"
      )
      
      m <- m |> 
        addLegend(pal = pal, values =   values(data_rt_map), title = "Potencial") |> 
        addRasterImage(data_rt_map, colors = pal, opacity = 0.8, group = "layer") |> 
        identity()

      m

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
        color = "#8CD47E88"
      ),
      list(
        from = data_list$um$u_maximo,
        to = data_list$um$u_minimo,
        color = "#F8D66D88"
      ),
      list(
        from = -5,
        to = data_list$um$u_maximo,
        color = "#FF696188"
      )
    )
    
    # axis
    # modificacion: se crean 4 para que la ultima se paree con la primera
    axis <- create_axis(naxis = 2, lineWidth = 2, title = list(text = NULL))
    axis[[1]]$plotBands <- pb
    axis[[1]]$min       <- -3
    axis[[1]]$max       <- 0
    
    axis[[1]]$title     <- list(text = "Potencial")
    # axis[[2]]$title     <- list(text = "Cantidad Riesgo")
    axis[[2]]$title     <- list(text = "Datos Meteorológicos")
    
    axis[[3]] <- axis[[1]]
    axis[[3]]$linkedTo <- 0
    axis[[3]]$opposite <- TRUE
    axis[[3]]$title <- list(text = "")
    
    # days
    fmax <- ymd(input$fecha)
    fmin <- ymd(input$fecha) - days(7 + 0)
    
    # data potential
    daux_pt <- data_list$pt |> 
      # filter(ymd(min(input$fecha)) <= fecha, fecha <= ymd(max(input$fecha))) |>
      filter(fmin <= date, date <= fmax) 
    
    # seleccionamos sector o el minimo si es que no está definido
    sec <- sector()
    sec <- coalesce(
      sec,
      daux_pt |> filter(date == max(date)) |> filter(potencial == min(potencial)) |> pull(id)
      )
    
    daux_pt <- daux_pt |> 
      filter(id == sec) |> 
      left_join(
        data_list$sf |>
          st_drop_geometry() |> 
          select(id, sector_equipo),
        by = join_by(id)
      ) |> 
      mutate(sector_equipo_lbl = sector_equipo_a_lbl(sector_equipo))

    axis[[3]]$tickPositioner = JS(
      "function(min,max){
               var data = this.chart.yAxis[0].series[0].processedYData;
               //last point
               return [Math.round(1000 * data[data.length-1])/1000];
            }"
    )
    
    # data clima
    daux_cl <- data_list$cl |> 
      filter(fmin <= date, date <= fmax) |> 
      select(
        date,
        temp_media = t_media,
        evapotransipracion_ref = eto,
        deficiencia_presión_vapor_promedio = vpd_medio
        ) |>
      pivot_longer(cols = -date) |>
      mutate(date = highcharter::datetime_to_timestamp(date)) |>
      mutate(
        name = str_replace_all(name, "_", " "),
        name = str_to_title(name)
        ) |> 
      rename(x = date, y = value)
    
    # data riego 
    # set.seed(fmax)
    # daux_rg <- daux_pt |> 
    #   select(name = sector_equipo_lbl, x = date) |> 
    #   mutate(x = datetime_to_timestamp(x)) |> 
    #   mutate(y = 100 * runif(n(), 0, 1) * rbinom(1, n = n(), p = 0.1)) |> 
    #   filter(y > 0) |> 
    #   mutate(name = str_glue("Riesgo en {name}"))
    
    hchart(daux_pt, "line", hcaes(date, potencial, group = sector_equipo_lbl), animation = TRUE, lineWidth = 4) |> 
      hc_tooltip(sort = FALSE, table = TRUE, valueDecimals = 2) |> 
      hc_plotOptions(series = list(animation = FALSE)) |> 
      hc_yAxis_multiples(axis) |> 
      hc_add_series(daux_cl, type = "line", hcaes(x, y, group = name), yAxis = 1, lineWidth = 4) |> 
      # hc_add_series(daux_rg, type = "column", hcaes(x, y, group = name), yAxis = 1, color = "#ADD8E6", pointWidth = 10, stacking = 'normal') |> 
      hc_xAxis(title = list(text = "Fecha")) |> 
      hc_subtitle(text = daux_pt |> distinct(sector_equipo_lbl) |> pull())
    
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
      # mutate(potencial = -potencial / 10) |> 
      hchart("line", hcaes(date, potencial, group = id)) |> 
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
  
  observeEvent(c(input$fecha, input$layer), {
    
    cli::cli_inform(str_glue("observeEvent inpt$fecha: {input$fecha[2]}"))
    cli::cli_inform(str_glue("observeEvent inpt$fecha: {input$fecha}"))
    
    data_list_fecha <- data_list_fecha()
      
    if(interactive()) plot(data_list_fecha$data_sf_map) 
    if(interactive()) plot(data_list_fecha$data_rt_map)
    
    m <-  leafletProxy("mapa") |>
      # leaflet() |> addTiles() |>
      # addProviderTiles(providers$Esri.WorldImagery) |>
      clearGroup(group = "layer") |>
      clearControls() |> # remueve la Legenda
      identity()
    
    if(input$layer == "shape") {
      
      data_sf_map <- data_list_fecha$data_sf_map |> 
        st_transform("+init=epsg:4326")

      pal <- colorNumeric(
        c("#0C2C84", "#41B6C4", "#FFFFCC"),
        data_sf_map$potencial,
        na.color = "transparent"
      )
      
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
          label            =  ~ str_glue("{sector} {equipo} Potencial {round(potencial,2)}"),
          labelOptions = labelOptions(style = lbl_opts_style)
        ) |>
        identity()

      m
    } else if (input$layer == "nivel") {
      
      data_sf_map <- data_list_fecha$data_sf_map |> 
        st_transform("+init=epsg:4326")
      
      pal <- colorFactor(
        palette = rev(colors_lvl),
        domain = data_sf_map$nivel,
        na.color = "#ccc"
      )
      
      m <- m |> 
        addLegend(pal = pal, values =  data_sf_map$nivel, title = "Nivel") |> 
        leaflet::addPolygons(
          group = "layer",
          data = data_sf_map,
          fillColor        = ~ pal(data_sf_map$nivel),
          weight           = .5,
          dashArray        = "3",
          stroke           = NULL,
          fillOpacity      = 0.7,
          layerId          = ~ id,
          label            =  ~ str_glue("{sector} {equipo} {nivel}"),
          labelOptions = labelOptions(style = lbl_opts_style)
        ) |>
        identity()

      m

    } else {
      
      data_rt_map <- data_list_fecha$data_rt_map

      pal <- colorNumeric(
        c("#0C2C84", "#41B6C4", "#FFFFCC"),
        values(data_rt_map),
        na.color = "transparent"
      )
      
      m <- m |> 
        addLegend(pal = pal, values =   values(data_rt_map), title = "Potencial") |> 
        addRasterImage(data_rt_map, colors = pal, opacity = 0.8, group = "layer") |> 
        identity()

      m

    }
    
    m
    
  })
  
  observeEvent(input$hoy, {
    
    data_list <- data_list()
    data_potencial <- data_list$pt
    data_potencial_last <- data_potencial |> 
      tail(1)
    
    # va a la ultima, no a la actual real.
    # updateSliderTextInput(session = session,
    updateSelectInput(
      session = session, 
      inbputId = "temporada",
      selected = data_potencial_last$temporada
    )
    updateDateInput(
      session = session,
      inputId = "fecha",
      value = data_potencial_last$date,
      # choices = opts_fecha,
      # selected = c(tail(opts_fecha, 8 * 7)[1], tail(opts_fecha, 1))
    )
    
  })
  
}