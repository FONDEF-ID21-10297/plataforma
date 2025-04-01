bslib::page_navbar(
  title =  tags$img(src = "logo1.png", height = "30px", style = "margin-top: -5px"),
  window_title = "SATORI",
  theme = theme_app,
  sidebar = sidebar_app,
  nav_panel(
    title = "Panel",
    class = "bslib-page-dashboard",
    tags$head(
      tags$link(href = "favicon.png", rel = "icon"),
      # tags$script(src = "https://www.googletagmanager.com/gtag/js?id=G-CYG993XQRT", async = ""),
      # tags$script(src = "js/ga.js"),
      includeScript("www/custom.js"),
      # includeCSS("www/styles.css"),
    ),
    useBusyIndicators(),
    uiOutput("value_boxes"),
    layout_columns(
      col_widths = c(5, 7),
      card(card_header(uiOutput("txt_mapa_huerto")), full_screen = TRUE, leafletOutput("mapa")),
      card(card_header("Datos última semana sector"), full_screen = TRUE, highchartOutput("potencial")),
      # card(card_header("Datos meteorológicos del huerto"), full_screen = TRUE, highchartOutput("clima")
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


