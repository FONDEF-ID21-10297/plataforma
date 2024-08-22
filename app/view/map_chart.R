# Echarts4r map module
box::use(
  shiny[NS, tagList, div, selectInput, callModule, reactive, tags],
  echarts4r[echarts4rOutput, renderEcharts4r, e_charts, e_map, e_visual_map, e_tooltip]
)

box::use(
  app/logic/get_metrics_choices[get_metrics_choices],
  app/logic/get_data_by_time_range[get_countries_data_by_date],
  # Load Constants
  app/constants
)

ui <- function(id) {
  ns <- NS(id)

  # select only those metrics that are available per country
  choices <- get_metrics_choices(
    constants$map_metrics, constants$metrics_list,
    suffix = "by country"
  )

  tagList(
    tags$div(
      class = "panel-header",
      selectInput(
        ns("metric"), "Select metric for the map",
        choices,
        width = NULL,
        selectize = TRUE,
        selected = constants$map_metrics[1]
      )
    ),
    echarts4rOutput(ns("countryMap"), height = "100%")
  )
}

init_server <- function(id, df, countries_geo_data, y, m) {
  callModule(server, id, df, countries_geo_data, y, m)
}

server <- function(input, output, session, df, countries_geo_data, y, m) {
  metric <- reactive({
    constants$metrics_list[[input$metric]]
  })

  countries_df <- reactive({
    get_countries_data_by_date(df, y(), m(), metric()$id, sum)
  })

  country_label <- function(visible = FALSE) {
    list(
      show = visible,
      backgroundColor = constants$colors$white,
      borderRadius = 4,
      borderWidth = 0,
      color = constants$colors$secondary,
      padding = c(10, 14),
      formatter = "{b}: {c}",
      shadowBlur = 12,
      shadowColor = "rgba(0,0,0,0.2)",
      shadowOffsetY = 3
    )
  }

  output$countryMap <- renderEcharts4r({
    countries_df() |>
      e_charts(country) |>
      e_map(
        value,
        name = constants$metrics_list[[metric()$id]]$title,
        roam = TRUE,
        scaleLimit = list(min = 1, max = 8),
        itemStyle = list(
          areaColor = constants$colors$ash_light,
          borderColor = constants$colors$white,
          borderWidth = 0.5
        ),
        emphasis = list(
          label = country_label(),
          itemStyle = list(areaColor = constants$colors$primary)
        ),
        select = list(
          label = country_label(visible = TRUE),
          itemStyle = list(areaColor = constants$colors$primary)
        )
      ) |>
      e_visual_map(
        value,
        inRange = list(color = c(constants$colors$ash_light, constants$colors$secondary))
      ) |>
      e_tooltip(
        trigger = "item",
        borderWidth = 0,
        extraCssText = "box-shadow: 0 3px 12px rgba(0,0,0,0.2);"
      )
  })
}
