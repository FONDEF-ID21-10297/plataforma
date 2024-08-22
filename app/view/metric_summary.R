# Metric module with summary
box::use(
  shiny[NS, tagList, selectInput, uiOutput, callModule, renderUI, reactive],
  magrittr[`%>%`],
  htmltools[HTML],
  glue[glue]
)

box::use(
  app/logic/get_metrics_choices[get_metrics_choices_category],
  app/logic/get_percent_change_span[get_percent_change_span],
  # Load Constants
  app/constants
)

ui <- function(id) {
  ns <- NS(id)
  choices <- get_metrics_choices_category(sub(pattern = ".*-", "", id))

  tagList(
    selectInput(
      ns("summary_metric"), "Metric",
      choices,
      width = NULL,
      selectize = TRUE,
      selected = choices[[1]]
    ),
    uiOutput(ns("summary"))
  )
}

init_server <- function(id, monthly_df, yearly_df,
                        y, m, previous_time_range) {
  callModule(
    server, id, monthly_df, yearly_df,
    y, m, previous_time_range
  )
}

server <- function(input, output, session, monthly_df, yearly_df,
                   y, m, previous_time_range) {
  metric <- reactive({
    constants$metrics_list[[input$summary_metric]]
  })

  output$summary <- renderUI({
    if (m() == 0) {
      df <- yearly_df
      prev_timerange_suffix <- "prev_year"
    } else {
      df <- monthly_df
      prev_timerange_suffix <- previous_time_range()
    }

    selected_date <-
      paste(y(), ifelse(m() == "0", "1", m()), "01", "sep" = "-") %>% as.Date()
    row <- df[df$date == selected_date, ]

    metric_total_value <- row[, metric()$id]

    invert_colors <- constants$metrics_list[[metric()$id]]$invert_colors
    metric_change_span <-
      row[, paste0(metric()$id, ".perc_", prev_timerange_suffix)] %>%
      get_percent_change_span(invert_colors)

    value_prefix <-
      ifelse(!is.null(metric()$currency),
        paste0(metric()$currency, " "),
        ""
      )

    # SVG graphics selected from SVG sprite map based on specified id
    glue('
        <svg class="icon">
          <use href="static/icons/icons-sprite-map.svg#{input$summary_metric}"></use>
        </svg>
        <span class="metric">{value_prefix}{metric_total_value}</span>
        {metric_change_span}
      ') %>% HTML()
  })
}
