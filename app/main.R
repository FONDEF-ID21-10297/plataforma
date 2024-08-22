box::use(
  shiny[NS, htmlTemplate, selectInput, moduleServer, observeEvent, updateSelectInput, reactive],
  htmltools[htmlTemplate],
  lubridate[month, ymd],
  dplyr[mutate],
  utils[read.csv],
  here[here]
)

box::use(
  # Load utility functions
  app/logic/get_time_filter_choices[get_months_choices, get_year_choices],
  app/logic/get_external_link[get_external_link],
  # Load constant variables
  app/constants,
  # Load modules
  app/view/about_section,
  app/view/breakdown_chart,
  app/view/map_chart,
  app/view/metric_summary,
  app/view/time_chart
)

daily_stats <-
  read.csv(here("data/daily_stats.csv"), header = TRUE, stringsAsFactors = TRUE) |>
  mutate(date = ymd(date))

monthly_stats <- read.csv(here("data/monthly_stats.csv"), header = TRUE) |>
  mutate(date = ymd(date))

yearly_stats <- read.csv(here("data/yearly_stats.csv"), header = TRUE) |>
  mutate(date = ymd(date))

countries_stats <- read.csv(here("data/countries_stats.csv"), header = TRUE) |>
  mutate(date = ymd(date))

#' @export
ui <- function(id) {
  ns <- NS(id)
  # Html template used to render UI
  htmlTemplate(
    "app/static/index.html",
    appTitle = constants$app_title,
    appVersion = constants$app_version,
    mainLogo = get_external_link("https://appsilon.com/", "main", constants$appsilon_logo),
    dashboardLogo = get_external_link(
      "https://shiny.rstudio.com/", "dashboard", constants$shiny_logo
    ),
    selectYear = selectInput(
      ns("selected_year"), "Year",
      choices = get_year_choices(constants$data_first_day, constants$data_last_day),
      selectize = TRUE
    ),
    selectMonth = selectInput(
      ns("selected_month"), "Month",
      choices = get_months_choices(year = NULL, constants$data_last_day),
      selected = month(constants$data_last_day),
      selectize = TRUE
    ),
    previousTimeRange = selectInput(
      ns("previous_time_range"), "Compare to",
      choices = constants$prev_time_range_choices,
      selected = "prev_year",
      selectize = TRUE
    ),
    salesSummary = metric_summary$ui(ns("sales")),
    productionSummary = metric_summary$ui(ns("production")),
    usersSummary = metric_summary$ui(ns("users")),
    complaintsSummary = metric_summary$ui(ns("complaints")),
    timeChart = time_chart$ui(ns("time_chart")),
    breakdownChart = breakdown_chart$ui(ns("breakdown_chart")),
    countryMap = map_chart$ui(ns("map_chart")),
    marketplace_website = constants$marketplace_website,
    info_icon = about_section$ui("about")
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(c(input$selected_year), {
      months_choices <-
        get_months_choices(input$selected_year, constants$data_last_day)
      selected_month <- ifelse(
        input$selected_month %in% months_choices,
        input$selected_month,
        "0"
      )
      updateSelectInput(
        session,
        "selected_month",
        selected = selected_month,
        choices = months_choices
      )
    })

    observeEvent(c(input$selected_month), {
      if (input$selected_month == "0") {
        updateSelectInput(
          session,
          "previous_time_range",
          choices = constants$prev_time_range_choices["Previous Year"],
          selected = constants$prev_time_range_choices[["Previous Year"]]
        )
      } else {
        updateSelectInput(
          session,
          "previous_time_range",
          choices = constants$prev_time_range_choices,
          selected = input$previous_time_range
        )
      }
    })

    selected_year <- reactive({
      input$selected_year
    })
    selected_month <- reactive({
      input$selected_month
    })
    previous_time_range <- reactive({
      input$previous_time_range
    })
    # Inititalize all modules
    metric_summary$init_server(
      "sales",
      monthly_df = monthly_stats,
      yearly_df = yearly_stats,
      y = selected_year,
      m = selected_month,
      previous_time_range = previous_time_range
    )
    metric_summary$init_server(
      "production",
      monthly_df = monthly_stats,
      yearly_df = yearly_stats,
      y = selected_year,
      m = selected_month,
      previous_time_range = previous_time_range
    )
    metric_summary$init_server(
      "users",
      monthly_df = monthly_stats,
      yearly_df = yearly_stats,
      y = selected_year,
      m = selected_month,
      previous_time_range = previous_time_range
    )
    metric_summary$init_server(
      "complaints",
      monthly_df = monthly_stats,
      yearly_df = yearly_stats,
      y = selected_year,
      m = selected_month,
      previous_time_range = previous_time_range
    )
    time_chart$init_server(
      "time_chart",
      df = daily_stats,
      y = selected_year,
      m = selected_month,
      previous_time_range = previous_time_range
    )
    breakdown_chart$init_server(
      "breakdown_chart",
      df = daily_stats,
      y = selected_year,
      m = selected_month,
      previous_time_range = previous_time_range
    )
    map_chart$init_server(
      "map_chart",
      df = countries_stats,
      countries_geo_data = countries_geo_data,
      y = selected_year,
      m = selected_month
    )
    about_section$init_server(
      id = "about"
    )
  })
}
