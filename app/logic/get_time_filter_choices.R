box::use(
  lubridate[year, month]
)

#' @export
get_months_choices <- function(year = NULL, data_last_day) {
  months <- c(1:12)
  names(months) <- month.name
  months_choices <- months

  if (is.null(year) || year == year(data_last_day)) {
    months_choices <- months[1:month(data_last_day)]
  }
  c("All months" = "0", months_choices)
}

#' @export
get_year_choices <- function(data_start_date, data_last_day) {
  c(year(data_last_day):year(data_start_date))
}
