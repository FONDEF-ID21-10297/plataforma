box::use(
  lubridate[year, floor_date, month],
  dplyr[mutate, group_by, summarize_at, select, rename, ungroup],
  magrittr[`%>%`]
)

#' @export
get_subset_by_time_range <- function(df, y, m = NULL, metric) {
  cols_to_select <- c("date", metric)
  if (is.null(m) || m == "0") {
    subset(
      x = df,
      subset = year(date) == y,
      select = cols_to_select
    )
  } else {
    subset(
      x = df,
      subset = year(date) == y & month(date) == m,
      select = cols_to_select
    )
  }
}

#' @export
get_monthly_data_by_year <- function(df, y, metric) {
  get_subset_by_time_range(df, y, m = NULL, metric) %>%
    mutate(date = floor_date(date, "month")) %>%
    group_by(date) %>%
    summarize_at(.vars = c(metric), .funs = sum_all_non_na_values)
}

sum_all_non_na_values <- function(v) {
  if (length(v[!is.na(v)]) != 0) {
    sum(v, na.rm = TRUE)
  } else {
    return(NA)
  }
}

#' @export
get_countries_data_by_date <- function(df, y, m, metric, f = sum) {
  get_subset_by_time_range(df, y, m, metric = c("country", metric)) %>%
    group_by(country) %>%
    select(metric) %>%
    rename(value = metric) %>%
    ungroup()
}
