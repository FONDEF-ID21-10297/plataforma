box::use(
  magrittr[`%>%`]
)

box::use(
  app/constants
)

create_options_list <- function(choices, suffix = "") {
  keys <- choices %>%
    lapply("[[", "title") %>%
    unname() %>%
    lapply(function(x) paste(x, suffix))

  values <- choices %>%
    lapply("[[", "id")

  names(values) <- keys
  return(values)
}

#' @export
get_metrics_choices <- function(available_metrics, metrics_list, suffix = "") {
  metrics_list[available_metrics] %>% create_options_list(suffix)
}

#' @export
get_metrics_choices_category <- function(category, suffix = "") {
  Filter(function(x) x$category == category, constants$metrics_list) %>% create_options_list(suffix)
}
