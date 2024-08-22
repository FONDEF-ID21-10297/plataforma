box::use(
  shiny[tags],
  glue[glue]
)

#' @export
get_external_link <- function(url, class, ...) {
  tags$a(
    href = url,
    class = glue("logo logo-{class}"),
    target = "_blank",
    rel = "noopener",
    ...
  )
}
