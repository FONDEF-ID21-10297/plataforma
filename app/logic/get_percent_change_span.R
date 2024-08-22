box::use(
  glue[glue]
)

#' @export
get_percent_change_span <- function(change_value, invert_colors = NULL) {
  if (is.na(change_value)) {
    return("<span class='change-value no-data'>NA*</span>")
  }

  change_value <- round(change_value, digits = 2)

  if (change_value > 0) {
    css_class <- "change-value positive-change"
    sign <- "+"
  } else if (change_value < 0) {
    css_class <- "change-value negative-change"
    sign <- ""
  } else {
    css_class <- "change-value zero-change"
    sign <- ""
  }

  if (!is.null(invert_colors) && invert_colors == TRUE) {
    css_class <- paste(css_class, "inverted-colors")
  }

  glue("<span class='{css_class}'> {sign}{change_value}%</span>")
}
