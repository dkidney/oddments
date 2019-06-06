
#' @importFrom rmarkdown html_document
html_template <- function(...) {
  args <- list(...)
  args$css %<>%
    replace_null(
      "rmarkdown/templates/html_template/resources/html_template.css" %>%
        system.file(package = "oddments")
    )
  do.call(html_document, args)
}

# -------------------------------------------------------------------------------------- #

# @rdname TODO
# @name kable2
#' @title TODO
#' @description A combination of \link[knitr]{kable}, \link[kableExtra]{kable_styling} and
#'   \link[kableExtra]{scroll_box}.
#' @param x (data.frame)
#' @param scroll (boolean) if \code{TRUE} then a \link[kableExtra]{scroll_box} will be
#'   used
#' @param ... additional arguments to pass to \link[knitr]{kable}
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra column_spec
#' @importFrom kableExtra row_spec
#' @importFrom kableExtra scroll_box
#' @export
#' @examples
#' \dontrun{
#'
#' mtcars %>%
#'   head() %>%
#'   kable2() %>%
#'   view_html()
#' }
#'
kable2 <- function(x, scroll = FALSE, ...) {
  df <- x %>%
    knitr::kable(
      format = "html",
      ...
    ) %>%
    kableExtra::kable_styling(
      full_width = FALSE,
      bootstrap_options = c("striped", "condensed"),
      position = "center",
      font_size = 11,
      fixed_thead = TRUE
    ) %>%
    kableExtra::row_spec(
      0,
      bold = TRUE,
      color = "white",
      background = "#337ab7" # bootstrap_button_blue
    ) %>%
    kableExtra::column_spec(
      1,
      bold = TRUE
    )
  if (!scroll) {
    return(df)
  }
  df %>%
    kableExtra::scroll_box(
      height = "500px",
      width = "100%",
      box_css = "border: 0px; padding: 0px"
    )
}
