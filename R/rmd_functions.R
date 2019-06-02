
html_template = function(...){
    args = list(...)
    args$css %<>%
        replace_null(
            "rmarkdown/templates/html_template/resources/template.css" %>%
            system.file(package = "oddments")
        )
    do.call(rmarkdown::html_document, args)
}

# -------------------------------------------------------------------------------------- #

# @rdname TODO
# @name kable2
#' @title TODO
#' @description A combination of \link[knitr]{kable}, \link[kableExtra]{kable_styling} and
#'   \link[kableExtra]{scroll_box}.
#' @param x [data.frame] TODO
# @details TODO
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra column_spec
#' @importFrom kableExtra row_spec
#' @importFrom kableExtra scroll_box
# @example inst/examples/example-kable2.R
#' @export
#' @examples
#' \dontrun{
#'
#' mtcars %>%
#'   head %>%
#'   kable2 %>%
#'   view_html
#' }

kable2 = function(x, scroll = FALSE, ...){
  df = x %>%
    kable(
      format = "html",
      ...
    ) %>%
    kable_styling(
      full_width = FALSE,
      bootstrap_options = c("striped", "condensed"),
      position = "center"
    ) %>%
    row_spec(
      0,
      bold = TRUE,
      color = "white",
      background = "#428bca"
    ) %>%
    column_spec(
      1,
      bold = TRUE
    )
  if(!scroll){
    return(df)
  }
  df %>%
    scroll_box(
      height = "500px",
      width = "100%",
      box_css = "border: 0px; padding: 0px"
    )
}

