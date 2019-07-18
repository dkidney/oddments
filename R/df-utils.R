
#' @rdname df-utils
#' @name df-utils
#' @title Data frame utils
#' @description 
#' \itemize{
#'   \item \code{df_kable()} - wraps \link[knitr]{kable}, \link[kableExtra]{kable_styling}
#'   and \link[kableExtra]{scroll_box}. Dots get passed to \link[knitr]{kable}.
#'   \item \code{df_datatable()} - wraps \link[DT]{datatable}
#'   \item \code{df_print()} - wraps \link[base]{print.data.frame} and uses
#'   defaults \code{row.names=FALSE} and \code{right=FALSE}
#' }
#' 
#' See: https://rstudio.github.io/DT
#' @param x (data.frame)
#' @param ... additional arguments to pass to the underlying function (see Details)
#' @examples
#' \dontrun{
#'
#' mtcars %>%
#'   head() %>%
#'   df_kable() %>%
#'   view_html()
#' }
NULL

#' @rdname df-utils
#' @name df_kable
#' @export
#' @param scroll (boolean) if \code{TRUE} then a \link[kableExtra]{scroll_box} will be
#'   used
#' \link[knitr]{kable}
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra column_spec
#' @importFrom kableExtra row_spec
#' @importFrom kableExtra scroll_box
df_kable <- function(x, scroll = FALSE, ...) {
  df <- x %>%
    knitr::kable(
      format = "html",
      digits = 5,
      row.names = TRUE,
      caption = "A caption",
      format.args = list(big.mark = ","),
      ...
    ) %>%
    kableExtra::kable_styling(
      full_width = FALSE,
      bootstrap_options = c("striped", "condensed"),
      position = "center",
      font_size = 11,
      fixed_thead = TRUE # dont need this if using scroll_box
    ) %>%
    kableExtra::row_spec(
      0,
      bold = TRUE,
      color = "white",
      background = "dodgerblue"
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

df_datatable = function(x, ...){
  args = list(...)
  # args$options %<>% replace_null(list())
  # args$options$pagelength %<>% replace_null(NULL)
  # args$options$dom %<>% replace_null(NULL)
  # args$rownames %<>% replace_null(FALSE)
  # args$class %<>% replace_null("cell-border stripe")
  # args$escape %<>% replace_null(FALSE)
  args$style %<>% replace_null("bootstrap")
  do.call(DT::datatable, args)
}

df_print = function(x, ...){
  args = list(...)
  args$right %<>% replace_null(FALSE)
  args$row.names %<>% replace_null(FALSE)
  do.call(print.data.frame, args)
}

