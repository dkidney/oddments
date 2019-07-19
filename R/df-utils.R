
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
#' iris %>% knitr::kable() %>% view_html()
#' iris %>% df_kable() %>% view_html()
#' iris %>% df_kable(scroll = TRUE) %>% view_html()
#' 
#' iris %>% DT::datatable()
#' iris %>% df_datatable()
#'     
#' iris %>% head %>% print.data.frame()
#' iris %>% head %>% df_print()
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
  args <- c(list(x = x), list(...))
  args$format %<>% replace_null("html")
  args$digits %<>% replace_null(5)
  args$row.names %<>% replace_null(FALSE)
  # args$caption %<>% replace_null("A caption")
  args$format.args %<>% replace_null(list(big.mark = ","))
  df <- do.call(knitr::kable, args) %>%
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
      height = "300px",
      width = "100%",
      box_css = "border: 0px; padding: 0px"
    )
}

#' @rdname df-utils
#' @name df_datatable
#' @export
df_datatable <- function(x, ...) {
  args <- c(list(data = x), list(...))
  args$class %<>% replace_null("cell-border compact stripe")
  # args$escape %<>% replace_null(FALSE)
  args$filter %<>% replace_null("top")
  args$options %<>% replace_null(list())
  args$options$autoWidth %<>% replace_null(TRUE)
  args$options$columnDefs %<>% replace_null(list(list(className = "dt-center")))
  # dom
  # l = length changing control
  # f = filtering box
  # t = table
  # i = information summay
  # p = pagination control
  args$options$dom %<>% replace_null("lftip")
  # args$options$language %<>% replace_null(list(search = "Filter:"))
  args$options$lengthMenu %<>% replace_null(c(5, 10, 15, 20))
  args$options$pageLength %<>% replace_null(10)
  args$rownames %<>% replace_null(FALSE)
  # args$style %<>% replace_null("bootstrap")
  do.call(DT::datatable, args)
}

#' @rdname df-utils
#' @name df_print
#' @export
df_print <- function(x, ...) {
  args <- c(list(x = x), list(...))
  args$right %<>% replace_null(FALSE)
  args$row.names %<>% replace_null(FALSE)
  do.call(print.data.frame, args)
}