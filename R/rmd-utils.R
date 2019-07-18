
#' @rdname rmd-utils
#' @name rmd_utils
#' @title Rmarkdown utility functions
#' @description
#' \itemize{
#'   \item \code{rmd_html_template()} -
#'   \item \code{rmd_insert_pagebreak()} - inserts a page break in an html, pdf or docx
#'   rmarkdown document
#' }
#' @details
#' \preformatted{
#' ```{r, echo=FALSE, results="asis"}
#' rmd_insert_pagebreak()
#' ```
#' }
#' @examples
#' \dontrun{
#'
#' # TODO
#' }
NULL

#' @rdname rmd-utils
#' @name rmd_utils
#' @export
rmd_html_template <- function(...) {
  args <- list(...)
  args$css %<>%
    replace_null(
      "rmarkdown/templates/html_template/resources/html_template.css" %>%
        system.file(package = "oddments")
    )
  do.call(rmarkdown::html_document, args)
}

#' @rdname rmd-utils
#' @name rmd_utils
#' @export
# @param hrule TODO
rmd_insert_pagebreak <- function() {
  output <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (!is.null(output)) {
    txt <- output %>%
      switch(
        html = "<P style='page-break-before: always'>",
        docx = "##### pagebreak",
        latex = "\\newpage"
      )
    # if (hrule) {
    #   txt %<>% c("****************************************\n\n", .)
    # }
    cat(txt)
  }
  invisible()
}
