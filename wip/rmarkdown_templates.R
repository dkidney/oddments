
html_template = function(...){
    # dots = list(...)
    # args = replace(
    #     x = list(
    #         css = "rmarkdown/templates/html_template/template.css" %>%
    #             system.file(package = "oddments")
    #     ),
    #     list = names(dots),
    #     values = dots
    # )
    args = list(...)
    if(is.null(args$css)){
        args$css = "rmarkdown/templates/html_template/template.css" %>%
            system.file(package = "templates")
    }
    do.call(rmarkdown::html_document, args)
}

#' @title Insert a page break
#' @description Insert a page break in an html or pdf rmarkdown document
#' @param hrule ???
#' \preformatted{
#' ```{r, echo=FALSE, results="asis"}
#' insert_pagebreak()
#' ```
#' }
#'
#' @export

insert_pagebreak = function(hrule = FALSE){
    output = knitr::opts_knit$get("rmarkdown.pandoc.to")
    if(!is.null(output)){
        txt = output %>%
            switch(
                html  = "<P style='page-break-before: always'>",
                docx  = "##### pagebreak",
                latex = "\\newpage"
            )
        if(hrule){
            txt %<>% c("**********\n\n", .)
        }
        cat(txt)
    }
    invisible()
}