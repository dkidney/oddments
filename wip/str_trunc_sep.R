
#' @title Truncate a character string at a separation character
#' @description A modified version of \link[stringr]{str_trunc} that right truncates a
#'   string to the nearest separation character, such that the number of characters before
#'   the ellipsis does not exceed the stated width.
#' @param string a character vector
#' @param width maximum width of string
#' @param sep separation character
#' @param ellipsis content of ellipsis that indicates content has been removed
#' @export
#' @examples
#' string = c(
#'   'one,two,three,four,five',
#'   'red,orange,yellow,green.blue,indigo,violet'
#' )
#' string %>% str_trunc_sep(1)
#' string %>% str_trunc_sep(5)
#' string %>% str_trunc_sep(20)
#' string %>% str_trunc_sep(100)

str_trunc_sep = function(string, width = 20, sep = ",", ellipsis = "..."){
    string %>% sapply(function(str){
        total = str %>% nchar
        nchars = str %>% str_locate_all(sep) %>% extract2(1) %>% `[`(,1) %>% c(total)
        limit = nchars %>% `[`(. <= width)
        if(length(limit) == 0) return(ellipsis)
        limit %<>% max
        str %<>% str_sub(1, limit)
        if(limit < total) str %<>% paste0(ellipsis)
        return(str)
    }) %>% unname
}
