
#' @title Convert colours to hexadecimal format
#' @description Convert a vector of colours to hexadecimal format.
#' @param x vector of colours value (character or integer)
#' @export
#' @examples
#' col2hex("blue")
#' col2hex(4)
#' col2hex("4")
#' col2hex(c(4,"blue"))

col2hex = function(x){
    grDevices::col2rgb(x) %>%
        apply(2, function(x){
            grDevices::rgb(x[1], x[2], x[3], maxColorValue = 255)
        })
}

# -------------------------------------------------------------------------------------- #

#' @title Replace null values
#' @description Replace \code{NULL} values with a default value. Wrapper for
#'   \code{rlang::\link[rlang]{\%||\%}}.
#' @param x input value
#' @param replacement default replacement value if \code{x} is \code{NULL}
#' @export
#' @examples
#' x = NULL
#' y = 9
#' x %>% replace_null(10)
#' y %>% replace_null(10)

replace_null = function(x, replacement = NULL){
    rlang::`%||%`(x, replacement)
}

# -------------------------------------------------------------------------------------- #

#' @title Round digits and convert to character
#' @description A pipe-friendly wrapper for a specific use case of \link[base]{sprintf}.
#' @param x numeric vector
#' @param digits integer indicating the number of decimal places
#' @export
#' @examples
#' sprintf("%.3f", pi)
#' pi %>% round_chr(3)

round_chr = function(x, digits = 0){
    sprintf(paste0("%.", digits, "f"), x)
}

# -------------------------------------------------------------------------------------- #

#' @title View html
#' @description View html code in RStudio viewer pane (requires \pkg{rstudioapi} to be
#'   installed to access \code{rstudioapi::\link[rstudioapi]{viewer}})
#' @param x object representing html code (e.g. a string)
#' @export
#' @examples
#' \dontrun{
#'
#' mtcars %>%
#'   head %>%
#'   knitr::kable(format = "html") %>%
#'   view_html
#' }

view_html = function(x){
    if(!interactive()) return(invisible())
    if(requireNamespace("rstudioapi", quietly = TRUE)){
        file_path = tempfile(fileext = ".html")
        writeLines(as.character(x), file_path)
        rstudioapi::viewer(file_path)
    }
}

# -------------------------------------------------------------------------------------- #

