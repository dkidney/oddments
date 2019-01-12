
#' @title Check R version
#' @description Check to see if a later R version is available for download.
#' @export

check_r_version = function(){
    if(interactive()){
        current_version = getRversion()
        message("Using R-", current_version)
        sysname = Sys.info()["sysname"]
        if(!sysname %in% c("Darwin", "Windows")){
            message("cant check available R version for ", sysname)
            return(invisible())
        }
        available_version = try(suppressWarnings({
            url = "https://cran.r-project.org/bin/"
            if(sysname == "Darwin") url = str_c(url, "macosx")
            if(sysname == "Windows") url = str_c(url, "windows/base")
            pattern = "R-[0-9]+\\.[0-9]+\\.[0-9]+"
            url %>%
                readLines(warn = FALSE) %>%
                keep(str_detect(., pattern)) %>%
                first %>%
                str_extract(pattern) %>%
                str_remove("R-") %>%
                numeric_version
        }), TRUE)
        if(inherits(available_version, "try-error")){
            message("no internet connection")
        }
        if(current_version < available_version){
            message("R-", available_version, " now available")
        }
    }
}

# -------------------------------------------------------------------------------------- #

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

#' @title Stop if condition is TRUE
#' @description This is intended to be the inverse of \link[base]{stopifnot}.
#' @param expr an expression representing a condition to be tested - if it evaluates to
#'   \code{TRUE} then an error will be thrown.
#' @export
#' @examples
#' \dontrun{
#'
#' x = "abc"
#' stop_if(nchar(x) >= 4)
#' stop_if(nchar(x) <= 3)
#'
#' }

stop_if = function(expr){
    expr = rlang::enexpr(expr)
    result = rlang::eval_bare(expr)
    if(isTRUE(result)){
        stop(rlang::expr_deparse(expr), call. = FALSE)
    }
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






