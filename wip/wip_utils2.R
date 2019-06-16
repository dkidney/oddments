
#' @rdname data_frame-methods
#' @name print_data_frame
#' @title Data frame methods
#' @description Alternative functions to standard data frame methods.
#'
#' \code{print_data_frame} is similar to \link[base]{print.data.frame}, but by default the
#' \code{row.names} and \code{right} arguments are both set to \code{FALSE}.
#' \code{print_data_frame} also has two additional arguments, \code{col.names} and
#' \code{nmax}.
#'
#' \code{split_data_frame} is similar to \link[base]{split.data.frame} but the \code{f}
#' argument is used to specify a column name in \code{x}.
#'
# does this work with SQLite?

#' @param x object of class data.frame
#' @param col.names logical indicating whether column names should be printed
#' @param nmax number of rows to show
#' @param f name of factor variable to split by
#' @param reorder logical indicating whether the list of data.frames should be reordered
#'   with respect to the values of \code{f}
#' @param ... additional arguments to pass to base method
#' @export
#' @examples
#' \dontrun{
#'
#' library(oddments)
#'
#' mtcars %>% print.data.frame
#' mtcars %>% print_data_frame
#' mtcars %>% print_data_frame(col.names = FALSE)
#' mtcars %>% print_data_frame(n = Inf)
#'
#' mtcars %>% split.data.frame(mtcars$gear)
#' mtcars %>% split_data_frame("gear")
#'
#'}
print_data_frame = function(x, col.names = TRUE, nmax = 10, ...){
    args = list(...)
    if(nmax < Inf){
        args$x = x %>% slice(1:nmax)
    }else{
        args$x = x
    }
    args$x %<>% collect
    args$right %<>% replace_null(FALSE)
    args$row.names %<>% replace_null(FALSE)
    out = capture.output({args %>% do_call(print.data.frame)})
    if(!col.names) out = out[-1]
    writeLines(out)
}


#' @rdname data_frame-methods
#' @name print_data_frame
#' @export
split_data_frame = function(x, f, reorder = FALSE, ...){
    if(!has_name(x, f)) stop(f, " is not a column in x")
    dots = list(...)
    dots$drop %<>% replace_null(FALSE)
    fvals = x %>%
        select(one_of(f)) %>%
        distinct %>%
        collect %>%
        pluck(f)
    if(reorder){
        fvals = fvals[order(fvals)]
    }
    cols = colnames(x)
    if(dots$drop){
        cols %<>% discard(. %in% f)
    }
    fvals %>%
        set_names(., .) %>%
        map(function(fval){
            x %>%
                select(one_of(cols)) %>%
                filter(.data[[!!f]] == !!fval)
        })
}

# ---------------------------------------------------------------------------- #

#' @title Convert unix time to datetime
#' @description Wrapper for \link[base]{as.POSIXct.numeric} using \code{origin =
#'   "1970-01-01"}. Assumes units are in seconds.
#'
#' \url{https://en.wikipedia.org/wiki/Unix_time}
#'
#' \url{https://www.epochconverter.com}
#' @param x numeric vector
#' @param tz time zone
#' @export
#' @examples
#' \dontrun{
#'
#' convert_unix_time(0)
#' convert_unix_time(1000000000)
#' convert_unix_time(1e9)
#'
#' tz = "Europe/London"
#' lubridate::now(tz) %>% as.numeric %>% convert_unix_time(tz)
#' }
convert_unix_time = function(x, tz = "Europe/London"){
    as.POSIXct.numeric(x, tz = tz, origin = "1970-01-01")
}

# ---------------------------------------------------------------------------- #

#' @title Replace null values
#' @description Replace \code{NULL} values with a default value.
#' @param x input value
#' @param replacement default replacement value if \code{x} is \code{NULL}
#' @export
#' @examples
#' \dontrun{
#'
#' library(oddments)
#'
#' x = NULL
#' y = 9
#' x %>% replace_null(10)
#' y %>% replace_null(10)
#'}
replace_null = function(x, replacement = NULL){
    x %||% replacement
}

# ---------------------------------------------------------------------------- #

#' @title Round digits and convert to character
#' @description Wrapper for \link[base]{sprintf}.
#' @param x numeric vector
#' @param digits integer indicating the number of decimal places
#' @export
round_chr = function(x, digits = 0){
    sprintf(str_c("%.", digits, "f"), x)
}

# ---------------------------------------------------------------------------- #

#' @title Convert colours to hexadecimal format
#' @description Convert a vector of colours to hexadecimal format.
#' @param x vector of colors value (character or integer)
#' @export
#' @examples
#' col2hex("blue")
#' col2hex(4)
#' col2hex(c(4,"blue"))
col2hex = function(x){
    col2rgb(x) %>%
        apply(2, function(x){
            rgb(x[1], x[2], x[3], maxColorValue = 255)
        })
}

# ---------------------------------------------------------------------------- #

mean_na = function(x) mean(is.na(x))

sum_na = function(x) sum(is.na(x))

any_na = function(x) any(is.na(x))

all_na = function(x) all(is.na(x))

not_any_na = function(x) !any(is.na(x))

not_all_na = function(x) !all(is.na(x))

is_number = function(x) inherits(x, c("integer", "numeric"))

check_is_scalar_character = function(x){
    if(!is_scalar_character(x)){
        stop(substitute(x), " should be a character scalar")
    }
    invisible(x)
}

check_is_scalar_logical = function(x){
    if(!is_scalar_logical(x)){
        stop(substitute(x), " should be a logical scalar")
    }
    invisible(x)
}

check_is_scalar_double = function(x){
    if(!is_scalar_double(x)){
        stop(substitute(x), " should be a numeric scalar")
    }
    invisible(x)
}

check_is_scalar_integer = function(x){
    if(!is_scalar_integer(x)){
        stop(substitute(x), " should be a integer scalar")
    }
    invisible(x)
}

is_square_matrix = function(x){
    inherits(x, 'matrix') && nrow(x) == ncol(x)
}

# extract the upper triangular part of a square matrix
# other elements are set to NA
# x = matrix(1:9, 3, 3) ; x
# upper_tri(x)
# lower_tri(x)
upper_tri = function(x){
    if(!is_square_matrix(x)) stop("expecting a square matrix")
    x[!upper.tri(x)] = NA
    return(x)
}

# extract the upper triangular part of a square matrix
# other elements are set to NA
lower_tri = function(x){
    if(!is_square_matrix(x)) stop("expecting a square matrix")
    x[!lower.tri(x)] = NA
    return(x)
}

logit = stats::qlogis

inv_logit = stats::plogis

print_microbench = function(x, ...){
    dots = list(
        x = x %>%
            summary %>%
            mutate(relative = .data$mean / min(.data$mean)) %>%
            arrange(.data$relative) %>%
            select(.data$expr, .data$relative, .data$mean, times = .data$neval),
        ...
    )
    dots$digits %<>% replace_null(getOption("digits"))
    dots$row.names %<>% replace_null(FALSE)
    dots %>% do_call(print.data.frame)
}

# ---------------------------------------------------------------------------- #

#' @title Execute a Function Call
#' @description Similar to \link[base]{do.call} but with the args as first arguemtn and
#'   what as second argument making it more convenient for use in piped.
#' @param what either a function or a non-empty character string naming the function to be called.
#' @param args a list of arguments to the function call. The names attribute of args gives the argument names.
#' @param quote	a logical value indicating whether to quote the arguments.
#' @param envir	an environment within which to evaluate the call. This will be most useful if what is a character string and the arguments are symbols or quoted expressions.
#' @export
do_call = function(args, what, quote = FALSE, envir = parent.frame()){
    do.call(what = what, args = args, quote = quote, envir = envir)
}

# ---------------------------------------------------------------------------- #

parse_escaped_unicode = function(x){
    eval(parse(text = str_c('"', x, '"')))
}

hex_to_utf8 = function(x){
    x %>%
        map(charToRaw) %>%
        map_chr(intToUtf8)
}

# ---------------------------------------------------------------------------- #

clear_warnings = function(){
    assign("last.warning", NULL, envir = baseenv())
}

# md5_hash = function(x, key = NULL, dashes = TRUE){
#     hashed = x %>% openssl::md5(key)
#     if(dashes) hashed %<>% str_replace("(.{8})(.{4})(.{4})(.{4})(.{12})$","\\1-\\2-\\3-\\4-\\5")
#     hashed
# }

# rebuild = function(source_dir = getwd(), reload = NULL){
#     source_dir %<>% replace_null(CIdata_source_dir) %>% check_file_exists
#     pkg = source_dir %>% basename
#     bullet("rebuilding ", pkg, "...")
#     reload %<>% replace_null(str_c("package:", pkg) %in% search())
#     check_is_scalar_logical(reload)
#     source_dir %>% devtools::install(reload = reload, quiet = TRUE)
#     invisible()
# }

# ---------------------------------------------------------------------------- #

#' @rdname cut
#' @name cut2
#' @title Alternative cut functions
#' @description
#' \code{cut2} is similar to \link[base]{cut}, but by default the \code{include.lowest}
#' argument is set to \code{TRUE} and the \code{right} argument is set to
#' \code{FALSE}.
#'
#' @param x a numeric vector which is to be converted to a factor by cutting
#' @param breaks either the number of breaks or a vector of two or more unique cut points
#' @param lower lower boundary
#' @param upper upper boundary
#' @param nbreaks number of breaks
#' @param relabel logical indicating whether generic labels should be used
#' @param step interval width
#' @param ... additional arguments to pass to \link[base]{cut}.
#' @examples
#' \dontrun{
#'
#' library(oddments)
#' library(ggplot2)
#'
#' x = rnorm(1000)
#' nbreaks = 4
#' peek = function(x) x %>% table(useNA = "ifany") %>% barplot
#'
#' cut(x, nbreaks) %>% peek
#' cut(x, quantile(x, seq(0, 1, length.out = nbreaks + 1))) %>% peek
#' cut_interval(x, nbreaks) %>% peek
#' cut_number(x, nbreaks) %>% peek
#' cut_width(x, 2) %>% peek
#' cut_width(x, 2, center = 1, closed = "right") %>% peek
#'
#' cut2(x, nbreaks) %>% peek
#' cut2(x, c(-1, 0, 1)) %>% peek
#' cut2(x, c(-1, 0, 1), lower = -4) %>% peek
#' cut_quantile(x, nbreaks) %>% peek
#' cut_quantile(x, nbreaks, relabel = FALSE) %>% peek
#' cut_step(x, 1) %>% peek
#' cut_pretty(x) %>% peek
#' }
#' @export
cut2 = function(x, breaks = 2, lower = -Inf, upper = Inf, ...){
    args = list(x = x, ...)
    args$right %<>% replace_null(FALSE)
    args$dig.lab %<>% replace_null(4)
    if(length(breaks) == 1){
        FUN = cut_interval
        args$n = breaks
    }else{
        FUN = cut
        args$breaks = breaks
        if(min(args$breaks) > min(x, na.rm = TRUE)) args$breaks %<>% c(lower, .)
        if(max(args$breaks) < max(x, na.rm = TRUE)) args$breaks %<>% c(upper)
        args$include.lowest %<>% replace_null(TRUE)
    }
    args %>% do_call(FUN)
}

#' @rdname cut
#' @name cut_quantile
#' @export
cut_quantile = function(x, nbreaks = 2, relabel = TRUE, ...){
    args = list(x = x, ...)
    args$breaks = args$x %>%
        quantile(seq(0, 1, length.out = nbreaks + 1), na.rm = TRUE, type = 3) %>%
        discard(duplicated(.))
    y = args %>% do_call(cut2)
    if(relabel){
        levels(y) = str_c(
            names(args$breaks)[1:(length(args$breaks)-1)],
            "-",
            names(args$breaks)[-1]
        )
    }
    y
}

#' @rdname cut
#' @name cut_round
#' @export
cut_step = function(x, step = 10, ...){
    dots = list(...)
    dots$right %<>% replace_null(FALSE)
    args = list(
        x = x,
        width = step,
        center = step / 2,
        closed = if(dots$right) "right" else "left",
        ...
    )
    args %>% do_call(cut_width)
}

#' @rdname cut
#' @name cut_pretty
#' @export
cut_pretty = function(x, ...){
    args = list(x = x, breaks = pretty(x), ...)
    args %>% do_call(cut2)
}


n_missing = function(x){
    sum(is.na(x))
}

is_binary = function(x){
    (1 %in% x) &&
        (0 %in% x) &&
        (all(x == 1 | x == 0, na.rm = TRUE))
}


