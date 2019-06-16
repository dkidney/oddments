

# path of active file
path_active = function(){
    rstudioapi::getActiveDocumentContext()$path
}


format_int = function(x){
    check_is_intish(x)
    format(floor(as.numeric(x)), scientific= FALSE, trim = TRUE)
}

# ---------------------------------------------------------------------------- #

#' @title Print exact decimal places
#' @description Print a value to the exact number of decimal places required. This function is
#'   a wrapper for \link[base]{sprintf}.
#' @param x a numeric vector
#' @param digits the number of decimal places required
#' @export
#' @examples
#' 1 %>% round(3)
#' 1 %>% round_chr(3)
round_chr = function(x, digits = 0){
    i = is.na(x)
    out = sprintf(str_c("%.", digits, "f"), x)
    out[i] = NA_character_
    out
}


# ---------------------------------------------------------------------------- #

#' @rdname tri
#' @name lower_tri
#' @title Get the lower or upper triangle of a matrix.
#' @description Convert the upper/lower elements of a square matrix to NA.
#' @param x a square matrix
#' @export
#' @examples
#' x = matrix(1:16, 4)
#' x %>% lower_tri
#' x %>% upper_tri
lower_tri = function(x){
    if(!(inherits(x, 'matrix') && nrow(x) == ncol(x)))
        stop("expecting a square matrix")
    x[!lower.tri(x)] = NA
    return(x)
}

#' @rdname tri
#' @name lower_tri
#' @export
upper_tri = function(x){
    if(!(inherits(x, 'matrix') && nrow(x) == ncol(x)))
        stop("expecting a square matrix")
    x[!upper.tri(x)] = NA
    return(x)
}


# ---------------------------------------------------------------------------- #

mean_na = function(x){
    mean(is.na(x))
}

sum_na = function(x){
    sum(is.na(x))
}

prop_na = function(x){
    mean(is.na(x))
}

any_na = function(x){
    any(is.na(x))
}

all_na = function(x){
    all(is.na(x))
}

not_any_na = function(x){
    !anyNA(x)
}

not_all_na = function(x){
    !all(is.na(x))
}

is_square_matrix = function(x){
    inherits(x, 'matrix') && nrow(x) == ncol(x)
}

get_hostname = function(){
    Sys.info()["nodename"] %>% unname
}

is_macbook = function(){
    get_hostname() %>% str_detect("MacBook")
}


# ---------------------------------------------------------------------------- #

isFALSE = function(x){
    identical(FALSE, x)
}

# ---------------------------------------------------------------------------- #

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


#' @export
split_data_frame2 = function(x, column, drop = FALSE, reorder = FALSE){
    f = x[[column]]
    if(!is.factor(f)){
        f %<>% as.factor
        if(!reorder) f %<>% fct_inorder
    }
    if(drop) x %<>% select(-one_of(!!column))
    x %>% split(f)
}


#' @export
print_data_frame2 = function(x, ..., digits = NULL, quote = FALSE, right = FALSE,
                            row.names = FALSE){
    print.data.frame(x, digits = digits, quote = quote, right = right,
                     row.names = row.names, ...)
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

f_chr = function(f){
    f %<>% as.formula %>% as.character
    if(length(f) == 2){
        str_c("~ ", f[2])
    }else{
        str_c(f[2], " ~ ", f[3])
    }
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


rebuild = function(source_dir = NULL, reload = NULL){
    source_dir %<>% replace_null(CIdata_source_dir) %>% check_file_exists
    pkg = source_dir %>% basename
    bullet("rebuilding ", pkg, "...")
    reload %<>% replace_null(str_c("package:", pkg) %in% search())
    check_is_scalar_logical(reload)
    source_dir %>% devtools::install(reload = reload, quiet = TRUE)
    invisible()
}


# ---------------------------------------------------------------------------- #

cut2 = function(x, breaks = NULL, relabel = FALSE, ...){
    dots = list(x = x, breaks = breaks, ...)
    dots$breaks %<>% replace_null(2)
    dots$include.lowest %<>% replace_null(TRUE)
    dots$right %<>% replace_null(FALSE)
    y = do.call(cut, dots)
    if(relabel && length(breaks) > 1){
        levels(y) %<>%
            str_remove("\\[") %>%
            str_remove("\\)") %>%
            str_split(",") %>%
            map_chr(first) %>%
            str_pad(max(nchar(.)), "left", "0")
    }
    y
}

cut3 = function(x, breaks = 2, lower = -Inf, upper = Inf, ...){
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

cut_quantile = function(x, quantiles = 0.5, ...){
    if(length(quantiles) == 1){
        if(quantiles %in% 0:1) stop("inbvalid quantiles")
        if(quantiles > 1) quantiles = seq(0, 1, length.out = quantiles + 1)
    }
    quantiles %<>% sort %>% unique
    if(min(quantiles) < 0) stop("quantiles cant be less than zero")
    if(max(quantiles) > 1) stop("quantiles cant be greater than one")
    if(min(quantiles) > 0) quantiles %<>% c(0, .)
    if(max(quantiles) < 1) quantiles %<>% c(1)
    x %>%
        quantile(probs = quantiles, type = 3) %>%
        cut2
}

cut_quantile2 = function(x, nbreaks = 2, relabel = TRUE, ...){
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

is_binary = function(x){
    (1 %in% x) &&
        (0 %in% x) &&
        (all(x == 1 | x == 0, na.rm = TRUE))
}

if(0){
    x = runif(20, 0, 10) %>% c(0:10) %>% sort
    data_frame(x, x %>% cut(2)) %>% print(n = Inf)
    data_frame(x, x %>% cut2(2)) %>% print(n = Inf)
    data_frame(x, x %>% cut(0:10)) %>% print(n = Inf)
    data_frame(x, x %>% cut2(0:10)) %>% print(n = Inf)
    data_frame(x, x %>% cut2(c(0:5,Inf))) %>% print(n = Inf)
    data_frame(x, x %>% cut_quantile) %>% print(n = Inf)
    data_frame(x, x %>% cut_quantile(3)) %>% print(n = Inf)
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

convert_unix_time = function(x, tz = "Europe/London"){
    as.POSIXct.numeric(x, tz = tz, origin = "1970-01-01")
}



#' @title Numeric range
#' @description Shorthand function for calculating the difference between the minimum and
#'   maximum of a numeric vector.
#' @param x a numeric vector
#' @export
#' @examples
#' library(magrittr)
#'
#' x = matrix(rnorm(100), 10, 10) %>% as.data.frame
#'
#' x %>% sapply(diff_range)
#'
diff_range = function(x) x %>% range %>% diff



