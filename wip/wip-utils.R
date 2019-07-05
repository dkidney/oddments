
#' Source all the .R files in a folder
#'
#' Loads one or more packages into the current session. Any packages that aren't already installed will be automatically downloaded and then loaded into the current session.
#'
#' @param folder.path the path of the folder in which the .R files are stored
#' @export

source.folder <- function(folder.path){

    # make a character vector with the names of all the files in the folder
    all.files <- list.files(folder.path)

    # extract only those files with the .r extension
    r.files <- all.files[grep("\\.[r,R]$", all.files)]

    # source all the .r files
    for(file in r.files) source(file.path(folder.path, file))

}
#
#' @rdname which_first
#' @name which_first
#' @title First or last match in a vector
#' @description Compute the first or last \code{TRUE} or \code{FALSE} element in a logical
#'   vector.
#' @param x logical vector
#' @param what logical scalar
#' @return Returns an integer scalar representing the position of the first/last match in
#'   \code{x}. If there are no matches, then \code{NA_integer_} is returned.
#' @examples
#' x = c(NA,T,T,F,F,F,T)
#'
#' which_first(x, TRUE)
#' which_first(x, FALSE)
#' which_last(x, TRUE)
#' which_last(x, FALSE)
#'
#' y = c(NA,T,T,NA,NA,T)
#'
#' which_first(y, TRUE)
#' which_first(y, FALSE)
#' which_last(y, TRUE)
#' which_last(y, FALSE)
#' @export

which_first = function(x, what = TRUE){
    if(!is_logical(x)) stop("x must be a logical vector")
    if(!is_scalar_logical(what)) stop("what must be a logical scalar")
    if(!any(x == what, na.rm = TRUE)){
        warning("no matches found for what=", what, " in x")
        return(NA_integer_)
    }
    if(what){
        min(which(x))
    }else{
        min(which(!x))
    }
}

#' @rdname which_first
#' @name which_last
#' @export

which_last = function(x, what = TRUE){
    length(x) - which_first(rev(x), what = what)
}

#' Inverse square root of a matrix
#'
#' Description.
#'
#' Details.
#'
#' @param x a square matrix
#' @return Returns a matrix A^(-1/2)
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' # Example:
#'
#' A = matrix(c(2,-1,0,-1,2,-1,0,-1,2), nc=3) ; A
#'
#' inv.sqrt.mat(A)
#'
#' @export

inv.sqrt.mat <- function(x){

    A.eigen <- eigen(as.matrix(A))

    if(all(A.eigen$values>0)){

        A.sqrt <- A.eigen$vectors %*% ( diag(sqrt(A.eigen$values)) %*% t(A.eigen$vectors) )

        } else {

            stop("Matrix must be positive semi-definite")

        }

    return(solve(A.sqrt))

}

my.model.matrix = function(formula, data){

    terms = colnames(attr(terms(formula), "factors")) ; terms

    X = sapply(terms, function(i){ # i = terms[3] ; i

        if(grepl(":", i)){

            j = strsplit(i, ":")[[1]] ; j

            apply(data[,j],1,prod)

        }else{

            data[,terms == i]

        }

    })

    colnames(X) = terms

    return(X)

}

if(0){

    form = ~ x * z

    n = 100000

    data = data.frame(a = 1:n, b = 1:n, c = 1:n, x = 1:n, z = n:1)

    X = model.matrix(form, data) ; head(X) ; dim(X)

    X = my.model.matrix(form, data) ; head(X) ; dim(X)



}

num2col = function(x, ncol = 10, palette = heat.colors){
    breaks = seq(min(x), max(x), length = ncol + 1)
    i = cut(x, breaks = breaks, include.lowest = TRUE)
    palette(ncol)[i]
}


if(0){
    data = expand.grid(x = seq(-2, 2, length = 20),
                       y = seq(-2, 2, length = 20))
    data$z = dnorm(data$x) * dnorm(data$y)
    plot(data$x, data$y, col = num2col(data$z), pch = 15, cex = 2)
}


# path of active file
path_active = function(){
    rstudioapi::getActiveDocumentContext()$path
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

get_hostname = function(){
    Sys.info()["nodename"] %>% unname
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


