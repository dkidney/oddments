#' @rdname bin
#' @name bin
#' @title Alternative cut functions
#' @description
#'
#' \code{bin} is equivalent to \link[base]{cut} but uses alternative defaults:
#' \code{include.lowest = TRUE, right = FALSE, dig.lab = 5}. \cr
#'
#' \code{bin_pretty} uses \link[base]{pretty} to determine the cut points. \cr
#'
#' \code{bin_quantile} uses \link[stats]{quantile} to determine the cut points. It works
#' similarly to \link[ggplot2]{cut_number} but if there is insufficient information for
#' \code{n} cut points it tries to find a lower value of \code{n} that does not produce an
#' error. \cr
#'
#' @param x numeric vector
#' @param breaks a numeric vector of cut points or a single number giving the number of
#'   intervals
#' @param n number of intervals
#' @param explicit_na if \code{TRUE} then NAs after binning are converted to an explici
#'   factor levels using \link[forcats]{fct_explicit_na}
#' @param ... additional arguments to pass to \link[base]{cut}
#' @examples
#' \dontrun{
#'
#' df = data.frame(
#'   x = rnorm(1000),
#'   y = sample(1:4, 1000, TRUE),
#'   z = sample(LETTERS[1:4], 1000, TRUE)
#' )
#'
#' df %>% pip(bin(x, breaks = 5))
#' df %>% pip(bin_pretty(x))
#' df %>% pip(bin_quantile(x, n = 5))
#' df %>% pip(ggplot2::cut_number(x, n = 5)) # identical
#'
#' df %>% pip(bin(y, breaks = 5))
#' df %>% pip(bin_pretty(y))
#' df %>% pip(bin_quantile(y, n = 5))
#' df %>% pip(ggplot2::cut_number(y, n = 5)) # throws an error
#'
#' df %>% pip(bin(z, breaks = 5))
#' df %>% pip(bin_pretty(z))
#' df %>% pip(bin_quantile(z, n = 5))
#' }
#' @export
bin = function(x, breaks = 10, explicit_na = TRUE, ...){
    if(!inherits(x, c("numeric", "integer"))){
        warn("can only bin numeric or integer")
        return(x)
    }
    args = list(x = x, breaks = breaks, ...)
    if(is.null(args$include.lowest)) args$include.lowest = TRUE
    if(is.null(args$right)) args$right = FALSE
    if(is.null(args$dig.lab)) args$dig.lab = 5
    binned = do.call(cut, args)
    if(explicit_na) binned %<>% fct_explicit_na
    binned
}

#' @rdname bin
#' @name bin_pretty
#' @export
bin_pretty = function(x, n = 10, ...){
    if(!inherits(x, c("numeric", "integer"))){
        warn("can only bin numeric or integer")
        return(x)
    }
    x %>% bin(., pretty(., n = n), ...)
}

#' @rdname bin
#' @name bin_quantile
#' @export
bin_quantile = function(x, n = 10, ...){
    if(!inherits(x, c("numeric", "integer"))){
        warn("can only bin numeric or integer")
        return(x)
    }
    quants = x %>% quantile(seq(0, 1, length.out = n + 1), na.rm = TRUE) %>% unique
    n_quants = length(quants)
    if(n_quants < 2){
        warn("fewer than two unique quantiles - returning original x")
        return(x)
    }
    if(n_quants < (n + 1)){
        warn(str_c("can only compute ", n_quants - 1, " quantiles"))
    }
    x %>% bin(quants, ...)
}

