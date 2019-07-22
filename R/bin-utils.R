#' @rdname bin-utils
#' @name bin-utils
#' @title Alternative cut functions
#' @description
#' \itemize{
#'     \item \code{bin()} - wraps \link[base]{cut} but uses alternative
#'     defaults: \code{include.lowest = TRUE, right = FALSE, dig.lab = 5}
#'     \item \code{bin_pretty()} - uses \link[base]{pretty} to determine the cut points
#'     \item \code{bin_quantile()} - uses \link[stats]{quantile} to determine the cut
#'     points. It works similarly to \link[ggplot2]{cut_number} but uses unique quantiles
#'     (and is therefore less prone to error than \link[ggplot2]{cut_number}). If class
#'     other than integer or numeric is supplied then a warning is given and the original
#'     variable is returned unchanged.
#' }
#' @param x numeric vector
#' @param breaks a numeric vector of cut points or a single number giving the number of
#'   intervals
#' @param n number of intervals
#' @param ... additional arguments to pass to \link[base]{cut}
#' @examples
#' \dontrun{
#' 
#' library(tidyverse)
#' theme_set(theme_oddments())
#' 
#' df <- tibble(
#'   x = rnorm(1000),
#'   y = sample(1:4, 1000, TRUE),
#'   z = sample(LETTERS[1:4], 1000, TRUE)
#' )
#'
#' df %>% ggplot + geom_bar(aes(bin(x, breaks = 5)))
#' df %>% ggplot + geom_bar(aes(bin_pretty(x)))
#' df %>% ggplot + geom_bar(aes(cut_number(x, n = 5))) 
#' df %>% ggplot + geom_bar(aes(bin_quantile(x, n = 5))) # identical to cut_number
#'
#' df %>% ggplot + geom_bar(aes(bin(y, breaks = 5)))
#' df %>% ggplot + geom_bar(aes(bin_pretty(y)))
#' df %>% ggplot + geom_bar(aes(cut_number(y, n = 5))) # throws an error
#' df %>% ggplot + geom_bar(aes(bin_quantile(y, n = 5))) # calculates fewer bins
#'
#' df %>% ggplot + geom_bar(aes(bin(z, breaks = 5)))
#' df %>% ggplot + geom_bar(aes(bin_pretty(z)))
#' df %>% ggplot + geom_bar(aes(cut_number(z, n = 5))) # throws an error
#' df %>% ggplot + geom_bar(aes(bin_quantile(z, n = 5)))
#' }
NULL

#' @rdname bin-utils
#' @name bin
#' @export
#' @param explicit_na if \code{TRUE} then NAs after binning are converted to an explici
#'   factor levels using \link[forcats]{fct_explicit_na}
bin <- function(x, breaks = 10, explicit_na = TRUE, ...) {
  if (!inherits(x, c("numeric", "integer"))) {
    warning("can only bin numeric or integer")
    return(x)
  }
  args <- list(x = x, breaks = breaks, ...)
  if (is.null(args$include.lowest)) args$include.lowest <- TRUE
  if (is.null(args$right)) args$right <- FALSE
  if (is.null(args$dig.lab)) args$dig.lab <- 5
  binned <- do.call(base::cut, args)
  if (explicit_na) binned %<>% forcats::fct_explicit_na()
  binned
}

#' @rdname bin-utils
#' @name bin_pretty
#' @export
bin_pretty <- function(x, n = 10, ...) {
  if (!inherits(x, c("numeric", "integer"))) {
    warning("can only bin numeric or integer")
    return(x)
  }
  x %>% bin(., pretty(., n = n), ...)
}

#' @rdname bin-utils
#' @name bin_quantile
#' @export
bin_quantile <- function(x, n = 10, ...) {
  if (!inherits(x, c("numeric", "integer"))) {
    warning("can only bin numeric or integer")
    return(x)
  }
  quants <- x %>%
    stats::quantile(seq(0, 1, length.out = n + 1), na.rm = TRUE) %>%
    unique()
  n_quants <- length(quants)
  if (n_quants < 2) {
    warning("fewer than two unique quantiles - returning original x")
    return(x)
  }
  if (n_quants < (n + 1)) {
    warning(str_c("can only compute ", n_quants - 1, " quantiles"))
  }
  x %>% bin(quants, ...)
}
