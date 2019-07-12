#' @rdname function-utils
#' @name function-utils
#' @title Function utils
#' @description
#' \itemize{
#'   \item \code{capture_args()} - returns a list of all argument values supplied to the
#'   caller function, including the default argument values that were not specified in the
#'   call plus and any additional arguments passed via \link[base]{dots}.
#'   (\link[base]{match.call} ignores default arguments)
#' }
NULL

#' @rdname function-utils
#' @name capture_args
#' @examples
#' \dontrun{
#' 
#' f <- function(a = 1, b = 2, c = 3, ...) as.list(base::match.call()[-1])
#' g <- function(a = 1, b = 2, c = 3, ...) oddments::capture_args()
#'
#' f(b = 22, d = 44, e = 55)
#' g(b = 22, d = 44, e = 55)
#'
#' f(b = 22, 11, e = 55)
#' g(b = 22, 11, e = 55)
#'
#' f(a = 11, b = 22, c = 33, 44)
#' g(a = 11, b = 22, c = 33, 44)
#' }
capture_args <- function() {
    sf <- sys.function(sys.parent())
    sc <- sys.call(sys.parent())
    args <- as.list(match.call(sf, sc, expand.dots = TRUE))[-1]
    defs <- formals(sf)
    defs[["..."]] <- NULL
    arg_names <- names(args)
    def_names <- names(defs)
    dot_names <- setdiff(arg_names, def_names)
    defs <- defs[setdiff(def_names, arg_names)]
    args <- c(args, defs)
    args[c(def_names, dot_names)]
}

# TODO - needs more work...
safely_and_quietly <- function(.f, otherwise = NULL, quiet = TRUE) {
    purrr::safely(quietly(.f), otherwise = NULL, quiet = TRUE)
}
