
#' @title Caller function argument values
#' @description Get a list of all argument values for the caller function, including
#'   default argument values and any additional arguments passed via \link[base]{dots}.
#' @details I made this function because \link[base]{match.call} doesn't return default
#'   argument values and I couldn't find an alternative that did in any other package.
#' @export
#' @examples
#' \dontrun{
#'
#' h = function(a = 1, b = 2, c = 3, ...) caller_fun_args()
#' h(b = 22, d = 44, e = 55)
#' }

caller_fun_args = function(){
    sf = sys.function(sys.parent())
    sc = sys.call(sys.parent())
    args = as.list(match.call(sf, sc, expand.dots = TRUE))[-1]
    defs = formals(sf)
    defs[["..."]] = NULL
    arg_names = names(args)
    def_names = names(defs)
    dot_names = setdiff(arg_names, def_names)
    defs = defs[setdiff(def_names, arg_names)]
    args = c(args, defs)
    args[c(def_names, dot_names)]
}



