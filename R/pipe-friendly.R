#' Pipe-friendly wrapper for \code{do.call}
#' @description See documentation for \link[base]{do.call}.
# @inherit base::do.call
#' @inheritParams base::do.call
#' @param ... additional arguments to pass to do.call
#' @export
#' @examples
#' x = list(1, 2, 3, 4, 5)
#' 
#' # base function 
#' do.call(c, x)
#' 
#' # base function with built-in pipe  
#' x |> (\(x) do.call(c, x))()
#' 
#' # wrapper function with built-in pipe  
#' x |> do_call(c)
do_call = function(args, what, ...) {
  do.call(what, args, ...)
}
