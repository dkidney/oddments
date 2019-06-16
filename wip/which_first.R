
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
