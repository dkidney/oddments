#' @rdname binary
#' @name is_binary
#' @title ???
#' @description ???
#' @param x ???
#' @param strict ???
#' @param verbose ???
#' @export
#' @examples
#' \dontrun{
#'
#' n = 100
#' a = sample(c(0L, 1L), n, replace = TRUE)
#' b = sample(c(0, 1), n, replace = TRUE)
#' c = b == 1
#' d = as.factor(b)
#' e = as.character(b)
#'
#' is_binary(a)
#' is_binary(b)
#' is_binary(c)
#' is_binary(d)
#' is_binary(e)
#'
#' is_binary(b, strict = FALSE)
#' is_binary(c, strict = FALSE)
#' is_binary(d, strict = FALSE)
#' is_binary(e, strict = FALSE)
#' }
is_binary = function(x, strict = TRUE, verbose = TRUE){
    type = type_sum(x)
    if(type %in% c("int", "dbl", "lgl")){
        if(type == "int"){
            out = n_distinct(x, na.rm = TRUE) == 2 &&
                all(x %in% c(0L, 1L), na.rm = TRUE)
        }
        if(type == "dbl"){
            if(strict){
                out = FALSE
            }else{
                out = n_distinct(x, na.rm = TRUE) == 2 &&
                    all(x %in% c(0, 1), na.rm = TRUE)
            }
            if(verbose) message("type dbl")
        }
        if(type == "lgl"){
            out = !strict
            if(verbose) message("type lgl")
        }
    }else{
        out = FALSE
        if(verbose) message("type ", type_sum(x))
    }
    out
}

# @rdname binary
# @name as_binary
# @export
# as_binary = function(x){
#     type = type_sum(x)
#     if(type %in% c("int", "dbl", "lgl", "chr", "fctr")){
#         if(is_binary(x)) return(x)
#         if(is_binary(x, strict = FALSE, verbose = FALSE)){
#             return(as.integer(x))
#         }else{
#
#         }
#
#         n_distinct(x, na.rm = TRUE)
#
#
#         if(type )
#
#     }else{
#         stop("type ", type)
#     }
#
#
#     if(is.logical(x)){
#         out = as.integer(x)
#     }
#     if(is.factor(x)){
#
#     }
#     if(is.factor(x)){
#
#     }
#     if(n_distinct(x, na.rm = TRUE) != 2){
#         stop("expecting two distinct values")
#     }
#     out
# }
