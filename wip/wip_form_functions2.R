
#' @rdname formulas
#' @name f_lhs
#' @title Extract formula info
#' @description Convenience functions for extracing information from formulas.
#' @param f a formula
#' @export
#' @examples
#' f = y ~ x*z
#' f_lhs(f)
#' f_rhs(f)
#' f_terms(f)
#' f_covs(f)
#' f_chr(f)
#'
#' f = y ~ 1
#' f_lhs(f)
#' f_rhs(f)
#' f_terms(f)
#' f_covs(f)
#' f_chr(f)
f_lhs = function(f){
    as.character(rlang::f_lhs(f))
}

#' @rdname formulas
#' @name f_rhs
#' @export
f_rhs = function(f){
    rlang::f_text(f)
}

#' @rdname formulas
#' @name f_terms
#' @export
f_terms = function(f){
    attr(stats::terms(f), "term.labels")
}

#' @rdname formulas
#' @name f_vars
#' @export
f_vars = function(f){
    all.vars(f)
}

#' @rdname formulas
#' @name f_covs
#' @export
f_covs = function(f){
    setdiff(all.vars(f), f_lhs(f))
}

#' @rdname formulas
#' @name f_chr
#' @export
f_chr = function(f){
    str_c(f_lhs(f), " ~ ", f_rhs(f))
}
