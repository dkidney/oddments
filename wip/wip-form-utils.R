
#' @rdname form
#' @aliases form_response
#' @name form_*
#' @title Formula manipulation
#' @description A selection of functions for formula maniputlation.
#'
#'   \code{form_response} returns the name of the response variable (or \code{NULL} if
#'   there is no left hand side).
#'
#'   \code{form_remove_response} returns a formula with the left hand side removed.
#'
#'   \code{form_covariates} returns the names of the variables on the right hand side of
#'   the formula (or \code{NULL} in the case of intercept-only formulas).
#' @param form formula
#' @examples
#' form = ~ 1
#' form %>% form_response
#' form %>% form_remove_response
#' form %>% form_covariates
#' form %>% form_as_character
#'
#' form = D ~ 1
#' form %>% form_response
#' form %>% form_remove_response
#' form %>% form_covariates
#' form %>% form_as_character
#'
#' form = ~ x + y + x:y
#' form %>% form_response
#' form %>% form_remove_response
#' form %>% form_covariates
#' form %>% form_as_character
#'
#' form = D ~ x + y + x:y
#' form %>% form_response
#' form %>% form_remove_response
#' form %>% form_update_response
#' form %>% form_covariates
#' form %>% form_as_character
#' @importFrom stats delete.response formula terms
#' @importFrom purrr is_formula
#' @export
form_response = function(form){
    if(is.null(form)) return(form)
    if(!is_formula(form)) stop("expecting a formula")
    out = form %>% all.vars %>% setdiff(form %>% form_covariates)
    if(length(out) == 0) NULL else out
}

#' @rdname form
#' @name form_remove_response
#' @importFrom stats delete.response formula terms
#' @export
form_remove_response = function(form){
    if(is.null(form)) return(form)
    if(!is_formula(form)) stop("expecting a formula")
    form %>% terms %>% delete.response %>% formula
}

#' @rdname form
#' @name form_update_response
#' @export
form_update_response = function(form, response = "y"){
    update(form, formula(paste(response, "~ .")))
}

#' @rdname form
#' @name form_covariates
#' @importFrom stats delete.response terms
#' @export
form_covariates = function(form){
    if(is.null(form)) return(form)
    if(!is_formula(form)) stop("expecting a formula")
    form %>% terms %>% delete.response %>% attr("factors") %>% rownames
}

#' @rdname form
#' @name form_covariates
#' @importFrom stats delete.response terms
#' @export
form_as_character = function(form){
    if(is.null(form)) return(form)
    if(!is_formula(form)) stop("expecting a formula")
    lhs = form %>% form_response
    rhs = form %>% form_remove_response %>% as.character %>% str_c(collapse = " ")
    str_c(lhs, rhs, sep = " ")
}


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


