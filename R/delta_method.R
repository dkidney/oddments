
#' @rdname delta_method
#' @name delta_method
#' @title Delta method
#' @description Estimate the variance of a function of normally distributed variables.
#'
#' All functions take arguments \code{.beta} (a vector of estimated coefficients) and
#' \code{.vcov} (the estimated variance covariance matrix for \code{.beta}).
#'
#' \itemize{
#'   \item \code{delta_method_f_pars()} takes function \code{.f_pars} which must be a
#'   function of separate beta paraemeters
#'   \item \code{delta_method_f_beta()} takes function \code{.f_beta} which must be a
#'   function of a single beta parameter vector
#'   \item \code{delta_method_X()} takes a design matrix as an input (e.g. the input to a
#'   linear model) instead of a function
#' }
#' @details Uses the \link[stats]{numericDeriv} function.
#' @param .f_pars a function of parameters beta1, beta2, etc.
#' @param .f_beta a function of parameter vector beta
#' @param .beta vector of estimated values for the beta parameters
#' @param .vcov variance-covariance matrix for the beta parameters
#' @param ... additional arguments to pass to \code{.f_beta}
#' @example inst/examples/examples-delta_method.R
#' @importFrom purrr discard map_dbl
#' @importFrom rlang call2 syms
#' @importFrom stringr str_c str_detect str_remove str_replace
NULL

#' @rdname delta_method
#' @name delta_method_f_pars
#' @export
delta_method_f_pars <- function(.f_pars, .beta, .vcov, ...) {
    stopifnot(length(.beta) == nrow(.vcov))
    stopifnot(length(.beta) == ncol(.vcov))
    stopifnot(!is.null(names(.beta)))
    stopifnot(names(.beta) == rownames(.vcov))
    stopifnot(names(.beta) == colnames(.vcov))
    args <- as.list(formals(.f_pars))
    stopifnot(all(names(.beta) %in% names(args)))
    dots <- list(...)
    for (i in names(dots)) {
        assign(i, dots[[i]])
        args[[i]] <- dots[[i]]
    }
    for (i in names(.beta)) {
        assign(i, .beta[i])
        args[[i]] <- .beta[[i]]
    }
    f_call <- call2(".f_pars", !!!syms(names(args)))
    grad <- f_call %>%
        stats::numericDeriv(theta = names(.beta)) %>%
        attr("gradient")
    var <- 1:nrow(grad) %>%
        map_dbl(function(i) {
            t(grad[i, ]) %*% .vcov %*% grad[i, ]
        })
    estimate <- do.call(.f_pars, args, )
    list(
        fit = estimate,
        lower = estimate - 1.96 * sqrt(var),
        upper = estimate + 1.96 * sqrt(var)
    )
}

#' @rdname delta_method
#' @name delta_method_f_beta
#' @export
delta_method_f_beta <- function(.f_beta, .beta, .vcov, ...) {
    stopifnot("beta" %in% names(formals(.f_beta)))
    f_chr <- deparse(.f_beta) %>%
        discard(str_detect(., "(\\{|\\})")) %>%
        str_remove("^\\s{4}")
    args <- f_chr[1] %>%
        str_replace("beta", str_c(names(.beta), collapse = ", "))
    body <- c(
        str_c("beta = c(", str_c(names(.beta), collapse = ", "), ")"),
        str_c("names(beta) <- c(", str_c("'", names(.beta), "'", collapse = ", "), ")"),
        str_c(f_chr[-1], collapse = "\n")
    ) %>%
        str_c("    ", ., collapse = "\n") %>%
        str_c("{", ., "}", sep = "\n")
    .f_pars <- eval(
        expr = parse(text = str_c(args, body)),
        envir = environment(.f_beta)
    )
    delta_method_f_pars(.f_pars, .beta, .vcov, ...)
}

#' @rdname delta_method
#' @name delta_method_X
#' @param .X a design matrix (ncol equal to length .beta)
#' @export
delta_method_X <- function(.X, .beta, .vcov) {
    stopifnot(ncol(.X) == length(.beta))
    .f_beta <- function(beta, .X) .X %*% beta
    delta_method_f_beta(.f_beta, .beta, .vcov, .X = .X)
}
