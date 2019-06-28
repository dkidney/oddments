
#' @title Cramer's V correlation for categorical variables
#' @description Compute the Cramer's V correlation.
#' \itemize{
#'  \item If \code{x} and \code{y} are vectors then the output is a scalar (in which case
#'  \code{type}, \code{parallel} and \code{n_cores} are ignored)
#'  \item If \code{x} is a data frame then the output is a matrix (in which \code{y} is
#'  ignored)
#' }
#'
#' @param x a categorical vector, or a data frame of categorical variables
#' @param y a categorical vector
#' @param type the type of correlation matrix to return
#' @param na.rm ???
#' @param parallel if \code{TRUE} parallel processing will be used
#' @param n_cores the number of cores to use for parallel processing
#' @examples
#' \dontrun{
#'
#' library(tidyverse)
#' library(oddments)
#'
#' # vector inputs -----
#'
#' n = 1e4
#' x = sample(letters[1:10], n, replace = TRUE) %>% c(NA)
#' y = sample(letters[1:20], n, replace = TRUE) %>% c(NA)
#' cramers_v(x, y)
#' cramers_v(x, y, na.rm = FALSE)
#'
#' # compare value with vcd::assocstats
#' vcd::assocstats(stats::xtabs(formula = ~x+y))$cramer
#' vcd::assocstats(table(x, y, useNA = "no"))$cramer
#' vcd::assocstats(table(x, y, useNA = "ifany"))$cramer
#'
#' # compare speed with vcd::assocstats
#' microbenchmark::microbenchmark(
#'   cramers_v(x, y),
#'   cramers_v(x, y, na.rm = FALSE),
#'   vcd::assocstats(table(x, y))$cramer,
#'   vcd::assocstats(table(x, y, useNA = "ifany"))$cramer,
#'   times = 1000
#' ) %>%
#'  oddments:::print_microbench()
#'
#' # data frame input -----
#'
#' df = 1:5 %>% set_names(., .) %>%
#'   map_df(function(x) sample(letters, 100, replace = TRUE))
#'
#' df %>% cramers_v
#' df %>% cramers_v(type = "lower")
#' df %>% cramers_v(type = "upper")
#'
#' # large data frame input
#' df = sample(letters, 1e5, replace = TRUE) %>% matrix(nc = 100) %>% as_data_frame
#' df %>% dim
#' system.time({X = df %>% cramers_v})["elapsed"]
#' system.time({Y = df %>% cramers_v(parallel = TRUE)})["elapsed"]
#' identical(X,Y)
#' }
#' @export

cramers_v = function(x,
                     y = NULL,
                     type = c("full", "upper", "lower"),
                     na.rm = TRUE,
                     parallel = FALSE,
                     n_cores = parallel::detectCores()
){

    if(is.data.frame(x)){
        type = match.arg(type)
        p = ncol(x)
        pnames = colnames(x)
        cor = matrix(NA, nrow = p, ncol = p, dimnames = list(pnames, pnames))
        upper = upper.tri(cor)
        indices = which(upper, arr.ind = TRUE)
        FUN = function(i){
            cramers_v(x[[indices[i,1]]],
                      x[[indices[i,2]]])
        }
        if(parallel && .Platform$OS.type == "windows"){
            warn("parallel not yet implemented for windows")
            parallel = FALSE
        }
        if(parallel){
            values = 1:nrow(indices) %>%
                parallel::mclapply(FUN, mc.cores = n_cores) %>%
                combine
        }else{
            values = 1:nrow(indices) %>% map_dbl(FUN)
        }
        cor[indices] = values
        if(type != "upper"){
            lower = lower.tri(cor)
            cor[lower] = t(cor)[lower]
            if(type == "lower") cor[upper] = NA
            if(type == "full") diag(cor) = 1
        }
    }else{
        if(is.null(y)) stop("y must be supplied if x is a vector")
        n = length(y)
        if(length(y) != length(x)) stop('x and y are different lengths')
        cor = try({
            xtab = table(x, y, useNA = if(na.rm) "no" else "ifany")
            dims = dim(xtab)
            # rs = .rowSums(xtab, dims[1], dims[2])
            # cs = .colSums(xtab, dims[1], dims[2])
            rs = rowSums(xtab)
            cs = colSums(xtab)
            n = sum(xtab)
            E = outer(rs, cs, "*") / n
            X2 = sum((xtab - E)^2/E)
            sqrt(X2 / n / min(dims - 1))
        }, TRUE)
        if(inherits(cor, "try-error")) return(NA)
    }
    cor
}



#' @title Cramer's V correlation for categorical variables
#' @description Computes the Cramer's V correlation for a pair of variables (in which case
#'   the output is a scalar) or a data frame (in which case the output is a matrix). This
#'   function is a wrapper for the \link[vcd]{assocstats} function from the \pkg{vcd}
#'   package.
#' @param x a vector representing a single categorical variable, or a data frame of
#'   categorical variables
#' @param y a vector representing a single categorical variable
#' @param type the type of correlation matrix to return
#' @param parallel if \code{TRUE} parallel processing will be used
#' @param n.cores number of cores to use for parallel processing
#' @details The \code{y} argument is ignored if x is a data.frame, and the \code{type},
#'   \code{parallel} and \code{n.cores} are ignored if \code{x} arguments are ignored if
#'   \code{x} is a vector.
#' @examples
#' \dontrun{
#'
#' library(dplyr)
#'
#' # example data
#' n = 1000
#' x = sample(letters, n, replace = TRUE)
#' y = sample(letters, n, replace = TRUE)
#' z = sample(letters, n, replace = TRUE)
#'
#' # correlation between two variables
#' cramers_v(x, y)
#'
#' # vcd package output for comparison
#' vcd::assocstats(xtabs(formula = ~ x + y))$cramer
#'
#' # data frame input
#' data_frame(x, y, z) %>% cramers_v
#' data_frame(x, y, z) %>% cramers_v(type = "lower")
#' data_frame(x, y, z) %>% cramers_v(type = "full")
#'
#' # large data frame input
#' df = letters %>% sample(1e5, replace = TRUE) %>% matrix(nc = 100) %>% as_data_frame
#' system.time({X = df %>% cramers_v()})["elapsed"]
#' system.time({Y = df %>% cramers_v(parallel = TRUE)})["elapsed"]
#' identical(X,Y)
#' }
#' @export
#' @importFrom vcd assocstats
cramers_v2 = function(x, y = NULL, type = c("full", "upper", "lower"),
                     parallel = FALSE, n.cores = detectCores()){
    if(is.data.frame(x)){
        type = match.arg(type)
        p = ncol(x)
        pnames = colnames(x)
        cor = matrix(NA, nrow = p, ncol = p, dimnames = list(pnames, pnames))
        upper = upper.tri(cor)
        indices = which(upper, arr.ind = TRUE)
        FUN = function(i) cramers_v(x[[indices[i,1]]],
                                    x[[indices[i,2]]])
        if(parallel){
            if(.Platform$OS.type == "windows"){
                cl = makePSOCKcluster(n.cores)
                clusterEvalQ(cl, library(oddments))
                clusterExport(cl, c("indices", "x"))
            }else{
                cl = makeForkCluster(n.cores)
            }
            values = parLapply(cl, 1:nrow(indices), FUN) %>% combine
            stopCluster(cl)
        }else{
            values = lapply(1:nrow(indices), FUN) %>% combine
        }
        cor[indices] = values
        if(type != "upper"){
            lower = lower.tri(cor)
            cor[lower] = t(cor)[lower]
            if(type == "lower") cor[upper] = NA
            if(type == "full") diag(cor) = 1
        }
    }else{
        if(is.null(y)) stop("y must be supplied if x is a vector")
        n = length(x)
        if(length(y) != n) stop('x and y are different lengths')
        cor = try({
            assocstats(stats::xtabs(formula = ~ x + y))$cramer
        }, TRUE)
        if(inherits(cor, "try-error")) cor = NA
    }
    return(cor)
}
