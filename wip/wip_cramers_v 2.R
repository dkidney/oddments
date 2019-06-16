
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
cramers_v = function(x, y = NULL, type = c("full", "upper", "lower"),
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
