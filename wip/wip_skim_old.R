
#' @title Summarise a data frame
#' @description Summarise the variables in a data frame in terms of:
#' \itemize{
#'   \item class
#'   \item missing values
#'   \item distinct values
#' }
#' @param x a data frame
#' @param verbose if \code{TRUE}, progress messages will be printed to the console
#' @export
#' @examples
#' \dontrun{
#'
#' data_summary(iris)
#' data_summary(mtcars)
#' }

data_summary = function(x, verbose = TRUE){

    if(!inherits(x, "data.frame")) stop("expecting a data frame")

    op = options(oddments_verbose = verbose)
    on.exit(options(op))

    heading(deparse(substitute(x)))

    bullet("dimensions:")
    n = nrow(x)
    p = ncol(x)
    item(n, " rows")
    item(p, " columns")

    bullet("variable types:")
    type = x %>% map_chr(type_sum)
    table(type) %>% as.list %>% unlist %>% str_c(names(.), ., sep = ": ") %>% items

    bullet("missing values:")
    n_missing = x %>% map_int(function(x) sum(is.na(x)))
    prop_missing = n_missing / n
    item(sum(n_missing != 0), " variables with missing values")
    item(str_c(round(range(prop_missing) * 100), collapse = "-"), "% missingness range")

    bullet("unique values")
    nzv = caret::nearZeroVar(x, saveMetrics = TRUE)
    zeroVars = rownames(nzv)[nzv$zeroVar]
    nearZeroVars = rownames(nzv)[nzv$nzv]
    item(length(zeroVars), " zeroVars")
    item(length(nearZeroVars), " nearZeroVars")

    out = data_frame(
        name = colnames(x),
        type = type,
        prop_missing = prop_missing,
        n_missing = n_missing,
        prop_distinct = nzv$percentUnique / 100,
        n_distinct = .data$prop_distinct * n,
        zero_variation = nzv$zeroVar,
        near_zero_variation = nzv$nzv,
        min      = x %>% map_dbl(function(x) if(is_number(x)) min(x, na.rm = TRUE) else NA_real_),
        max      = x %>% map_dbl(function(x) if(is_number(x)) max(x, na.rm = TRUE) else NA_real_),
        mean     = x %>% map_dbl(function(x) if(is_number(x)) mean(x, na.rm = TRUE) else NA_real_),
        median   = x %>% map_dbl(function(x) if(is_number(x)) stats::median(x, na.rm = TRUE) else NA_real_),
        sd       = x %>% map_dbl(function(x) if(is_number(x)) stats::sd(x, na.rm = TRUE) else NA_real_)
    )

    tab = x %>% map(table)
    out$mode = tab %>% map_chr(function(x){
        if(length(x) == 0) NA_character_ else names(x[which.max(x)])
    })
    out$prop_mode = tab %>% map_dbl(function(x){
        if(length(x) == 0) NA_real_ else x[which.max(x)] / n
    })

   out

}
