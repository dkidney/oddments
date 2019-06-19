
#' @title Summarise a data frame
#' @description Summarise the variables in a data frame in terms of:
#' \itemize{
#'   \item class
#'   \item missing values
#'   \item distinct values
#' }
#' @param x a data frame
#' @export
#' @examples
#' \dontrun{
#'
#' library(eda)
#' iris %>% data_summary
#' }

data_summary = function(x){

    if(!inherits(x, "data.frame")) stop("expecting a data frame")

    n = nrow(x)

    out = data_frame(
        name = colnames(x),
        type = map_chr(x, type_sum),
        n_missing = map_int(x, function(x) sum(is.na(x))),
        prop_missing = .data$n_missing / n,
        n_distinct = map_int(x, n_distinct, na.rm = TRUE),
        prop_distinct = .data$n_distinct / n,
        min      = x %>% map_dbl(function(x) if(is_number(x)) min(x, na.rm = TRUE)    else NA_real_),
        max      = x %>% map_dbl(function(x) if(is_number(x)) max(x, na.rm = TRUE)    else NA_real_),
        mean     = x %>% map_dbl(function(x) if(is_number(x)) mean(x, na.rm = TRUE)   else NA_real_),
        median   = x %>% map_dbl(function(x) if(is_number(x)) median(x, na.rm = TRUE) else NA_real_),
        sd       = x %>% map_dbl(function(x) if(is_number(x)) sd(x, na.rm = TRUE)     else NA_real_)
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
