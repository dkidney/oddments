
#' @title Summarise a data frame
#' @description Summarise
#' @param x data frame
#' @param digits the number of signficant figures to use when stating the range of numeric
#'   variables
#' @param width the total width for printing
#' @return A data frame with one row per column of the input data frame.
#' @export
#' @examples
#' n = 26
#' df = dplyr::data_frame(
#'   u = letters[1:n],
#'   v = LETTERS[1:n] %>% factor,
#'   w = rnorm(n),
#'   x = runif(n),
#'   y = runif(n,0,10) %>% round %>% as.integer,
#'   z = runif(n) > 0.5
#' )
#'
#' df %>% summary_df
#' df %>% summary_df(digits = 3)
#' df %>% summary_df(digits = 3, width = 50)

summary_df = function(x, digits = getOption("digits"), width = getOption("width")){
    out = data_frame(
        name     = x %>% colnames,
        class    = x %>% sapply(class),
        n_unique = x %>% sapply(n_distinct),
        range    = x %>% sapply(function(x){
                    if(inherits(x, c("integer", "numeric"))){
                        x %>%
                            range %>%
                            signif(digits) %>%
                            paste(collapse = ",") %>%
                            paste0("(", ., ")")
                    }else{
                        x %>%
                            as.factor %>%
                            levels %>%
                            paste(collapse = ",")
                    }
                })
    )
    left =
        floor(nrow(out) / 10) + 2 +
        max(c(5, nchar(out$name))) + 1 +
        max(c(5, nchar(out$class))) + 1 +
        max(c(8, nchar(out$n_unique))) + 1
    right = width - (left + 3)
    out %<>%
        mutate(range = range %>% str_trunc_sep(right))
    return(out)
}
