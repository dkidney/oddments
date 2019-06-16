
#' @title Factorise predictor variables
#' @description Convert continuous predictor variables to factors or simplify factor
#'   predictor variables by merging levels. Uses \link[rpart]{rpart} (i.e. recursive
#'   partitioning and regression tree models) under the hood.
#' @param x predictor variable
#' @param y response variable
#' @param sep character to use to separate existing level names when making new names for
#'   merged factor levels
#' @param ... additional arguments to pass to \link[rpart]{rpart}
#' @return Returns a re-binned predictor variable.
#' @importFrom rpart rpart
#' @importFrom forcats fct_collapse
#' @export
#' @example inst/examples/example-rebin.R

rebin = function(x, y, sep = ",", ...){
    is.number = inherits(x, c("integer", "numeric"))
    if(!is.number) x %<>% as.factor
    model = rpart(y ~ x, ...)
    if(is.number){
        nodes = model$splits %>% .[,"index"] %>% sort
        # cut x using splits
        newx = x %>% cut(breaks = c(min(x), nodes, max(x)), include.lowest = TRUE)
    }else{
        # get node membership for each level
        node = model$csplit %>% .[1,] %>% setNames(levels(x))
        # make an list of args for fct_collapse
        args = unique(node) %>% lapply(function(i) names(node)[node == i])
        labs = args %>% sapply(paste0, collapse = sep)
        args %<>% setNames(labs)
        args$f = x
        newx = do.call(fct_collapse, args)
    }
    return(newx)
}



