
#' @rdname characteristic_table
#' @name characteristic
#' @title Calculate WoE and IV values
#' @description Functions to calculate information value and weight-of-evidence. Each
#'   function can take a single predictor or a data frame of predictors as an input. The
#'   \code{characteristic_table} function produces the most detailed output. The \code{iv}
#'   and \code{woe} functions are just wrappers to the \code{characteristic_table}
#'   function and produce more concise output.
#' @param y binary response vector - 1 = bad, 0 = good
#' @param x a predictor variable vector or data frame of predictors (character or factor)
#' @param w weights
#' @param adjust an adjustment factor to avoid undefined WoE values
#' @param min.total the minimum (unweighted) sample size per factor level
#' @param min.bads the minimum (unweighted) number of bads per factor level
#' @param woe.min.range ???
#' @param woe.min.level ???
#' @param woe.ignore.level ???
#' @param ... ???
#' @details ...
#' @return A data_frame of statistics.
# @example inst/examples/example-characteristic_table.r
#' @export
characteristic_table = function(y, x, w = NULL, adjust = 0,
                                min.total = 1, min.bads = 0,
                                woe.min.range = 0, woe.min.level = 0, woe.ignore.level = NULL){

    # if x is data frame, apply function to each column
    if(inherits(x, c("data.frame", "matrix"))){

        ct = x %>%
            as_data_frame %>%
            lapply(characteristic_table, y = y, w = w, adjust = adjust,
                   min.total        = min.total,
                   min.bads         = min.bads,
                   woe.min.range    = woe.min.range,
                   woe.min.level    = woe.min.level,
                   woe.ignore.level = woe.ignore.level)

        # add predictor column and cbind rows - bind_rows with id arg doesn't seem to work here...
        ct %>%
            names %>%
            lapply(function(i){
                ct[[i]] %>%
                    mutate(predictor = i) %>%
                    select(predictor, everything())
            }) %>%
            bind_rows %>%
            mutate(predictor = factor(predictor) %>% fct_inorder)

    }else{

        # check inputs ----
        w = if(is.null(w)) rep(1, length(y)) else w / sum(w) * length(y)
        # check lengths
        if(length(x) != length(y)) stop("length x not equal to length y")
        if(length(w) != length(y)) stop("length w not equal to length y")
        # check classes
        if(!inherits(y, c("integer", "numeric")) ||
           any(!unique(y, na.rm = TRUE) %in% c(0,1)))
            stop("y must be a numeric or integer vector of binary values")
        if(!inherits(x, c("character", "factor")))
            return(data_frame(n = length(x)))
        # stop("x must be a character or factor variable")
        # check NAs
        if(anyNA(y)) stop("y has missing values")
        if(anyNA(x)) stop("x has missing values")
        if(anyNA(w)) stop("w has missing values")
        # check values
        if(!all(y %in% c(0,1))) stop("y must be a binary variable")

        # calculate weighted stats ----
        data_frame(x = x, y = y, w = w) %>%
            mutate(x = as.factor(x)) %>%
            group_by(x) %>%
            summarise(
                # unweighted sample sizes per level
                unweighted.n.bad  = sum(y %in% 1),
                unweighted.n.good = sum(y %in% 0),
                unweighted.n = unweighted.n.bad + unweighted.n.good,
                # weighted sample sizes per level
                n.bad  = sum((y %in% 1) * w),
                n.good = sum((y %in% 0) * w),
                n = n.bad + n.good
            ) %>%
            tidyr::complete(x) %>%
            ungroup() %>%
            mutate_if(colnames(.) != "x", funs(ifelse(is.na(.), 0, .))) %>%
            mutate(
                # weighted total sample sizes
                N = sum(n),
                N.bad  = sum(n.bad),
                N.good = sum(n.good),
                # weighted bad rate
                bad.rate = n.bad / n,
                # weighted proportions
                proportion = n / N,
                prop.bad  = (n.bad + adjust) / N.bad,
                prop.good = (n.good + adjust) / N.good,
                # woe using weighted proportions
                woe = log(prop.bad / prop.good),
                # min bads per level
                woe = ifelse(unweighted.n.bad < min.bads, 0, woe),
                # min sample size per level
                woe = ifelse(unweighted.n < min.total, 0, woe),
                # min abs(woe) per level
                woe = ifelse(abs(woe) < woe.min.level, 0, woe),
                # min woe range
                woe = if(abs(diff(range(woe))) < woe.min.range) 0 else woe,
                # levels to ignore
                woe = ifelse(x %in% woe.ignore.level, 0, woe),
                # iv using weighted proportions and corrected woe
                iv = sum((prop.bad - prop.good) * woe)
            ) %>%
            # select(-prop.bad, -prop.good) %>%
            # select(-N, -N.bad, -N.good) %>%
            mutate_if(colnames(.) != "x", funs(ifelse(is.finite(.), ., NA))) %>%
            rename(level = x) %>%
            mutate(level.order = row_number()) %>%
            select(level, level.order, n, proportion, everything()) %>%
            mutate_if(function(x) inherits(x, "factor"), as.character)
    }

}

#' @rdname characteristic_table
#' @name iv
#' @export
iv = function(y, x, ...){
    result = characteristic_table(y, x, ...)
    if(inherits(x, c("data.frame", "matrix")))
        result %<>% group_by(predictor)
    result %<>% summarise(
        N      = sum(n),
        N.bad  = sum(n.bad),
        N.good = sum(n.good),
        bad.rate = N.bad / N,
        iv = unique(iv)
    )
    if(inherits(x, c("data.frame", "matrix"))){
        result %>% select(predictor, iv)
    }else{
        result$iv
    }
}

#' @rdname characteristic_table
#' @name woe
#' @export
woe = function(y, x, ...){
    characteristic_table(y, x, ...) %>%
        select(-prop.bad, -prop.good) %>%
        select(-N, -N.bad, -N.good) %>%
        select(-level.order, -proportion, -iv, -starts_with("unweighted"))
}

