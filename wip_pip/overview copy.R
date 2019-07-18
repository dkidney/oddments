
#' @rdname overview
#' @name overview
#' @title Summarise a data frame
#' @description Summarises the variables in a data frame.
#' @param x a data frame
#' @param verbose if \code{TRUE}, progress messages will be printed to the console
#' @param n max rows to print
#' @param sort_columns ???
#' @param n_cores ???
#' @param ... ???
#' @example inst/examples/examples.R
#' @export
overview = function(x, ...){
    UseMethod("overview")
}

#' @method overview character
#' @export
overview.character = function(x, ...){
    x %>% overview_default(...)
}

#' @method overview Date
#' @export
overview.Date = function(x, ...){
    x %>% overview_default(...)
}

#' @method overview logical
#' @export
overview.logical = function(x, ...){
    x %>% overview_default(...)
}

#' @method overview factor
#' @export
overview.factor = function(x, ...){
    x %>% overview_default(...)
}

#' @method overview numeric
#' @export
overview.numeric = function(x, ...){
    x %>% overview_numeric(...)
}

#' @method overview integer
#' @export
overview.integer = function(x, ...){
    x %>% overview_numeric(...)
}

#' @rdname overview
#' @name overview
#' @method overview data.frame
#' @export
overview.data.frame = function(x, verbose = TRUE, n = 1000, sort_columns = TRUE, n_cores = 1, ...){

    if(!inherits(x, "data.frame")) stop("expecting a data frame")
    out = list(dim = NULL, classes = NULL, stats = NULL)

    # stats
    cl = if(n_cores > 1) parallel::makeCluster(n_cores) else NULL
    pbopts = pbapply::pboptions(type = "timer", char = "=")
    on.exit(pbapply::pboptions(pbopts))
    out$stats = x %>%
        pbapply::pblapply(overview, cl = cl) %>%
        bind_rows(.id = "name")
    if(n_cores > 1) parallel::stopCluster(cl)
    if(sort_columns) out$stats %<>% arrange(.data$name)
    if(verbose){
        message("\nstats")
        print(out$stats, n = n)
    }

    # classes
    out$classes = x %>%
        map_chr(type_sum) %>%
        table %>%
        as.list %>%
        unlist
    if(verbose){
        message("\nclasses")
        str_c(format(out$classes, big.mark = ","),
              names(out$classes), sep = " ") %>% writeLines
    }

    # dims
    out$dim = x %>%
        dim %>%
        set_names(c("rows", "cols"))
    if(verbose){
        message("\ndims")
        str_c(format(out$dim, big.mark = ","),
              names(out$dim), sep = " ") %>% writeLines
    }

    invisible(out)
}

overview_default = function(x, mode = TRUE){
    out = data_frame(
        type = x %>% type_sum,
        n_distinct = x %>% n_distinct(na.rm = TRUE),
        n_na = sum(is.na(x)),
        p_na = .data$n_na / length(x)
    )
    if(mode){
        out %<>%
            bind_cols(overview_mode(x))
    }
    out %>%
        bind_cols(
            data_frame(
                binary = is_binary(x),
                nzv = is_near_zero_var(x)
            )
        )
}

overview_numeric = function(x){
    data_frame(
        mean = x %>% mean(na.rm = TRUE),
        median = x %>% median(na.rm = TRUE),
        min = x %>% min(na.rm = TRUE),
        max = x %>% max(na.rm = TRUE),
        n_inf = sum(is.infinite(x)),
        n_nan = sum(is.nan(x))
    ) %>%
        bind_cols(overview_default(x), .)
}

overview_mode = function(x){
    if(!type_sum(x) %in% c("chr", "fct", "lgl", "int", "dbl")){
        return(data_frame(value = NA, n = NA, prop = NA))
    }
    data_frame(x = x) %>%
        count(.data$x) %>%
        set_names(c("mode", "n_mode")) %>%
        filter(!is.na(.data$mode)) %>%
        arrange(desc(.data$n_mode)) %>%
        mutate(p_mode = .data$n_mode / sum(.data$n_mode)) %>%
        mutate_at("mode", as.character) %>%
        slice(1)
}

