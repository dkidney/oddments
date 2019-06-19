
#' @rdname plot_dist
#' @name plot_dist
#' @title Plot variable distributions
#' @description Plot the distribution of a variable and optionally overlay the univariate
#'   relationship with a second variable. The first variable can be any class, the second
#'   variable must be continuous or binary integer.
#' @param x main variable to plot (vector)
#' @param y optional response variable (vector)
#' @param w optional weights variable (numeric vector)
#' @param na_action string determining how missing values are to be treated (see Details)
#' @param bin if \code{TRUE}, numeric \code{x} variables will be converted to character
#' @param n_bins number of bins to use when converting variables to character
#' @param bin_method = c("interval", "number")
#' @param log if \code{TRUE}, numeric \code{x} varibles will be logged
#' @param smooth_method ???
#' @param k ???
#' @param points ???
#' @param ... additional values to pass to ???
#' @details
#' \code{na_action}\cr
#' ???
#' @export
#' @examples
#' \dontrun{
#'
#' library(tidyverse)
#' library(eda)
#' theme_set(eda_ggplot_theme())
#'
#' mtcars %>% glimpse
#'
#' # univariate - categorical x
#' mtcars %$% plot_dist(as.integer(cyl))
#' mtcars %$% plot_dist(as.character(am))
#'
#' # univariate - continuous x
#' mtcars %$% plot_dist(mpg)
#' mtcars %$% plot_dist(mpg, log = TRUE)
#' mtcars %$% plot_dist(mpg, log = TRUE, bin = TRUE)
#' mtcars %$% plot_dist(mpg, log = TRUE, bin = TRUE, bin_method = "number")
#' mtcars %$% plot_dist(mpg, log = TRUE, bin = TRUE, n_bins = 3)
#'
#' # bivariate - categorical x, continuous y
#' mtcars %$% plot_dist(as.integer(gear), qsec)
#'
#' # bivariate - categorical x, binary y
#' mtcars %$% plot_dist(as.integer(gear), as.integer(vs))
#'
#' # bivariate - continuous x, continuous y
#' mtcars %$% plot_dist(mpg, disp)
#' mtcars %$% plot_dist(mpg, disp, smooth_method = "glm")
#' mtcars %$% plot_dist(mpg, disp, smooth_method = "glm", points = TRUE)
#'
#' # bivariate - continuous x, binary y
#' mtcars %$% plot_dist(disp, as.integer(vs))
#' mtcars %$% plot_dist(mpg, as.integer(vs))
#' mtcars %$% plot_dist(mpg, as.integer(vs), bin = TRUE)
#' }

plot_dist = function(x,
                     y = NULL,
                     w = NULL,
                     na_action = c("remove", "compare"),
                     log = FALSE,
                     bin = FALSE,
                     n_bins = 4,
                     bin_method = c("interval", "number"),
                     smooth_method = c("gam", "glm"),
                     k = 3,
                     points = FALSE,
                     ...
){

    if(!is.atomic(x)) stop("x must be an atomic vector")
    if(!is.null(y)) if(!is.atomic(y)) stop("y must be an atomic vector")
    if(!is.null(w)) if(!is.atomic(w)) stop("w must be an atomic vector")

    # make data frame
    df = data_frame(x = x)
    if(!is.null(y)) df$y = y
    if(!is.null(w)) df$w = w
    is_double_x = typeof(x) == "double"
    if(!is.null(y)){
        is_double_y = typeof(y) == "double"
        is_binary_y = is.integer(y) && all(y %in% c(0L, 1L), na.rm = TRUE)
        if(!is_double_y && !is_binary_y)
            stop("y must be numeric or integer binary")
        family_y = if(is_binary_y) "binomial" else "gaussian"
    }

    # process NAs
    na_action = match.arg(na_action)
    if(na_action == "remove"){
        df %<>% filter(!is.na(.data$x))
        if(!is.null(y)) df %<>% filter(!is.na(.data$y))
        if(!is.null(w)) df %<>% filter(!is.na(.data$w))
    }
    if(na_action == "compare"){
        df %<>% mutate_at("x", function(x){
            if_else(is.na(x), "Missing", "Not missing")
        })
        is_double_x = FALSE
    }

    # log numeric
    if(is_double_x){
        log %>% check_is_scalar_logical
        if(log){
            if(any(df$x == 0, na.rm = TRUE))
                stop("x contains zeros - logging will create non-finite values")
            df %<>% mutate_at("x", base::log)
        }
    }

    # bin numeric
    if(is_double_x){
        bin %>% check_is_scalar_logical
        if(bin){
            bin_method = match.arg(bin_method)
            n_bins %<>% as.integer %>% check_is_scalar_integer
            if(bin_method == "interval")
                df$x %<>% ggplot2::cut_interval(n_bins)
            if(bin_method == "number")
                df$x %<>% ggplot2::cut_number(n_bins)
            is_double_x = FALSE
        }
    }

    # plot x -----

    if(is_double_x){

        plot_x = df %>%
            ggplot(aes_string("x")) +
            geom_histogram(fill = "blue", alpha = 0.5)

    }else{

        counts_x = df %>%
            group_by(.data$x) %>%
            summarise(n = n())
        plot_x = counts_x %>%
            ggplot(aes_string("x", "n")) +
            geom_bar(stat = "identity", fill = "blue", alpha = 0.5)

    }

    if(is.null(y)){

        plot = plot_x

    }else{

        # plot y -----

        # helper functions for making second y axis
        y1_to_y2 = function(y1) (y1 / y1_max * y2_rng) + y2_min
        y2_to_y1 = function(y2) (y2 - y2_min) / y2_rng * y1_max

        if(is_double_x){

            smooth_method = match.arg(smooth_method)
            k %<>% as.integer %>% check_is_scalar_integer

            plot_x_df = suppressMessages({
                ggplot_build(plot_x)$data[[1]] %>% as_data_frame
            })

            plot_y = df %>%
                ggplot(aes_string("x", "y")) +
                geom_smooth(
                    method = smooth_method,
                    method.args = list(family = family_y),
                    formula = if(smooth_method == "gam"){
                        formula(str_c("y ~ s(x, k = ", k, ")"))
                    }else{
                        y ~ x
                    }
                )

            plot_y_df = suppressMessages({
                ggplot_build(plot_y)$data[[1]] %>% as_data_frame
            })

            if(nrow(plot_y_df) == 0){

                plot = plot_x

            }else{

                # y2 values
                y1_max = max(plot_x_df$y)
                y2_min = min(plot_y_df$ymin)
                y2_max = max(plot_y_df$ymax)
                y2_rng = y2_max - y2_min
                plot_y_df %<>% mutate_at(c("y", "ymin", "ymax"), y2_to_y1)
                df %<>% mutate_at("y", y2_to_y1)

                # y2 points
                if(points){
                    y2_min %<>%  min(df$y)
                    y2_max %<>%  max(df$y)
                    plot_x = plot_x +
                        geom_point(aes_string("x", "y"),  df, col = "red")
                }

                # y2 ribbon
                plot = plot_x +
                    geom_line(aes_string("x", "y"), plot_y_df, col = "red") +
                    geom_ribbon(aes_string("x", ymin = "ymin", ymax = "ymax"), plot_y_df,
                                fill = "red", alpha = 0.2) +
                    scale_y_continuous(sec.axis = sec_axis(~ y1_to_y2(.), substitute(y)))

            }

        }else{

            if(is_binary_y){

                df %<>%
                    group_by(.data$x) %>%
                    summarise(
                        n    = .data$y %>% length,
                        mean = .data$y %>% mean,
                        nbad = .data$y %>% sum
                    )

                # exact confidence intervals
                binom_ci = binom.confint(df$nbad, df$n, methods = "exact")
                df$lower = binom_ci$lower
                df$upper = binom_ci$upper

                # y2 values
                y1_max = max(counts_x$n)
                y2_min = min(df$lower)
                y2_max = max(df$upper)
                y2_rng = y2_max - y2_min
                df %<>% mutate_at(c("mean", "lower", "upper"), y2_to_y1)

                # y2 errorbars
                plot = plot_x +
                    # geom_line(aes_string("x", "mean", group = 1), df, col = "red") +
                    geom_point(aes_string("x", "mean"), df, col = "red") +
                    geom_errorbar(aes_string("x", ymin = "lower", ymax = "upper"), df,
                                  colour = "red", width = 0.5) +
                    scale_y_continuous(sec.axis = sec_axis(~ y1_to_y2(.), substitute(y)))

            }else{

                y1_max = max(counts_x$n)
                y2_min = min(df$y)
                y2_max = max(df$y)
                y2_rng = y2_max - y2_min
                df %<>% mutate_at("y", y2_to_y1)

                plot = plot_x +
                    geom_boxplot(aes_string("x", "y", group = "x"), df, col = "red", fill = NA) +
                    scale_y_continuous(sec.axis = sec_axis(~ y1_to_y2(.), substitute(y)))

            }

        }

    }

    plot +
        labs(x = substitute(x), y = "count")

}



