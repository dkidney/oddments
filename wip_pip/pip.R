
#' @title Predictor insight plots
#' @description Plot the distribution of a variable and (optionally) overlay the
#'   relationship with a second variable.
#' @param data a data frame
#' @param x x variable name (unquoted)
#' @param y y variable name (unquoted)
#' @param group grouping variable name (unquoted)
#' @param y_method string giving the method to use for the y-smooth (see Details)
#' @param y_family string giving the family to use for the y-smooth (see Details)
#' @param y_ci ???
#' @param y_points logical indicating whether points should be plotted
#' @param bins passed to geom_histogram (for numeric x)
#' @param k passed to geom_smooth (for numeric x, when y_method = "gam")
#' @param max_n_distinct_cat for categorical x, if the number of unique values exceeds
#'   this number the plot will not be drawn
#' @param min_n_distinct_num for numeric x, if the number of unique values is less than
#'   this number then x will be converted to factor
#' @param max_n_distinct_group for grouping variable, if the number of unique values
#'   exceeds this number the plot will not be drawn
#' @param ... additional arguments to pass to underlying functions ???
#' @example inst/examples/examples.R
#' @details
#' numeric x
#'
#' double, integer and date
#'
#' y_method %in% c("gam", "glm", "loess", "none")
#'
#' categorical x
#'
#' character factor and logical
#'
#' y_method %in% c("glm", "boxplot", "none")
#' @export

pip = function(data,
               x,
               y,
               group,
               y_method = NULL,
               y_family = NULL,
               y_points = FALSE,
               y_ci = TRUE,
               bins = 30,
               k = 10,
               max_n_distinct_cat = 50,
               min_n_distinct_num = 10,
               max_n_distinct_group = 3,
               ...){

    # > check inputs ----

    if(!inherits(data, "data.frame")) stop("expecting 'data' to be a data frame")
    data %<>% as_data_frame
    # x_col = "dodgerblue"
    # x_col = "#428bca"
    x_col = "#317eac"
    y_col = "firebrick1"

    # > make df -----

    quo = name = list()
    quo$x = enquo(x)
    name$x = quo_name(quo$x)
    if(!missing(y)){
        quo$y = enquo(y)
        name$y = quo_name(quo$y)
    }
    if(!missing(group)){
        quo$group = enquo(group)
        name$group = quo_name(quo$group)
    }
    df = data %>% transmute(!!!quo)

    # > check x ----

    x_type = df$x %>% type_sum
    if(!x_type %in% c("chr", "date", "dbl", "fct", "int", "lgl")){
        warn(str_c("cant plot x variable type '", x_type, "' - returning NULL"))
        return(invisible())
    }
    is_num = x_type %in% c("dbl", "int", "date")
    is_date = x_type == "date"
    if(is_num){
        if(n_distinct(df$x, na.rm = TRUE) < min_n_distinct_num){
            warn(str_c("x variable is ", x_type,
                       " but has fewer than ", min_n_distinct_num, " distinct values",
                       "- treating as fct"))
            df$x %<>% factor(., sort(na.omit(unique(.))))
            x_type = "fct"
            is_num = FALSE
        }
    }
    if(!is_num){
        if(n_distinct(df$x) > max_n_distinct_cat){
            warn(str_c("x variable is ", x_type,
                       " and has more than ", max_n_distinct_cat, " distinct values",
                       "- returning NULL"))
            return(invisible())
        }
        df$x %<>% fct_explicit_na
    }

    # > check group -----

    use_group = !is.null(name$group)
    if(use_group){
        if(n_distinct(df$group) > max_n_distinct_group){
            warn(str_c("group variable has more than ", max_n_distinct_group, " distinct values"))
            use_group = FALSE
        }
    }
    if(use_group){
        df$group %<>% as.factor
        levels(df$group) = str_c(name$group, " = ", levels(df$group))
    }else{
        df$group = 1
    }

    # > plot x -----

    if(is_num){

        # ~~ plot x num -----

        group_levs = levels(as.factor(df$group))
        group_fct = factor(group_levs, group_levs)

        plot_temp = df %>%
            ggplot() +
            geom_histogram(aes(.data$x, group = .data$group), bins = bins, ...)

        df_x = ggplot_build(plot_temp)$data[[1]] %>%
            as_data_frame %>%
            mutate(group = group_fct[.data$group]) %>%
            mutate(width = .data$xmax - .data$xmin) %>%
            group_by(.data$group) %>%
            mutate(prop = .data$count / sum(.data$count, na.rm = TRUE)) %>%
            ungroup %>%
            select(one_of("x", "prop", "group", "width"))

        if(is_date) df_x$x %<>% as.Date(origin = "1970-01-01 UTC")

    }else{

        # ~~ plot x cat -----

        df_x = df %>%
            group_by(.data$x, .data$group) %>%
            summarise(n = n()) %>%
            group_by(.data$group) %>%
            mutate(prop = .data$n / sum(.data$n), width = 0.9) %>%
            ungroup

    }

    suppressWarnings({
        plot = df_x %>%
            ggplot() +
            geom_bar(aes(x = .data$x,
                         y = .data$prop,
                         group = .data$group,
                         width = .data$width), stat = "identity", fill = x_col, alpha = 0.8) +
            labs(title = name$x, y = "Density", x = name$x) +
            theme(legend.position = "none") +
            theme_pip()
    })

    if(use_group){
        plot = plot + facet_wrap(~group, ncol = 1)
    }

    # > check y -----

    use_y = !is.null(name$y)
    if(use_y){
        y_type = df$y %>% type_sum
        if(!y_type %in% c("dbl", "int")){
            warn(str_c("cant plot y variable type '", y_type, "'"))
            use_y = FALSE
        }
    }
    if(use_y){
        if(is.null(y_method)) y_method = if(is_num) "gam" else "glm"
        if(is_num){
            if(!y_method %in% c("gam", "glm", "loess", "none")){
                warn("expecting y_method to be one of: 'gam', 'glm', 'loess', 'none'")
                y_method = "none"
            }
        }else{
            if(!y_method %in% c("glm", "boxplot", "none")){
                warn("expecting y_method to be one of: 'glm', 'boxplot', 'none'")
                y_method = "none"
            }
        }
    }
    if(use_y){
        if(is.null(y_points)) y_points = FALSE
        y_points = isTRUE(y_points)
        if(!y_points && y_method == "none"){
            use_y = FALSE
        }
    }
    if(use_y){
        y_smooth = y_method != "none"
        if(y_smooth){
            if(is.null(y_family)){
                y_family = if(y_method == "loess") "gaussian" else guess_family(df$y)
            }
            if(y_family == "binomial" && y_method == "boxplot"){
                warn(str_c("boxplot method unsuitable for y variable type ", y_family))
            }
        }
    }

    # > plot y -----

    if(use_y){

        y1_breaks = c(0, df_x$prop) %>% pretty
        y2_breaks = NULL
        y2_y1 = function(y2) scales::rescale(y2, range(y1_breaks), range(y2_breaks))
        if(y_points || y_method == "boxplot"){
            y2_breaks %<>% c(range(df$y, na.rm = TRUE)) %>% pretty
        }
        df_y_mean = df %>%
            group_by(.data$group) %>%
            summarise(mean = mean(.data$y, na.rm = TRUE))

        if(is_num){

            # ~~ plot y num -----

            if(y_smooth){
                if(y_method == "gam"){
                    y_formula = str_c("y ~ s(x, k = ", k, ")")
                }else{
                    y_formula = "y ~ x"
                }
                smooth_plot = df %>%
                    ggplot() +
                    geom_smooth(
                        aes(.data$x, .data$y, group = .data$group),
                        method = y_method,
                        method.args = list(family = y_family),
                        formula = formula(y_formula)
                    )
                df_y = ggplot_build(smooth_plot)$data[[1]] %>%
                    as_data_frame %>%
                    mutate(group = group_fct[.data$group]) %>%
                    select(one_of("x", "y", "ymin", "ymax", "group"))
                if(is_date){
                    df_y$x %<>% as.Date(origin = "1970-01-01 UTC")
                }
                if(y_ci){
                    y2_breaks %<>%
                        c(range(df_y$ymin, na.rm = TRUE),
                          range(df_y$ymax, na.rm = TRUE)) %>%
                        pretty
                }else{
                    y2_breaks %<>%
                        c(range(df_y$y, na.rm = TRUE)) %>%
                        pretty
                }
            }
            if(y_smooth){
                plot = plot +
                    geom_line(aes(x = .data$x,
                                  y = y2_y1(.data$y),
                                  group = .data$group), df_y, col = y_col)
                if(y_ci){
                    plot = plot +
                        geom_ribbon(aes(x = .data$x,
                                        ymin = y2_y1(.data$ymin),
                                        ymax = y2_y1(.data$ymax),
                                        group = .data$group), df_y, fill = y_col, alpha = 0.3)
                }
            }

        }else{

            # ~~ plot y cat -----

            if(y_method == "boxplot"){
                plot = plot +
                    geom_boxplot(aes(.data$x, y2_y1(.data$y)), col = y_col, fill = y_col, df, alpha = 0.3)
            }else{
                df_y = df %>%
                    split(., .[["group"]]) %>%
                    map(function(df_y){
                        model = try(glm("y ~ x", y_family, df_y), TRUE)
                        if(!inherits(model, "try-error")){
                            df_y %<>% select(-.data$y) %>% distinct
                            preds = predict(model, df_y, type = "link", se.fit = TRUE)
                            df_y %<>%
                                mutate(
                                    y = preds$fit,
                                    ymin = preds$fit + 1.96 * preds$se.fit,
                                    ymax = preds$fit - 1.96 * preds$se.fit
                                ) %>%
                                mutate_at(c("y", "ymax", "ymin"), inv_link(y_family))
                        }
                        df_y
                    }) %>%
                    bind_rows
                if(y_smooth){
                    if(y_ci){
                        y2_breaks %<>%
                            c(range(df_y$ymin, na.rm = TRUE),
                              range(df_y$ymax, na.rm = TRUE)) %>%
                            pretty
                    }else{
                        y2_breaks %<>%
                            c(range(df_y$y, na.rm = TRUE)) %>%
                            pretty
                    }
                }
                if(y_smooth){
                    plot = plot +
                        geom_point(aes(x = .data$x,
                                       y = y2_y1(.data$y),
                                       group = .data$group), df_y, col = y_col)
                    if(y_ci){
                        plot = plot +
                            geom_errorbar(aes(x = .data$x,
                                              ymin = y2_y1(.data$ymin),
                                              ymax = y2_y1(.data$ymax),
                                              group = .data$group), df_y, col = y_col, width = 0.25)
                    }
                }
            }
        }

        if(y_points){
            plot = plot +
                geom_point(aes(.data$x, y2_y1(.data$y), group = .data$group), df, col = y_col, alpha = 0.8)
        }

        plot = plot +
            geom_hline(aes(yintercept = y2_y1(.data$mean), group = .data$group), col = y_col, df_y_mean, lty = 2) +
            scale_y_continuous(sec.axis = dup_axis(breaks = y2_y1(y2_breaks), labels = format(y2_breaks), name = name$y)) +
            labs(title = str_c(name$x, " vs ", name$y)) +
            labs(subtitle = str_c("y_method = ", y_method, ", y_family = ", y_family)) +
            theme(
                axis.text.y = element_text(color = x_col),
                axis.title.y = element_text(color = x_col),
                axis.text.y.right = element_text(color = y_col),
                axis.title.y.right = element_text(color = y_col)
            )
    }

    plot

}

guess_family = function(x){
    type = x %>% type_sum
    family = NULL
    if(type == "lgl"){
        family = "binomial"
    }
    if(is.null(family) &&
       min(x, na.rm = TRUE) == 0 &&
       max(x, na.rm = TRUE) == 1 &&
       n_distinct(x, na.rm = TRUE) == 2){
        family = "binomial"
    }
    if(is.null(family) &&
       type == "int" &&
       min(x, na.rm = TRUE) >= 0){
        family = "poisson"
    }
    if(is.null(family) &&
       type == "int" &&
       min(x, na.rm = TRUE) >= 0){
        family = "quasipoisson"
    }
    if(is.null(family) &&
       type == "dbl" &&
       min(x, na.rm = TRUE) >= 0 &&
       max(x, na.rm = TRUE) <= 1){
        family = "quasibinomial"
    }
    if(is.null(family) &&
       type == "dbl" &&
       min(x, na.rm = TRUE) > 0){
        family = "Gamma"
    }
    if(is.null(family)){
        family = "gaussian"
    }
    family
}

inv_link = function(family){
    family %>%
        switch(
            "binomial" = plogis,
            "quasibinomial" = plogis,
            "poisson" = exp,
            "quasipoisson" = exp,
            "Gamma" = function(x) 1 / x,
            identity
        )
}
