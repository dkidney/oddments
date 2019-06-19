
plot_dist_chr = function(data, x, y = NULL, group = NULL, max_n_distinct = 30){
    if(!inherits(data, "data.frame")){
        warn("expecting data to be a data.frame")
        return(invisible())
    }
    if(!has_name(data, x)){
        warn("column ", x, " not in data")
        return(invisible())
    }
    if(!is.null(y) && !has_name(data, y)){
        warn("column ", y, " not in data - using y=NULL")
        y = NULL
    }
    if(!is.null(group) && !has_name(data, group)){
        warn("column ", group, " not in data - using group=NULL")
        group = NULL
    }
    if(!is.character(data[[x]])){
        warn(x, " coercing x to character")
    }
    if(n_distinct(data[[x]]) == 1){
        warn(x, " has zero variance")
        return(invisible())
    }
    if(length(caret::nzv(data[[x]])) == 1){
        warn(x, " has near zero variance")
    }
    if(n_distinct(data[[x]]) > max_n_distinct){
        warn(x, " has more than ", max_n_distinct, " distinct values")
        return(invisible())
    }

    # x plot -----

    df_x = data %>%
        group_by(.data[[!!x]]) %>%
        summarise(n = n()) %>%
        ungroup %>%
        mutate(prop = .data$n / nrow(data)) %>%
        mutate(label = .data$n %>% format(big.mark = ",", trim = TRUE))
    plot_obj = df_x %>%
        ggplot(aes_string(x, "prop")) +
        geom_bar(stat = "identity", fill = "blue", alpha = 0.5, width = 0.9) +
        geom_text(aes_string(label = "label"), vjust = 0) +
        labs(y = "Proportion") +
        scale_y_continuous(breaks = seq(0, 1, 0.1))

    # y plot -----

    if(!is_binary(data[[y]])){
        warn("y is not binary - can't deal with this yet")
        y = NULL
    }

    if(!is.null(y)){
        y1_to_y2 = function(y1) (y1 / y1_max * y2_rng) + y2_min
        y2_to_y1 = function(y2) (y2 - y2_min) / y2_rng * y1_max

        df_y = data %>%
            group_by(.data[[!!x]]) %>%
            summarise(
                n = sum(.data[[!!y]] %in% 0:1),
                n_ones = sum(.data[[!!y]] %in% 1)
            ) %>%
            mutate(
                n_zeros = .data$n - .data$n_ones,
                mean = .data$n_ones / .data$n,
            )

        # exact confidence intervals
        binom_ci = binom.confint(df_y$n_ones, df_y$n, methods = "exact")
        df_y$lower = binom_ci$lower
        df_y$upper = binom_ci$upper

        # y2 values
        y1_max = max(df_x$prop)
        # y2_min = 0
        y2_min = min(df_y$lower)
        y2_max = max(df_y$upper)
        y2_rng = y2_max - y2_min
        df_y %<>% mutate_at(vars(one_of("mean", "lower", "upper")), y2_to_y1)

        pop_n = sum(data[[y]] %in% 0:1)
        pop_n_ones = sum(data[[y]] %in% 1)
        pop_mean = y2_to_y1(pop_n_ones / pop_n)

        # y2 errorbars
        plot_obj = plot_obj +
            # geom_line(aes_string("x", "mean", group = 1), df, col = "red") +
            geom_point(aes_string(x, "mean"), df_y, col = "red") +
            geom_errorbar(aes_string(x, ymin = "lower", ymax = "upper"), df_y,
                          colour = "red", width = 0.5, inherit.aes = FALSE) +
            geom_hline(yintercept = pop_mean, col = "red", lty = 2)
        suppressMessages({
            plot_obj = plot_obj +
                scale_y_continuous(sec.axis = sec_axis(
                    trans = ~ y1_to_y2(.),
                    name = str_c(substitute(y), " (mean)"))
                )
        })
    }

    plot_obj


}
#
# N = sum(data[[y]] %in% 0:1)
# N_BAD = sum(data[[y]] %in% 1)
# BAD_RATE = N_BAD / N
# data %>%
#     mutate_at(x, as.character) %>%
#     group_by(.data[[!!x]]) %>%
#     summarise(
#         n = sum(.data[[!!y]] %in% 0:1),
#         n_bad = sum(.data[[!!y]] %in% 1)
#     ) %>%
#     mutate(
#         bad_rate = .data$n_bad / .data$n,
#         prop = .data$n / N
#     ) %>%
#     ggplot() +
#     geom_bar(aes_string(x, "bad_rate", fill = "prop"), stat = "identity") +
#     geom_hline(yintercept = BAD_RATE, lty = 2)
# }