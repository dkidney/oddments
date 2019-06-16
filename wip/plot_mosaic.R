
#' @title Mosaic plot
#' @description A ggplot implementation of mosaic plots. The function is Based on
#'   \link[ggplot2]{geom_bar}, with \code{stat = "identity"} and \code{position = "fill"},
#'   but with a modified \code{width} aesthetic so that the width of the bars reflects the
#'   sample sizes of the x-category levels and the spacings between the bars are equally
#'   sized.
#' @param x categorical variable for the x-axis
#' @param y categorical variable for the y-axis
#' @param gap numeric scalar controlling the spacing between the bars (giving the
#'   percentage of the combined bar width to be used for the spacing between bars
#' @param border_col colour of the border around the bars
#' @examples
#' n = 100
#'
#' category_1 = c('one','two','three','four') %>%
#'     factor %>%
#'     forcats::fct_inorder() %>%
#'     sample(n, replace = TRUE)
#'
#' category_2 = LETTERS[1:4] %>%
#'     sample(n, replace = TRUE)
#'
#' table(category_2, category_1) %>%
#'     addmargins(1)
#'
#' plot_mosaic(category_1, category_2)
#' plot_mosaic(category_1, category_2, gap = 0, border_col = grey(0.4))
#' @export

plot_mosaic = function(x, y, gap = 10, border_col = NA){

    if(gap < 0) stop("gap must be greater than or equal to 0")

    df = data_frame(
        x = as.factor(x),
        y = as.factor(y)
    ) %>%
        group_by(x, y) %>%
        summarise(n = n()) %>%
        ungroup

    breaks = df %>%
        group_by(x) %>%
        summarise(width = sum(n)) %>%
        mutate(
            half_width = width / 2,
            lag_half_width = half_width %>% lag %>% if_else(is.na(.), 0, .),
            gap = sum(width) * gap / 100 / (nrow(.) - 1),
            breaks = cumsum(half_width + lag_half_width + gap)
        )

    df %>%
        left_join(breaks, by = "x") %>%
        ggplot(aes(x = breaks, y = n, fill = y, width = width)) +
        geom_bar(stat = "identity", position = "fill", colour = border_col) +
        scale_x_continuous(breaks = breaks$breaks, label = breaks$x, expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) +
        labs(x = substitute(x), fill = substitute(y)) +
        theme(
            axis.text.y  = element_blank(),
            # axis.text.x  = element_text(angle = angle, hjust = 1, vjust = 0.5),
            axis.ticks   = element_blank(),
            axis.title.y = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
        )

}
