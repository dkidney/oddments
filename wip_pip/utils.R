
#' @title Bespoke ggplot theme
#' @description Bespoke ggplot theme
#' @param base_size	base font size
#' @param base_family base font family
#' @param line_size base line size
#' @export
theme_pip = function(base_size = 11, base_family = "", line_size = 0.5){
    theme_bw(base_size, base_family) +
        theme(
            line = element_line(colour = "#2D2D2D", size = line_size),
            rect = element_rect(colour = "#2D2D2D", size = line_size),
            text = element_text(colour = "#2D2D2D", size = base_size),
            axis.title.x = element_text(margin = margin(t = 1, unit = "lines")),
            axis.title.y = element_text(margin = margin(r = 1, unit = "lines")),
            axis.title.y.right = element_text(margin = margin(l = 1, unit = "lines")),
            panel.border = element_rect(colour = "#2D2D2D", size = line_size),
            panel.grid.major = element_line(colour = "#D9D9D9", size = line_size),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(colour = "#999999", fill = "#999999", size = line_size),
            strip.text = element_text(colour = "white", face = "plain", size = base_size)
        )
}

is_binary = function(x){
    suppressWarnings({
        inherits(x, "logical") || (
            inherits(x, c("numeric", "integer")) &&
                # min(x, na.rm = TRUE) %in% 0 &&
                # max(x, na.rm = TRUE) %in% 1 &&
                isTRUE(all(x %in% 0:1, na.rm = TRUE))
        )
    })
}

is_near_zero_var = function(x, ...){
    1 %in% caret::nearZeroVar(x, saveMetrics = FALSE, ...)
}
