
#' @title Bespoke ggplot theme
#' @description Bespoke ggplot theme
#' @param base_size	base font size
#' @param base_family base font family
#' @export
theme_oddments = function(base_size = 11, base_family = ""){
    theme_bw(
        base_size = base_size,
        base_family = base_family
    ) +
        theme(
            panel.grid.minor = element_blank()
        )
}