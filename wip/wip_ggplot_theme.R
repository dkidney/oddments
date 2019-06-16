
#' @title ggplot base grapics theme
#' @description Theme to make ggplots look like R base graphics plots.
#' @param base_size	base font size
#' @param base_family base font family
#' @examples
#' x = rnorm(100)
#' y = rnorm(100)
#' op = par(mar = c(4, 4, 2, 0.5))
#' plot(x, y, pch = 19, cex = 0.75, main = "Title")
#' par(op)
#'
#' library(ggplot2)
#' ggplot(data.frame(x = x, y = y), aes(x, y)) +
#'   geom_point() +
#'   base_theme() +
#'   labs(title = "Title")
#' @export

base_theme = function(base_size = 12, base_family = "Helvetica"){

    half_line = base_size / 2
    line_size = 0.5

    theme(
        line = element_line(colour = "black", size = line_size, linetype = 1, lineend  = "butt"),
        rect = element_rect(fill = "white", colour = "black", size = line_size, linetype = 1),
        text = element_text(
            family     = base_family,
            # face       = "plain",
            # colour     = "black",
            size       = base_size,
            # lineheight = 0.9,
            # hjust      = 0.5,
            # vjust      = 0.5,
            # angle      = 0,
            # margin     = margin(),
            debug      = FALSE
        ),
        #
        # # axis ----
        # axis.line   = element_line(),
        # axis.line.x = element_blank(),
        # axis.line.y = element_blank(),
        axis.text = element_text(size = rel(1), colour = "black"),
        axis.text.x = element_text(margin = margin(t = half_line), vjust = 1),
        axis.text.y = element_text(margin = margin(r = half_line), hjust = 0.5, angle = 90),
        axis.ticks.length = unit(half_line, "pt"),
        axis.title.x = element_text(size = rel(1), margin = margin(t = base_size, b = 0)),
        axis.title.y = element_text(size = rel(1), margin = margin(r = base_size, l = 0)),
        # axis.title.y = element_text(angle = 90, margin = margin(r = 0.8 * half_line, l = 0.8 * half_line/2)),
        axis.ticks = element_line(size = line_size, colour = "black"),
        #
        # # legend ----
        # legend.background = element_rect(colour = NA),
        # legend.margin = unit(0.2, "cm"),
        # legend.key = element_rect(fill = "grey95", colour = "white"), legend.key.size = unit(1.2, "lines"),
        # legend.key.height = NULL,
        # legend.key.width = NULL,
        # legend.text = element_text(size = rel(0.8)),
        # legend.text.align = NULL,
        # legend.title = element_text(hjust = 0),
        # legend.title.align = NULL,
        # legend.position = "right",
        # legend.direction = NULL,
        # legend.justification = "center",
        # legend.box = NULL,
        # legend.key = element_rect(colour = "grey80"),
        #
        # # panel ----
        # panel.background = element_rect(fill = "grey92", colour = NA),
        # panel.border = element_blank(), panel.grid.major = element_line(colour = "white"),
        # panel.grid.minor = element_line(colour = "white", size = 0.25),
        # panel.margin = unit(half_line, "pt"),
        # panel.margin.x = NULL,
        # panel.margin.y = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = "white", colour = NA),
        panel.border = element_rect(fill = NA, colour = "black"),
        # panel.grid.major = element_line(colour = "grey90", size = 0.2),
        # panel.grid.minor = element_line(colour = "grey98", size = 0.5),
        #
        # # plot ----
        # plot.background = element_rect(colour = "white"),
        plot.title = element_text(size = rel(1.2), face = "bold", margin = margin(b = half_line * 1.2)),
        # plot.margin = margin(half_line, half_line, half_line, half_line),

        # strip ----
        strip.background = element_rect(fill = "grey85", colour = NA),
        strip.background = element_rect(fill = "grey80", colour = "grey50", size = 0.2),
        strip.text = element_text(colour = "grey10", size = rel(0.8)),
        strip.text.x = element_text(margin = margin(t = half_line, b = half_line)),
        strip.text.y = element_text(angle = -90, margin = margin(l = half_line, r = half_line)),
        strip.switch.pad.grid = unit(0.1, "cm"),
        strip.switch.pad.wrap = unit(0.1, "cm")

    )

}

