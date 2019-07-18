
#' @rdname ggplot-utils
#' @name ggplot-utils
#' @title ggplot utility functions
#' @description
#' \itemize{
#'   \item \code{theme_oddments()} - bespoke ggplot theme
#'   \item \code{theme_base()} - experimental theme to to make ggplots look like R base
#'   plots
#'   \item \code{ggplot2_colours()} - returns a character vector of (hexadecimal) colour
#'   definitions
#' }
#' @importFrom ggplot2 margin rel theme theme_grey unit
#' @importFrom ggplot2 element_blank element_line element_rect element_text
#' @examples
#' \dontrun{
#' library(tidyverse)
#' ncols <- 8
#' df <- tibble(x = sample(letters[1:ncols], 100, replace = TRUE))
#' plot <- df %>%
#'   ggplot() +
#'   geom_bar(aes(x, fill = x), col = 1) +
#'   facet_grid(. ~ x, scales = "free_x") +
#'   labs(title = "title", subtitle = "subtitle")
#' plot
#' plot + theme_bw()
#' plot + theme_oddments()
#' plot + theme_base()
#' plot + theme_random()
#' plot + theme_dmp()
#' barplot(table(df$x), col = ggplot2_colours(ncols))
#' }
NULL

#' @rdname ggplot-utils
#' @name ggplot-utils
#' @export
#' @inheritParams ggplot2::theme_bw
#' @importFrom ggplot2 %+replace%
theme_oddments <- function(base_size = 11,
                           base_family = "",
                           base_line_size = base_size / 22,
                           base_rect_size = base_size / 22) {
    theme_grey(
        base_size = base_size,
        base_family = base_family
    ) %+replace%
        theme(
            # line = element_line(colour = 1, size = 0.1, linetype = 1, lineend = "butt"),
            # rect = element_rect(fill = "white", colour = 1, size = 1, linetype = 1),
            # rect = element_rect,
            # text,
            # title,
            # aspect.ratio,
            # axis.title,
            # axis.title.x,
            # axis.title.x.top,
            # axis.title.x.bottom,
            # axis.title.y,
            # axis.title.y.left,
            # axis.title.y.right,
            # axis.text,
            # axis.text.x,
            # axis.text.x.top,
            # axis.text.x.bottom,
            # axis.text.y,
            # axis.text.y.left,
            # axis.text.y.right,
            # axis.ticks,
            # axis.ticks.x,
            # axis.ticks.x.top,
            # axis.ticks.x.bottom,
            # axis.ticks.y,
            # axis.ticks.y.left,
            # axis.ticks.y.right,
            # axis.ticks.length,
            # axis.ticks.length.x,
            # axis.ticks.length.x.top,
            # axis.ticks.length.x.bottom,
            # axis.ticks.length.y,
            # axis.ticks.length.y.left,
            # axis.ticks.length.y.right,
            # axis.line,
            # axis.line.x,
            # axis.line.x.top,
            # axis.line.x.bottom,
            # axis.line.y,
            # axis.line.y.left,
            # axis.line.y.right,
            # legend.background,
            # legend.margin,
            # legend.spacing,
            # legend.spacing.x,
            # legend.spacing.y,
            # legend.key = element_rect(fill = "white", colour = NA),
            # legend.key.size,
            # legend.key.height,
            # legend.key.width,
            # legend.text,
            # legend.text.align,
            # legend.title,
            # legend.title.align,
            # legend.position,
            # legend.direction,
            # legend.justification,
            # legend.box,
            # legend.box.just,
            # legend.box.margin,
            # legend.box.background,
            # legend.box.spacing,
            panel.background = element_rect(fill = "white", colour = NA),
            # panel.border = element_rect(fill = NA, colour = "green"),
            panel.border = element_rect(fill = NA, colour = "grey20"),
            # panel.spacing,
            # panel.spacing.x,
            # panel.spacing.y,
            panel.grid = element_line(colour = "grey90"),
            # panel.grid.major,
            panel.grid.minor = element_blank(),
            # panel.grid.major.x,
            # panel.grid.major.y,
            # panel.grid.minor.x,
            # panel.grid.minor.y,
            # panel.ontop,
            # plot.background,
            # plot.title,
            # plot.subtitle,
            # plot.caption,
            # plot.tag,
            # plot.tag.position,
            # plot.margin,
            # strip.background = element_rect(fill = "grey85", colour = "grey20"),
            strip.background = element_rect(fill = "grey", colour = "grey"),
            # strip.background.x,
            # strip.background.y,
            # strip.placement,
            strip.text = element_text(
                colour = "white",
                size = rel(1),
                face = "bold",
                margin = margin(4.4, 4.4, 4.4, 4.4)
            ),
            # strip.text.x,
            # strip.text.y,
            # strip.switch.pad.grid,
            # strip.switch.pad.wrap,
            # complete = TRUE,
            validate = TRUE
        )
}

#' @rdname ggplot-utils
#' @name theme_base
#' @export
theme_base <- function(base_size = 12,
                       base_family = "Helvetica",
                       base_line_size = base_size / 22,
                       base_rect_size = base_size / 22) {
    half_line <- base_size / 2
    line_size <- 0.5

    theme(
        line = element_line(
            colour = "black",
            size = line_size,
            linetype = 1,
            lineend = "butt"
        ),
        rect = element_rect(
            fill = "white",
            colour = "black",
            size = line_size,
            linetype = 1
        ),
        text = element_text(
            family = base_family,
            # face       = "plain",
            # colour     = "black",
            size = base_size,
            # lineheight = 0.9,
            # hjust      = 0.5,
            # vjust      = 0.5,
            # angle      = 0,
            # margin     = margin(),
            debug = FALSE
        ),

        # axis ----
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

        # legend ----
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

        # panel ----
        # panel.background = element_rect(fill = "grey92", colour = NA),
        # panel.border = element_blank(), panel.grid.major = element_line(colour = "white"),
        # panel.grid.minor = element_line(colour = "white", size = 0.25),
        # panel.spacing = unit(half_line, "pt"),
        # panel.spacing.x = NULL,
        # panel.spacing.y = NULL, panel.ontop = FALSE,
        panel.background = element_rect(fill = "white", colour = NA),
        panel.border = element_rect(fill = NA, colour = "black"),
        # panel.grid.major = element_line(colour = "grey90", size = 0.2),
        # panel.grid.minor = element_line(colour = "grey98", size = 0.5),

        # plot ----
        # plot.background = element_rect(colour = "white"),
        plot.title = element_text(size = rel(1.2), face = "bold", margin = margin(b = half_line * 1.2)),
        # plot.margin = margin(half_line, half_line, half_line, half_line),

        # strip ----
        # strip.background = element_rect(fill = "grey85", colour = NA),
        strip.background = element_rect(fill = "grey80", colour = "grey50", size = 0.2),
        strip.text = element_text(colour = "grey10", size = rel(0.8)),
        strip.text.x = element_text(margin = margin(t = half_line, b = half_line)),
        strip.text.y = element_text(angle = -90, margin = margin(l = half_line, r = half_line)),
        strip.switch.pad.grid = unit(0.1, "cm"),
        strip.switch.pad.wrap = unit(0.1, "cm")
    )
}

#' @rdname ggplot-utils
#' @name theme_dmp
#' @export
theme_random = function(pointsize = 8,
                        family = "serif",
                        linesize = 0.25,
                        legend = FALSE){
    mytheme = theme(
        axis.text = element_text(size   = pointsize,
                                 family = family,
                                 colour = 1),
        axis.ticks = element_line(colour    = 1,
                                  size      = linesize),
        legend.text = element_text(size     = pointsize,
                                   family   = family,
                                   colour   = 1),
        legend.position = "top",
        legend.background = element_rect(fill = 'white'),
        legend.key = element_rect(fill = 'white'),
        panel.background = element_rect(fill       = "white",
                                        colour     = 1,
                                        size       = linesize,
                                        linetype   = 1),
        panel.border = element_rect(fill        = NA,
                                    colour      = 1,
                                    size        = linesize,
                                    linetype    = NULL),
        panel.spacing = unit(0.2, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0,0.25,0,0), "cm"),
        rect = element_rect(size = linesize),
        strip.background = element_rect(fill        = "grey80",
                                        colour      = 1,
                                        size        = linesize,
                                        linetype    = NULL),
        strip.text  = element_text(size     = pointsize,
                                   family   = family,
                                   colour   = 1),
        text = element_text(size    = pointsize,
                            family  = family,
                            colour  = 1)
    )
    if(!legend) mytheme = mytheme + theme(legend.position = "none")
    return(mytheme)
}

#' @rdname ggplot-utils
#' @name theme_dmp
#' @export
theme_dmp <- function(base_size = 8,
                      base_family = "Helvetica",
                      base_line_size = base_size / 22,
                      base_rect_size = base_size / 22) {

    # dmpGreen1 = "Tomatoe"
    dmpGreen1 = "green"
    dmpGreen2 = "purple"
    dmpGreen3 = "blue"
    dmpGreen4 = "orange"

    # Starts with theme_grey and then modifies some parts
    ggplot2::theme_grey(
        base_size = base_size,
        base_family = base_family
    ) +
        theme(
            axis.ticks = element_line(colour = dmpGreen3),
            panel.background  = element_rect(fill = "white", colour = NA),
            panel.border = element_rect(fill = NA, colour =  dmpGreen3), #element_blank(),
            panel.grid.major = element_line(colour = dmpGreen1),
            panel.grid.minor = element_line(colour = dmpGreen2, size = 0.25),
            panel.spacing = unit(0.25, "lines"),
            strip.background = element_rect(fill = dmpGreen4, colour = NA),
            strip.text = element_text(face = "bold", colour = "white", size = base_size, family = base_family),
            #strip.text.x = element_text(),
            strip.text.y = element_text(angle = -90, size = base_size, family = base_family)
        )
}

#' @rdname ggplot-utils
#' @name ggplot2_colours
#' @export
#' @param ncols number of colours to generate
ggplot2_colours <- function(ncols) {
    h <- c(0, 360) + 15
    if ((diff(h) %% 360) < 1) {
        h[2] <- h[2] - 360 / ncols
    }
    grDevices::hcl(
        h = seq(h[1], h[2], length.out = ncols),
        c = 100,
        l = 65
    )
}

#' @rdname ggplot-utils
#' @name ggplot2_colors
#' @export
ggplot2_colors <- ggplot2_colours

if(0){
    tomato         = "#FF6347"
    orange         = "#FFA500"
    dodgerblue     = "#1E90FF"
    mediumseagreen = "#3CB371"
    gray           = "#BEBEBE"
    slateblue      = "#6A5ACD"
    violet         = "#EE82EE"
    lightgray      = "#D3D3D3"
}



