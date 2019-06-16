## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##

#' Colours used by ggplot2 
#'
#' Description
#' 
#' Details
#'
#' @param n the number of colors
#' @param h not sure what this does
#' @return Returns a ggplot2 theme object.
#' @export

ggplotCols <- function(n = 6, h = c(0, 360) + 15){
    
    if ((diff(h)%%360) < 1) h[2] <- h[2] - 360/n
    
    hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
    
}

################################################################################
################################################################################
################################################################################
################################################################################

#' My ggplot2 theme
#'
#' A theme for making plots with ggplot2.
#' 
#' The theme I used for the ggplot2 figures in my PhD.
#'
#' @param pointsize font size (default is 8)
#' @param family font type (default is "serif") 
#' @param linesize width of lines for plot borders (default is 0.25)
#' @param legend if \code{FALSE} then legend is suppressed
#' @return Returns a ggplot2 theme object.
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @export

mytheme = function(pointsize = 8, family = "serif", linesize = 0.25, legend = FALSE){
    
    mytheme = theme(
        
        axis.text = element_text(size   = pointsize, 
                                 family = family, 
                                 colour = 1),
        
        axis.ticks = element_line(colour    = 1,
                                  size      = linesize),
        
        #---------------------------------------------------------------------------
        
        legend.text = element_text(size     = pointsize, 
                                   family   = family, 
                                   colour   = 1),
        
        legend.position = "top",
        
        legend.background = element_rect(fill = 'white'),
        
        legend.key = element_rect(fill = 'white'),
        
        #---------------------------------------------------------------------------
        
        panel.background = element_rect(fill       = "white",
                                        colour     = 1,
                                        size       = linesize,
                                        linetype   = 1),
        
        panel.border = element_rect(fill        = NA, 
                                    colour      = 1, 
                                    size        = linesize, 
                                    linetype    = NULL),
        
        panel.margin = unit(0.2, "cm"),
        
        panel.grid.major = element_blank(),
        
        panel.grid.minor = element_blank(),
        
        #-----------------------------------------------------------------------
        
        plot.margin = unit(c(0,0.25,0,0), "cm"),
        
        #---------------------------------------------------------------------------
        
        rect = element_rect(size = linesize),
        
        #---------------------------------------------------------------------------
        
        strip.background = element_rect(fill        = "grey80",
                                        colour      = 1,
                                        size        = linesize,
                                        linetype    = NULL),
        
        strip.text  = element_text(size     = pointsize, 
                                   family   = family, 
                                   colour   = 1),
        
        #---------------------------------------------------------------------------
        
        text = element_text(size    = pointsize, 
                            family  = family, 
                            colour  = 1)
        
    )
    
    if(!legend) mytheme = mytheme + theme(legend.position = "none")
    
    return(mytheme)
        
}

################################################################################
################################################################################
