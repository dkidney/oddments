## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##

#' @rdname ggplot2.replacement.functions
#' @name ggplot2-replacement-functions
#' @title Replacement ggplot2 functions
#' @description Replacement versions of \code{ggplot.data.frame} and \code{ggsave}. There is no need to call \code{ggplot.data.frame} directly. It updates the existing ggplot function so that it uses the DMP theme by default.
#' @author Darren
#' @import ggplot2
#' @family DMP ggplot2 functions
#' @seealso \code{\link{ggsave}} \code{\link{ggplot}}
#' @seealso \code{\link{dmp-devices}}
NULL

#' @export
ggsave = function(width = 10, height = 8, units = "cm", dpi = 300, ...){
    
    ggplot2:::ggsave(width = width, height = height, units = units, ...)
    
}    

ggplot.data.frame = function(...){
   
   ggplot2:::ggplot.data.frame(...) + theme_dmp()
   
}

## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
