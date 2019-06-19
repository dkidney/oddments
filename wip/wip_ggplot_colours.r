## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##

#' @rdname ggplot2.colors
#' @title Generate a vector of colours as used by ggplot2
#' @description Description.
#' @details Behaves like \code{\link{heat.colors}}.
#' @param ncols number of colours to generate
#' @author Darren
#' @examples
#' ncols = 8
#' 
#' # standard colours
#' barplot(rep(1,ncols), col = 1:ncols)
#' 
#' # ggplot2 colours
#' barplot(rep(1,ncols), col = ggplot2.colors(ncols))
#' @return Returns a character vector of (hexadecimal) colour definitions.
#' @family DMP ggplot2 functions
#' @export

ggplot2.colors = function(ncols){
    
    h = c(0, 360) + 15
    
    if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360 / ncols
    
    hcl(h = (seq(h[1], h[2], length = ncols)), c = 100, l = 65)
    
}

#' @rdname ggplot2.colors
#' @family ggplot2.functions

ggplot2.colours = ggplot2.colors

## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
