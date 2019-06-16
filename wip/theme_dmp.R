## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##

#' @rdname theme_dmp
#' @name theme_dmp
#' @title DMP ggplot2 theme
#' @param base_size font size (default is 8 point)
#' @param base_family font family (default is Calibri)
#' @author Bruno
#' @export
#' @importFrom grid unit
#' @family DMP ggplot2 functions
#' @examples
#' # unload packages
#' detach("package:DMP", unload = TRUE, force = TRUE)
#' detach("package:ggplot2", unload = TRUE, force = TRUE)
#' 
#' # some data to plot
#' data = data.frame(x = rnorm(100), y = rnorm(100))
#' 
#' # default ggplot theme
#' require(ggplot2)
#' ggplot(data, aes(x,y)) + geom_point()
#' 
#' # default ggplot theme = DMP theme
#' require(DMP)
#' ggplot(data, aes(x,y)) + geom_point()
#' 
#' # use default theme with DMP package loaded
#' ggplot(data, aes(x,y)) + geom_point() + theme_grey()
#' 
#' # use DMP theme but change default settings
#' ggplot(data, aes(x,y)) + geom_point() + theme_dmp(base_size = 20)

theme_dmp <- function(base_size = 8, base_family = "Calibri"){

  dmpGreen1 <- rgb(red = 206, green = 237, blue = 238, maxColorValue = 255) 
  dmpGreen2 <- rgb(red = 230, green = 246, blue = 246, maxColorValue = 255)
  #dmpGreen1 <- rgb(red = 221, green = 242, blue = 243, maxColorValue = 255)
  #dmpGreen2 <- rgb(red = 236, green = 248, blue = 248, maxColorValue = 255)
  dmpGreen3 <- rgb(red = 102, green = 200, blue = 202, maxColorValue = 255)
  dmpGreen4 <- rgb(red =  52, green = 150, blue = 152, maxColorValue = 255)
  
  # Starts with theme_grey and then modifies some parts
  theme_grey(base_size = base_size, base_family = base_family) +
    theme(
      axis.ticks = element_line(colour = dmpGreen3),
      
      panel.background  = element_rect(fill = "white", colour = NA),
      panel.border = element_rect(fill = NA, colour =  dmpGreen3), #element_blank(),
      panel.grid.major = element_line(colour = dmpGreen1),
      panel.grid.minor = element_line(colour = dmpGreen2, size = 0.25),
      panel.margin = unit(0.25, "lines"),
      
      strip.background = element_rect(fill = dmpGreen4, colour = NA), 
      strip.text = element_text(face = "bold", colour = "white", size = base_size, family = base_family),
      #strip.text.x = element_text(), 
      strip.text.y = element_text(angle = -90, size = base_size, family = base_family)
    )
  
}

## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
