## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##

#' @title Rotate a set of points by a given angle
#' @description Description.
#' @details Details.
#' @param x a matrix of coordinates to rotate, with x values in first column and y values in second column
#' @param angle angle of rotation
#' @param axis axis of rotation
#' @param clockwise direction of rotation (default is \code{TRUE})
#' @param units units of the \code{angle} argument (default is \code{"radians"})
#' @return Value.
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' # original coordinates
#' x = cbind(x = c(1,1,2,2), y = c(1,2,2,1))
#' plot(x, type = "n", xlim = c(-0.5,2.5), ylim = c(-0.5,2.5), asp = 1, xlab = "x")
#' polygon(x)
#' 
#' # axis of rotation
#' axis = c(1,1)
#' points(axis[1], axis[2], col=2, pch=19)
#' 
#' # rotate clockwise by 2/3 pi radians
#' new = rotate.points(x, angle = 2/3*pi, axis = axis) 
#' polygon(new, border = 4)
#' 
#' # rotate anti-clockwise by 2/3 pi radians
#' new = rotate.points(x, angle = 2/3*pi, axis = axis, clockwise = FALSE) 
#' polygon(new, border = 3)
#' @export

rotate.points = function(x, angle, axis = c(0,0), clockwise = TRUE, units = c("radians", "degrees")){
    
    units = match.arg(units)
    
    # check coords
    if(!inherits(x, c("matrix", "data.frame", "array")) || ncol(x) != 2) stop("'coords' must be a 2-column matrix")
    
    # make sure coords is a matrix
    x = as.matrix(x)
    
    # if units are in degrees, convert angle to radians
    if(units == "degrees") angle = angle*(2*pi)/360
    
    # if anti-clockwise, invert angle
    if(!clockwise) angle = 2*pi - angle
    
    # make the rotaion matrix
    R = matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)), ncol=2)

    # subtract the axis point from each coordinate
    for(i in 1:2) x[,i] = x[,i] - axis[i] 
    
    # rotate the centred coordinates
    new = x %*% R 

    # add dimnames to new
    dimnames(new) = dimnames(x)
    
    # add the axis value to the rotated coordinates
    for(i in 1:2) new[,i] = new[,i] + axis[i]
    
    # return new coordinates
    return(new)
    
}

################################################################################
################################################################################