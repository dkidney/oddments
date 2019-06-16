
#' @title Rotate 2D coordinates
#' @description Rotate a set of 2-D coordinates
#' @param x a 2-column matrix of coordinates to rotate
#' @param angle angle of rotation
#' @param axis axis of rotation
#' @param clockwise logical scalar indicating whether the direction of rotation should be clockwise or anti-clockwise
#' @param units angle units
#' @return TODO
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' # original coordinates
#' x = cbind(x = c(1,1,2,2), y = c(1,2,2,1))
#' plot(x, type = "n", xlim = c(-0.5,2.5), ylim = c(-0.5,2.25), asp = 1, xlab = "x")
#' polygon(x)
#'
#' # axis of rotation
#' axis = c(1,1)
#' points(axis[1], axis[2], col=2, pch=19)
#'
#' # rotate clockwise by 2/3 pi radians
#' rotated = rotate(x, angle = 2/3*pi, axis = axis)
#' polygon(rotated, border = 4)
#'
#' # rotate anti-clockwise by 2/3 pi radians
#' rotated = rotate(x, angle = 2/3*pi, axis = axis, clockwise = FALSE)
#' polygon(rotated, border = 3)
#' @export

rotate = function(x, angle, axis = c(0,0), clockwise = TRUE, units = c("radians", "degrees")){

    units = match.arg(units)
    # check coords
    if(!inherits(x, c("matrix", "data.frame", "array")) || ncol(x) != 2)
        stop("'coords' must be a 2-column matrix")
    # make sure coords is a matrix
    x = as.matrix(x)
    # if units are in degrees, convert angle to radians
    if(units == "degrees") angle = angle * (2 * pi) / 360
    # if anti-clockwise, calculate the clockwise angle
    if(!clockwise) angle = 2 * pi - angle
    # make the rotaion matrix
    R = matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)), ncol = 2)
    # subtract the axis coordinate from each coordinate in x
    for(i in 1:2) x[,i] = x[,i] - axis[i]
    # rotate the centred coordinates
    rotated = x %*% R
    # add dimnames to rotated coordinates
    dimnames(rotated) = dimnames(x)
    # add the axis coordinates to the rotated coordinates
    for(i in 1:2) rotated[,i] = rotated[,i] + axis[i]
    # return rotated coordinates
    return(rotated)
}

