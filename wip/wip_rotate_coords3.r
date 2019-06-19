################################################################################
################################################################################

rotate.points <- function(coords, theta, pivot=c(0,0), theta.units="radians"){
    
    # rotates an arbitrary set of coordinates clockwise by angle theta using a specified pivot point

    # Inputs:
    #   coords - a matrix of coordinates with X values in column 1 and Y values in column 2
    #   theta - the angle of clockwise rotation
    #   pivot - the axis of rotation
    
    # if theta is in degrees, convert to radians
    if(theta.units=="degrees") theta <- theta*(2*pi)/360 ; theta

    # centre the points by subtracting the pivot coordinates
    for(i in 1:2) coords[,i] <- coords[,i] - pivot[i] ; coords

    # contruct the rotaion matrix
    R <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), ncol=2, byrow=TRUE)

    # rotate the centred coordinates
    newcoords <- coords %*% R ; newcoords
    if(length(dimnames(coords))>0) dimnames(newcoords) <- dimnames(coords) ; newcoords
    
    # add the pivot value to the rotated coordinates
    for(i in 1:2) newcoords[,i] <- newcoords[,i] + pivot[i]

    # return new coordinates
    return(newcoords)

}

################################################################################

# Testing

if(0){
    
    # E.g. rotate a square using the bottom left hand corner as the pivot point
    X <- c(1,1,2,2)
    Y <- c(1,2,2,1)
    coords <- cbind(X,Y)
    pivot <- c(X[1],Y[1])
    theta <- 45
    newpoly <- rotate.points(coords, theta, pivot) ; newpoly
    plot(X,Y,type="n", xlim=c(0,3), ylim=c(0,3), asp=1)
    polygon(coords)
    points(pivot[1],pivot[2],col=2,pch=19)
    polygon(newpoly, border=4)
    
}

################################################################################
################################################################################
