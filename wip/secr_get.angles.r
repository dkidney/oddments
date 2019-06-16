################################################################################
################################################################################

get.angles <- function (X, Y) {
#-------------------------------------------------------------------------------
# X and Y are 2-column matrices of coordinates
# Returns angles (0,360] from points in X to points in Y 
#         in matrix of dimension nrow(X) x nrow(Y) 
#
# testing code:
# X=as.matrix(data.frame(x=c(0,0,0,1,-1),y=c(0,1,-1,0,0)))
# Y=as.matrix(data.frame(x=c(0,1,1,1,0,-1,-1,-1),y=c(1,1,0,-1,-1,-1,0,1)))
# plot(X,pch=as.character(1:nrow(X)))
# points(Y,col="red")
# angles(X,Y)*180/pi
#-------------------------------------------------------------------------------
    onerow <- function (xy) {
        d <- function(xy2) {
           denom=sqrt(sum((xy2-xy)^2))
           if(denom!=0) {
             sintheta=(xy2[1]-xy[1])/denom
             theta=asin(sintheta)
           } else theta=0
           if(xy2[2]<xy[2] & xy2[1]>=xy[1]) theta=theta=pi - theta
           if(xy2[2]<xy[2] & xy2[1]<xy[1]) theta=pi - theta
           if(xy2[2]<xy[2] & xy2[1]==xy[1]) theta=pi
           if(xy2[2]>=xy[2] & xy2[1]<xy[1]) theta=2*pi + theta
           return(theta)
        }
        apply(Y, 1, d)
    }
    t(apply(X, 1, onerow))
}

################################################################################
################################################################################