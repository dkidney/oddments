################################################################################
################################################################################

# Distance 1.0 Functions

################################################################################
################################################################################

make.segment.table <- function(transect.table, w, segment.length=NULL){
    
    # transect table needs to have the following columns
    # - start.utm.x
    # - start.utm.y
    # - stop.utm.x
    # - stop.utm.y
    # - transect.id - MUST BE UNIQUE
    
    if(is.null(segment.length)) segment.length <- 2*w ; segment.length
    
    segment.table <- do.call(rbind, lapply(1:nrow(transect.table), function(i){ # i=i
        
        transect.start <- matrix(c(transect.table$start.utm.x[i], transect.table$start.utm.y[i]), ncol=2, dimnames=list("start", c("x","y"))) ; transect.start
        transect.stop  <- matrix(c(transect.table$stop.utm.x[i], transect.table$stop.utm.y[i]),  ncol=2, dimnames=list("stop", c("x","y"))) ; transect.stop
        
        # calculate bearing
        theta <- get.angles(transect.start, transect.stop) ; theta/(2*pi)*360
        
        # centre and rotate the trip to point north 
        pivot.point <- c(x=transect.table$start.utm.x[i], y=transect.table$start.utm.y[i]) ; pivot.point
        rotated.points <- rotate.points(coords = rbind(transect.start,transect.stop), theta = 2*pi - theta, pivot.point) ; rotated.points
        
        # divide into segments
        y.breaks <- seq(rotated.points["start","y"], rotated.points["stop","y"], by=segment.length) ; y.breaks
        if(max(y.breaks) < rotated.points["stop","y"]) y.breaks <- c(y.breaks, rotated.points["stop","y"]) ; y.breaks
        n.segments <- length(y.breaks)-1 ; n.segments
        
        # calculate segment lengths
        segment.effort <- y.breaks[-1] - y.breaks[1:n.segments] ; segment.effort
        
        # get centre, left and right hand side segment coordinates
        c.points <- cbind(x=rotated.points["start","x"]+0, y=y.breaks[1:n.segments] + segment.effort/2) ; c.points
        l.points <- cbind(x=rotated.points["start","x"]-w, y=y.breaks) ; l.points
        r.points <- cbind(x=rotated.points["start","x"]+w, y=y.breaks) ; r.points
        
        # rotate segment coordinates onto to original bearing
        c.points <- rotate.points(c.points, theta, pivot.point) ; c.points
        l.points <- rotate.points(l.points, theta, pivot.point) ; l.points
        r.points <- rotate.points(r.points, theta, pivot.point) ; r.points
        
        # store coordinates for each segment
        data.frame(
            Region.Label = as.character(transect.table$Region.Label[i]),
            Sample.Label = paste(transect.table$Sample.Label[i], 1:n.segments, sep="_s"),
            transect.label = as.character(transect.table$Sample.Label[i]),
            segment.number = 1:n.segments,
            Effort = segment.effort,
            centre.x = c.points[, "x"],
            centre.y = c.points[, "y"],
            corner1.x = l.points[1:n.segments, "x"],
            corner1.y = l.points[1:n.segments, "y"],
            corner2.x = l.points[-1, "x"],
            corner2.y = l.points[-1, "y"],
            corner3.x = r.points[-1, "x"],
            corner3.y = r.points[-1, "y"],
            corner4.x = r.points[1:n.segments, "x"],
            corner4.y = r.points[1:n.segments, "y"],
            stringsAsFactors = FALSE
        )
        
    }))
    
    return(segment.table)
}


################################################################################
################################################################################

match.sightings.to.segments <- function(segment.table, obs.table){
    
    new.obs.table <- obs.table ; head(new.obs.table) ; dim(new.obs.table)
    
    new.obs.table$transect.label <- new.obs.table$Sample.Label ; head(new.obs.table) ; dim(new.obs.table)
    
    new.obs.table$segment.number <- new.obs.table$Sample.Label <- NA ; head(new.obs.table) ; dim(new.obs.table)
    
    obs.coords <- as.matrix(new.obs.table[,c("utm.x","utm.y")]) ; head(obs.coords) ; dim(obs.coords)
    
    unique.transects <- unique(as.character(new.obs.table$transect.label)) ; head(unique.transects) ; length(unique.transects)
    
    for(trans in unique.transects){ # trans = unique(as.character(new.obs.table$transect.label))[1] ; trans
        
        segs.in.trans <- which(segment.table$transect.label==trans) ; segs.in.trans
        obs.in.trans  <- which(new.obs.table$transect.label==trans) ; obs.in.trans
        
        if(length(obs.in.trans) > 0){
            
            for(seg in 1:length(segs.in.trans)){ # seg = 2
                
                poly.matrix <- with(segment.table[segs.in.trans,][seg,], {
                    cbind(x = c(corner1.x, corner2.x, corner3.x, corner4.x),
                          y = c(corner1.y, corner2.y, corner3.y, corner4.y))
                }) ; poly.matrix
                
                obs.in.seg <- which(pinpoly(poly.matrix, obs.coords[obs.in.trans,]) != 0) ; obs.in.seg
                
                if(length(obs.in.seg) > 0){
                    
                    new.obs.table$Sample.Label[obs.in.trans][obs.in.seg] <- as.character(segment.table$Sample.Label[segs.in.trans][seg])
                    new.obs.table$segment.number[obs.in.trans][obs.in.seg] <- segment.table$segment.number[segs.in.trans][seg]
                    
                }
            }
            
        }
        
    }
    
    return(new.obs.table)
}

if(0){
    
    transect.table <- data.frame(Region.Label = 1,
                                 Sample.Label = 1,
                                 Effort = sqrt(2),
                                 start.utm.x = 0,
                                 start.utm.y = 0,
                                 stop.utm.x = 1,
                                 stop.utm.y = 1
    )
    
    segment.table <- segment.transects(transect.table, w=0.1) ; segment.table
    
    plot.segments(segment.table)
    
    n <- 100
    obs.table <- data.frame(Region.Label = 1,
                            Sample.Label = 1,
                            object = 1:n,
                            observer = 1,
                            detected = 1,
                            size = 1,
                            utm.x = runif(n),
                            utm.y = runif(n)
    )
    
    points(obs.table$utm.x, obs.table$utm.y)
    
    obs.table <- match.sightings.to.segments(segment.table, obs.table) ; head(obs.table)
    
    points(obs.table$utm.x, obs.table$utm.y, pch=19, col=obs.table$segment.number)
    
    
}

################################################################################
################################################################################

plot.segments <- function(segment.table, add=FALSE, numbers=TRUE){
    
    xlim = range(segment.table[,c("corner1.x","corner2.x","corner3.x","corner4.x")])
    ylim = range(segment.table[,c("corner1.y","corner2.y","corner3.y","corner4.y")])
    
    if(!add) plot(1,1, type="n", xlim=xlim, ylim=ylim, asp=1, ylab="x (m)", xlab="y (m)")
    
    transect.labels <- unique(segment.table$transect.label) ; transect.labels
    
    for(tran in 1:length(transect.labels)){ # tran = 1
        
        segments <- which(segment.table$transect.label == transect.labels[tran]) ; segments
        
        for(seg in segments){ # seg = 1
            
            with(segment.table[seg,], {
                
                polygon(x = c(corner1.x, corner2.x, corner3.x, corner4.x),
                        y = c(corner1.y, corner2.y, corner3.y, corner4.y),
                        border = segment.number)
                
                if(numbers) text(centre.x, centre.y, labels = as.character(segment.number), col = segment.number)
                
            })
            
        }

    }

}

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
