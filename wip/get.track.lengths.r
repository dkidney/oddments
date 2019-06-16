################################################################################
# Calculate length of track segments in each grid cell 
################################################################################

# require(sp)
# require(rgeos)

# track = matrix or data frame of track waypoints 
# grid = matrix or data frame of grid centre points

# For both of the above:
# - x coordinate in first column
# - y coordinate in second column
# - use same coordinate system
# - use an appropriate coordinate system for calculating distances - e.g. utm

get.track.lengths <- function(track, grid){
    
    w = sapply(1:2, function(i) abs(diff(unique(grid[,i])[1:2]))) ; w

    track.sp <- SpatialLines(list(Lines(list(Line(track)), ID = "a")))
    
    lengths <- sapply(1:nrow(grid), function(i){ # i = 1
        
        x = grid[i,1] + c(-1,1) * w[1]/2
        
        y = grid[i,2] + c(-1,1) * w[2]/2
        
        points = cbind(x[c(1,1,2,2,1)], y[c(1,2,2,1,1)])
        
        poly = SpatialPolygons(list(Polygons(list(Polygon(points)), ID = i)))
        
        int = gIntersection(track.sp, poly)
        
        if(!is.null(int)) SpatialLinesLengths(int, FALSE) else 0
        
    })
    
    return(lengths)
    
}

################################################################################
# Plot grid and track and optionally colour cells by length
################################################################################

# require(grDevices)

plot.track <- function(track, grid, lengths = NULL, labels = TRUE, add = FALSE, ncols = 100, alpha = 0.5){
    
    w = sapply(1:2, function(i) abs(diff(unique(grid[,i])[1:2]))) ; w
    
    if(!add){
        
        lims = lapply(1:2, function(i) range(grid[,i]) + c(-1,1) * w[i]/2) ; lims
        
        plot(grid, asp = 1, pch = 19, cex = 0.5, bty = "n", axes = FALSE, xlim = lims[[1]], ylim = lims[[2]], xaxs = "i", yaxs = "i", type = "n", xlab = "", ylab = "")
        
    }
    
    if(!(is.null(lengths))){
        
        cells = which(lengths != 0) ; cells
        
        cols = colorRampPalette(c("white", "red"))(ncols)[round(100 * lengths[cells] / max(lengths))]; cols
        
        for(i in cells){ # i = cells[1] ; i
            
            rect(grid[i,1] - w[1]/2,
                 grid[i,2] - w[2]/2,
                 grid[i,1] + w[1]/2,
                 grid[i,2] + w[2]/2,
                 col = cols[cells == i],
                 border = NA)
            
        }
        
    }
    
    for(x in unique(grid[,1])){
        
        y = grid[grid[,1] == x, 2] ; y
        
        segments(x0 = x + c(-1,1) * w[1]/2,
                 y0 = range(y)[1] - w[2]/2,
                 x1 = x + c(-1,1) * w[1]/2,
                 y1 = range(y)[2] + w[2]/2,
                 lty = 2, col = "grey")
        
    }
    
    for(y in unique(grid[,2])){
        
        x = grid[grid[,2] ==  y, 1] ; x
        
        segments(y0 = y + c(-1,1) * w[2]/2,
                 x0 = range(x)[1] - w[1]/2,
                 y1 = y + c(-1,1) * w[2]/2,
                 x1 = range(x)[2] + w[1]/2,
                 lty = 2, col = "grey")
        
    }
    
    if(labels) text(grid, labels = paste(1:nrow(grid)))
    
    lines(track, col = 4, type = "b", lwd = 2, pch = 19)
    
}

################################################################################
# Example
################################################################################

if(0){
    
    require(sp)
    require(rgeos)
    require(grDevices)

    # make grid centre points
    L = 2
    w = 0.5
    x = y = seq(0, L, w)
    grid = expand.grid(x = x, y = rev(y))
    
    # make a track
    n = 10
#     track = cbind(x = sort(runif(n, -w/2, L+w/2)),
#                   y = sort(runif(n, -w/2, L+w/2)))
    track = cbind(x = runif(n, -w/2, L+w/2),
                  y = runif(n, -w/2, L+w/2))

    # plot the track
    op <- par()
    par(mar = c(0,0,0,0))
    plot.track(track, grid)
    
    # calculate track lengths
    lengths = get.track.lengths(track, grid)
    head(lengths)
    sum(lengths)
    
    # check total track length is correct
    sum(sapply(2:nrow(track), function(i){
        sqrt(sum(sapply(1:2, function(j) track[i-1, j] - track[i, j])^2))    
    }))
    
    # plot lengths
    plot.track(track, grid, lengths)    
    suppressWarnings(par(op))

}

################################################################################
################################################################################
