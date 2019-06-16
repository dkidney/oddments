require(fields)

# units of area needs to be square of units used for points

make.prediction.grid = function(points, buffer, cell.area){
    
    xlim = range(points[,1]) + c(-1,1) * buffer * 1.5
    ylim = range(points[,2]) + c(-1,1) * buffer * 1.5
    
    l = sqrt(cell.area)
    
    x = seq(xlim[1], xlim[2], by = l)
    y = seq(ylim[1], ylim[2], by = l)
    
    grid = as.matrix(expand.grid(x = x, y = y))
    
    z = apply(rdist(grid, points), 1, min)
    
    temp = contourLines(x, y, matrix(z, nrow = length(x)), levels = buffer)
    
    boundary = cbind(x = temp[[1]]$x, y = temp[[1]]$y)
    
    grid = grid[in.out(boundary, grid),]
    
    attr(grid, "boundary") = boundary
    
    attr(grid, "cell.area") = cell.area

    attr(grid, "buffer") = buffer

    return(grid)    
    
}

# example

points = rbind(c(0,0), c(1,0), c(2,0))

buffer = 1

area = 0.01

grid = make.prediction.grid(points, buffer, area)

plot(attr(grid, "boundary"), asp = 1, type = "l")

points(grid, pch = 15, col = "grey", cex = 0.5)

points(points, pch = 19, col = 2)

# area of boundary
require(splancs)
areapl(attr(grid, "boundary"))

# area of grid
nrow(grid) * attr(grid, "cell.area")





