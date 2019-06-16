

segment.transects = function(data, x0 = "x0", x1 = "x1", y0 = "y0", y1 = "y1", id = "id", width = 2000, length = 2000, sep = "_"){
    
    data = as.data.frame(data)
    
    n = nrow(data)
    
    # check that columns exist
    if(is.null(data[[x0]])) stop("column ", deparse(substitute(x0)), " not found in data", call. = FALSE)
    if(is.null(data[[x1]])) stop("column ", deparse(substitute(x1)), " not found in data", call. = FALSE)
    if(is.null(data[[y0]])) stop("column ", deparse(substitute(y0)), " not found in data", call. = FALSE)
    if(is.null(data[[y1]])) stop("column ", deparse(substitute(y1)), " not found in data", call. = FALSE)
    
    if(is.null(data[[id]])){
        data[[id]] = as.character(1:n)
    }else{
        if(length(unique(data[[id]])) != n)
            stop("ids not unique", call. = FALSE)
    }
    
    # convert start and stop to matrices
    start = as.matrix(data[,c(x0, y0)])
    stop  = as.matrix(data[,c(x1, y1)])
    
    # start and stop colnames
    colnames(start) = colnames(stop) = c("x","y")
    rownames(start) = rownames(stop) = data[[id]]
    
    # loop over transects
    polygons = do.call(c, lapply(data[[id]], function(i){ # i = data[[id]][1] ; i
        
        # calculate the bearing
        theta = as.numeric(calc.bearings(start[i,,drop = FALSE], stop[i,,drop = FALSE])) ; theta / (2*pi) * 360
        
        # temporary object to store transect coordinates
        original = rbind(start = start[i,], stop = stop[i,]) # original
        
        # rotate it so it points north
        rotated = rotate.points(original, theta, axis = start[i,], clockwise = FALSE, units = "radians") # rotated
        
        # divide the rotated transect into vertical sections
        yvals = seq(rotated["start","y"], rotated["stop","y"], by = length) # yvals
        
        # add on the maximum y-value if necessary
        if(max(yvals) < rotated["stop","y"]) yvals = c(yvals, rotated["stop","y"]) # yvals
        
        # make a list of polygons
        xvals = start[i,1] + c(-1,-1,1,1) * width/2 # xvals
        polygons = lapply(2:length(yvals), function(s){ # s = 2
            
            # calculate polygon coordinates
            poly = cbind(x = xvals, y = c(yvals[s-1], yvals[s], yvals[s], yvals[s-1])) # poly
            
            # rotate back onto original axis
            rotate.points(poly, theta, axis = start[i,], clockwise = TRUE, units = "radians")
            
        })
        
        # add names - <id>.<segment number>
        names(polygons) = paste(i, 1:length(polygons), sep = sep) #  names(polygons)
        
        return(polygons)
        
    }))
    
    # segment centre points
    centre = t(sapply(polygons, function(x) apply(x, 2, mean)))
    
    segments = data.frame(
        x          = centre[,1],
        y          = centre[,2],
        area       = sapply(polygons, areapl),
        id         = sapply(strsplit(names(polygons), sep), function(x) x[1]),
        segment    = sapply(strsplit(names(polygons), sep), function(x) x[2]),
        segment_id = names(polygons)
    )
    
    segments = base::merge(segments, data[,!colnames(data) %in% c(x0, x1, y0, y1)], by = "id")
    
    attr(segments, "polygons") = polygons
    
    return(segments)
    
}

