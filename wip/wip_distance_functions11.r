## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##

#' @title Make a table of segments from transect data
#' @description Takes start and stop coordinates for a set of transects and 
#'   returns a data frame of segments and a list of segment polygons as an
#'   attribute.
#' @details \code{x0}, \code{y0}, \code{x1}, \code{y1}, \code{width} and
#'   \code{length} must be in \strong{metric units}. The \code{\link{areapl}}
#'   function from the \code{splancs} package is used to calculate segment
#'   areas.
#' @param data dataframe of transect data (one row per transect)
#' @param x0 column name in \code{data} giving start x-coordinates
#' @param y0 column name in \code{data} giving start y-coordinates
#' @param x1 column name in \code{data} giving stop x-coordinates
#' @param y1 column name in \code{data} giving stop y-coordinates
#' @param id column name in \code{data} giving unique transect id
#' @param width segment width
#' @param length segment length
#' @param sep character to use as a separator for segment id
#' @author Darren
#' @examples
#' # dataframe of transect start and stop coordinates
#' transects = data.frame(
#'     x0 = c(0.0, 2.0),
#'     x1 = c(1.5, 0.5), 
#'     y0 = c(0.5, 1.5),
#'     y1 = c(2.0, 0.0)
#' ) * 10000 ; 
#' 
#' # add a transect id column
#' transects$id = c("A","B")
#' 
#' # add some arbitrary covariates
#' transects$cov1 = c(1,2)
#' transects$cov2 = c("a","b")
#' 
#' # plot the transects
#' plot(0,0, type = "n", asp = 1, 
#'     xlim = range(transects[,c("x0","x1")]), 
#'     ylim = range(transects[,c("y0","y1")]))
#' arrows(
#'     x0 = transects[,"x0"],
#'     x1 = transects[,"x1"],
#'     y0 = transects[,"y0"],
#'     y1 = transects[,"y1"]
#' )
#' 
#' # make segment table
#' segments = segment.transects(transects)
#' print(segments)
#' 
#' # plot segments
#' invisible(sapply(attr(segments, "polygons"), polygon))
#' text(segments$x, segments$y, labels = names(attr(segments, "polygons")))
#' @return Returns a data frame with the original columns in \code{data} plus 
#'   the following additional columns:
#'   
#'   \itemize{
#'   
#'   \item{\code{x} - a vector of x coordinates of the polygon centre points}
#'   
#'   \item{\code{y} - a vector of y coordinates of the polygon centre points}
#'   
#'   \item{\code{area} - a vector of segment areas in \strong{square metres}}
#'   
#'   \item{\code{segment} - a vector of segment numbers}
#'   
#'   \item{\code{segment_id} - a vector of unique segment IDs (<transect 
#'   ID><separator><segment number>)}
#'   
#'   }
#'   
#'   The returned object also has a polygons attribute, which is a named list of
#'   polygons, each element corresponding to a unique segment (and consisting of
#'   a two-column matrix containing the coordinates of the polygon vertices)
#' @family distance sampling helper functions
#' @importFrom splancs areapl
#' @export

segment.transects = function(data, x0 = "x0", x1 = "x1", y0 = "y0", y1 = "y1", id = "id", width = 2000, length = 2000, sep = "_"){
    
    data = as.data.frame(data)
    
    n = nrow(data)
    
    # check that columns exist
    if(is.null(data[[x0]])) stop("column ", deparse(substitute(x0)), " not found in data", call. = FALSE)
    if(is.null(data[[x1]])) stop("column ", deparse(substitute(x1)), " not found in data", call. = FALSE)
    if(is.null(data[[y0]])) stop("column ", deparse(substitute(y0)), " not found in data", call. = FALSE)
    if(is.null(data[[y1]])) stop("column ", deparse(substitute(y1)), " not found in data", call. = FALSE)
    
    if(is.null(data[[id]])){
        data[[id]] = 1:n
    }else{
        if(length(unique(data[[id]])) != n)
            stop("ids not unique", call. = FALSE)
    }
    
    data[[id]] = as.character(data[[id]])
    
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
    
    # make segments data frame
    segments = data.frame(
        x          = centre[,1],
        y          = centre[,2],
        area       = sapply(polygons, areapl),
#         segment    = sapply(strsplit(names(polygons), sep), function(x) x[2]),
        segment    = sapply(strsplit(names(polygons), sep), function(x) paste(x[length(x)], collapse = sep)),
        segment_id = names(polygons)
    )
    
    # use original transect id
    segments[[id]] = sapply(strsplit(names(polygons), sep), function(x) paste(x[-length(x)], collapse = sep))
    
    # merge with transect data
    segments = base::merge(segments, data[,!colnames(data) %in% c(x0, x1, y0, y1)], by = id, sort = FALSE)
    
    # add list of polygons as an attribute
    attr(segments, "polygons") = polygons
    
    return(segments)
    
}


## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
