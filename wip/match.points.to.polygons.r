## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##

#' @title Match points to polygons
#' @description Description.
#' @details Uses the \code{\link{gCoveredBy}} and \code{\link{gDistance}} 
#'   functions from the \pkg{rgeos} package.
#' @param points a two-column matrix of coordinates or a 
#'   \code{\link{SpatialPoints}} object
#' @param polygons a list of polygons or a \code{\link{SpatialPolygons}} object
#' @param proj4string optional \code{\link{CRS}} projection system to use if points are not 
#'   supplied as a SpatialPoints object and/or polygons are not supplied as a 
#'   SpatialPolygons object.
#' @param snap.to.nearest if \code{TRUE}, any points that aren't covered by at 
#'   least one polygon will be matched to the nearest polygon.
#' @param threshold the maximum distance between point and polygon to consider 
#'   when snapping to nearest
#' @param first.match if \code{TRUE} returns a vector of indices indicating the first matched 
#'   polygon for each point (or \code{NA} if no matches), if \code{FALSE} a
#'   logical matrix is returned with one element for each point (row) and polygon (column) combination
#' @author Darren, Bruno
#' @examples
#' # matrix of points: 
#' n = 100
#' points = cbind(x = rnorm(n), y = rnorm(n))
#' plot(points, xlim = c(-2,2), ylim = c(-2,2))
#' 
#' # list of polygons:
#' polygons = list(
#'   a = cbind(x = c(-1,-1,0,0), y = c(-1,0,0,-1)),
#'   b = cbind(x = c(-1,-1,0,0), y = c(0,1,1,0)),
#'   c = cbind(x = c(0,0,1,1), y = c(0,1,1,0)),
#'   d = cbind(x = c(0,0,1,1), y = c(-1,0,0,-1)) 
#' )
#' invisible(lapply(polygons, polygon))
#' 
#' # match enclosed points to polygons
#' matches = match.points.to.polygons(points, polygons)
#' points(points[!is.na(matches),], col = as.factor(matches)[!is.na(matches)], pch = 19)
#' 
#' # snap to grid, using threshold distance
#' matches = match.points.to.polygons(points, polygons, snap.to.nearest = TRUE, threshold = 0.5)
#' require(rgeos)
#' plot(gBuffer(as.SpatialPolygons(polygons), width = 0.5), add = TRUE, lty = 2)
#' points(points, col = as.factor(matches), pch = 19)
#' 
#' # match all points to polygons
#' matches = match.points.to.polygons(points, polygons, snap.to.nearest = TRUE)
#' points(points, col = as.factor(matches), pch = 19)
#' @importFrom rgeos gDistance gCoveredBy
#' @family distance sampling helper functions
#' @export

match.points.to.polygons = function(points, polygons, proj4string = CRS(as.character(NA)), snap.to.nearest = FALSE, threshold = Inf, first.match = TRUE){
    
    # check points class
    if(!inherits(points, "SpatialPoints")){
        if(inherits(points, c("matrix","data.frame"))){
            points = SpatialPoints(points, proj4string = proj4string)            
        }else{
            stop("points must be a matrix, a data.frame or an SpatialPoints object")
        }
    }
    
    # check polygons class
    if(!inherits(polygons, "SpatialPolygons")){
        if(inherits(polygons, "list")){
            polynames = if(is.null(names(polygons))) 1:length(polygons) else names(polygons)
                           polygons = as.SpatialPolygons(polygons, proj4string = proj4string)            
        }else{
            stop("polygons must be a list of matrices or data.frames, or a SpatialPolygons object")
        }
    }
    
    # make a logical matrix to indicate matches (row = point, col = polygon)
    matches = gCoveredBy(points, polygons, byid = TRUE) # allows points on border to be included
    n = nrow(matches) 
    
    # snap to nearest polygon
    if(snap.to.nearest){
        
        no.match = unname(which(apply(matches, 1, function(x) !any(x)))) ; no.match
        
        if(length(no.match) > 0){
            
            distances = gDistance(points[no.match,], polygons, byid = TRUE)
            
            matches[no.match,] = t(apply(distances, 2, function(x){
                
                1:nrow(distances) == which.min(x) & x <= threshold
                
            }))
            
        }
        
    }
    
    # if more than one match, then choose the polygon whose centre is closest?
    
    # use the first match per point
    if(first.match){
        
        matches = apply(matches, 1, function(x){
            
            polynumber = which(x)
            
            if(length(polynumber) == 0) NA else polynumber[1]
            
        })
        
    }
    
    match.names = rep(NA, n)
    i = !is.na(matches) ; i
    match.names[i] = polynames[matches[i]]
    
    return(match.names)
    
}

## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
