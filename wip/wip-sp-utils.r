## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##

#' @title Convert polygons to SpatialPolygons.
#' @description Convert a list of polygons to a \code{SpatialPolygons} object.
#' @details Addional arguments accepted by \code{\link{SpatialPolygons}} include
#'   an optional \code{proj4string} (see \code{\link{CRS-class}}). For example,
#'   use \code{CRS("+proj=longlat")} for degrees and minutes,
#'   \code{CRS("+proj=utm +zone=30")} for UTM 30, and I think
#'   \code{CRS("+init=epsg:27700")} is for OSGB.
#' @param a single polygon, a list of polygons, or a list of lists of polygons
#'   (with each individual polygon represented as a matrix or dataframe)
#' @param closed if \code{FALSE} then each polygon will be automatically closed
#'   before converting to \code{\link{Polygon}} class.
#' @param ... additional arguments to pass to \code{SpatialPolygons} (see
#'   Details)
#' @author Darren
#' @family DMP functions for spatial data
#' @examples
#' # polygons
#' p1 = cbind(x = c(-1,-1,0,0), y = c(-1,0,0,-1))
#' p2 = cbind(x = c(-1,-1,0,0), y = c(0,1,1,0))
#' p3 = cbind(x = c(0,0,1,1), y = c(0,1,1,0))
#' p4 = cbind(x = c(0,0,1,1), y = c(-1,0,0,-1))
#'
#' polygon(p1, col = "grey80")
#' polygon(p2, col = "grey60")
#' polygon(p3, col = "grey40")
#' polygon(p4, col = "grey20")
#'
#' # convert a single polygon
#' sp1 = as.SpatialPolygons(p1)
#' lapply(slot(sp1, polygons), function(x) names(slot(x, Polygons)))
#' plot(sp1, col = 2, add = TRUE)
#'
#' # convert a list of polygons
#' sp2 = as.SpatialPolygons(list(p2 = p2, p3 = p3, p4 = p4))
#' lapply(slot(sp2, polygons), function(x) names(slot(x, Polygons)))
#' plot(sp2, col = 3, add = TRUE)
#'
#' # convert a list of lists of polygons
#' sp3 = as.SpatialPolygons(list(A = list(p1 = p1, p3 = p3), B = list(p2 = p2, p4 = p4)))
#' lapply(slot(sp2, polygons), function(x) names(slot(x, Polygons)))
#' plot(sp3, col = c(4,5), add = TRUE)
#' @import sp
#' @export

as.SpatialPolygons = function(polygons, closed = FALSE, ...){

    # check class
    if(!inherits(polygons, c("matrix", "data.frame", "list")))
        stop("polygons must be a matrix, data.frame or list")

    if(!inherits(polygons, "list")){
        # convert single polygon to two-level list
        polygons = list(setNames(list(polygons), deparse(substitute(polygons))))
    }else{
        # if polygons is a list, check each element is also a list
        if(is.list(polygons)){
            each = sapply(polygons, is.list) # THIS IS AMBIGUOUS - ALLOWS DATAFRAMES!
            if(all(!each)){
                # if no elements are lists, convert them all to lists
                polygons = lapply(polygons, list)
                if(!is.null(names(polygons))){
                    for(i in 1:length(polygons))
                        names(polygons[[i]]) = names(polygons)[i]
                    names(polygons) = NULL
                }
            }else{
                # elements aren't lists, throw an error
                if(!all(each)) stop("polygons structure not recognised")
            }
        }
    }

    # check element class
    lapply(polygons, function(x){
        lapply(x, function(x){
            if(!inherits(x, c("matrix","data.frame")))
                stop("each polygon must be a matrix or a data.frame")
        })
    })

    polynames = if(is.null(names(polygons))) 1:length(polygons) else names(polygons)

    polygons = SpatialPolygons(

        Srl = lapply(1:length(polygons), function(i){

            if(is.null(names(polygons[[i]]))) names(polygons[[i]]) = 1:length(polygons[[i]])

            x = lapply(polygons[[i]], function(x){

                # close ring if not already closed
                if(!closed) x = rbind(x, x[1,])

                Polygon(x, FALSE)

            })

            # convert to Polygons
            Polygons(x, ID = polynames[i])

        }), ...)

    names(polygons@polygons) = polynames

    polygons

}

## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##

#' @title Convert SpatialPolygons to polygons
#' @description Convert a SpatialPolygons object to a list of polygons.
#' @details Details.
#' @param x a \code{\link{SpatialPolygons}} object
#' @param simplify if \code{TRUE} and \code{x} only has a single polygon per layer then: (i) if there is more than one layer a list of matrices is returned, or (ii) if there is only one layer a single matrix is returned.
#' @author Darren
#' @family DMP functions for spatial data
#' @import sp
#' @export

as.polygons = function(x, simplify = TRUE){

    if(!inherits(x, "SpatialPolygons"))
        stop("x is not a SpatialPolygons object")

    x = lapply(x@polygons, function(x){

        lapply(x@Polygons, function(x){

            x@coords

        })

    })

    if(simplify){

        # if each sub-list is length one then simplify
        if(all(sapply(x, length) == 1)){

            x = lapply(x, function(x) x[[1]])

            # if only one sub-list then simplify
            if(length(x) == 1) x = x[[1]]

        }

    }

    return(x)

}

## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##

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
