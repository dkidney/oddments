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
