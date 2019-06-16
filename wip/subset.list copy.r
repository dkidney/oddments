################################################################################
################################################################################

#' Subset a list
#'
#' Description.
#' 
#' Details.
#'
#' @param x a list to subset
#' @param index a vector of indices specifiying list elements (can be character, numeric, integer or logical)
#' @param drop if \code{FALSE} then indexed elements are returned, if \code{TRUE} then indexed elements are dropped (default is \code{FALSE})
#' @return Returns a sub-list of the input list.
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' # Example: 
#' 
#' # initial list
#' x = list(a = 1:10, b = letters[1:3], c = matrix(0,2,2))
#' x
#' 
#' # subset using names
#' subset.list(x, c("c", "a"))
#' subset.list(x, "b", drop = TRUE)
#' 
#' # subset using numbers
#' subset.list(x, 1:2)
#' subset.list(x, 3, drop = TRUE)
#' 
#' # subset using logical
#' subset.list(x, c(TRUE, TRUE, FALSE))
#' subset.list(x, c(TRUE, TRUE, FALSE), drop = TRUE)
#' 
#' @export

subset.list = function(x, index, drop = FALSE){
    
    # x must be a list
    if(!inherits(x, "list")) stop("x must be a list")

    # if index s character then x must be named
    if(inherits(index, "character") && is.null(names(x)))
        stop("can's use character index if list elements are unnamed")

    # if index is logical
    if(inherits(index, "logical")){
        
        # check length
        if(length(x) != length(index)){
            
            stop("length of logical index must equal the number of elements in list")

        # if length ok then convert to integer
        }else{
            
            index = which(index)
            
        }
        
    }
    
    # if drop = TRUE, then use inverse index
    if(drop){
        
        index = if(inherits(index, "character")){
            
            names(x)[!names(x) %in% index]
            
        }else{

            which(!1:length(x) %in% index)
        
        }
        
    } ; index
    
    # extract list elements
    new = lapply(index, function(i) x[[i]])
    
    # rename elements
    if(!is.null(names(x))){
        
        names(new) = if(inherits(index, "character")){
            
            index
            
        }else{
            
            names(x)[index]
            
        }
        
    }
    
    return(new)
    
}

################################################################################
################################################################################
