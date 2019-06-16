################################################################################
################################################################################

# x must be a list
# index is a vector of integers or vector of names

subset.list(x, index){
    
    if(!inherits(x, "list")) 
        stop("x must be a list")
    
    if(!inherits(index, c("integer", "character")))
        stop("index must be of class 'integer' or 'character'")
    
    lapply(index, function(i) x[[i]])
    
}

################################################################################
################################################################################
