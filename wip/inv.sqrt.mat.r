################################################################################
################################################################################

#' Inverse square root of a matrix
#'
#' Description.
#' 
#' Details.
#'
#' @param x a square matrix
#' @return Returns a matrix A^(-1/2)
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' # Example: 
#' 
#' A = matrix(c(2,-1,0,-1,2,-1,0,-1,2), nc=3) ; A
#' 
#' inv.sqrt.mat(A)
#' 
#' @export

inv.sqrt.mat <- function(x){
    
    A.eigen <- eigen(as.matrix(A))
    
    if(all(A.eigen$values>0)){
        
        A.sqrt <- A.eigen$vectors %*% ( diag(sqrt(A.eigen$values)) %*% t(A.eigen$vectors) ) 
        
        } else {
            
            stop("Matrix must be positive semi-definite")
    
        }
    
    return(solve(A.sqrt))
    
}

################################################################################
################################################################################
