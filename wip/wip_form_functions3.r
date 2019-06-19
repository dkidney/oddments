################################################################################
################################################################################

#' Get nested model formulas from a full formula
#'
#' Description.
#' 
#' A quick way to get a list or candidate formulas. 
#'
#' @param full the full model formula
#' @param max.int.level the maximum level of interactions to include
#' @param include.main if \code{TRUE}, then formulas with interaction terms will only be returned if the corresponding main effects are also present. If \code{FALSE} then all combinations are returned (default is \code{TRUE}).
#' @param fix.intercept if \code{TRUE}, the intercept will be included in all models by default. If \code{FALSE} then models with and without intercept terms will be returned (default is \code{TRUE})
#' @return Returns character vector of formulas.
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' # Examples: 
#' 
#' get.nested.formulas(~x*y)
#' 
#' get.nested.formulas(~x*y, max.int.level = 1)
#' 
#' get.nested.formulas(~x*y, fix.intercept = FALSE)
#' 
#' get.nested.formulas(~x*y, include.main = FALSE)
#' 
#' @export

get.nested.formulas = function(full, max.int.level = 2, include.main = TRUE, fix.intercept = TRUE){
    
    #---------------------------------------------------------------------------
    # error checks
    #---------------------------------------------------------------------------
    
    if(!inherits(full, c("formula")))
        stop("'full' must be a formula object")
    
    if(max.int.level < 1 || !identical(max.int.level %% 1, 0)) 
        stop("'max.int.level' must be a positive integer")
    
    if(!inherits(fix.intercept, c("logical", "vector")) || length(fix.intercept) != 1)
       stop("'fix.intercept' must be 'TRUE' or 'FALSE'")
    
    if(!inherits(include.main, c("logical", "vector")) || length(include.main) != 1)
        stop("'include.main' must be 'TRUE' or 'FALSE'")
    
    #---------------------------------------------------------------------------
    # get input formula terms
    #---------------------------------------------------------------------------
    
    terms.full = terms(full) ; terms.full
    
    term.names = attr(terms.full, "term.labels") ; term.names
    
    term.order = attr(terms.full, "order") ; term.order
    
    #---------------------------------------------------------------------------
    # delete terms with interaction levels greater than the max
    #---------------------------------------------------------------------------
    
    keep = term.order <= max.int.level
    
    term.names = term.names[keep] ; term.names
    
    if(!fix.intercept) term.names = c("-1", term.names) ; term.names
    
    #---------------------------------------------------------------------------
    # make all possible subsets
    #---------------------------------------------------------------------------
    
    nterms = length(term.names) ; nterms
    
    index = as.matrix(do.call(expand.grid, lapply(1:nterms, function(i) c(FALSE, TRUE)))) ; head(index)
    
    candidates = sapply(1:nrow(index), function(i){ # i = 1
        
        paste("~", paste(term.names[index[i,]], collapse=" + "))
        
    }) ; candidates
    
    candidates[candidates == "~ "] = "~ 1" ; candidates
    
    if(!fix.intercept) candidates = candidates[candidates != "~ -1"] ; candidates

    nformulas = length(candidates) ; nformulas
    
    #---------------------------------------------------------------------------
    # delete formulas with interactions but without the corresponding main effects
    #---------------------------------------------------------------------------
    
    if(include.main){
        
        keep = sapply(1:nformulas, function(i){ 
            
            terms.candidate = terms(as.formula(candidates[i])) ; terms.candidate
            
            term.order = attr(terms.candidate, "order") ; term.order
            
            if(all(term.order == 1)){
                
                return(TRUE)
                
            }else{
                
                term.names = attr(terms.candidate, "term.labels") ; term.names
                
                covariate.names = unique(unlist(strsplit(term.names[term.order > 1], ":"))) ; covariate.names
                
                all.present = all(covariate.names %in% term.names[term.order == 1]) ; all.present
                
                if(all.present) TRUE else FALSE
                
            }
            
        }) ; keep
        
        candidates = candidates[keep] ; candidates
        
    }
    
    #---------------------------------------------------------------------------
    
    return(candidates)    
    
}

################################################################################
################################################################################
