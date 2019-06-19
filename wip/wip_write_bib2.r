################################################################################
################################################################################

#' Write a bib file for R packages
#'
#' Description.
#' 
#' Details.
#'
#' @param file the name of the bib file
#' @param path the file address
#' @param ... the packages to include in the bib file
#' @return Value.
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#' @examples
#' # Example: 
#' 
#' write.bib(fda, nlme, file = "R_packages.bib")
#' 
#' @export
write.bib <- function(..., file = NULL, path = NULL){
    
    if(is.null(file)){
        
        # default file name
        file = "R_packages.bib"
        
    }else{
     
        # add .bib suffix if not already present
        if(!grepl("\\.bib$", file)) file = paste(file, ".bib", sep="")
        
    }
    
    # add path to file name
    filename = if(is.null(path)) file else file.path(file, path) 
    
    # convert dots to character vector of package names
    packages <- sapply(match.call(expand.dots = FALSE)$..., as.character)
    
    # if 'base' package not included in the list of package packages, then add it to the list
    if(!"base" %in% packages) packages <- c("base", packages)

    for(package in packages){

        # is package already loaded
        loaded = paste("package", package, sep = ":") %in% search() ; loaded
        
        # load package if not previously loaded
        if(!loaded)
            eval(parse(text = paste("suppressMessages(require(",package,"))",sep="")))
        
        # get bibtex citation
        cit <- as.character(toBibtex(citation(package))) ; cit
        
        # if any lines are empty this implies more than one citation
        # only the first citation is used
        if(any(nchar(cit) == 0)) 
            cit <- cit[1: (which(nchar(cit) == 0)[1] - 1)] ; cit
        
        # delete any returns in title
        cit[2] = gsub("\\n", "", cit[2])
        
        # give the reference a unique name        
        for(type in c("Manual","Article"))
            cit[grep(paste("@",type,sep=""), cit)] <- paste("@",type,"{Rpackage_",package,",",sep="") ; cit
        
        # add a blank line 
        cit <- paste(cit, "\n") ; cit

        # if first name - make new file
        # if not first name - append existing file 
        append <- if(package == packages[1]) FALSE else TRUE ; append
        
        # write to file
        cat(cit, file = filename, append = append)
        
        # add blank lines at end of file
        if(package != packages[length(packages)]) 
            cat(",\n\n", file = filename, append = TRUE)
        
        # detach package if not previously loaded
        if(!loaded) 
            eval(parse(text = paste("detach('package:",package,"')", sep="")))
        
    }

}

################################################################################
################################################################################
