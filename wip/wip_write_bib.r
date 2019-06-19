################################################################################
################################################################################

# Write bib file for R packages

################################################################################
################################################################################

write.bib.file <- function(filename, ...){
    
    dots <- match.call(expand.dots = FALSE)$...

    names <- sapply(dots, as.character)
    
    if(!"base" %in% names) names <- c("base", names)

    for(i in names){
        
        eval(parse(text = paste("suppressMessages(require(",i,"))",sep="")))
        
        cit <- as.character(toBibtex(citation(i))) ; cit
        
        if(any(nchar(cit) == 0)) cit <- cit[1: (which(nchar(cit) == 0) - 1)] ; cit
        
        for(type in c("Manual","Article"))
            cit[grep(paste("@",type,sep=""), cit)] <- paste("@",type,"{Rpackage_",i,",",sep="") ; cit
        
        cit <- paste(cit, "\n") ; cit

        append <- if(i == names[1]) FALSE else TRUE ; append
        
        cat(cit, file = filename, append = append)
        
        if(i != names[length(names)]) cat(",\n\n", file = filename, append = TRUE)
        
    }

}

if(0){
    
    setwd("~/PhD/Thesis/Latex")

    write.bib.file(filename = "bib_R_packages.bib", fda, kohonen, nlme, mclust, base, stats, Rcpp, RcppArmadillo, secr)
    
    setwd("~/PhD/Chapters/4 Gibbons/Gibbons paper/Latex")
    
    write.bib.file(filename = "bib_R_packages.bib", base, Rcpp, RcppArmadillo, secr)
    
}

################################################################################
################################################################################
