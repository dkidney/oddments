################################################################################
################################################################################

#' Require multiple packages
#'
#' Loads one or more packages into the current session. 
#' 
#' Under detault settings, all of the packages in \code{package.names} will be loaded into the current session. Any that aren't already in the library/libraries (as specified by \code{.libPaths()}) will be automatically downloaded and installed.
#'
#' @param package.names a character vector of package names
#' @param install.only if \code{TRUE} then install but don't load package (default is \code{FALSE}) 
#' @param install.all if \code{TRUE} then install latest versions of all packages (default is \code{FALSE}) 
#' @param ... any arguments applicable to the \code{require} function (e.g. \code{warn.conflicts = FALSE})
#' @author Darren Kidney
#' @examples
#' # Example 1: require two packages
#' require.multiple(c("secr","CircStats"))
#' 
#' # Example 2: require two packages and make sure you have the latest versions
#' require.multiple.packages(c("secr","CircStats"), install.all = TRUE)
#' 
#' # Example 3: install (but dont load) all the packages installed in a previous version of R
#' old.library <- "C:/R/R-2.15.1/library"
#' names <- list.files(old.library)
#' require.multiple(names, install.only = TRUE)
#' @export

require.multiple <- function(package.names, install.only = FALSE, install.all = FALSE, ...){
    
  # this function needs updating - there is a better way find installed packages using rownames(installed.packages())
  
  if(install.all){

        install.packages(package.names)
        
    } else {
        
        packages.to.install <- package.names[!is.element(package.names, list.files(.libPaths()))]
        
        if(length(packages.to.install) > 0) install.packages(packages.to.install)
        
    }
    
    if(!install.only){
        
        for(name in package.names){ 
            
            eval(parse(text = paste("require(",name,", ...)",sep="")))
            
        }
        
    }
    
}

################################################################################
################################################################################
