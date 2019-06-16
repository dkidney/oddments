################################################################################
################################################################################

#' Source all the .R files in a folder
#'
#' Loads one or more packages into the current session. Any packages that aren't already installed will be automatically downloaded and then loaded into the current session.
#'
#' @param folder.path the path of the folder in which the .R files are stored
#' @export

source.folder <- function(folder.path){

    # make a character vector with the names of all the files in the folder
    all.files <- list.files(folder.path)

    # extract only those files with the .r extension
    r.files <- all.files[grep("\\.[r,R]$", all.files)]
    
    # source all the .r files
    for(file in r.files) source(file.path(folder.path, file))
    
}

################################################################################
################################################################################
