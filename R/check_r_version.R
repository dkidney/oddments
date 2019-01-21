
#' @title Check R version
#' @description Check to see if a later R version is available for download.
#' \itemize{
#'   \item Prints the current R version to console
#'   \item Checks CRAN to see if a later version is available and, if so, prints a message
#'   to the console
#'   \item Only works in interactive mode
#' }
#' @export

check_r_version = function(){
    if(interactive()){
        current_version = getRversion()
        message("Using R-", current_version)
        sysname = Sys.info()["sysname"]
        if(!sysname %in% c("Darwin", "Windows")){
            message("cant check available R version for ", sysname)
            return(invisible())
        }
        url = "https://cran.r-project.org/bin/"
        if(sysname == "Darwin") url = str_c(url, "macosx")
        if(sysname == "Windows") url = str_c(url, "windows/base")
        pattern = "R-[0-9]+\\.[0-9]+\\.[0-9]+"
        url_dump = try(suppressWarnings({
            url %>% readLines(warn = FALSE)
        }), TRUE)
        if(inherits(url_dump, "try-error")){
            message("can't connect to ", url, " (check your internet connection)")
        }else{
            available_version = url_dump %>%
                keep(str_detect(., pattern)) %>%
                first %>%
                str_extract(pattern) %>%
                str_remove("R-") %>%
                numeric_version
            if(inherits(available_version, "numeric_version")){
                if(current_version < available_version){
                    message("R-", available_version, " now available")
                }
            }else{
                message("can't extract current version from ", url, " (try debugging)")
            }
        }
    }
}
