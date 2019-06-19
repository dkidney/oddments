
#' @rdname session_info
#' @name session_info
#' @title R session and package information
#' @description \code{session_info()} prints the following information to the console:
#' \itemize{
#'   \item timestamp (local UK time)
#'   \item user name
#'   \item machine nodename
#'   \item platform
#'   \item operating system
#'   \item R version
#'   \item whether the session is interactive
#'   \item list of attached packages
#' }
#'
#' \code{attached_packages}, \code{loaded_packages}, and \code{installed_packages} return
#' data frames with columns \code{name} and \code{version}.
#'
#' \code{is_attached}, \code{is_loaded}, and \code{is_installed} return logical vectors
#' with the same length as their inputs.
#'
#' \code{nodename} and \code{user} are convenience functions for extracting the
#' \code{"nodename"} and \code{"user"} elements of the character vector returned from
#' \link[base]{Sys.info}.
#'
#' @param pkgs character vector of package names
#' @examples
#' \dontrun{
#'
#' library(oddments)
#'
#' session_info()
#' nodename()
#' user()
#'
#' installed_packages()
#' loaded_packages()
#' attached_packages()
#'
#' is_installed("mgcv")
#' is_loaded("mgcv")
#' is_attached("mgcv")
#' }
#' @export
session_info = function(){
    now("Europe/London") %>% format(usetz = TRUE) %>% cat("\n")
    cat("User:", user(), "\n")
    cat("Nodename:", nodename(), "\n")
    cat("Platform:", R.version[["platform"]], "\n")
    cat("OS:", sessionInfo()[["running"]], "\n")
    cat("R version: ", R.version[["major"]], ".", R.version[["minor"]], "\n", sep = "")
    cat("Interactive:", interactive(), "\n")
    # loaded packages
    attached = attached_packages()
    if(is.null(attached)){
        cat("no packages loaded\n")
        return(invisible())
    }
    attached %<>%
        arrange(.data$name) %>%
        as.data.frame %>%
        column_to_rownames("name") %>%
        `colnames<-`("") %>%
        print.data.frame(right = FALSE)
    invisible()
}

#' @rdname session_info
#' @name session_info
#' @export
attached_packages = function(){
    attached = sessionInfo()[["otherPkgs"]]
    if(is.null(attached)) return(NULL)
    data_frame(
        name = attached %>% names,
        version = attached %>% map_chr("Version")
    )
}

#' @rdname session_info
#' @name session_info
#' @export
loaded_packages = function(){
    installed_packages() %>%
        filter(.data$name %in% loadedNamespaces())
}

#' @rdname session_info
#' @name session_info
#' @export
installed_packages = function(){
    installed = installed.packages()
    data_frame(name = rownames(installed),
               version = installed[, "Version"])
}

#' @rdname session_info
#' @name session_info
#' @export
is_attached = function(pkgs){
    attached = attached_packages()
    if(is.null(attached)) return(rep(FALSE, length(pkgs)))
    pkgs %in% attached$name
}

#' @rdname session_info
#' @name session_info
#' @export
is_loaded = function(pkgs){
    loaded = loaded_packages()
    if(is.null(loaded)) return(rep(FALSE, length(pkgs)))
    pkgs %in% loaded$name
}

#' @rdname session_info
#' @name session_info
#' @export
is_installed = function(pkgs){
    installed = installed_packages()
    if(is.null(installed)) return(rep(FALSE, length(pkgs)))
    pkgs %in% installed$name
}

#' @rdname session_info
#' @name nodename
#' @export
nodename = function(){
    unname(Sys.info()["nodename"])
}

#' @rdname session_info
#' @name user
#' @export
user = function(){
    unname(Sys.info()["user"])
}


