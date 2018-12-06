
#' Upgrade packages
#'
#' \itemize{
#'   \item Installs any packages in \code{pkgs} that are not already installed
#'   \item Updates any packages in \code{pkgs} that are installed but out-of-date.
#' }
#'
#' Note that using \code{type=.Platform$pkgType} will only install binary versions (e.g.
#' if using OSX 10.13 or higher the type will probably be \code{"mac.binary.el-capitan"}).
#' Sometimes there are more up-to-date source versions available, in which case you will
#' either need to change the arguement to \code{type=getOption("pkgType")} or
#' \code{type="source"}, or update those packages manually (e.g. using the 'Update
#' packages' facility within RStudio).
#' @param pkgs character vector giving the names of the packages to upgrade - if
#'   \code{NULL} then all currently installed packages will be used
#' @param lib string giving the the library directory in which to install the packages
#' @param repos string giving the URL of the repositories to use
#' @param type string giving the type of package to download and install (see Details)
#' @param dependencies character vector giving the types of dependencies to
#'   install/update, a subset of c("Depends", "Imports", "LinkingTo", "Suggests",
#'   "Enhances")
#' @export
#' @examples
#' \dontrun{
#'
#' upgrade_packages("devtools")
#' upgrade_packages("tidyverse")
#' upgrade_packages("tidymodels")
#' }

upgrade_packages = function(pkgs = NULL,
                            lib = .libPaths()[1],
                            repos = getOption("repos"),
                            type = .Platform$pkgType,
                            dependencies = c("Depends", "Imports", "LinkingTo")){
    
    inst = utils::installed.packages(lib.loc = lib)
    if(is.null(pkgs)) pkgs = rownames(inst)
    cran = utils::available.packages(repos = repos, type = type)
    if(nrow(cran) == 0) return(invisible())
    pkgs = pkgs %>% intersect(rownames(cran))
    if(length(pkgs) == 0){
        message("none of these packages are available at ", repos, 
                " (for type ", type, ")")
        return(invisible())
    }
    if(length(dependencies) > 0){
        deps = tools::package_dependencies(
            packages = pkgs,
            db = cran, 
            which = dependencies, 
            recursive = TRUE, 
            verbose = FALSE
        )
        pkgs = unique(sort(c(pkgs, unlist(deps))))
    }
    base = utils::installed.packages(lib.loc = lib, priority = "base")
    pkgs = setdiff(pkgs, rownames(base))
    if(length(pkgs) == 0) return(invisible())
    pkgs = cran %>%
        as_tibble %>%
        select(one_of("Package", "Version")) %>%
        filter(.data$Package %in% !!pkgs) %>%
        left_join(
            inst %>%
                as_tibble %>%
                select(one_of("Package", "Version")),
            by = "Package",
            suffix = c(".cran", ".inst")
        ) %>%
        filter(!(.data$Version.cran > .data$Version.inst) %in% FALSE) %>% 
        `[[`("Package")
    # TDOD: tell user which packages will be updated?
    for(pkg in pkgs){ # pkg = pkgs[1]
        cran_version = numeric_version(cran[pkg, "Version"])
        inst = utils::installed.packages(lib.loc = lib)
        pkg_missing = !pkg %in% rownames(inst)
        if(pkg_missing){
            inst_version = numeric_version(0)
            msg = paste("installing", pkg, cran_version)
        }else{
            inst_version = numeric_version(inst[pkg, "Version"])
            msg = paste("upgrading", pkg, inst_version, "->", cran_version)
        }
        if(inst_version < cran_version){
            message(msg)
            utils::capture.output({
                suppressMessages({
                    utils::install.packages(
                        pkgs = pkg, 
                        lib = lib, 
                        repos = repos,
                        type = type, 
                        quiet = TRUE,
                        verbose = FALSE
                    )
                })
            })
        }
    }
    # TDOD: tell user if later packages are available from source?
    if(!any(c("source", "both") %in% type)){
        src_pkgs = dplyr::inner_join(
            utils::installed.packages(lib.loc = lib) %>%
                as_tibble %>%
                select(pkg = .data$Package, installed = .data$Version),
            utils::available.packages(repos = repos, type = "source") %>%
                as_tibble %>%
                select(pkg = .data$Package, available = .data$Version),
            by = "pkg"
        ) %>%
            filter(!(.data$available > .data$installed) %in% FALSE)
        if(nrow(src_pkgs) > 0){
            message("later versions are available from source for the following packages:")
            src_pkgs %>% 
                `colnames<-`(c("", "installed", "available")) %>% 
                print.data.frame(row.names = FALSE, right = FALSE)
        }
    }
    return(invisible())
}


