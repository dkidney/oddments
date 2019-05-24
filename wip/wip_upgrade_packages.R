
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
#' @param check_source if \code{TRUE} check to see if there are later versions available
#'   from source, but don't install/update (ignored if \code{type = "source"} or
#'   \code{"both"})
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
                            dependencies = c("Depends", "Imports", "LinkingTo"),
                            check_source = FALSE){

    # helper function to extract the versions of currently installed packages
    installed_packages = function(priority = NULL){
        utils::installed.packages(lib.loc = lib, priority = priority) %>%
            as_tibble %>%
            transmute(
                pkg = .data$Package,
                version = numeric_version(.data$Version)
            )
    }

    # get versions of currently installed packages
    installed = installed_packages()
    if(is.null(pkgs)) pkgs = installed$pkg

    # get list of available packages in the repos
    available = utils::available.packages(repos = repos, type = type)
    if(nrow(available) == 0) return(invisible())

    # ignore pkgs that aren't available in the repos
    pkgs = pkgs %>% intersect(rownames(available))
    if(length(pkgs) == 0){
        message("none of these packages are available at ", repos,
                " (for type ", type, ")")
        return(invisible())
    }

    # get a list of dependecies for all pkgs (ignoring base pkgs)
    if(length(dependencies) > 0){
        deps = tools::package_dependencies(
            packages = pkgs,
            db = available,
            which = dependencies,
            recursive = TRUE,
            verbose = FALSE
        )
        pkgs = unique(sort(c(pkgs, unlist(deps))))
    }
    base = installed_packages("base")$pkg
    pkgs = setdiff(pkgs, rownames(base))
    if(length(pkgs) == 0) return(invisible())

    # simplify 'available' for more convenient usage
    available = available %>%
        as_tibble %>%
        transmute(
            pkg = .data$Package,
            version = numeric_version(.data$Version)
        ) %>%
        filter(.data$pkg %in% !!pkgs)

    # get a list of all pkgs that are either not installed or are out-of-date
    pkgs = available %>%
        left_join(installed, "pkg", suffix = c(".available", ".installed")) %>%
        filter(!(.data$version.available > .data$version.installed) %in% FALSE) %>%
        `[[`("pkg")

    # loop over the pkgs to install/update
    for(pkg in pkgs){ # pkg = pkgs[1]

        installed_version = installed %>%
            filter(.data$pkg == !!pkg) %>%
            `[[`("version") %>%
            numeric_version

        available_version = available %>%
            filter(.data$pkg == !!pkg) %>%
            `[[`("version") %>%
            numeric_version

        if(!isFALSE(installed_version < available_version)){
            if(length(installed_version) == 0){
                message(paste("installing", pkg, available_version))
            }else{
                message(paste("upgrading", pkg, installed_version, "->", available_version))
            }
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
            installed = installed_packages()
        }
    }

    # tell user if later packages are available from source
    if(check_source){
        if(!any(c("source", "both") %in% type)){
            src_pkgs = utils::available.packages(repos = repos, type = "source") %>%
                as_tibble %>%
                transmute(
                    pkg = .data$Package,
                    version = numeric_version(.data$Version)
                ) %>%
                inner_join(x = installed, by = "pkg", suffix = c(".installed", ".available")) %>%
                filter(!(.data$version.available > .data$version.installed) %in% FALSE)
            if(nrow(src_pkgs) > 0){
                message("later versions are available from source:")
                src_pkgs %>%
                    `colnames<-`(c("", "installed", "available")) %>%
                    print.data.frame(row.names = FALSE, right = FALSE)
            }
        }
    }
    return(invisible())
}


