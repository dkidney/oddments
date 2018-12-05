
#' Upgrade packages
#'
#' \itemize{
#'   \item Installs any packages in \code{pkgs} that are not already installed
#'   \item Updates any packages in \code{pkgs} that are installed but out-of-date.
#' }
#'
#' @param pkgs TODO
#' @param lib TODO
#' @param repos TODO
#' @param type TODO
#' @param quiet TODO
#' @param dependencies TODO
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
                            quiet = TRUE){

    inst = utils::installed.packages(lib.loc = lib)
    if(is.null(pkgs)) pkgs = rownames(inst)
    cran = utils::available.packages(repos = repos, type = type)
    pkgs = pkgs %>% intersect(rownames(cran))
    if(length(pkgs) == 0){
        message("no packages available on ", repos)
        return(invisible())
    }
    deps = tools::package_dependencies(pkgs, db = cran, which = dependencies,
                                       recursive = TRUE, verbose = FALSE)
    pkgs = unique(sort(c(pkgs, unlist(deps))))
    base = utils::installed.packages(lib.loc = lib, priority = "base")
    pkgs = setdiff(pkgs, rownames(base))
    if(length(pkgs) == 0) return(invisible())
    pkgs = cran %>%
        as_tibble %>%
        filter(.data$Package %in% pkgs) %>%
        select(one_of("Package", "Version")) %>%
        left_join(
            inst %>%
                as_tibble %>%
                select(one_of("Package", "Version")),
            by = "Package",
            suffix = c(".cran", ".inst")
        ) %>%
        filter(!((.data$Version.cran <= .data$Version.inst) %in% TRUE)) %>%
        `[[`("Package")
    if(length(pkgs) == 0) return(invisible())
    for(pkg in pkgs){ # pkg = pkgs[1]
        cran_version = numeric_version(cran[pkg, "Version"])
        inst = utils::installed.packages(lib.loc = lib)
        pkg_missing = !pkg %in% rownames(inst)
        if(pkg_missing){
            inst_version = numeric_version(0)
            msg = paste("installing", pkg, cran_version)
        }else{
            inst_version = numeric_version(inst[pkg, "Version"])
            msg = paste("upgrading", pkg, ":", inst_version, "->", cran_version)
        }
        if(inst_version < cran_version){
            message(msg)
            utils::capture.output({
                suppressMessages({
                    utils::install.packages(pkg, repos = repos, type = type, quiet = TRUE)
                })
            })
        }
    }
    return(invisible())
}


