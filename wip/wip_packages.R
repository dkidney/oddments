
#' @rdname packages
#' @name pkg_upgrade
#' @title ???
#' @description ???
#' @param pkgs ????
#' @param which_deps ????
#' @param lib ????
#' @param repos ????
#' @param type ????
#' @details ???
#' @export

pkg_upgrade <- function(pkgs = NULL,
                        which_deps = TRUE,
                        lib = .libPaths()[1],
                        repos = getOption("repos"),
                        type = .Platform$pkgType) {
    if(!has_internet()){
        message("no internet connection - cant check available packages")
        return(invisible())
    }
    available <- utils::available.packages(type = type, repos = repos)
    installed <- utils::installed.packages(lib)
    if (is.null(pkgs)) {
        pkgs <- rownames(installed)[!installed[, "Priority"] %in% "base"]
    }
    updates <- pkg_updates(
        pkgs = pkgs,
        which_deps = which_deps,
        lib = lib,
        repos = repos,
        type = type,
        available = available,
        installed = installed,
        verbose = FALSE
    )
    if (is.null(updates)) {
        return(invisible())
    }
    updates <- dplyr::mutate(
        updates,
        deps = pkg %>% purrr::map(
            pkg_deps,
            lib = lib,
            repos = repos,
            type = type,
            available = available,
            installed = installed
        )
    )
    if (nrow(updates) > 1) {
        for (pkg in updates$pkg) {
            i <- which(updates$pkg == pkg)
            in_deps <- updates$pkg %in% updates$deps[[i]]
            if (any(in_deps)) {
                j <- which(in_deps)
                if (any(j > i)) {
                    # message(pkg)
                    j <- j[j > i]
                    j <- min(j)
                    index <- 1:nrow(updates)
                    index[i] <- j
                    index[j] <- i
                    updates <- updates[index, ]
                }
            }
        }
    }
    for (pkg in updates$pkg) {
        i <- which(updates$pkg == pkg)
        local_version <- updates$local_version[i]
        repos_version <- updates$repos_version[i]
        if (is.na(local_version)) {
            message(paste("installing", pkg, repos_version))
        } else {
            message(paste("upgrading", pkg, local_version, "->", repos_version))
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
    }
    updates <- pkg_updates(
        pkgs = pkgs,
        which_deps = which_deps,
        lib = lib,
        repos = repos,
        type = type,
        available = available,
        installed = utils::installed.packages(lib),
        verbose = FALSE
    )
    if (!is.null(updates)) {
        warning("some pakcages were not updated")
        print(updates)
    }
    return(invisible())
}

#' @rdname packages
#' @name pkg_updates
#' @export
# see tidyverse::tidyverse_update
pkg_updates <- function(pkgs = NULL,
                        lib = .libPaths()[1],
                        repos = getOption("repos"),
                        type = .Platform$pkgType,
                        which_deps = c("Depends", "Imports", "LinkingTo"),
                        available = NULL,
                        installed = NULL,
                        verbose = TRUE) {
    if (is.null(available)) {
        if(!has_internet()){
            message("no internet connection - cant check available packages")
            return(invisible())
        }
        available <- utils::available.packages(type = type, repos = repos)
    }
    if (is.null(installed)) {
        installed <- utils::installed.packages(lib)
    }
    if (is.null(pkgs)) {
        pkgs <- rownames(installed)[!installed[, "Priority"] %in% "base"]
    }
    if (length(which_deps) > 0) {
        deps <- pkg_deps(
            pkgs = pkgs,
            recursive = TRUE,
            include_base = FALSE,
            available = available,
            installed = installed
        )
        pkgs <- sort(unique(c(pkgs, deps)))
    }
    suppressWarnings({
        updates <- dplyr::tibble(pkg = pkgs) %>%
            left_join(
                installed %>%
                    dplyr::as_tibble() %>%
                    dplyr::mutate_at("Version", base::numeric_version) %>%
                    select(pkg = Package, local_version = Version),
                by = "pkg"
            ) %>%
            left_join(
                available %>%
                    dplyr::as_tibble() %>%
                    dplyr::mutate_at("Version", base::numeric_version) %>%
                    select(pkg = Package, repos_version = Version),
                by = "pkg"
            ) %>%
            filter(!is.na(repos_version)) %>%
            filter(!(local_version < repos_version) %in% FALSE)
    })
    if (nrow(updates) == 0) {
        return(invisible())
    }
    rev_deps = updates$pkg %>%
        purrr::set_names(., .) %>%
        purrr::map_dfr(.id = "pkg",
                       function(pkg){
                           which_deps %>%
                               purrr::set_names(., .) %>%
                               purrr::map(
                                   function(dep){ # i = "Depends"
                                       rev_deps = pkg %>%
                                           tools::dependsOnPkgs(
                                               dependencies = dep,
                                               installed = installed
                                           )
                                       if(length(rev_deps) == 0) return(NA_character_)
                                       rev_deps %>%
                                           sort %>%
                                           stringr::str_flatten(",")
                                   }
                               ) %>%
                               as_tibble
                       }
        )
    updates %<>%
        left_join(rev_deps, "pkg")
    if(verbose){
        # print.data.frame(updates)
        print(updates)
    }
    invisible(updates)
}

#' @rdname packages
#' @name pkg_deps
#' @export
# see tidyverse::tidyverse_deps
pkg_deps <- function(pkgs = NULL,
                     recursive = TRUE,
                     include_base = FALSE,
                     dep = c("Depends", "Imports", "LinkingTo"),
                     lib = .libPaths()[1],
                     repos = getOption("repos"),
                     type = .Platform$pkgType,
                     available = NULL,
                     installed = NULL) {

    # if installed not supplied use utils::installed.packages
    if (is.null(installed)) {
        installed <- utils::installed.packages(lib)
    }
    # if pkgs not supplied used rownames(installed)
    if(is.null(pkgs)){
        pkgs = rownames(installed)
    }
    # check pkgs is character
    stopifnot(is.character(pkgs))
    # check pkg status
    warning("ignoring base packages: ", paste(pkgs[is_base], collapse = ","))
    results = pkgs %>% pkg_status()
    # check if any status unknown
    results %<>% filter(!is.na(status))
    # check if any base pkgs
    if(!include_base){
        results %<>% filter(!status %in% "base")
        if(any(results$status %in% "base")){
        base_pkgs = rownames(installed)[installed[, "Priority"] %in% "base"]
        is_base = pkgs %in% base_pkgs
        if(any(is_base)){
            warning("ignoring base packages: ", paste(pkgs[is_base], collapse = ","))
            pkgs = setdiff(pkgs, base_pkgs)
            if(length(pkgs) == 0) return()
        }
    }
    # check which installed
    is_installed = pkgs %in% rownames(installed)
    if(any(is_installed)){
        # get deps for installed pkgs
        deps_installed <- tools::package_dependencies(
            packages = pkgs[is_installed],
            db = installed,
            recursive = recursive,
            which = dep
        )
    }
    if(any(!is_installed)){
        # check which available
        if (is.null(available)) {
            if(!has_internet()){
                message("no internet connection - cant check non-installed packages")
            }else{
                available <- utils::available.packages(type = type, repos = repos)
            }
        }
        if (is.null(available)) {
            deps_available <- NULL
        }else{
            is_available = pkgs[is_installed] %in% rownames(available)
            if(any(is_available)){
                # get deps for non-installed but available pkgs
                deps_available <- tools::package_dependencies(
                    packages = pkgs[!is_installed],
                    db = available,
                    recursive = recursive,
                    which = dep
                )
            }
        }
        pkgs = pkgs[is_installed | is_available]

    }


    if (all(pkgs %in% rownames(installed))) {
        deps <- tools::package_dependencies(
            packages = pkgs,
            db = installed,
            recursive = recursive,
            which = dep
        )
    } else {
        if (is.null(available)) {
            if(!has_internet()){
                message("no internet connection - cant check available packages")
                return(invisible())
            }
            available <- utils::available.packages(type = type, repos = repos)
        }
        if (all(pkgs %in% rownames(available))) {
            deps <- tools::package_dependencies(
                pkgs,
                available,
                recursive = recursive
            )
        } else {
            stop(
                "the following packages are not available:\n",
                paste(pkgs[!pkgs %in% rownames(available)], collapse = "\n")
            )
        }
    }
    deps <- sort(unique(unlist(deps)))
    if (!include_base) {
        base_pkgs <- rownames(installed)[installed[, "Priority"] %in% "base"]
        deps <- setdiff(deps, base_pkgs)
    }
    return(deps)
}


#' @rdname packages
#' @name pkg_revdeps
#' @export
pkg_revdeps = function(){

}


#' @rdname packages
#' @name pkg_detach
#' @export
pkg_detach = function(pkgs = NULL){
    if(is.null(pkgs)){
        pkgs = names(sessionInfo()$otherPkgs)
    }
    for(pkg in pkgs){
        message("detaching ",  pkg)
        txt = paste0('detach("package:', pkg, '")')
        eval(parse(text = txt))
    }
}



