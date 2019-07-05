
#' @rdname pkg-utils
#' @name pkg-utils
#' @title Package utility functions
#' @description
#' \describe{
#'   \item{pkg_status}{
#'     Returns a character vector giving the status of each package - each element is one
#'     of: \code{"base"}; \code{"installed"}; \code{"available"} or \code{NA} (i.e.
#'     unknown status)
#'   }
#'   \item{pkg_deps}{
#'     ???
#'   }
#'   \item{pkg_revdeps}{
#'     ???
#'   }
#'   \item{pkg_updates}{
#'     ???
#'   }
#'   \item{pkg_upgrade}{
#'     ???
#'   }
#' }
#' @param pkgs `[character vector]` package names
#' @param which_deps `[character vector]` names of dependency packages to include
#' @param lib `[character string]` the location of R library tree to search
#' @param repos `[character string]` the URL of the repository to use
#' @param type `[character string]` the package type
#' @param installed either \code{NULL} or an object returned by
#'   \link[utils]{installed.packages}
#' @param available either \code{NULL} or an object returned by
#'   \link[utils]{available.packages}
#' @param recursive ???
#' @param reverse ???
#' @details ???
#' @importFrom dplyr as_tibble
#' @importFrom dplyr case_when
#' @importFrom dplyr ends_with
#' @importFrom dplyr if_else
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom dplyr tibble
#' @importFrom magrittr %<>%
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom purrr map_if
#' @importFrom purrr map_int
#' @importFrom purrr map_lgl
#' @importFrom purrr pluck
#' @importFrom rlang .data
#' @importFrom stringr str_flatten
#' @importFrom tools package_dependencies
#' @importFrom withr local_options
#' @example inst/examples/examples-pkg-utils.R

#' @rdname pkg-utils
#' @name pkg_status
#' @export
pkg_status <- function(pkgs,
                       lib = .libPaths()[1],
                       repos = getOption("repos"),
                       type = .Platform$pkgType,
                       installed = NULL,
                       available = NULL) {
    stopifnot(is.character(pkgs) && is.null(dim(pkgs)))
    local_options(list(repos = repos, pkgType = type))
    installed %<>% replace_null(installed_packages(lib))
    results <- tibble(
        pkg = pkgs,
        status = case_when(
            .data$pkg %>% is_base(installed) ~ "base",
            .data$pkg %>% is_installed(installed) ~ "installed",
            TRUE ~ NA_character_
        )
    )
    if (!any(is.na(results$status))) {
        return(results)
    }
    available %<>% replace_null(available_packages())
    if (is.null(available)) {
        return(results)
    }
    results %>%
        mutate(
            status = case_when(
                !is.na(.data$status) ~ .data$status,
                .data$pkg %>% is_available(available) ~ "available",
                TRUE ~ NA_character_
            )
        )
}

#' @rdname pkg-utils
#' @name pkg_deps
#' @export
pkg_deps <- function(pkgs,
                     recursive = TRUE,
                     reverse = FALSE,
                     which_deps = c("Depends", "Imports", "LinkingTo"),
                     lib = .libPaths()[1],
                     repos = "https://cran.rstudio.com",
                     type = .Platform$pkgType,
                     available = NULL,
                     installed = NULL) {
    stopifnot(is.character(pkgs) && is.null(dim(pkgs)))
    local_options(list(repos = repos, pkgType = type))
    installed %<>% replace_null(installed_packages(lib))
    available %<>% replace_null(available_packages())
    results <- pkgs %>%
        pkg_status(
            lib = lib,
            repos = repos,
            type = type,
            installed = installed,
            available = available
        ) %>%
        mutate(
            deps_installed = .data$pkg %>% map(~ character()),
            deps_available = .data$deps_installed
        )
    if (all(is.na(results[["status"]]))) {
        warning("no recognised packages")
        return(results)
    }
    i <- results$status %in% c("base", "installed")
    if (any(i)) {
        results$deps_installed[i] <- results$pkg[i] %>%
            package_dependencies(
                db = installed,
                which = which_deps,
                recursive = recursive,
                reverse = reverse
            ) %>%
            map(sort)
    }
    i <- results$status %in% c("base", "installed", "available")
    if (any(i)) {
        results$deps_available[i] <- results$pkg[i] %>%
            package_dependencies(
                db = available,
                which = which_deps,
                recursive = recursive,
                reverse = reverse
            ) %>%
            map(sort)
    }
    results
}


#' @rdname pkg-utils
#' @name pkg_revdeps
#' @export
pkg_revdeps <- function(pkgs,
                        recursive = TRUE,
                        which_deps = c("Depends", "Imports", "LinkingTo"),
                        lib = .libPaths()[1],
                        repos = "https://cran.rstudio.com",
                        type = .Platform$pkgType,
                        available = NULL,
                        installed = NULL) {
    pkgs %>%
        pkg_deps(
            recursive = recursive,
            reverse = TRUE,
            which_deps = which_deps,
            lib = lib,
            repos = repos,
            type = type,
            available = available,
            installed = installed
        ) %>%
        rename(
            local_revdeps = .data$deps_installed,
            repos_revdeps = .data$deps_available
        )
}


#' @rdname pkg-utils
#' @name pkg_updates
#' @export
pkg_updates <- function(pkgs = NULL,
                        lib = .libPaths()[1],
                        repos = getOption("repos"),
                        type = .Platform$pkgType,
                        which_deps = c("Depends", "Imports", "LinkingTo"),
                        available = NULL,
                        installed = NULL) {
    if (!is.null(pkgs)) {
        stopifnot(is.character(pkgs) && is.null(dim(pkgs)))
    }
    local_options(list(repos = repos, pkgType = type))
    installed %<>% replace_null(installed_packages(lib))
    available %<>% replace_null(available_packages())
    if(is.null(available)){
        return(invisible())
    }
    pkgs %<>% replace_null(rownames(installed))
    if (length(which_deps) > 0) {
        deps <- pkg_deps(
            pkgs,
            recursive = TRUE,
            which_deps = which_deps,
            lib = lib,
            repos = repos,
            type = type,
            available = available,
            installed = installed
        )[["deps_available"]]
        pkgs <- sort(unique(c(pkgs, unlist(deps))))
    }
    results <- pkgs %>%
        pkg_status() %>%
        filter(.data$status %in% c("installed", "available"))
    i <- results$status %in% "installed"
    if (any(i)) {
        j <- rownames(installed) %in% results$pkg[i]
        suppressWarnings({
            results %<>%
                left_join(
                    installed[j, , drop = FALSE] %>%
                        as_tibble() %>%
                        mutate_at("Version", base::numeric_version) %>%
                        select(pkg = .data$Package, local_version = .data$Version),
                    by = "pkg"
                )
        })
    }
    j <- rownames(available) %in% results$pkg
    suppressWarnings({
        results %<>%
            left_join(
                available[j, , drop = FALSE] %>%
                    as_tibble() %>%
                    mutate_at("Version", base::numeric_version) %>%
                    select(pkg = .data$Package, repos_version = .data$Version),
                by = "pkg"
            )
    })
    pkg_scope <- union(results$pkg, rownames(installed))
    results %<>%
        filter(!is.na(.data$repos_version)) %>%
        filter(!(.data$local_version < .data$repos_version) %in% FALSE)
    if (nrow(results) == 0) {
        # message("All packages are up-to-date :)")
        return(invisible())
    }
    revdeps <- pkg_revdeps(
        results$pkg,
        recursive = TRUE,
        which_deps = which_deps,
        lib = lib,
        repos = repos,
        type = type,
        available = available[rownames(available) %in% pkg_scope, , drop = FALSE],
        installed = installed
    )
    results %>%
        left_join(revdeps, "pkg") %>%
        select(.data$pkg, ends_with("version"), ends_with("revdeps"))
}

#' @rdname pkg-utils
#' @name pkg_upgrades
#' @export
pkg_upgrades <- function(pkgs = NULL,
                        lib = .libPaths()[1],
                        repos = getOption("repos"),
                        type = .Platform$pkgType,
                        which_deps = c("Depends", "Imports", "LinkingTo"),
                        available = NULL,
                        installed = NULL) {
    results <- pkgs %>%
        pkg_updates(
            lib = lib,
            repos = repos,
            type = type,
            which_deps = which_deps,
            available = available,
            installed = installed
        )
    if(is.null(results)){
        return(invisible())
    }
    n <- nrow(results)
    for (pkg in results$pkg) {
        in_revdeps <- map_lgl(results$repos_revdeps, function(x) any(x %in% pkg))
        if (any(in_revdeps)) {
            i <- which(results$pkg %in% pkg)
            j <- max(which(in_revdeps))
            if (i > j) next
            index <- c((1:j)[-i], i)
            if (j < n) {
                index %<>% c((j + 1):n)
            }
            stopifnot(length(index) == n)
            stopifnot(all(sort(index) == 1:n))
            results %<>% dplyr::slice(index)
        }
    }
    for (pkg in results$pkg) {
        i <- which(results$pkg %in% pkg)
        local_version <- results$local_version[i]
        repos_version <- results$repos_version[i]
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
        installed = utils::installed.packages(lib)
    )
    if (!is.null(updates)) {
        warning("some pakcages were not updated")
        print(updates)
    }
    invisible()
}

# not exported -----

is_base <- function(pkgs, installed = NULL, ...) {
    installed %<>% replace_null(installed_packages(...))
    pkgs %in% rownames(installed)[installed[, "Priority"] %in% "base"]
}

is_installed <- function(pkgs, installed = NULL, ...) {
    installed %<>% replace_null(installed_packages(...))
    pkgs %in% rownames(installed)
}

is_available <- function(pkgs, available = NULL, ...) {
    available %<>% replace_null(available_packages(...))
    pkgs %in% rownames(available)
}

installed_packages <- function(lib = .libPaths()[1], ...) {
    utils::installed.packages(lib = lib, ...)
}

available_packages <- function(repos = getOption("repos"),
                               type = getOption("pkgType"),
                               ...) {
    if(!has_internet()){
        message("no internet connection - cant check available packages")
        return(invisible())
    }
    available <- try({
        utils::available.packages(
            repos = repos,
            type = type,
            ...
        )
    })
    if (inherits(available, "try-error")) {
        message(available)
        return(invisible())
    }
    available
}
