
#' @rdname check_versions
#' @name check_r_version
#' @title Check R version
#' @description Check to see if a later R version is available for download.
#' \itemize{
#'   \item Prints the current R/RStudio version to console
#'   \item Checks to see if a later version is available and, if so, prints a message to
#'   the console
#'   \item Only works in interactive mode
#' }
#' @param check_for_updates if \code{FALSE} then only the current version is printed
#' @importFrom dplyr first
#' @importFrom purrr keep
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove
#' @export
check_r_version <- function(check_for_updates = TRUE) {
    if (interactive()) {
        current_version <- getRversion()
        message("R ", current_version)
        if (check_for_updates) {
            if (!has_internet()) {
                message("no internet connection - cant check for R updates")
            } else {
                sysname <- Sys.info()["sysname"]
                if (!sysname %in% c("Darwin", "Windows")) {
                    message("cant check available R version for ", sysname)
                    return(invisible())
                }
                url <- "https://cran.r-project.org/bin/"
                if (sysname == "Darwin") url <- str_c(url, "macosx")
                if (sysname == "Windows") url <- str_c(url, "windows/base")
                url_dump <- try(suppressWarnings({
                    url %>% readLines(warn = FALSE)
                }), TRUE)
                if (inherits(url_dump, "try-error")) {
                    message(
                        "can't connect to ", url,
                        " (check your internet connection)"
                    )
                } else {
                    pattern <- "R-[0-9]+\\.[0-9]+\\.[0-9]+"
                    available_version <- url_dump %>%
                        keep(str_detect(., pattern)) %>%
                        first() %>%
                        str_extract(pattern) %>%
                        str_remove("R-") %>%
                        numeric_version()
                    if (inherits(available_version, "numeric_version")) {
                        if (current_version < available_version) {
                            message("R-", available_version, " now available")
                        }
                    } else {
                        message(
                            "can't extract current version from ", url,
                            " (try debugging)"
                        )
                    }
                }
            }
        }
    }
    invisible()
}

#' @rdname check_versions
#' @name check_rstudio_version
#' @export
check_rstudio_version <- function(check_for_updates = TRUE) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && interactive()) {
        current_version <- rstudioapi::getVersion()
        message("Using RStudio-", current_version)
        if (check_for_updates) {
            if (!has_internet()) {
                message("no internet connection - cant check for RStudio updates")
            } else {
                sysname <- Sys.info()["sysname"]
                if (!sysname %in% c("Darwin", "Windows")) {
                    message("cant check available RStudio version for ", sysname)
                    return(invisible())
                }
                url <- "https://www.rstudio.com/products/rstudio/download/"
                if (sysname == "Darwin") pattern <- "desktop/macos/RStudio-.+\\.dmg"
                if (sysname == "Windows") pattern <- "desktop/windows/RStudio-.+\\.dmg"
                url_dump <- try(suppressWarnings({
                    url %>% readLines(warn = FALSE)
                }), TRUE)
                if (inherits(url_dump, "try-error")) {
                    message(
                        "can't connect to ", url,
                        " (check your internet connection)"
                    )
                } else {
                    # pattern = "R-[0-9]+\\.[0-9]+\\.[0-9]+"
                    available_version <- url_dump %>%
                        keep(str_detect(., pattern)) %>%
                        first() %>%
                        str_extract(pattern) %>%
                        str_remove("desktop/.+/RStudio-") %>%
                        str_remove("\\.dmg?+$") %>%
                        numeric_version()
                    if (inherits(available_version, "numeric_version")) {
                        if (current_version < available_version) {
                            message("RStudio-", available_version, " now available")
                        }
                    } else {
                        message(
                            "can't extract current version from ", url,
                            " (try debugging)"
                        )
                    }
                }
            }
        }
    }
    invisible()
}
