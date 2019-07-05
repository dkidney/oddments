\dontrun{

library(oddments)

pkgs = c(
    "abcd",
    "base",
    "devtools",
    "furrr",
    "knitr",
    "rmarkdown",
    "stats",
    "tools"
)

pkgs %>% pkg_status()

pkgs %>% pkg_deps()

pkgs %>% pkg_revdeps()

pkgs %>% pkg_updates()

pkgs %>% pkg_upgrade()
}