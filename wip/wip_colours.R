
.onAttach = function(libname, pkgname){
    packageStartupMessage(
        rep("\u2500", options("width")), "\n",
        "oddments version ", utils::packageVersion("oddments"), "\n",
        "\u2022 for help, type: ?oddments\n",
        "\u2022 for links to vignettes, type: browseVignettes(package = \"oddments\")\n",
        rep("\u2500", options("width"))
    )
    options(oddments_verbose = TRUE)
    options(oddments_heading_col = "lawngreen")
    options(oddments_heading_char = "\u2500")
    options(oddments_bullet_col = "deepskyblue")
    options(oddments_bullet_char = "\u2022")
    options(oddments_item_col = "grey85")
    options(oddments_item_char = "\u2500")
    options(oddments_warn_col = "gold")
    options(oddments_error_col = "red1")
}

