
.onAttach = function(libname, pkgname){
    packageStartupMessage(
        rep("\u2500", options("width")), "\n",
        "pip version ", utils::packageVersion("pip"), "\n",
        # "\u2022 for help, type: ?pips\n",
        # "\u2022 for links to vignettes, type: browseVignettes(package = \"pips\")\n",
        rep("\u2500", options("width"))
    )
}

