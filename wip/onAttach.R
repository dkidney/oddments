
.onAttach = function(libname, pkgname){
    packageStartupMessage(
        rep("-", getOption("width")), "\n",
        "This is oddments version ", utils::packageVersion("oddments"), "\n",
        rep("-", getOption("width"))
    )
}
