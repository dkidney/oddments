## ------------------------------------------------------------------------------------ ##

#' @title Install uninstalled packages
#' @description Behaves a bit like \link[utils]{install.packages}, but only installs
#'   packages that aren't already installed.
#' @param packages character vector of package names
#' @param ... additional arguments to pass to \link[utils]{install.packages}
#' @return On a Mac: XQuartz might need to be installed for some packages to install
#'   properly (e.g. \pkg{rgl})
#' @details
#' @export
#' @examples
install_packages = function(pkgs, ...){
    already.installed = pkgs %in% rownames(installed.packages())
    if(any(!already.installed)){
        message("installing the following packages:\n\n",
                paste(pkgs[!already.installed], collapse = ", "), "\n\n")
        install.packages(pkgs[!already.installed], ...)
    }else{
        message("packages already installed")
    }
    invisible()
}

## ------------------------------------------------------------------------------------ ##

install_packages(c(
    "AppliedPredictiveModeling",
    "abind",
    "C50",
    "car",
    "caret",
    "CircStats",
    "class",
    "coda",
    "data.table",
    "devtools",
    # "Distance",
    "dplyr",
    # "DSim",
    # "dsm",
    # "ElemStatLearn",
    # "extrafont",
    "fda",
    "fields",
    "foreign",
    "geosphere",
    "ggmap",
    "ggplot2",
    "grid",
    # "ISLR",
    "knitr",
    # "lineprof",
    "lme4",
    "lubridate",
    "maps",
    "maptools",
    "MASS",
    "mclust",
    "mrds",
    "MuMIn",
    "mvtnorm",
    # "neuralnet",
    "nlme",
    # "nnet",
    "party",
    # "pgirmess",
    "plyr",
    "pryr",
    "psych",
    "raster",
    "rbenchmark",
    # "Rcmdr",
    "Rcpp",
    "RcppArmadillo",
    "readr",
    "reshape2",
    "rgdal",
    "rgeos",
    "rgl",
    # "rjags",
    "rmarkdown",
    "roxygen2",
    # "rpanel",
    # "rticles",
    "secr",
    "shiny",
    "sp",
    # "SPACECAP",
    "splancs",
    "stringi",
    "stringr",
    "tcltk2",
    "testthat",
    "tidyr",
    # "tkrplot",
    "xgboost",
    "xtable"
))

## --------------------------------------------------------------------- ##
## caret suggests
## --------------------------------------------------------------------- ##

if(0){

    install_packages(c(
        "BradleyTerry2",
        "e1071",
        "earth",
        "fastICA",
        "ipred",
        "kernlab",
        "klaR",
        "MASS",
        "ellipse",
        "mda",
        "mgcv",
        "mlbench",
        "nnet",
        "party",
        "pls",
        "pROC",
        "proxy",
        "randomForest",
        "RANN",
        "spls",
        "subselect",
        "pamr",
        "superpc",
        "Cubist",
        "testthat"
        # "rPython"
    ))

}

# rmr2 dependencies -------------------------------------------------------

if(0){
    install_packages(
        "functional"
    )
}

## --------------------------------------------------------------------- ##
## --------------------------------------------------------------------- ##