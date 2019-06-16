##################################################################################
##################################################################################

# How to set up Rcpp on Windows 7

##################################################################################
##################################################################################

# install the latest version of R
#   - install to a path without spaces - e.g. "C:\R\R-2.15.2"
# install the latest version of Rtools ("http://cran.r-project.org/bin/windows/Rtools/")
#   - install to a path without spaces - e.g. "C:\R\Rtools"
#   - allow the installation process to change the 'Path' settings for environment variables
#   - if it doesn't do the above step automatically, modify the 'Path' settings for Rtools manually
#       - go to the Control Panel and click on System
#       - click on Advanced System Settings
#       - in the Advanced tab click on Environment Variables.
#       - in the System Variables box scroll down until you get to Path then click on it and press Edit.
#       - in the Variable value: box go to the front of the line and insert the following:
#           - c:\R\Rtools\bin;c:\R\Rtools\gcc-4.6.3\bin;
#       - then click OK
#       - restart your computer


# check everything works by trying the following code in R
# install the latest version of Rcpp
require(Rcpp)
require(inline)
src <- '
NumericMatrix mat(X);
int nrows = mat.nrow();
int ncols = mat.ncol();
NumericVector prods(ncols);
for (int col=0; col<ncols; col++) {
    prods(col) = 1;
    for (int row=0; row<nrows; row++) {
        prods(col) *= mat(row,col);
    }
}
return wrap(prods);
'
colProds.cpp <- cxxfunction(signature(X="integer"), body=src, plugin="Rcpp")
X <- matrix(1:9, nc=3)
apply(X,2,prod)
colProds.cpp(X)

require(rbenchmark)
X <- matrix(runif(500000), nr=2)
benchmark(apply(X,2,prod), colProds.cpp(X),
          columns=c("test", "replications","elapsed", "relative"),
          order="relative", replications=10)

# Suppress the cygwin warning message
#   - Navigate back to the Environment Variables. window
#   - Under the System variables box click on New.
#   - Under Variable name: type CYGWIN
#   - Under Variable value: type nodosfilewarning
#   - Click OK
#   - Restart your computer



