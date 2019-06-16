################################################################################
################################################################################

# R USER GROUP - RCPP II - 27 June 2013

################################################################################
################################################################################

# Recap

# Example function - calculating column products of a matrix

colProds_r1 <- function(x){
    
    prods <- numeric(0)
    
    for(col in 1:ncol(x)){
        
        prods[col] <- prod(x[,col])
        
    }
    
    return(prods)
    
}

colProds_r2 <- function(x){
    
    prods <- numeric(ncol(x))
    
    for(col in 1:ncol(x)){
        
        prods[col] <- prod(x[,col])
        
    }
    
    return(prods)
    
}

colProds_r3 <- function(x) apply(x, 2, prod)

A <- matrix(1:9, 3) ; A

colProds_r1(A)

colProds_r2(A)

colProds_r3(A)

#-------------------------------------------------------------------------------

# Benchmarking

require(rbenchmark)

nrows <- 10000

ncols <- 10

B <- matrix(rnorm(nrows*ncols), nrows, ncols)

benchmark(colProds_r1(B),
          colProds_r2(B),
          colProds_r3(B),
          columns = c("test", "replications", "elapsed", "relative"), 
          order = "relative", 
          replications = 50
)

nrows <- 1000

ncols <- 1000

C <- matrix(rnorm(nrows*ncols), nrows, ncols)

benchmark(colProds_r1(C),
          colProds_r2(C),
          colProds_r3(C),
          columns = c("test", "replications", "elapsed", "relative"), 
          order = "relative", 
          replications = 50
)

#-------------------------------------------------------------------------------

require(Rcpp)

cppFunction('NumericVector colProds_rcpp1(NumericMatrix x){

    NumericVector col_prods(x.ncol()) ;

    col_prods.fill(1) ;

    for (int col = 0 ; col < x.ncol() ; col++) {

        for (int row = 0 ; row < x.nrow() ; row++) {

            col_prods(col) *= x(row,col) ;

        }

    }

    return col_prods ;

}')

colProds_rcpp1(A)

benchmark(colProds_r1(B),
          colProds_r2(B),
          colProds_r3(B),
          colProds_rcpp1(B),
          columns = c("test", "replications", "elapsed", "relative"), 
          order = "relative", 
          replications = 50
)

benchmark(colProds_r1(C),
          colProds_r2(C),
          colProds_r3(C),
          colProds_rcpp1(C),
          columns = c("test", "replications", "elapsed", "relative"), 
          order = "relative", 
          replications = 50
)

################################################################################

# Using sourceCpp()

setwd("~/Resources/R/CREEM R user group/Rcpp")

sourceCpp("colProds_rcpp2.cpp")

colProds_rcpp2(A) # look at the syntax highlighting

colProds_rcpp3(A) # press the source button

# show example error message

################################################################################

# making a package that includes Rcpp functions

setwd("~/Resources/R/CREEM R user group/Rcpp")

unlink(c(file.path(getwd(), "aPackage"), file.path(.libPaths(), "aPackage")), recursive=TRUE, force=TRUE)

colProds_rcpp4(matrix(1:9, 3))

# ?Rcpp.package.skeleton
require(Rcpp)
Rcpp.package.skeleton(name = "aPackage",
                      path = getwd(),
                      force = TRUE,
                      cpp_files = "colProds_rcpp4.cpp",
                      example_code = FALSE)

# Look at DESCRIPTION, Depends, LinkingTo
# Look at NAMESPACE, 
# Look in src
# Look at r/RcppExports

require(roxygen2)

if(1){

    # don't update description or namespace 
    roxygenize(file.path(getwd(), "aPackage"), roclets = "rd")

}else{
    
    roxygenize(file.path(getwd(), "aPackage"))
    namespace <- readLines(file.path(getwd(), "aPackage", "NAMESPACE")) ; namespace
    namespace <- c("useDynLib(aPackage)", namespace) ; namespace
    namespace[length(namespace) + 1] <- paste("export(colProds_rcpp4)") ; namespace
    cat(paste(namespace,"\n"), sep="", file = file.path(file.path(getwd(), "aPackage", "NAMESPACE")))

}

# build package
system('R CMD INSTALL --build aPackage')

require(aPackage)

help(package='aPackage')

colProds_rcpp4(matrix(1:9, 3))

################################################################################

# Rcpp functions that use other Rcpp functions

timesfour(1)

################################################################################

# making a package that includes Rcpp functions that use other Rcpp functions

rm("timesfour")

Rcpp.package.skeleton(name = "anewPackage",
                      path = getwd(),
                      force = TRUE,
                      cpp_files = "timesfour.cpp",
                      example_code = FALSE)

roxygenize(file.path(getwd(), "anewPackage"), roclets = "rd")

# put the .h files in the src folder
cat(paste(readLines("timestwo.h"),"\n"), file = "anewPackage/src/timestwo.h")

system('R CMD INSTALL --build anewPackage')

require(anewPackage)

timesfour(1)

################################################################################

# making a package that includes RcppArmadillo functions

setwd("~/Resources/R/CREEM R user group/Rcpp")

require(RcppArmadillo)

sourceCpp("colProds_arma.cpp")

rm("colProds_arma")

RcppArmadillo.package.skeleton(name = "anotherPackage",
                               path = getwd(),
                               force = TRUE,
                               example_code = FALSE)

# put the .cpp files in the src folder
cat(paste(readLines("colProds_arma.cpp"),"\n"), file = "anotherPackage/src/colProds_arma.cpp", sep="")

compileAttributes(file.path(getwd(), "anotherPackage"))

require(roxygen2)
roxygenize(file.path(getwd(), "anotherPackage"), roclets = "rd")

system('R CMD INSTALL --build anotherPackage')

require(anotherPackage)

colProds_arma(matrix(1:9, nr=3))

################################################################################
################################################################################
