################################################################
################################################################

# R USER GROUP - RCPP

################################################################
################################################################


# Rcpp stands for "R C plus plus"

# Offers a relatively easy way to use call C++ code inside R

# Written by Dirk Eddelbuettel and Roman Francois

# Potential to speed up your functions 

# - especially if you have loops that you can't vectorize

# - or if you want to call a function lots of times (lower overheads)



# Talk divided into three parts:

# 1. Calling normal C++ code using .Call - a simple example

# 2. Intro to Rcpp - overview + examples

# 3. Putting Rcpp code in a package



# Disclaimer:

# I AM A NOVICE!! (IN BOTH C++ AND RCPP!) 

# so lower your expectations...



################################################################
################################################################

setwd("~/Resources/R/CREEM R user group/Rcpp")

################################################################

# Using .Call

################################################################

# Vecsum - function to calculate the sum of a vector

cat('
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <Rdefines.h>    
SEXP vecSum_cpp(SEXP x){
    SEXP result;
    PROTECT(result = allocVector(REALSXP,1));
    double *input, value = 0;
    input = REAL(x);
    int n = length(x);
    for (int i=0; i<n; i++) value += input[i];
    REAL(result)[0] = value;
    UNPROTECT(1);
    return result;
}', file = "vecSum_cpp.c")

# compiles the c code
system('R CMD SHLIB vecSum_cpp.c') 

# .dll on Windows
# .so on a Mac)
dyn.load('vecSum_cpp.dll')

.Call("vecSum_cpp", x=as.numeric(1:10))

.Call("vecSum_cpp", x=1:10)

vecSum <- function(x) .Call("vecSum_cpp", as.numeric(x))

vecSum(1:10)

sum(1:10)

# it works, but what a pain for such a simple function (took me hours)

# difficulties include:
# - the inputs and outputs need to be coerced into the correct type
# - limited object types / structures




################################################################

# Rcpp - basics

################################################################

# Installation on a Mac - easy, just download the Rcpp package

# Installation on Windows - more of a pain
# - requires Rtools to be installed
# - no spaces allowed in address path for Rtools or R (i.e. can't put in "Program Files")

# See:
# 'how to - set up Rcpp on windows.r'

require(Rcpp)

# Rcpp version of the function above 

cppFunction('
  double vecSum2(NumericVector x){
    int n = x.size();
    double result = 0;
    for(int i=0; i<n; i++) result += x[i];
    return result;
  }'
)

vecSum2(1:10)

# still need to use some C++ syntax:
# - declare the class of the output objects (double in this case) 
# - declare the class(es) of the input object(s) (NumericVector - special Rcpp object class)
# - declare class of iterator in loops
# - element indexes start with 0 instead of 1
# - never use <- for assign
#   result += x[i] equivalent to result = result + x[i]
# - finish each line with a semi-colon
# - curved brackets for indexing matrices

# Extra Rcpp objects classes available - e.g.
# - NumericVector, NumericMatrix
# - IntegerVector, IntegerMatrix
# - LogicalVector, LogicalMatrix
# - Also allows lists and data frames

# Rcpp also has a bunch of vectorizeed functions - can make writing code much easier

# euivalent to the above:

cppFunction('
  double vecSum3(NumericVector x){
    return sum(x);
  }'
)

vecSum3(1:10)

# other built-in functions include dnorm, runif, log, ifelse, etc.




# Performance?

require(rbenchmark)

n <- 10000

benchmark(vecSum(1:n), vecSum2(1:n), vecSum3(1:n), sum(1:n),
          columns=c("test", "replications","elapsed", "relative"), 
          order="relative", replications=1000) # about 10 secs

# Not a good use of Rcpp...





################################################################

# Rcpp - more interesting examples

################################################################

# Calculating the distances between traps and points on a 'mask'

# - used in SECR likelihood where numerical integration performed using a grid of points (the mask)

edge <- seq(-10,10,length=100)
mask <- as.matrix(expand.grid(edge, edge)) ; head(mask) ; dim(mask)

edge <- seq(-3, 3, length=3)
traps <- as.matrix(expand.grid(edge, edge)) ; dim(traps) ; traps

plot(mask, pch=15, cex=0.75, col="grey")
points(traps, pch=19, col=4)



# A simple function in R using loops

distances_r_loops <- function (traps, mask) {
    
    K <- nrow(traps)
    M <- nrow(mask)
    
    distances <- matrix(NA, nr=K, nc=M)
    
    for(k in 1:K){
        
        for(m in 1:M){
            
            distances[k,m] <- sqrt( (traps[k,1]-mask[m,1])^2 + (traps[k,2]-mask[m,2])^2 )
        }
        
    }
    
    return(distances)
    
}

dists <- distances_r_loops(traps, mask) ; dists[,1:10] ; dim(dists)

# rows = traps
# cols = mask points

# plot distances for trap 1
n.colours <- 10
plot(mask, pch=15, cex=0.75, col=colorRampPalette(c("yellow","red"))(n.colours)[cut(dists[1,], n.colours)])
points(traps, pch=19, col=4)



# Another way to write the same function, but avoiding loops (written by Murray Efford)

distances_r_apply <- function (X, Y) {
    onerow <- function (xy) {
        d <- function(xy2) {
            sqrt(sum((xy2 - xy)^2))
        }
        apply(Y, 1, d)
    }
    t(apply(X, 1, onerow))
}

all.equal(dists, distances_r_apply(traps, mask))

benchmark(distances_r_loops(traps, mask),
          distances_r_apply(traps, mask),
          columns=c("test", "replications","elapsed", "relative"), 
          order="relative", replications=1)

# nothing spectacular





# Equivalent Rcpp function

# - looks very similar to the R function with loops

cppFunction('
NumericMatrix distances_rcpp(NumericMatrix traps, NumericMatrix mask){
    int K = traps.nrow();
    int M = mask.nrow();
    NumericMatrix distances(K,M);
    for (int m=0; m<M; m++) {
        for (int k=0; k<K; k++) {
            distances(k,m) = pow( pow( traps(k,0)-mask(m,0), 2 ) + pow( traps(k,1)-mask(m,1), 2 ), 0.5 );
        }
    }
    return distances;
}
')

# Things to noice:
# - need tp use pow() instead of ^
# - the .nrow function 
#   - example of a different kind of function to those you get in R
#   - these start with the name of the variable and take no arguments (e.g. .nrow, .ncol, .size)

all.equal(dists, distances_rcpp(traps, mask))

benchmark(distances_rcpp(traps, mask),
          distances_r_loops(traps, mask),
          columns=c("test", "replications","elapsed", "relative"), 
          order="relative", replications=5) 

# Much better!





# But R already has a function to do this...

require(fields)

?rdist

all.equal(dists, rdist(traps, mask))

benchmark(distances_rcpp(traps, mask), 
          rdist(traps, mask),
          columns=c("test", "replications","elapsed", "relative"), 
          order="relative", replications=100) # about 15 secs

# A modest improvement






# How about calculating the ANGLES between traps and points on a mask

cppFunction('
    NumericMatrix angles_rcpp(NumericMatrix traps, NumericMatrix mask){
    int K = traps.nrow();
	int M = mask.nrow();
	NumericMatrix angles(K,M);
	double opp;
	double adj;
	for (int k=0; k<K; k++) {
		for (int m=0; m<M; m++) {
			opp = mask(m,0)-traps(k,0);
			adj = mask(m,1)-traps(k,1);
			angles(k,m) = atan(opp/adj);
			if(adj<0) angles(k,m) += M_PI;
			if(opp<0 & adj>=0) angles(k,m) += M_2PI;
		}
	}
	return angles;
    }
')

# - you can use 'if' statements quite easily
# - you can use constants - e.g. M_PI for pi and M_2PI for 2*pi
# (I think both of these are C++ features rather than Rcpp)

angs <- angles_rcpp(traps, mask) ; angs[,1:10] ; dim(angs)

plot(mask, pch=15, cex=0.75, col=colorRampPalette(c("yellow","red"))(n.colours)[cut(angs[1,], n.colours)])
points(traps, pch=19, col=4)


# an equivalent R function - avoiding loops (written by Murray Efford)

angles_r <- function (X, Y) {
    onerow <- function (xy) {
        d <- function(xy2) {
            denom=sqrt(sum((xy2-xy)^2))
            if(denom!=0) {
                sintheta=(xy2[1]-xy[1])/denom
                theta=asin(sintheta)
            } else theta=0
            if(xy2[2]< xy[2] & xy2[1]>=xy[1]) theta = theta=pi - theta
            if(xy2[2]< xy[2] & xy2[1]< xy[1]) theta = pi - theta
            if(xy2[2]< xy[2] & xy2[1]==xy[1]) theta = pi
            if(xy2[2]>=xy[2] & xy2[1]< xy[1]) theta = 2*pi + theta
            return(theta)
        }
        apply(Y, 1, d)
    }
    t(apply(X, 1, onerow))
}

all.equal(angs, angles_r(traps, mask))

benchmark(angles_rcpp(traps, mask),
          angles_r(traps, mask),
          columns=c("test", "replications","elapsed", "relative"), 
          order="relative", replications=1) 

# !!!!!!!!!!!!!

# No equivalent function in any packages (as far as know...)

# See:
# 'Rcpp - examples.r'
# 'RcppArmadillo - examples.r'

# Note that these examples use a slightly out of date way of using Rcpp - via the 'cxxfunction' function in the 'inline' package - but the code is very similar (I just haven't got round to changing all the examples into the cppFunction format)



################################################################

# Putting Rcpp in a package

################################################################

package.name <- "NewPackage"

unlink(c(file.path(getwd(), package.name), paste(.libPaths(),"/",package.name,sep="")), recursive=TRUE, force=TRUE)

# Use special function to make the skeleton

Rcpp.package.skeleton(package.name, example_code = FALSE, force = TRUE)

# Differences:

# - src folder
# - Description - depends on Rcpp
# - Namespace - uses dynamic loading for compiled code

# Need to write THREE files
# - if you use example_code = TRUE when making the skeleton it gives you a simple "hello world" example


# 1. a .cpp file

# Need to use SEXP method with .Call
# - more fiddly than the cppFunction function but not as bad as C++ on it's own (e.g. no pointers!)
# - declare the inputs inside the function (similar to using 'inline')

cat('#include "get_angles.h"
using namespace Rcpp ;
SEXP get_angles(SEXP x, SEXP y){
    NumericMatrix TRAPS(x);
    NumericMatrix MASK(y);
    int K = TRAPS.nrow();
	int M = MASK.nrow();
	NumericMatrix ANGLES(K,M);
	double OPP;
	double ADJ;
	for (int k=0; k<K; k++) {
		for (int m=0; m<M; m++) {
			OPP = MASK(m,0)-TRAPS(k,0);
			ADJ = MASK(m,1)-TRAPS(k,1);
			ANGLES(k,m) = atan(OPP/ADJ);
			if(ADJ<0) ANGLES(k,m) += M_PI;
			if(OPP<0 & ADJ>=0) ANGLES(k,m) += M_2PI;
		}
	}
	return ANGLES;
}', file = file.path(package.name, "src/get_angles.cpp"))


# 2. a .r file

# - can Roxygenize
# - see the .Call bit at the bottom - this is where you declare inputs

cat('
#\' Calculate bearings between points
#\'
#\' Function to calculate bearings (in radians) from all points in x to all points in y.
#\' X and Y must use the same coordinate system.
#\' 
#\' Used specifically in this package to calculate bearings from all detectors (x) to all mask points (y).
#\' Function has been optimized using Rcpp.
#\'
#\' @param x a K by 2 matrix of detector coordinates. 
#\' Column one contains x-coordinates, column two contains y-coordinates.
#\' Each row corresponds to a unique detector (K detectors in total).
#\' @param y an M by 2 matrix of mask point coordiantes. 
#\' Column one contains x-coordinates, column two contains y-coordinates
#\' @return Returns a K by M matrix of radians.
#\' @author Darren Kidney
#\' @seealso \\code{\\link{get.distances}}
#\' @examples
#\' X <- matrix(0, nc=2) ; X
#\' Y <- as.matrix(rbind(c(0,1),c(1,1))) ; Y
#\' get.angles(X, Y) # radians
#\' get.angles(X, Y) * 360/(2*pi) # degrees
#\' @export
    
get.angles <- function(x,y){
    .Call( "get_angles", x = as.matrix(x), y = as.matrix(y))
}
', file = file.path(package.name, "r/get.angles.r"), sep="")


# 3 a .h file

# - see also the Rcpp skeleton example

cat("
#ifndef _",package.name,"_get_angles_H
#define _",package.name,"_get_angles_H
#include <Rcpp.h>
RcppExport SEXP get_angles(SEXP x, SEXP y) ;
#endif
", file = file.path(package.name, "src/get_angles.h"), sep="")

# roxygenize
require(roxygen2)
roxygenize(package.name, roclets = c("collate", "rd"))

# build
eval(parse(text = paste("system('Rcmd build ", package.name,"')", sep="")))

# install
require(devtools)
install(package.name)

# load package
eval(parse(text=paste("require(",package.name,")",sep="")))

# check
eval(parse(text=paste("help(package=",package.name,")",sep="")))

# Example
get.angles(traps, mask)[,1:10]

all.equal(angles_rcpp(traps, mask), get.angles(traps, mask))




################################################################
################################################################

