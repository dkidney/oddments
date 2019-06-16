
// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h> 
using namespace Rcpp ;

//' Calculate column products of a matrix. 
//' 
//' details
//' 
//' @param x a matrix
//' @return Returns a vector of column products. Only works for input matrices with no missing values. 
//' @examples
//' # Example 1: usage
//' A <- matrix(1:9, 3) ; A
//' colProds_rcpp4(A)
//' 
//' # Example 2: relative speed
//' require(rbenchmark)
//' B <- matrix(rnorm(1000000), nr = 10) ; dim(B)
//' benchmark(colProds_rcpp4(B),
//'           apply(B,2,prod),
//'           replications = 1,
//'           order = 'relative',
//'           columns = c('test', 'replications', 'elapsed', 'relative'))
//' 
// [[Rcpp::export]]
arma::rowvec colProds_arma2(NumericMatrix x){
    arma::mat X = arma::mat(x.begin(), x.nrow(), x.ncol(), false) ; 
    arma::rowvec col_prods = prod(X,0) ;
    return col_prods ; 
}

/*** R 

colProds_arma2(matrix(1:9, nc=3))

if(0){
    
nrows <- 100

ncols <- 100000

A <- matrix(rnorm(nrows*ncols), nrows, ncols)

benchmark(colProds_arma(A),
          colProds_arma2(A),
          columns = c("test", "replications", "elapsed", "relative"), 
          order = "relative", 
          replications = 100
)

}

*/