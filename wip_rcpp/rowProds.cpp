////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// rowProds

////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h> 
using namespace Rcpp ;

//' Calculate row products of a matrix. 
//' 
//' Written using RcppArmadillo
//' 
//' @param x a matrix
//' @return Returns a column vector of products. Only works for input matrices with no missing values. 
//' @export
//' @author Darren Kidney
//' @seealso \code{\link{colProds}}
//' @examples
//' # Example 1: usage
//' A <- matrix(1:9, 3) ; A
//' rowProds(A)
//' 
//' # Example 2: relative speed
//' require(colProdsRcpp)
//' A <- matrix(rnorm(10000000), nr = 10) ; dim(A)
//' benchmark(rowProds(A),
//'           apply(A,1,prod),
//'           replications = 1,
//'           order = 'relative',
//'           columns = c('test', 'replications', 'elapsed', 'relative'))
// [[Rcpp::export]]
arma::colvec rowProds(NumericMatrix x){
    arma::mat X = arma::mat(x.begin(), x.nrow(), x.ncol(), false) ; 
    arma::colvec row_prods = prod(X,1) ;
    return row_prods ; 
}
////////////////////////////////////////////////////////////////////////////////
