
// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h> 
using namespace Rcpp ;

//' @title Square root of a matrix
//' @description TODO
//' @details TODO
//' @param x a square matrix
//' @return Returns a matrix of the same dimensions as x
//' @author Darren Kidney \email{darrenkidney@@googlemail.com}
//' @examples
//' A = matrix(c(2,-1,0,-1,2,-1,0,-1,2), nc=3) ; A
//' A.inv.sqrt = inv_sqrt_mat(A) ; A.inv.sqrt
//' # checking
//' solve(chol(A))
// [[Rcpp::export]]

arma::mat inv_sqrt_mat(const NumericMatrix& x){
    // check x is square    
    if(x.nrow() != x.ncol()) stop("Matrix is not square") ;
    // convert x to an arma mat
//    arma::mat X(x.begin(), x.nrow(), x.nrow(), false) ; 
    arma::mat X = as<arma::mat>(x) ; 
    // calculate inverse square root
    arma::mat inv_sqrt_mat = inv(chol(X)) ;
    return inv_sqrt_mat ;
}
