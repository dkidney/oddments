
#include <Rcpp.h> 
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
NumericVector colProds_rcpp4(NumericMatrix x){

    NumericVector col_prods(x.ncol()) ;

    col_prods.fill(1) ;

    for (int col = 0 ; col < x.ncol() ; col++) {

        for (int row = 0 ; row < x.nrow() ; row++) {

            col_prods(col) *= x(row,col) ;

        }

    }

    return col_prods ;

}

/*** R 

colProds_rcpp4(matrix(1:9, nc=3))

*/